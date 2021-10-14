library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)
library(ggmap)
load("C:/dev/git/covid-project/spatial_correlation/covid_flows_sorted.RData")

geo_centers <- 
  read.csv(
  "C:/dev/git/covid-project/spatial_correlation/GeographicCenters.csv", 
  skip = 0
) %>% 
  select(!state) %>%  # get rid of the 2-letter abbreviation
  mutate(state = name %>% tolower()) %>% 
  select(state, latitude, longitude)

df_sorted_2 <- 
  df_sorted %>% 
  group_by(state, neighbor) %>% 
  mutate(
    directional_flow = if_else(switched, -max_lag, max_lag)
  ) %>% 
  summarize(flow = sum(directional_flow)) %>% 
  ungroup()

all_states <- 
  full_join(
    df_sorted %>% 
      select(state), 
    df_sorted %>% 
      select(neighbor), 
    by = c("state" = "neighbor")
  ) %>% 
  unique()

# difference between df_sorted and geo_centers:
# geo_centers contains alaska, hawaii, puerto rico, and DC
anti_join(
  geo_centers, 
  all_states, 
  by = "state"
)

geo_states <- geo_centers %>% 
  right_join(all_states, by = c("state" = "state")) %>% 
  arrange(state) %>% 
  rename(x = longitude, y = latitude)

## the locations of the states are correct
geo_states %>%
  ggplot(mapping = aes(x = x, y = y, label = state)) +
  geom_text()


######################################################################


mygraph <- 
  graph_from_data_frame(
    df_sorted_2 %>% arrange(state, neighbor), 
    directed = TRUE
  )

v <- get.data.frame(mygraph, what = "vertices")

geo_states_ordered <- 
  v %>% 
  left_join(geo_states, by = c("name" = "state"))

layout <- create_layout(mygraph, geo_states_ordered %>% select(!name))


######################################################################

g <- 
  ggraph(graph = layout) + 
  # geom_edge_link0() + 
  # geom_node_text(aes(label = name), color = 'blue', size = 3) + 
  # geom_node_point(colour = 'forestgreen') + 
  # geom_edge_link() + 
  geom_edge_link(
    aes(
      start_cap = label_rect(node1.name),
      end_cap = label_rect(node2.name)
    ), 
    arrow = arrow(length = unit(4, 'mm'))
  ) + 
  geom_node_text(
    aes(
      label = name
    ), 
    size = 3
  )
g

us <- map_data("state")

g + geom_map(data = us, aes(map_id = region), map = us, fill = "transparent", color = "black") +
  expand_limits(x = us$long, y = us$lat)


ggplot(us) + 
  geom_map(aes(map_id = region), map = us, fill = "transparent", color = "black") + 
  expand_limits(x = us$long, y = us$lat)
