library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)
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

ggraph(graph = layout) + 
  geom_edge_link0() + 
  geom_node_text(aes(label = name), color = 'blue', size = 3) +
  geom_node_point(colour = 'forestgreen')

length(E(whigsGraph))
length(V(whigsGraph))

# ggraph(whigsGraph, 'igraph', algorithm = 'kk') + 
#   geom_edge_link0(aes(width = weight), edge_alpha = 0.1) + 
#   geom_node_point(aes(size = degree), colour = 'forestgreen') + 
#   geom_node_text(aes(label = name, filter = degree > 150), color = 'white', 
#     size = 3)

# layoutdf <- data.frame(
#   x = 1:48, 
#   y = (1:48)^2
#   # name = df_sorted_2 %>% 
#   #   distinct(state) %>% 
#   #   full_join(distinct(df_sorted_2 %>% distinct(neighbor)), by = c("state" = "neighbor")) %>% 
#   #   pull(state)
# )