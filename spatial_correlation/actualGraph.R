library(tidyverse)
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

whigsGraph <- 
  graph_from_data_frame(
    df_sorted_2, 
    directed = TRUE
  )

layoutdf <- data.frame(
  x = 1:48, 
  y = (1:48)^2
  # name = df_sorted_2 %>% 
  #   distinct(state) %>% 
  #   full_join(distinct(df_sorted_2 %>% distinct(neighbor)), by = c("state" = "neighbor")) %>% 
  #   pull(state)
  )

X <- 1:48
Y <- (1:48)^2

layout <- create_layout(whigsGraph, layoutdf, FALSE)

V(whigsGraph)$degree <- degree(whigsGraph)

ggraph(layout = layout) + 
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

