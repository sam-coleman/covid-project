library(tidyverse)
library(igraph)
library(ggraph)
load("C:/dev/git/covid-project/spatial_correlation/covid_flows_sorted.RData")

df_sorted_2 <- 
  df_sorted %>% 
  group_by(state, neighbor) %>% 
  mutate(
    directional_flow = if_else(switched, -max_lag, max_lag)
  ) %>% 
  summarize(flow = sum(directional_flow)) %>% 
  ungroup()

whigsGraph <- 
  graph_from_edgelist(
    df_sorted_2 %>% 
      select(state, neighbor) %>% 
      data.matrix()
  )

whigsGraph <- graph_from_adjacency_matrix(whigs %*% t(whigs), mode = 'upper', 
  weighted = TRUE, diag = FALSE)

V(whigsGraph)$degree <- degree(whigsGraph)

ggraph(whigsGraph, 'igraph', algorithm = 'kk') + 
  geom_edge_link0() + 
  geom_node_point(colour = 'forestgreen')


# ggraph(whigsGraph, 'igraph', algorithm = 'kk') + 
#   geom_edge_link0(aes(width = weight), edge_alpha = 0.1) + 
#   geom_node_point(aes(size = degree), colour = 'forestgreen') + 
#   geom_node_text(aes(label = name, filter = degree > 150), color = 'white', 
#     size = 3)

