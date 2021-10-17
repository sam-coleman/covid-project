library(tidyverse)
load("C:/dev/git/covid-project/spatial_correlation/stateCorrs_different_lags.RData")
# load("covid2/stateCorrs_100days.RData")

stateCorrs %>%
  ggplot(mapping = aes(x = neighbor, y = corr, color = lag)) +
  geom_jitter(
    data = . %>% filter(state == "oregon", neighbor == "washington"), 
    width = 0.25, 
    height = 0
  ) + 
  geom_jitter(
    data = . %>% filter(state == "washington", neighbor == "oregon"), 
    width = 0.25, 
    height = 0
  ) + 
  labs(
    x = "State", 
    y = "corr", 
    title = "Oregon"
  )

###################
###################

df_2 <- 
  stateCorrs %>% 
  group_by(state, neighbor) %>% 
  filter(corr == max(corr)) %>% 
  ungroup() %>% 
  rename(max_lag = lag)

df_2 %>%
  ggplot(mapping = aes(x = max_lag, y = corr)) +
  geom_point()

df_2 %>%
  group_by(state, neighbor) %>% 
  summarize() %>% 
  ggplot(mapping = aes(x = x)) +
  geom_point()

df_2 %>%
  ggplot(mapping = aes(x = max_lag)) +
  geom_histogram(binwidth = 1)

df_2 %>% 
  summarize(median = median(max_lag)) %>% 
  print()

df_sorted <- 
  df_2 %>% 
  mutate(
    switched = state > neighbor, 
    # if switched is TRUE, it's [neighbor, state], else it's [state, neighbor]
    tmp_state    = if_else(switched, neighbor, state), 
    tmp_neighbor = if_else(switched, state, neighbor), 
    # now replace with the switched values
    state = tmp_state, 
    neighbor = tmp_neighbor
  ) %>% 
  select(!c("tmp_state", "tmp_neighbor")) %>% 
  arrange(state, neighbor)

df_sorted_2 <- 
  df_sorted %>% 
  group_by(state, neighbor) %>% 
  mutate(
    directional_flow = if_else(switched, -max_lag, max_lag), 
    directional_corr = if_else(switched, -corr, corr)
  ) %>% 
  summarize(
    flow = sum(directional_flow), 
    corr = sum(directional_corr), 
  ) %>% 
  ungroup()

df_sorted_2 %>%
  ggplot(mapping = aes(x = flow, y = corr)) +
  geom_point(aes(color = state)) + 
  geom_smooth(method = "lm")

save(
  df_sorted, 
  file = "C:/dev/git/covid-project/spatial_correlation/covid_flows_sorted.RData"
)
