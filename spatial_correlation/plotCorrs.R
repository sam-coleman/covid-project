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
  ) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1))

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
  ggplot(mapping = aes(x = max_lag)) +
  geom_histogram(binwidth = 1)

df_2 %>% 
  summarize(median = median(max_lag))

View(df_2)

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


