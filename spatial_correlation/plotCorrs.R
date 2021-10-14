library(tidyverse)
load("stateCorrs_different_lags.RData")
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
  rename(max_lag = lag)

df_2 %>%
  ggplot(mapping = aes(x = max_lag, y = corr)) +
  geom_point()

df_2 %>%
  ggplot(mapping = aes(x = max_lag)) +
  geom_histogram(binwidth = 1)

df_2 %>% 
  ungroup() %>% 
  summarize(median = median(max_lag))
