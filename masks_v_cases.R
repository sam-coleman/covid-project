library(tidyverse)
root <- "C:/dev/git/covid-project/"
load(paste0(root, "data/cases_mandate_new_york_fixed.RData"))
load(paste0(root, "data/mask_deaths"))

mandate_cases_state %>%
  ggplot(mapping = aes(x = days_with_mandate, y = cases_per100k)) +
  geom_point() + 
  geom_smooth(method = "lm")

mylm <- mandate_cases_state %>% 
  lm(cases_per100k ~ days_with_mandate, .)
print(mylm)
inter <- mylm$coefficients[[1]]
slope <- mylm$coefficients[[2]]

x <-  mandate_cases_state %>%
  na.omit() %>% 
  select(days_with_mandate, cases_per100k) %>% 
  cor()

us_3 %>%
  distinct(region, .keep_all = TRUE) %>% 
  pivot_longer(
    cols = c(cases_per100k, deaths_per100k),
    names_to = "cases_or_deaths",
    values_to = "per100k"
  ) %>% 
  ggplot(mapping = aes(x = days_with_mandate, y = per100k, color = cases_or_deaths)) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_grid(cases_or_deaths ~ ., scales = "free_y")


mylm <- us_3 %>% 
  lm(cases_per100k ~ days_with_mandate, .)
print(mylm)
inter <- mylm$coefficients[[1]]
slope <- mylm$coefficients[[2]]

us_3 %>%
  na.omit() %>% 
  select(days_with_mandate, cases_per100k) %>% 
  cor()

