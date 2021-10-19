library(tidyverse)
root <- "C:/dev/git/covid-project/"
load(paste0(root, "data/cases_mandate_new_york_fixed.RData"))

mandate_cases_state %>%
  ggplot(mapping = aes(x = days_with_mandate, y = cases_per100k)) +
  geom_point() + 
  geom_smooth(method = "lm")

mylm <- mandate_cases_state %>% 
  lm(cases_per100k ~ days_with_mandate, .)
print(mylm)
inter <- mylm$coefficients[[1]]
slope <- mylm$coefficients[[2]]