library(tidyverse)
root <- "C:/Users/iserrato/Documents/5th Semester/Data Science/covid-project/"
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
  facet_grid(cases_or_deaths ~ ., scales = "free_y", 
             labeller = as_labeller(c(`cases_per100k` = "Cases per 100,000", `deaths_per100k` = "Deaths per 100,000"))) +
  theme_test() +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    panel.border = element_rect(fill = NA, size = 1),
    # panel.background = element_rect(fill = "grey90"),
    panel.grid.major = element_line(color = "grey50"),
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    strip.text.y = element_text(size = 14),
    
  ) +
  labs(
    x = "Cummulative percentage of people under mask mandate",
    y = "",
    title = "Cases vary with mask mandates for every state, deaths not so much",
    subtitle = "Data is from Jan 21, 2020 to Oct 17, 2021"
  ) 
# ggsave(
#   "./images/.png", 
#   width = 10, 
#   height = 6,
#   bg = "white"
# )


mylm <- us_3 %>% 
  lm(cases_per100k ~ days_with_mandate, .)
print(mylm)
inter <- mylm$coefficients[[1]]
slope <- mylm$coefficients[[2]]

us_3 %>%
  na.omit() %>% 
  select(days_with_mandate, cases_per100k) %>% 
  cor()

