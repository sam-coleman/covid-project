library(tidyverse)
library(ggmap)
library(ggiraph)
library(tools)
library(cowplot)
load("C:/dev/git/covid-project/data/mask_county.RData")
# load("C:/dev/git/covid-project/data/normalized_population.RData")
load("C:/dev/git/covid-project/data/covid_and_pop.RData")

state_abbrev <- 
  read.csv("C:/dev/git/covid-project/data/stateShortToLong.csv") %>% 
  rename(State = ï..State)

mask_county_2 <- 
  mask_county %>% 
  rename(masks = Face_Masks_Required_in_Public) %>% 
# the variable masks is currently "Yes", "No", or NA.
# we will treat NA as "No" and convert to logical
  mutate(
    isNA = is.na(masks), 
    masks = if_else(is.na(masks), "No", masks), 
    masks = masks == "Yes"
  ) %>% 
# now add the state names based on abbreviations
  left_join(state_abbrev, by = c("State" = "Code")) %>% 
  mutate(
    state_abbr = State, 
    State = tolower(State.y), 
    .keep = "unused"
  )

# mask_covid_pop <- 
#   mask_county_2 %>% 
#   mutate(date = as.POSIXct())
#   left_join(df_data, by = c("fips" = "fips", ))

df_pop_2 <- 
  df_data %>% 
  group_by(fips, population) %>% 
  summarize() %>% # collapse all the different dates
  ungroup()

us_0 <- 
  mask_county_2 %>% 
  left_join(df_pop_2, by = c("fips" = "fips")) %>% 
  # first summarize over all dates
  group_by(State, County, population) %>% 
  summarize(            
    days_with_mandate = 
      if_else(
        all(isNA), 
        NA_real_, 
        mean(masks) * 100
      )
  ) %>% 
  # then aggregate, weighting by population
  mutate(
    weighted_days_with_mandate = days_with_mandate * population
  ) %>% 
  group_by(State) %>% 
  summarize(
    days_with_mandate = 
      sum(weighted_days_with_mandate, na.rm = TRUE) / 
        sum(population, na.rm = TRUE)
  ) %>% 
  mutate(
    days_with_mandate = 
      if_else(
        days_with_mandate == 0, 
        NA_real_, 
        days_with_mandate
      )
  ) %>% 
  rename(state = State)

us_1 <- 
  df_data %>% 
  # first summarize over all dates
  group_by(state, county, population) %>% 
  summarize(
    cases = max(cases)
  ) %>% 
  group_by(state) %>% 
  summarize(
    cases_per100k = sum(cases) / sum(population, na.rm = TRUE) * 1e5
  ) %>% 
  mutate(state = tolower(state), .keep = "unused")

us_2 <- 
  us_0 %>% 
    left_join(
      us_1, 
      by = c("state" = "state")
    )

us_3 <- map_data("state") %>% 
  left_join(
    us_2, 
    by = c("region" = "state")
  )
    
  
us <- map_data("state") %>% 
  left_join(
    mask_county_2 %>% 
      group_by(State) %>% 
      summarize(
        days_with_mandate = 
          if_else(
            all(isNA), 
            NA_real_, 
            mean(masks) * 100
          )
      ), 
    df_2, 
    by = c("region" = "State")
  )

us_2 <- us %>% 
  left_join(
    df_normalized %>% 
      mutate(state = tolower(state)) %>% 
      filter(date == max(date)) %>% 
      group_by(state) %>% 
      summarize(
        deaths_per100k = mean(deaths_per100k * population, na.rm = TRUE), 
        cases_per100k = mean(cases_per100k * population, na.rm = TRUE)
      ), 
    df_2, 
    by = c("region" = "state")
  )

make_imap <- function(fill_var, viridis_scale, fill_name) {
  g_2 <- 
    ggplot(us_2, mapping = aes_string(fill = fill_var)) + 
    geom_map_interactive(
      data = us_2, 
      mapping = aes(
        map_id = region, 
        # fill = fill_var, # cases_per100k, 
        tooltip = sprintf(
          "%s<br/>days with mandate: %s%%<br/>cumulative cases: %s", 
          toTitleCase(region), 
          round(county_days_with_mandate, digits = 1), 
          prettyNum(floor(cases_per100k), big.mark = ",")
        ), 
        data_id = region
      ), 
      map = us_2, 
      # fill = "transparent", 
      color = "black"
    ) + 
    expand_limits(x = us_2$long, y = us_2$lat) + 
    coord_map() + 
    viridis::scale_fill_viridis(option = viridis_scale) + 
    labs(
      x = "Long.", 
      y = "Lat.",
      fill = fill_name, # "Cases per 100k", 
      title = "Percent Days With Mask Mandates", 
      subtitle = "Grey = No Data"
    )
}

w <- widgetframe::frameWidget(girafe(code=print(
    # make_imap(
    #   "county_days_with_mandate", 
    #   "mako", 
    #   "Percent Days\nwith Mandate")
    make_imap(
      "cases_per100k",
      "rocket", 
      "Cases per 100k")
  )))
w

p1 <- make_imap("days_with_mandate", "mako")
p2 <- make_imap("cases_per100k", "rocket")

girafe(
  ggobj = plot_grid(
    p1, 
    p2
  ), # , ncol = 1
  width_svg = 12, 
  height_svg = 8
)

