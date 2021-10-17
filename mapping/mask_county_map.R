library(tidyverse)
library(ggmap)
library(ggiraph)
library(tools)
library(stringr)
load("C:/dev/git/covid-project/data/mask_county.RData")

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
  ) %>% 
  mutate(
    match_county = tolower(County) %>% 
    match_county = paste0(match_county, State) %>% 
      str_sub(1, -8) %>%
      str_remove_all("[\\W]"), 
  )

us_1 <- map_data("county") %>% 
  mutate(
    match_county = tolower(subregion) %>% 
      paste0(region) %>% 
      str_remove_all("\\W")
  )

mask_county_2_1 <- 
mask_county_2 %>% 
  group_by(match_county, State) %>% 
  summarize(
    county_days_with_mandate = 
      if_else(
        all(isNA), 
        NA_real_, # I know this isn't in the style guide, but this line just had to be indented
        mean(masks)
      )
  ) %>% 
  select(state = State, match_county, county_days_with_mandate)

us <- us_1 %>% 
  left_join(
  mask_county_2_1, 
  by = c("match_county" = "match_county"), 
  # keep = TRUE
) %>% 
  # select(long, lat, group, order, region, match_county, county_days_with_mandate) %>% 
  filter(region <= "arkansas", match_county < "carroll")
# us[1, 4] <- "bbbbbbb"
us[1, 7] <- "bbbbbbb"

g <- 
  ggplot(us) + 
  geom_map_interactive(
    mapping = aes(
      map_id = region, 
      group = order, 
      fill = match_county, 
      tooltip = match_county
        # sprintf(
        # "%s<br/>%s%%", 
        # # toTitleCase(subregion), 
        # match_county, 
        # county_days_with_mandate
      ), 
    map = us, 
    color = "black"
  ) + 
  expand_limits(x = us$long, y = us$lat) + 
  coord_map() + 
  # viridis::scale_fill_viridis() + 
  labs(
    x = "Long.", 
    y = "Lat.",
    fill = "Mask Mandate\nPercentage", 
    title = "Cumulative Mask Mandates", 
    subtitle = "Aggregated for each county in the state"
  )
w <- widgetframe::frameWidget(girafe(code=print(g)))
w

