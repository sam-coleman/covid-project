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
      str_sub(1, -8) %>%
      str_remove_all("[\\W]")
  )


us_counties <- map_data("county") %>% 
  distinct(subregion) %>% 
  mutate(
    subregion = subregion %>% 
      str_remove_all("\\W")
  )
head(counties, 1)
length(us_counties$subregion)

ms_counties <- mask_county_2 %>% 
  distinct(County) %>% 
  mutate(
    county = tolower(County) %>% 
      str_sub(1, -8) %>% 
      str_remove_all("[\W]"), 
    .keep = "unused"
  )
head(ms_counties, 5)
length(ms_counties$county)

df <- us_counties %>% filter(substr(subregion, 1, 1) == 's')
df_2 <- ms_counties %>% filter(substr(county, 1, 1) == 's')


us_1 <- map_data("county") %>% 
  mutate(
    match_county = subregion %>% 
      str_remove_all("\\W")
  ) 
mask_county_2_1 <- 
mask_county_2 %>% 
  group_by(State, match_county) %>% 
  summarize(
    county_days_with_mandate = 
      if_else(
        all(isNA), 
        NA_real_, # I know this isn't in the style guide, but this line just had to be indented
        mean(masks)
      )
  ) %>% 
  select(state = State, match_county, county_days_with_mandate)

mask_county_2_1a <- mask_county_2_1 %>% filter(state < "mississippi")
mask_county_2_1b <- mask_county_2_1 %>% filter(state >= "mississippi")

us_1a <- us_1 %>% 
  left_join(
    mask_county_2_1a, 
    by = c("match_county" = "match_county", "region" = "state")
  )
us_1b <- us_1 %>% 
  left_join(
    mask_county_2_1b, 
    by = c("match_county" = "match_county", "region" = "state")
  )

us <- 
  bind_rows(us_1a, us_1b)

g <- 
  ggplot(us) + 
  geom_map_interactive(
    data = us, 
    mapping = aes(
      map_id = match_county, 
      fill = county_days_with_mandate, 
      group = region, 
      tooltip = sprintf(
        "%s<br/>%s%%", 
        toTitleCase(subregion), 
        county_days_with_mandate * 100
      )
    ), 
    map = us, 
    # fill = "transparent", 
    color = "black"
  ) + 
  expand_limits(x = us$long, y = us$lat) + 
  coord_map() + 
  viridis::scale_fill_viridis() + 
  labs(
    x = "Long.", 
    y = "Lat.",
    fill = "Mask Mandate\nPercentage", 
    title = "Cumulative Mask Mandates", 
    subtitle = "Aggregated for each county in the state"
  )

w <- widgetframe::frameWidget(girafe(code=print(g)))
w

