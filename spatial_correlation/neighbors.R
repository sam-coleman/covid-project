library(tidyverse)
load("covid2/covid.RData")

# initialize df_covid
df_covid <- 
  df_covid %>% 
  mutate(
    state = state %>% tolower, 
    county = county %>% tolower
  )

neighbors <- 
  read.csv("covid2/NeighboringStates.csv") %>% 
  mutate(
    state = State.Name %>% tolower(), 
    borders = Bordering.States %>% 
      tolower() %>% 
      str_split(", "), 
    .keep = "none"
  ) %>% 
  filter(
    borders != "none", 
    !str_detect(borders, "water border")
  )

# process one State's data into new cases per day, 
# summarizing by state, with a lag of nDiff = 1
processState <- function(state1, nDiff) {
  return <- 
    df_covid %>% 
    filter(state == state1) %>% 
    group_by(date, state) %>% 
    mutate(cases = sum(cases)) %>% 
    ungroup() %>% 
    distinct(date, state, .keep_all = TRUE) %>% 
    select(date, cases) %>% 
    mutate(
      cases = c(rep(NA, nDiff), diff(cases, lag = nDiff))
    ) %>% 
    filter(!weekdays(date) %in% c("Saturday", "Sunday"))
}

# computes and returns the correlation between
# two States' covid trends over time 
# (neglecting the date of observations)
corrStates <- function(state1, state2, lagDays) {
  print(paste(state1, state2, lagDays, sep = " + "))
  return <- 
    processState(state1, lagDays) %>% 
    inner_join(
      processState(state2, 1), 
      by = c("date" = "date")
    ) %>% 
    # print()
    # print() %>% 
    select(!date) %>%
    cor(use = "complete.obs") %>%
    .[2, 1] # select the 2, 1th element (the r value)
}

corrDiffs <- function(state1, state2) {
  corrTibble <- 
    tibble(
      state = character(), 
      neighbor = character(), 
      corr = numeric(), 
      lag = numeric()
    )
  for (lagDays in 1:100) {
    corr <- corrStates(state1, state2, lagDays)
    corrRow <- tibble(state = state1, neighbor = state2, corr = corr, lag = lagDays)
    corrTibble <- corrTibble %>% bind_rows(corrRow)
  }
  return <- 
    corrTibble
}

# loops through every pair of neighboring states 
# and calculates the correlation between them.
findNeighbors <- function(stateCorrs, state, borders) {
  for (neighbor in borders[[1]]) {
    # corr <- corrStates(state, neighbor)
    # print(corr)
    # stateCorr2 <- tibble(state = state, neighbor = neighbor, corr = corr)
    stateCorr2 <- corrDiffs(state, neighbor)
    stateCorrs <- stateCorrs %>% bind_rows(stateCorr2)
  }
  return <- stateCorrs
}

stateCorrs <- 
  tibble(
    state = character(), 
    neighbor = character(), 
    corr = numeric(), 
    lag = numeric()
  )
# loops through every state, finds neighboring states, 
# and calculates the correlation between them.
for (row in 1:nrow(neighbors)) {
  state <- neighbors[row, "state"]
  borders <- neighbors[row, "borders"]
  stateCorrs <- findNeighbors(stateCorrs, state, borders)
}

save(stateCorrs, file = "stateCorrs_different_lags.RData")
