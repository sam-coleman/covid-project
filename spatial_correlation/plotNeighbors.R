mycorr <- corrStates("oregon", "washington")

mycorr %>%
  pivot_longer(
    cols = c("cases.x", "cases.y"),
    names_to = "state",
    values_to = "cases"
  ) %>% 
  filter(
    !weekdays(date) %in% c("Saturday", "Sunday")
    # date != as.POSIXct("2020-09-06")
  ) %>% 
  ggplot(mapping = aes(x = date, y = cases, color= state)) +
  geom_point()