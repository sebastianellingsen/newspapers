# Map1: Newspaper markets
counties <- map_data("county")
states <- map_data("state")
data(us.cities)

counties <- subset(counties, region == "kansas" | region == "missouri")
states <- subset(states, region == "kansas" | region == "missouri")
cities <- subset(us.cities, name == "Kansas City KS")

np_data_sm <- np_data %>%
  filter(np == 124750) %>%
  group_by(staten, countyn) %>%
  summarise(count = n(), circulation = log(mean(pop_))) %>%
  rename(subregion = countyn, region = staten) %>%
  full_join(counties, np_data_sm, by = c("region", "subregion")) %>%
  drop_na(circulation)

statenames <- c("Kansas", "Missouri", "Kansas City")
long <- c(-99.7, -91, -94.73)
lat <- c(38.83241, 37.6, 39)
labels = data.frame(statenames, long, lat)
