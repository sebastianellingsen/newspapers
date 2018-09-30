# Figure 1: RoW index by state
counties <- map_data("county")
states <- map_data("state")
data(us.cities)
cities <- map.cities(us.cities)

np_data_sm <- np_data %>%
  group_by(staten) %>%
  summarise(count = n(), RoW = mean(total)) %>%
  rename(region = staten) %>%
  full_join(states, np_data_sm, by = c("region"))

ggplot() + geom_polygon(data = np_data_sm, aes(x=long, y = lat, group = group, fill = RoW)) +
    coord_fixed(1.3) + theme_void()
