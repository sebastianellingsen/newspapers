
# Pew research broadband surveys
# Broadband by year
bbusers <- read_csv("Data/pew/home_broadband_users.csv") %>%
  mutate(datetime = mdy(date)) %>%
  select(-date) %>%
  mutate(year = year(datetime)) %>%
  group_by(year) %>%
  summarise(frac_year = mean(frac)) %>%
  filter(year <= 2014)

np_pew <- read_csv("Data/pew/np_circulation_pew.csv") %>%
  select(year, weekday, sunday) %>%
  filter(year >= 1995, year <= 2013)

# Broadband by year and education
bbusers_edu <- read_csv("Data/pew/home_broadband_users_education.csv") %>%
  mutate(datetime = mdy(date)) %>%
  select(-date) %>%
  mutate(year = year(datetime)) %>%
  group_by(year, edu) %>%
  summarise(frac = mean(frac)) %>%
  filter(year <= 2011)

# Broadband by year and income
bbusers_income <- read_csv("Data/pew/home_broadband_users_income.csv") %>%
  mutate(datetime = mdy(date)) %>%
  select(-date) %>%
  mutate(year = year(datetime)) %>%
  group_by(year, income) %>%
  summarise(frac = mean(frac)) %>%
  filter(year <= 2011)

# Broadband by year and age
bbusers_age <- read_csv("Data/pew/home_broadband_users_age.csv") %>%
  mutate(datetime = mdy(date)) %>%
  select(-date) %>%
  mutate(year = year(datetime)) %>%
  group_by(year, age) %>%
  summarise(frac = mean(frac)) %>%
  filter(year <= 2011)

# Broadband by year and community type
bbusers_ctype <- read_csv("Data/pew/home_broadband_users_community.csv") %>%
  mutate(datetime = mdy(date)) %>%
  select(-date) %>%
  mutate(year = year(datetime)) %>%
  group_by(year, ctype) %>%
  summarise(frac = mean(frac)) %>%
  filter(year <= 2011)

## Plotting data
p1 <- ggplot() + geom_line(data=bbusers, aes(x=year, y=frac_year)) + theme_bw() +
  geom_point(data=bbusers, aes(x=year, y=frac_year), size = 3) + xlab("") +
  ylab("") + labs(caption = "") +
  ggtitle("Broadbad users") + xlim(1995, 2012)

p2 <- ggplot() + geom_line(data=np_pew, aes(x=year, y=weekday)) + theme_bw() +
  geom_point(data=np_pew, aes(x=year, y=weekday), size = 3, shape = 1) + xlab("") +
  ylab("") + labs(caption = "Source: Pew Research (2018)") +
  ggtitle("Newspaper circulation") + xlim(1995, 2012)

plot_grid(p1, p2, ncol=1, align = "v")

p3 <- ggplot(data = bbusers_edu) +
  geom_line(mapping = aes(x = year, y = frac, linetype = edu, color = edu)) +
  geom_point(mapping = aes(x = year, y = frac, shape = edu, color = edu), size = 2) + theme_bw() +
  xlab("") + ylab("Broadband users") + labs(linetype = "Education", color = "Education") +
  labs(shape = "Education", color = "Education", caption = "") +
  theme(text = element_text(size = 9))

p4 <- ggplot(data = bbusers_income) +
  geom_line(mapping = aes(x = year, y = frac, linetype = income, colour = income)) +
  geom_point(mapping = aes(x = year, y = frac, shape = income, colour = income), size = 2) + theme_bw() +
  xlab("") + ylab("") + labs(linetype = "Income", color = "Income", caption = "") +
  labs(shape = "Income") +
  theme(text = element_text(size = 9))

p5 <- ggplot(data = bbusers_age) +
  geom_line(mapping = aes(x = year, y = frac, linetype = age, colour = age)) +
  geom_point(mapping = aes(x = year, y = frac, shape = age, colour = age), size = 2) + theme_bw() +
  xlab("") + ylab("Broadband users") + labs(linetype = "Age", color = "Age", caption = "") +
  labs(shape = "Age") +
  theme(text = element_text(size = 9))

p6 <- ggplot(data = bbusers_ctype) +
  geom_line(mapping = aes(x = year, y = frac, linetype = ctype, colour = ctype)) +
  geom_point(mapping = aes(x = year, y = frac, shape = ctype, colour = ctype), size = 2) + theme_bw() +
  xlab("") + ylab("") + labs(linetype = "Community", color = "Community") +
  labs(shape = "Community", caption = "Source: Pew Research (20110)") +
  theme(text = element_text(size = 9))

plot_grid(p3, p4, p5, p6, ncol=2, align = "v")
