#########################################
## Loading packages and preparing data ##
#########################################

## Loading packages, data and external functions
library(tidyverse); library(estimatr); library(haven); library(aod);
library(ggpubr); library(prediction); library(statar); library(texreg);
library(ggridges); library(plm)

theme_set(theme_bw() + theme(panel.grid.minor = element_line(colour = "white", size = 0.5),
  panel.grid.major = element_line(colour = "white", size = 0.2)))

setwd("~/Dropbox/US_Internet_Newspapers")




## Maps

# Map1: Newspaper markets
counties <- map_data("county")
states <- map_data("state")
data(us.cities)

counties <- subset(counties, region == "kansas" | region == "missouri")
states <- subset(states, region == "kansas" | region == "missouri")
cities <- subset(us.cities, name == "Kansas City KS")

np_data_sm <- read_dta("broadband_newspaper.dta") %>%
  filter(np == 124750) %>%
  group_by(staten, countyn) %>%
  summarise(count = n(), Circulation = mean(circ)) %>%
  rename(subregion = countyn, region = staten) %>%
  full_join(counties, np_data_sm, by = c("region", "subregion")) %>%
  drop_na(Circulation)

statenames <- c("Kansas", "Missouri", "Kansas City")
long <- c(-99.7, -91, -94.73)
lat <- c(38.83241, 37.6, 39)
labels = data.frame(statenames, long, lat)

p2 <- ggplot() +
geom_path(data = counties, aes(x = long, y = lat, group = group), color = "black", size = 0.08) +
geom_polygon(data = np_data_sm, aes(x=long, y = lat, group = group, fill = log(Circulation))) +
  coord_fixed(1.3) + theme_void() +geom_polygon(data = states, aes(x=long, y = lat, group = group), fill = NA, color = "black", size = 0.06) +
  geom_point(data = cities, aes(x=long, y = lat)) + geom_text(data=labels, aes(label = statenames, x = long, y = lat))

ggsave("figures/market_map.pdf", units='in')


# Map 2: RoW index by state
counties <- map_data("county")
states <- map_data("state")
data(us.cities)
cities <- map.cities(us.cities)

np_data_sm <- read_dta("broadband_newspaper.dta")  %>%
  group_by(staten) %>%
  summarise(count = n(), RoW = mean(total)) %>%
  rename(region = staten) %>%
  full_join(states, np_data_sm, by = c("region"))

p1 <- ggplot() + geom_polygon(data = np_data_sm, aes(x=long, y = lat, group = group, fill = RoW)) +
    coord_fixed(1.3) + theme_void()

ggsave("figures/row_map.pdf", units='in')

ggarrange(p1, p2, ncol=1, nrow=2, align = "v") %>%
  ggexport(filename = "figures/maps.pdf", units='in')


## Placebo exercise

# Timing of first stage and intention to treat
# Circulation
circ <- read_dta("data/placebo_circ.dta")
p1 <- ggplot(data=circ, mapping=aes(x=year, y=coef, group=1)) +
                scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
                scale_y_continuous(breaks = scales::pretty_breaks(n = 11)) +
                geom_errorbar(aes(x = year, ymin = coef - 1.96*stderr, ymax = coef+1.96*stderr), width = .3)  +
                xlab("") + ylab("") + geom_point(size = 2, shape = 21, fill = "black") +
                geom_hline(yintercept=0, color="black", linetype="dashed", size=0.3, alpha = 0.5) +
                geom_vline(xintercept=c(2001), linetype = "longdash") +
                theme(text = element_text(size=12))

ggsave("figures/circ_placebo.pdf", height=5, width=5, units='in')

# isp
isp <- read_dta("data/placebo_num_ISPs_ipo.dta")
p2 <- ggplot(data=isp, mapping=aes(x=year, y=coef, group=1)) +
                scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
                scale_y_continuous(breaks = scales::pretty_breaks(n = 11)) +
                geom_errorbar(aes(x = year, ymin = coef - 1.96*stderr, ymax = coef+1.96*stderr), width = .3)  +
                xlab("") + ylab("") + geom_point(size = 2, shape = 21, fill = "black") +
                geom_hline(yintercept=0, color="black", linetype="dashed", size=0.3, alpha = 0.5) +
                geom_vline(xintercept=c(2001), linetype = "longdash") +
                theme(text = element_text(size=12))

ggsave("figures/isp_placebo.pdf", height=5, width=5, units='in')

# Placebo outcomes

d1 <- read_dta("data/placebo_pop_.dta")
p1 <- ggplot(data=d1, mapping=aes(x=year, y=coef, group=1)) +
                scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
                scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
                geom_errorbar(aes(x = year, ymin = coef - 1.96*stderr, ymax = coef+1.96*stderr), width = .3)  +
                xlab("") + ylab("") + geom_point(size = 2, shape = 21, fill = "black") +
                geom_hline(yintercept=0, color="black", linetype="dashed", size=0.3, alpha = 0.5) +
                geom_vline(xintercept=c(2001), linetype = "longdash") +
                theme(text = element_text(size=12)) + ggtitle("Population")

d2 <- read_dta("data/placebo_share_black.dta")
p2 <- ggplot(data=d2, mapping=aes(x=year, y=coef, group=1)) +
                scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
                scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
                geom_errorbar(aes(x = year, ymin = coef - 1.96*stderr, ymax = coef+1.96*stderr), width = .3)  +
                xlab("") + ylab("") + geom_point(size = 2, shape = 21, fill = "black") +
                geom_hline(yintercept=0, color="black", linetype="dashed", size=0.3, alpha = 0.5) +
                geom_vline(xintercept=c(2001), linetype = "longdash") +
                theme(text = element_text(size=12)) + ggtitle("Share black")

d3 <- read_dta("data/placebo_share_hisp.dta")
p3 <- ggplot(data=d3, mapping=aes(x=year, y=coef, group=1)) +
                scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
                scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
                geom_errorbar(aes(x = year, ymin = coef - 1.96*stderr, ymax = coef+1.96*stderr), width = .3)  +
                xlab("") + ylab("") + geom_point(size = 2, shape = 21, fill = "black") +
                geom_hline(yintercept=0, color="black", linetype="dashed", size=0.3, alpha = 0.5) +
                geom_vline(xintercept=c(2001), linetype = "longdash") +
                theme(text = element_text(size=12)) + ggtitle("Share hispanic")

d4 <- read_dta("data/placebo_share_white.dta")
p4 <- ggplot(data=d4, mapping=aes(x=year, y=coef, group=1)) +
                scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
                scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
                geom_errorbar(aes(x = year, ymin = coef - 1.96*stderr, ymax = coef+1.96*stderr), width = .3)  +
                xlab("") + ylab("") + geom_point(size = 2, shape = 21, fill = "black") +
                geom_hline(yintercept=0, color="black", linetype="dashed", size=0.3, alpha = 0.5) +
                geom_vline(xintercept=c(2001), linetype = "longdash") +
                theme(text = element_text(size=12)) + ggtitle("Share white")

d5 <- read_dta("data/placebo_voting_pop_.dta")
p5 <- ggplot(data=d5, mapping=aes(x=year, y=coef, group=1)) +
                scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
                scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
                geom_errorbar(aes(x = year, ymin = coef - 1.96*stderr, ymax = coef+1.96*stderr), width = .3)  +
                xlab("") + ylab("") + geom_point(size = 2, shape = 21, fill = "black") +
                geom_hline(yintercept=0, color="black", linetype="dashed", size=0.3, alpha = 0.5) +
                geom_vline(xintercept=c(2001), linetype = "longdash") +
                theme(text = element_text(size=12)) + ggtitle("Voting population")

ggarrange(p1, p2, p3, p4, p5, ncol=2, nrow=4, align = "v") %>%
  ggexport(filename = "figures/placebo_outcomes.pdf", units='in')

file.remove("data/placebo_pop_.dta", "data/placebo_share_black.dta",
            "data/placebo_share_hisp.dta", "data/placebo_share_white.dta",
            "data/placebo_voting_pop_.dta", "data/placebo_num_ISPs_ipo.dta",
            "data/placebo_circ.dta")


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
p1 <- ggplot() + geom_line(data=bbusers, aes(x=year, y=frac_year))  +
  geom_point(data=bbusers, aes(x=year, y=frac_year), size = 3) + xlab("") +
  ylab("") + labs(caption = "") +
  ggtitle("Broadbad users") + xlim(1995, 2012)

p2 <- ggplot() + geom_line(data=np_pew, aes(x=year, y=weekday))  +
  geom_point(data=np_pew, aes(x=year, y=weekday), size = 3, shape = 1) + xlab("") +
  ylab("") + labs(caption = "Source: Pew Research (2018)") +
  ggtitle("Newspaper circulation") + xlim(1995, 2012)

plot_grid(p1, p2, ncol=1, align = "v") %>%
ggexport(filename = "figures/desc1.pdf", units='in')

p3 <- ggplot(data = bbusers_edu) +
  geom_line(mapping = aes(x = year, y = frac, linetype = edu, color = edu)) +
  geom_point(mapping = aes(x = year, y = frac, shape = edu, color = edu), size = 2)  +
  xlab("") + ylab("Broadband users") + labs(linetype = "Education", color = "Education") +
  labs(shape = "Education", color = "Education", caption = "") +
  theme(text = element_text(size = 8))

p4 <- ggplot(data = bbusers_income) +
  geom_line(mapping = aes(x = year, y = frac, linetype = income, colour = income)) +
  geom_point(mapping = aes(x = year, y = frac, shape = income, colour = income), size = 2)  +
  xlab("") + ylab("") + labs(linetype = "Income", color = "Income", caption = "") +
  labs(shape = "Income") +
  theme(text = element_text(size = 8))

p5 <- ggplot(data = bbusers_age) +
  geom_line(mapping = aes(x = year, y = frac, linetype = age, colour = age)) +
  geom_point(mapping = aes(x = year, y = frac, shape = age, colour = age), size = 2)  +
  xlab("") + ylab("Broadband users") + labs(linetype = "Age", color = "Age", caption = "") +
  labs(shape = "Age") +
  theme(text = element_text(size = 8))

p6 <- ggplot(data = bbusers_ctype) +
  geom_line(mapping = aes(x = year, y = frac, linetype = ctype, colour = ctype)) +
  geom_point(mapping = aes(x = year, y = frac, shape = ctype, colour = ctype), size = 2)  +
  xlab("") + ylab("") + labs(linetype = "Community", color = "Community") +
  labs(shape = "Community", caption = "Source: Pew Research (20110)") +
  theme(text = element_text(size = 8))

plot_grid(p3, p4, p5, p6, ncol=2, align = "v") %>%
  ggexport(filename = "figures/desc2.pdf", units='in')
