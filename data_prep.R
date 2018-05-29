setwd("~/Desktop/newspapers_broadband")

library(tidyverse)
library(haven)
library(plm)

# Loading and cleaning datasets
#Dataset 1: Newspaper circulation
np_data <- read_dta("Stromberg_ABC_data.dta") %>%
  drop_na(daily) %>%
  filter(year > 1994, state != 2, state != 15) %>%
  select(year, np = member, newspaper, countyn, staten, fips = countyid, daily, state) %>%
  group_by(np) %>%
  mutate(no_states = n_distinct(state))

#Dataset 2: ROW
row <- read_csv("bbindex.csv") %>%
  drop_na(staten) %>%
  filter(staten != "HAWAII", staten != "ALASKA")

#Dataset 3: Population
pop <- read_dta("white_pop.dta") %>%
  inner_join(read_dta("black_pop.dta"), by = c("fips", "year")) %>%
  inner_join(read_dta("hisp_pop.dta"), by = c("fips", "year")) %>%
  inner_join(read_dta("total_pop.dta"), by = c("fips", "year")) %>%
  inner_join(read_dta("voting_age_pop.dta"), by = c("fips", "year")) %>%
  filter(year > 1994)

# Combining datasets
np_data <- np_data %>%
  inner_join(row, by = "staten") %>%
  inner_join(pop, by = c("fips", "year")) %>%
  filter(no_states > 1, no_states <= 5) %>%
  mutate(circ = log(2*daily/voting_pop_)) %>%
  filter(circ > -10000000)

summary(plm(circ ~ factor(year) + total + white_pop + black_pop + hisp_pop + total*factor(year), data = np_data, model = "within", index = c("np")))

#Do the variables uniquely identify observations?
pop %>%
  group_by(var) %>%
  summarise(count = n()) %>%
  filter(count>1)
