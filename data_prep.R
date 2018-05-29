setwd("~/Desktop/newspapers_broadband")

library(tidyverse)
library(haven)

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
  filter(no_states > 1, no_states <= 5)

#skal med sjasann







  # drop_na(voting_age_pop_)


  # np_data <- mutate(np_data, circ = daily / voting_age_pop_)


summary(plm(circ ~ factor(year) + total + total*factor(year), data = np_data, model = "within", index = c("np")))



np_data %>%
  filter(daily>0)



#Do the variables uniquely identify?
pop %>%
  group_by(var) %>%
  summarise(count = n()) %>%
  filter(count>1)
