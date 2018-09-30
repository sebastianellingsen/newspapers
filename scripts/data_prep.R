##------------------------------------------------------------------------------
# This file joins and cleans datasets for the main analysis
# Inputs: Stromberg_ABC_data.dta, bbindex.csv, white_pop.dta,
#         black_pop.dta, hisp_pop.dta.
# Outputs: np_data, bbusers, np_pew
# Date: 28. 5. 2018.
##------------------------------------------------------------------------------
setwd("~/Dropbox/US_Internet_Newspapers")
# Loading and cleaning datasets
#Dataset 1: Newspaper circulation
np_data <- read_dta("Data/circulation/__Stromberg_ABC_data.dta") %>%
  drop_na(daily) %>%
  filter(year > 1994, year <= 2006, state != 2, state != 15) %>%
  select(year, np = member, newspaper, countyn, staten, fips = countyid,
         daily, state) %>%
  group_by(np) %>%
  mutate(no_states = n_distinct(state), no_counties = n_distinct(fips))

#Dataset 2: ROW
row <- read_csv("Data/row/bbindex.csv") %>%
  drop_na(staten) %>%
  filter(staten != "HAWAII", staten != "ALASKA") %>%
  mutate(ltotal = log(total))

# Dataset 3: County level controls
county_controls <- read_dta("Data/county_controls/___county_controls.dta") %>%
  filter(year > 1994, year <= 2006)

# Dataset 4: ISP
isp <- read_dta("Data/isp/isps.dta") %>%
  group_by(fips, year) %>%
  mutate(num_ISPs = if_else(is.na(num_ISPs), 0, num_ISPs))

# Dataset 5: State mentions
statem <-
read_dta("Data/data_newspaper_content/state_mentions/statementions.dta") %>%
  filter(year > 1994, year <= 2006) %>%
  rename(staten = ABC_state)

# Combining datasets , no_states <= 5
np_data <- np_data %>%
  inner_join(row, by = "staten") %>%
  inner_join(county_controls, by = c("fips", "year")) %>%
  inner_join(isp, by = c("fips", "year")) %>%
  # inner_join(statem, by = c("np", "year", "staten")) %>%
  filter(no_states > 1, no_states <= 7) %>%
  mutate(circ = log(2*daily/voting_pop_)) %>%
  # mutate(countyn = tolower(countyn), staten = tolower(staten)) %>%
  filter(is.finite(circ))
  # %>%
  # filter(is.finite(num_statementions))

  write.csv(np_data, file = "np_data.csv")
