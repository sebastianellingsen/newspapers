# Newspaper level summary
np_sum <- np_data %>%
  group_by(year) %>%
  # filter(year==2000) %>%
  summarise(circ = mean(circ), daily = mean(2*daily/voting_pop_), no_states =
            mean(no_states), total = mean(total), pct_college_2000 =
            mean(pct_college_2000, na.rm = TRUE), income_2000 = mean(
            income_2000, na.rm = TRUE), age_2000 = mean(age_2000, na.rm = TRUE),
            unemployment_2000 = mean(unemployment_2000, na.rm = TRUE), pop =
            mean(pop_), share_white = mean(share_white), share_black =
            mean(share_black), share_hisp = mean(share_hisp), count = n(),
            num_statementions = mean(num_statementions))

# County level summary
county_sum <- np_data %>%
  group_by(fips) %>%
  summarise(circ = mean(circ), daily = mean(2*daily/voting_pop_), no_states =
            mean(no_states), total = mean(total), pct_college_2000 =
            mean(pct_college_2000), income_2000 = mean(income_2000),
            age_2000 = mean(age_2000), unemployment_2000 =
            mean(unemployment_2000), pop = mean(pop_), share_white =
            mean(share_white), share_black = mean(share_black), share_hisp =
            mean(share_hisp), count = n())

# describe(county_sum)
