#########################################
## Loading packages and preparing data ##
#########################################

# Loading packages and external functions
library(tidyverse); library(haven); library(broom) ;library(plm)
library(lmtest); library(lubridate); library(estimatr); library(psych)
library(ggridges); library(plm); library(gridExtra); library(cowplot)
library(plm); library(texreg); library(aod)

source("Scripts/stata_typefe_model.R")

# Setting max print
options(max.print=1000000)

############################
## Descriptive statistics ##
############################

# Newspaper level summary
np_sum <- np_data %>%
  group_by(np) %>%
  # filter(year==2000) %>%
  summarise(circ = mean(circ), daily = mean(2*daily/voting_pop_), no_states =
            mean(no_states), total = mean(total), pct_college_2000 =
            mean(pct_college_2000, na.rm = TRUE), income_2000 = mean(
            income_2000, na.rm = TRUE), age_2000 = mean(age_2000, na.rm = TRUE),
            unemployment_2000 = mean(unemployment_2000, na.rm = TRUE), pop =
            mean(pop_), share_white = mean(share_white), share_black =
            mean(share_black), share_hisp = mean(share_hisp), count = n())

# County level summary
county_sum <- np_data %>%
  group_by(fips) %>%
  summarise(circ = mean(circ), daily = mean(2*daily/voting_pop_), no_states =
            mean(no_states), total = mean(total), pct_college_2000 =
            mean(pct_college_2000), income_2000 = mean(income_2000),
            age_2000 = mean(age_2000), unemployment_2000 =
            mean(unemployment_2000), pop = mean(pop_), share_white =
            mean(share_white), share_black = mean(share_black), share_hisp =
            mean(share_hisp), count = n(), isp= 10000*mean(num_ISPs_ipo/pop_))

describe(county_sum)

# ggplot(data = county_sum, mapping = aes(x = log(isp), y = circ)) +
#   geom_point(aes(), alpha = 1/5) +
#   geom_smooth()
#
# ggplot(data=np_sum, mapping=aes(x=count)) +
#   geom_histogram(bins = 30)


##################
## Main results ##
##################

## Difference-in-difference
# Defining dataset
dd <- np_data %>%
  filter(year == 2000|year == 2006) %>%
  mutate(post = as.numeric(year > 2000),
         treat = post*total)

# Reduced form and first stage estimates
dd.circ <- lm_robust(circ ~ total + post + treat, data = dd, fixed_effects =
                     ~ np, clusters = np, se_type = "stata")
            extract(dd.circ)

dd.isp <- lm_robust(num_ISPs_ipo ~ total + post + treat, data = dd,
                    fixed_effects = ~ np, clusters = np, se_type = "stata")
          extract(dd.isp)

dd.isp.controls <- lm_robust(num_ISPs_ipo ~ total + post + treat + voting_pop_ +
                             share_white + share_black, data = dd,
                             fixed_effects = ~ np, clusters = np, se_type =
                             "stata")
                   extract(dd.isp.controls)


# IV estimates
iv <- iv_robust(circ ~ num_ISPs_ipo + total + post | treat + total + post,
                    data = dd, fixed_effects = ~ np, clusters = np, se_type =
                    "stata")
      extract(iv)

iv.controls <- iv_robust(circ ~ num_ISPs_ipo + total + post + voting_pop_ +
                    share_white + share_black | treat + total + post +
                    voting_pop_ + share_white + share_black, data = dd,
                    fixed_effects = ~ np, clusters = np, se_type = "stata")
               extract(iv.controls)

# Table 1
screenreg(list(dd.circ, dd.isp, iv, iv.controls), dcolumn = TRUE,
          booktabs = TRUE, caption = "Two linear models.", stars = 0,
          digits = 3, include.ci = FALSE)


#################################
## Asessing the IV - strategy ##
################################

# Setting the baseline value
np_data$year <- factor(np_data$year, ordered = FALSE)
np_data$year <- relevel(np_data$year, ref = "2000")

## Exclusion restriction

# Estimating pre-trends
# Baseline specification
model1_circ <- model_baseline(np_data, np)

# Demographic controls
model2_circ <- model_controls(np_data, np)

# Trend interactions
model3_circ <- model_trends(np_data, np)

# County fixed effects
model4_circ <- model_countyfe(np_data, fips)


## Instrument relevance

# # Estimating pre-trends
# # Baseline specification
# model1_isp <- model_baseline(np_data, np)
#
# # Demographic controls
# model2_isp <- model_controls(np_data, np)
#
# # Trend interactions
# model3_isp <- model_trends(np_data, np)
#
# # County fixed effects
# model4_isp <- model_countyfe(np_data, fips)
#
# # First stage F-test
# wald.test(b = coef(dd.isp), Sigma = vcov(dd.isp), Terms = 3:3)
# wald.test(b = coef(dd.isp.controls), Sigma = vcov(dd.isp.controls), Terms = 3:3)


##########################
## Market level results ##
##########################

















#######################
## Robustness checks ##
#######################

# Removing outliers
# outliers <- np_data %>%
#   filter(circ>-7.5)
#
#   lmr_out <- lm_robust(circ ~ total*factor(year), data = outliers,
#                        fixed_effects = ~ np, clusters = np, se_type = "stata")
#
#   lmr_out <- tidy(lmr_out)
#   tm <- lmr_out[c(13:23),]
