setwd("~/Desktop/newspapers_broadband")

library(tidyverse)
library(haven)
library(broom)
library(plm)
library(multiwayvcov)
library(lmtest)

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
  filter(circ > -1000000000)

# coef(summary(plm(circ ~ factor(year) + total + white_pop + black_pop + hisp_pop + total*factor(year), data = np_data, model = "within", index = c("np"))))
model.plm <- plm(circ ~ factor(year) + total + white_pop + black_pop + hisp_pop + total*factor(year), data = np_data, model = "within", index = c("np"))

# compute Stata like df-adjustment
G <- length(unique(np_data$year))
N <- length(np_data$year)
dfa <- (G/(G - 1)) * (N - 1)/model.plm$df.residual
# dfa <- 1

# display with cluster VCE and df-adjustment
c_vcov <- dfa * vcovHC(model.plm, type = "HC0", cluster = "group", adjust = T)

d <- tidy(coeftest(model.plm, vcov = c_vcov))
# %>%
#   mutate(lower = estimate - 1.96*std.error, upper = estimate + 1.96*std.error)

d <- d[c(16:26),]

ggplot() +
geom_point(data=d, mapping=aes(x=term, y=estimate), size=2, shape=21, fill="black") +
geom_errorbar(data = d, aes(x = term, ymin=estimate-1.654*std.error, ymax=estimate+1.654*std.error), width=.1) +
geom_line(data=d, mapping=aes(x=term, y=estimate, group=1), linetype = "dashed") + theme_bw()









#Do the variables uniquely identify observations?
pop %>%
  group_by(var) %>%
  summarise(count = n()) %>%
  filter(count>1)

library(ggridges)
ggplot(np_data, aes(x = circ, y = staten)) + geom_ridgeline()

ggplot(np_data, aes(x = log(daily), y = staten)) + geom_density_ridges(scale = 0.01) +
  geom_density_ridges_gradient(scale = 7, rel_min_height = 0.01)



  library(tidyverse)
  library(broom) # tidies model objects

  # Data set  with variation - (2 models)
  x <- rnorm(10)
  y <- rnorm(10)
  z <- rnorm(10)

  mod.A <- lm(y ~ x)
  mod.B <- lm(y ~ x+z)

  # Messy model output
  summary(mod.A)

  # Tidy approach
  # Use broom to easier extract model components
  # Three functions to learn - tidy, glance & augment
  # can use pipe %>% - pronounced 'then'

  # Model co-efficents
  tidy(mod.A)

  # Alternatively write with magritter pipe
  mod.A %>% tidy

  # Model statistics
  mod.A %>% glance

  # Model data
  # same as head(augment(mod.A),3)
  mod.A %>% augment() %>% head(3)

  # Use these to extract and save into data frame objects.
  # Model names
  Mod.Compare <- data.frame(Mod.Name= c("mod.A", "mod.B"))
  # Bind with model stats extracted with glance
  Mod.Compare <- cbind(Mod.Compare,
                  data.frame(rbind(glance(mod.A),glance(mod.B))))
  # Compare models
  Mod.Compare

  # This Mod.Compare object form also makes it easier to publish in RMarkdown tables or write to a csv or Excel file.

  # btw also helps with quick eda
  mod.A %>% augment() %>% ggplot(aes(x=.fitted,y=.resid ,colour=z)) + geom_point()



  #Making a coefficent plot
  x <- rnorm(10)
  y <- rnorm(10)
  mod.A <- lm(y ~ x)
  d <- tidy(mod.A) %>%
    mutate(lower = 1.96*std.error - estimate, upper = 1.96*std.error + estimate)

  #Plotting standard errors

  ggplot() +
  geom_point(data=d, mapping=aes(x=term, y=estimate), size=2, shape=21, fill="black") +
  geom_errorbar(data = d, aes(x = term, ymin=estimate-std.error, ymax=estimate+std.error), width=.1, position=pd) +
  geom_line(data=d, mapping=aes(x=term, y=estimate, group=1), linetype = "dashed")




  geom_line()
  geom_point(position=pd)


  +
      geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1, position=pd) +
d, aes(x=term, y=estimate)

  ggplot() +
  geom_errorbar(data=d, mapping=aes(x=term, ymin=upper, ymax=lower), width=0.2, size=1) +
  geom_point(data=d, mapping=aes(x=term, y=estimate), size=2, shape=21, fill="white")
  # opts(title="geom_errorbar", plot.title=theme_text(size=40, vjust=1.5))
