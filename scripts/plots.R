##------------------------------------------------------------------------------
# This file plots results from the main analysis
# Date: 25. 6. 2018.
##------------------------------------------------------------------------------


######################
## Loading packages ##
######################

library(stargazer)
library(texreg)
library(xtable)

###################
## Summary stats ##
###################



#######################
## Coefficient plots ##
#######################

## Exclusion restriction

# Figure 11: Baseline specification
p1_circ <- ggplot(data=model1_circ, mapping=aes(x=term, y=estimate, group=1)) +
  geom_point(size = 2, shape = 21, fill = "black") + geom_errorbar(aes(x = term,
  ymin = estimate - 1.96*std.error, ymax = estimate+1.96*std.error), width = .3) +
  geom_line(linetype = "dashed", size = 1) + theme_bw() + scale_x_discrete(
  labels = c('95','96','97','98', '99', '01', '02', '03', '04',
  '05', '06')) + geom_vline(xintercept = which(model1_circ ==
  'total:factor(year)1999'), linetype = "dashed" ) + xlab("") + ylab("") +
   ggtitle("Baseline") + theme(text = element_text(size = 20))

# Figure 12: County fixed effects
p2_circ <- ggplot(data=model4_circ, mapping=aes(x=term, y=estimate, group=1)) + geom_point(
  size = 2, shape = 21, fill = "black") + geom_errorbar(aes(x = term, ymin =
  estimate - 1.96*std.error, ymax = estimate+1.96*std.error), width = .3) +
  geom_line(linetype = "dashed", size = 1) + theme_bw() + scale_x_discrete(
  labels = c('95','96','97','98','99', '01', '02', '03', '04',
  '05', '06')) + geom_vline(xintercept = which(model4_circ ==
  'total:factor(year)1999'), linetype = "dashed" ) + xlab("") + ylab("") +
  ggtitle("County FE") + theme(text = element_text(size = 20))

# Figure 13: Demographic controls
p3_circ <- ggplot(data=model2_circ, mapping=aes(x=term, y=estimate, group=1)) + geom_point(
  size = 2, shape = 21, fill = "black") + geom_errorbar(aes(x = term, ymin =
  estimate - 1.96*std.error, ymax = estimate+1.96*std.error), width = .3) +
  geom_line(linetype = "dashed", size = 1) + theme_bw() + scale_x_discrete(labels
  = c('95','96','97','98','99', '01', '02', '03', '04',
  '05', '06')) + geom_vline(xintercept = which(model2_circ ==
  'total:factor(year)1999'), linetype = "dashed" ) + xlab("") + ylab("") +
  ggtitle("Demographic controls") + theme(text = element_text(size = 20))

# Figure 14: Trend interactions
p4_circ <- ggplot(data=model3_circ, mapping=aes(x = term, y = estimate,
  group = 1)) + geom_point(size = 2, shape = 21, fill = "black") +
  geom_errorbar(aes(x = term, ymin = estimate - 1.96*std.error, ymax =
  estimate + 1.96*std.error), width = .3) + geom_line(linetype = "dashed", size
  = 1) + theme_bw() + scale_x_discrete(labels = c('95','96','97','98',
  '99', '01', '02', '03', '04','05', '06')) + geom_vline(
  xintercept = which(model3_circ == 'total:factor(year)1999'), linetype = "dashed" ) +
  xlab("") + ylab("") + ggtitle("Trend interactions") + theme(text =
  element_text(size = 20))

plot_grid(p1_circ, p2_circ, p3_circ, p4_circ, ncol=2, align = "v")

## Exclusion restriction

# Figure 21: Baseline specification
p1_isp <- ggplot(data=model1_isp, mapping=aes(x=term, y=estimate, group=1)) + geom_point(
  size = 2, shape = 21, fill = "black") + geom_errorbar(aes(x = term, ymin =
  estimate - 1.96*std.error, ymax = estimate+1.96*std.error), width = .3) +
  geom_line(linetype = "dashed", size = 1) + theme_bw() + scale_x_discrete(
  labels = c('1995','1996','1997','1998', '2000', '2001', '2002', '2003', '2004',
  '2005', '2006')) + geom_vline(xintercept = which(model1_isp ==
  'total:year1998'), linetype = "dashed" ) + xlab("") + ylab("") +
   ggtitle("Baseline") + theme(text = element_text(size = 25))

# Figure 22: County fixed effects
p2_isp <- ggplot(data=model4_isp, mapping=aes(x=term, y=estimate, group=1)) + geom_point(
  size = 2, shape = 21, fill = "black") + geom_errorbar(aes(x = term, ymin =
  estimate - 1.96*std.error, ymax = estimate+1.96*std.error), width = .3) +
  geom_line(linetype = "dashed", size = 1) + theme_bw() + scale_x_discrete(
  labels = c('1995','1996','1997','1998','2000', '2001', '2002', '2003', '2004',
  '2005', '2006')) + geom_vline(xintercept = which(model4_isp ==
  'total:year1999'), linetype = "dashed" ) + xlab("") + ylab("") +
  ggtitle("County FE") + theme(text = element_text(size = 25))

# Figure 23: Demographic controls
p3_isp <- ggplot(data=model2_isp, mapping=aes(x=term, y=estimate, group=1)) + geom_point(
  size = 2, shape = 21, fill = "black") + geom_errorbar(aes(x = term, ymin =
  estimate - 1.96*std.error, ymax = estimate+1.96*std.error), width = .3) +
  geom_line(linetype = "dashed", size = 1) + theme_bw() + scale_x_discrete(
  labels = c('1995','1996','1997','1998','2000', '2001', '2002', '2003', '2004',
  '2005', '2006')) + geom_vline(xintercept = which(model2_isp ==
  'total:year1999'), linetype = "dashed" ) + xlab("") + ylab("") +
  ggtitle("Demographic controls") + theme(text = element_text(size = 25))

# Figure 24: Trend interactions
p4_isp <- ggplot(data=model3_isp, mapping=aes(x = term, y = estimate, group = 1)) +
  geom_point(size = 2, shape = 21, fill = "black") + geom_errorbar(aes(x = term,
  ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width
  = .3) + geom_line(linetype = "dashed", size = 1) + theme_bw() +
  scale_x_discrete(labels = c('1995','1996','1997','1998','2000', '2001',
  '2002', '2003', '2004','2005', '2006')) + geom_vline(xintercept =
  which(model3_isp == 'total:year1999'), linetype = "dashed" ) + xlab("") +
  ylab("") + ggtitle("Trend interactions") + theme(text =
  element_text(size = 25))

plot_grid(p1_isp, p2_isp, p3_isp, p4_isp, ncol=2, align = "v")


##########
## Maps ##
##########

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

ggplot() +
geom_path(data = counties, aes(x = long, y = lat, group = group), color = "black", size = 0.03) +
geom_polygon(data = np_data_sm, aes(x=long, y = lat, group = group, fill = circulation)) +
  coord_fixed(1.3) + theme_void() +geom_polygon(data = states, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
  geom_point(data = cities, aes(x=long, y = lat)) + geom_text(data=labels, aes(label = statenames, x = long, y = lat))

# Map 2: RoW index by state
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


## This section makes a plot of every market in the sample
for (i in np_sum$np){

  # Defining the data
  np_data_sm <- np_data %>%
    filter(np == i, year == 1999) %>%
    group_by(staten, countyn) %>%
    summarise(count = n(), circulation = log(mean(pop_))) %>%
    rename(subregion = countyn, region = staten) %>%
    full_join(counties, np_data_sm, by = c("region", "subregion")) %>%
    drop_na(circulation)

  # Plotting the market
  g <- ggplot() +
  geom_path(data = counties, aes(x = long, y = lat, group = group), color = "black", size = 0.03) +
  geom_polygon(data = np_data_sm, aes(x=long, y = lat, group = group, fill = circulation)) +
    coord_fixed(1.3) + theme_void() + geom_polygon(data = states, aes(x=long, y = lat, group = group), fill = NA, color = "black")

  Sys.sleep(5)

  print(g)
  print(i)
}













#################
## Survey data ##
#################

library(gridExtra)
library(cowplot)

p1 <- ggplot() + geom_line(data=bbusers, aes(x=year, y=frac_year)) + theme_bw() +
  geom_point(data=bbusers, aes(x=year, y=frac_year), size = 3) + xlab("") +
  ylab("") + labs(caption = "") +
  ggtitle("Broadbad users") + xlim(1995, 2012)

p2 <- ggplot() + geom_line(data=np_pew, aes(x=year, y=weekday)) + theme_bw() +
  geom_point(data=np_pew, aes(x=year, y=weekday), size = 3, shape = 1) + xlab("") +
  ylab("") + labs(caption = "Source: Pew Research (2018)") +
  ggtitle("Newspaper circulation") + xlim(1995, 2012)

plot_grid(p1, p2, ncol=1, align = "v")

p1 <- ggplot(data = bbusers_edu) +
  geom_line(mapping = aes(x = year, y = frac, linetype = edu, color = edu)) +
  geom_point(mapping = aes(x = year, y = frac, shape = edu, color = edu), size = 2) + theme_bw() +
  xlab("") + ylab("Broadband users") + labs(linetype = "Education", color = "Education") +
  labs(shape = "Education", color = "Education", caption = "") +
  theme(text = element_text(size = 12))

p2 <- ggplot(data = bbusers_income) +
  geom_line(mapping = aes(x = year, y = frac, linetype = income, colour = income)) +
  geom_point(mapping = aes(x = year, y = frac, shape = income, colour = income), size = 2) + theme_bw() +
  xlab("") + ylab("") + labs(linetype = "Income", color = "Income", caption = "") +
  labs(shape = "Income") +
  theme(text = element_text(size = 12))

p3 <- ggplot(data = bbusers_age) +
  geom_line(mapping = aes(x = year, y = frac, linetype = age, colour = age)) +
  geom_point(mapping = aes(x = year, y = frac, shape = age, colour = age), size = 2) + theme_bw() +
  xlab("") + ylab("Broadband users") + labs(linetype = "Age", color = "Age", caption = "") +
  labs(shape = "Age") +
  theme(text = element_text(size = 12))

p4 <- ggplot(data = bbusers_ctype) +
  geom_line(mapping = aes(x = year, y = frac, linetype = ctype, colour = ctype)) +
  geom_point(mapping = aes(x = year, y = frac, shape = ctype, colour = ctype), size = 2) + theme_bw() +
  xlab("") + ylab("") + labs(linetype = "Community", color = "Community") +
  labs(shape = "Community", caption = "Source: Pew Research (2018)") +
  theme(text = element_text(size = 12))

plot_grid(p1, p2, p3, p4, ncol=2, align = "v")


#########################
## Constructing tables ##
#########################

# Table 1
table1 <- texreg(list(dd.circ, dd.isp, iv, iv.controls), dcolumn = TRUE,
          booktabs = TRUE, caption = "Two linear models.", stars = 0,
          digits = 3, include.ci = FALSE)

out <- capture.output(table1)
cat("My title", out, file="presentation/table1.tex", sep="n", append=TRUE)
