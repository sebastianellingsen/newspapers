# Setting the baseline value
np_data$year <- factor(np_data$year, ordered = FALSE)
np_data$year <- relevel(np_data$year, ref = "1999")

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

# Figure 11: Baseline specification
p1_circ <- ggplot(data=model1_circ, mapping=aes(x=term, y=estimate, group=1)) +
  geom_point(size = 2, shape = 21, fill = "black") + geom_errorbar(aes(x = term,
  ymin = estimate - 1.96*std.error, ymax = estimate+1.96*std.error), width = .3) +
  geom_line(linetype = "dashed", size = 1) + theme_bw() + scale_x_discrete(
  labels = c('95','96','97','98', '99', '01', '02', '03', '04',
  '05', '06')) + geom_vline(xintercept = which(model1_circ ==
  'total:factor(year)2001'), linetype = "dashed" ) + xlab("") + ylab("") +
   ggtitle("Baseline") + theme(text = element_text(size = 10))

# Figure 12: County fixed effects
p2_circ <- ggplot(data=model4_circ, mapping=aes(x=term, y=estimate, group=1)) + geom_point(
  size = 2, shape = 21, fill = "black") + geom_errorbar(aes(x = term, ymin =
  estimate - 1.96*std.error, ymax = estimate+1.96*std.error), width = .3) +
  geom_line(linetype = "dashed", size = 1) + theme_bw() + scale_x_discrete(
  labels = c('95','96','97','98','99', '01', '02', '03', '04',
  '05', '06')) + geom_vline(xintercept = which(model4_circ ==
  'total:factor(year)2001'), linetype = "dashed" ) + xlab("") + ylab("") +
  ggtitle("County FE") + theme(text = element_text(size = 10))

# Figure 13: Demographic controls
p3_circ <- ggplot(data=model2_circ, mapping=aes(x=term, y=estimate, group=1)) + geom_point(
  size = 2, shape = 21, fill = "black") + geom_errorbar(aes(x = term, ymin =
  estimate - 1.96*std.error, ymax = estimate+1.96*std.error), width = .3) +
  geom_line(linetype = "dashed", size = 1) + theme_bw() + scale_x_discrete(labels
  = c('95','96','97','98','99', '01', '02', '03', '04',
  '05', '06')) + geom_vline(xintercept = which(model2_circ ==
  'total:factor(year)2001'), linetype = "dashed" ) + xlab("") + ylab("") +
  ggtitle("Demographic controls") + theme(text = element_text(size = 10))

# Figure 14: Trend interactions
p4_circ <- ggplot(data=model3_circ, mapping=aes(x = term, y = estimate,
  group = 1)) + geom_point(size = 2, shape = 21, fill = "black") +
  geom_errorbar(aes(x = term, ymin = estimate - 1.96*std.error, ymax =
  estimate + 1.96*std.error), width = .3) + geom_line(linetype = "dashed", size
  = 1) + theme_bw() + scale_x_discrete(labels = c('95','96','97','98',
  '99', '01', '02', '03', '04','05', '06')) + geom_vline(
  xintercept = which(model3_circ == 'total:factor(year)2001'), linetype = "dashed" ) +
  xlab("") + ylab("") + ggtitle("Trend interactions") + theme(text =
  element_text(size = 10))

plot_grid(p1_circ, p2_circ, p3_circ, p4_circ, ncol=2, align = "v")
