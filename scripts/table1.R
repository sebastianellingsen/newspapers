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
