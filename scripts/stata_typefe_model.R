##------------------------------------------------------------------------------
# This function estimates a "stata-type" fixed effects model with clustered
# standard errors using the stata degree of freedom adjustment.
# Date: 25. 6. 2018.
# Sebastian
##------------------------------------------------------------------------------

model_baseline <- function(dataf, group) {

  group <- deparse(substitute(group))

  # Estimating the model
  model.plm <- plm(circ ~ total + total*factor(year), data = dataf, model = "within", index = c(group))

  # Stata type degree of freedom adjustment
  G <- length(unique(dataf[[group]]))
  N <- length(dataf[[group]])
  dfa <- (G/(G - 1)) * (N - 1)/model.plm$df.residual

  # Display with cluster VCE and df-adjustment
  c_vcov <- dfa * vcovHC(model.plm, type = "HC0", cluster = "group", adjust = T)
  d <- tidy(coeftest(model.plm, vcov = c_vcov))

  return(d[c(13:23),])
}

model_controls <- function(dataf, group) {

  group <- deparse(substitute(group))

  # Estimating the model
  model.plm <- plm(circ ~ total + total*factor(year) + share_white +
                   share_black + pop_, data = dataf, model = "within",
                   index = c(group))

  # Stata type degree of freedom adjustment
  G <- length(unique(dataf[[group]]))
  N <- length(dataf[[group]])
  dfa <- (G/(G - 1)) * (N - 1)/model.plm$df.residual

  # Display with cluster VCE and df-adjustment
  c_vcov <- dfa * vcovHC(model.plm, type = "HC0", cluster = "group", adjust = T)
  d <- tidy(coeftest(model.plm, vcov = c_vcov))

  return(d[c(16:26),])
}

model_trends <- function(dataf, group) {

  group <- deparse(substitute(group))

  # Estimating the model
  model.plm <- plm(circ ~ total + total*factor(year) + year*income_2000 +
                   + year*unemployment_2000 + year*pct_college_2000 +
                   year*age_2000, data = dataf, model = "within",
                   index = c(group))

  # Stata type degree of freedom adjustment
  G <- length(unique(dataf[[group]]))
  N <- length(dataf[[group]])
  dfa <- (G/(G - 1)) * (N - 1)/model.plm$df.residual

  # Display with cluster VCE and df-adjustment
  c_vcov <- dfa * vcovHC(model.plm, type = "HC0", cluster = "group", adjust = T)
  d <- tidy(coeftest(model.plm, vcov = c_vcov))

  return(d[c(17:27),])
}

model_countyfe <- function(dataf, group) {

  group <- deparse(substitute(group))

  # Estimating the model
  model.plm <- plm(circ ~ total + total*factor(year), data = dataf, model = "within", index = c(group))

  # Stata type degree of freedom adjustment
  G <- length(unique(dataf[[group]]))
  N <- length(dataf[[group]])
  dfa <- (G/(G - 1)) * (N - 1)/model.plm$df.residual

  # Display with cluster VCE and df-adjustment
  c_vcov <- dfa * vcovHC(model.plm, type = "HC0", cluster = "group", adjust = T)
  d <- tidy(coeftest(model.plm, vcov = c_vcov))

  return(d[c(12:22),])
}

# Example how to call the function:
# model_countyfe(np_data, fips)

# This function extracts for a table output
extract <- function(model) {

  extract.lm_robust(model, include.ci = FALSE, include.rsquared = TRUE,
                    stars = 0, include.adjrs = TRUE, include.nobs = TRUE,
                    include.fstatistic = FALSE, include.rmse = TRUE)
}
