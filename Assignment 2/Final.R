setwd("D:\\Personal\\College Stuff\\2nd Year\\ECON F215 Assignment 2\\Banerjee_Iyer_dataset")

# Load libraries
library(AER)
library(modelsummary)
library(stargazer)
library(dplyr)
library(haven)
library(knitr)
library(knitr)
library(kableExtra)
library(webshot)

# Load the data
data <- read_dta("yld_sett_aug03.dta")

data$other_hyv <- data$ahyv - (data$hyvrice + data$hyvwheat)


# Panel A: Output
ols_rice   <- lm(lyrice ~ p_nland + lat + alt + totrain, data = data)
iv_rice    <- ivreg(lyrice ~ p_nland + lat + alt + totrain | britdum + lat + alt + totrain, data = data)

ols_wheat  <- lm(lywhet ~ p_nland + lat + alt + totrain, data = data)
iv_wheat   <- ivreg(lywhet ~ p_nland + lat + alt + totrain | britdum + lat + alt + totrain, data = data)

ols_lyld   <- lm(lyld ~ p_nland + lat + alt + totrain, data = data)
iv_lyld    <- ivreg(lyld ~ p_nland + lat + alt + totrain | britdum + lat + alt + totrain, data = data)

ols_prod15 <- lm(lyld ~ p_nland + lat + alt + totrain, data = data)
iv_prod15  <- ivreg(lyld ~ p_nland + lat + alt + totrain | britdum + lat + alt + totrain, data = data)


# Panel B: Inputs
ols_fert   <- lm(pfert ~ p_nland + lat + alt + totrain, data = data)
iv_fert    <- ivreg(pfert ~ p_nland + lat + alt + totrain | britdum + lat + alt + totrain, data = data)

ols_irr    <- lm(irr_g ~ p_nland + lat + alt + totrain, data = data)
iv_irr     <- ivreg(irr_g ~ p_nland + lat + alt + totrain | britdum + lat + alt + totrain, data = data)

# Panel C: Technology
ols_hyvrice  <- lm(hyvrice ~ p_nland + lat + alt + totrain, data = data)
iv_hyvrice   <- ivreg(hyvrice ~ p_nland + lat + alt + totrain | britdum + lat + alt + totrain, data = data)

ols_hyvwheat <- lm(hyvwheat ~ p_nland + lat + alt + totrain, data = data)
iv_hyvwheat  <- ivreg(hyvwheat ~ p_nland + lat + alt + totrain | britdum + lat + alt + totrain, data = data)

ols_hyvcereal <- lm(other_hyv ~ p_nland + lat + alt + totrain, data = data)
iv_hyvcereal <- ivreg(other_hyv ~ p_nland + lat + alt + totrain | britdum + lat + alt + totrain, data = data)



# Assume all your models are saved as below:
# ols_irr, iv_irr, ols_fert, iv_fert, ols_hyvrice, iv_hyvrice, ols_hyvwheat, iv_hyvwheat, ols_prod15, iv_prod15, ols_rice, iv_rice, ols_wheat, iv_wheat
# (14 models total)

# Organize the models into a list
models_panelA <- list(
  ols_irr, iv_irr,
  ols_fert, iv_fert,
  ols_hyvrice, iv_hyvrice,
  ols_hyvwheat, iv_hyvwheat,
  ols_hyvcereal, iv_hyvcereal,
  ols_prod15, iv_prod15,
  ols_rice, iv_rice,
  ols_wheat, iv_wheat
)


# ========== PANEL A ==========
# Define all OLS and IV models
models_ols <- list(
  irr_g       = lm(irr_g ~ p_nland + lat + alt + totrain, data = data),
  pfert       = lm(pfert ~ p_nland + lat + alt + totrain, data = data),
  hyvrice     = lm(hyvrice ~ p_nland + lat + alt + totrain, data = data),
  hyvwheat    = lm(hyvwheat ~ p_nland + lat + alt + totrain, data = data),
  other_hyv   = lm(other_hyv ~ p_nland + lat + alt + totrain, data = data),
  lyld        = lm(lyld ~ p_nland + lat + alt + totrain, data = data),
  lyrice      = lm(lyrice ~ p_nland + lat + alt + totrain, data = data),
  lywhet      = lm(lywhet ~ p_nland + lat + alt + totrain, data = data)
)

models_iv <- list(
  irr_g       = ivreg(irr_g ~ p_nland + lat + alt + totrain | britdum + lat + alt + totrain, data = data),
  pfert       = ivreg(pfert ~ p_nland + lat + alt + totrain | britdum + lat + alt + totrain, data = data),
  hyvrice     = ivreg(hyvrice ~ p_nland + lat + alt + totrain | britdum + lat + alt + totrain, data = data),
  hyvwheat    = ivreg(hyvwheat ~ p_nland + lat + alt + totrain | britdum + lat + alt + totrain, data = data),
  other_hyv   = ivreg(other_hyv ~ p_nland + lat + alt + totrain | britdum + lat + alt + totrain, data = data),
  lyld        = ivreg(lyld ~ p_nland + lat + alt + totrain | britdum + lat + alt + totrain, data = data),
  lyrice      = ivreg(lyrice ~ p_nland + lat + alt + totrain | britdum + lat + alt + totrain, data = data),
  lywhet      = ivreg(lywhet ~ p_nland + lat + alt + totrain | britdum + lat + alt + totrain, data = data)
)

# Pick out only the coefficients for p_nland
extract_pnland <- function(model) {
  coef <- coef(summary(model))
  if ("p_nland" %in% rownames(coef)) {
    est <- coef["p_nland", "Estimate"]
    se  <- coef["p_nland", "Std. Error"]
    return(c(est, se))
  } else {
    return(c(NA, NA))
  }
}

# Build a dataframe manually
results <- data.frame(
  Variable = c(
    "Proportion of gross cropped area irrigated",
    "Fertilizer use (kg/ha)",
    "Area of rice area under HYV (hectares)",
    "Area of wheat area under HYV (hectares)",
    "Area of other cereals area under HYV (hectares)",
    "log(yield of 15 major crops)",
    "log(rice yield)",
    "log(wheat yield)"
  ),
  OLS_Coeff = NA,
  OLS_SE = NA,
  IV_Coeff = NA,
  IV_SE = NA
)

for (i in 1:nrow(results)) {
  ols_vals <- extract_pnland(models_ols[[i]])
  iv_vals  <- extract_pnland(models_iv[[i]])
  results$OLS_Coeff[i] <- ols_vals[1]
  results$OLS_SE[i]    <- ols_vals[2]
  results$IV_Coeff[i]  <- iv_vals[1]
  results$IV_SE[i]     <- iv_vals[2]
}

# Finally format nicely
results_table <- results %>%
  mutate(
    `OLS (1)` = sprintf("%.3f (%.3f)", OLS_Coeff, OLS_SE),
    `IV (2)`  = sprintf("%.3f (%.3f)", IV_Coeff, IV_SE)
  ) %>%
  select(Variable, `OLS (1)`, `IV (2)`)
# Make a nice formatted table for Panel A
panelA <- kable(
  results_table,
  col.names = c("Dependent Variable", "OLS (1)", "IV (2)"),
  align = c("l", "c", "c"),
  caption = "Table 4: Robustness of OLS Results — Panel A: Robustness checks",
  format = "html"
) %>%
  kable_styling(full_width = FALSE)
# Save Panel A as HTML first
save_kable(panelA, "PanelA.html")

# Then convert HTML to PNG
webshot::webshot("PanelA.html", "PanelA.png", zoom = 2)


# First-stage without state fixed effects
first_stage1 <- lm(p_nland ~ britdum + lat + alt + totrain, data = data)

# First-stage with state fixed effects
first_stage2 <- lm(p_nland ~ britdum + lat + alt + totrain + factor(state), data = data)

models_panelB <- list(first_stage1, first_stage2)

# Stargazer for Panel B
stargazer(
  models_panelB,
  type = "html",
  out="PanelB.html",
  title = "Table 4: Robustness of OLS Results - Panel B: First-stage regressions for IV",
  align = TRUE,
  column.labels = c("(1)", "(2)"),
  model.names = FALSE,
  dep.var.caption = "Dependent variable: Non-landlord proportion",
  covariate.labels = c("Instrument (=1 if British control 1820-1856)"),
  omit.stat = c("f", "ser"),
  add.lines = list(
    c("No. of observations", rep(nobs(first_stage1), 2)),
    c("R-squared", 
      round(summary(first_stage1)$r.squared, 2), 
      round(summary(first_stage2)$r.squared, 2)),
    c("Geographic controls", "YES", "YES"),
    c("State fixed effects", "NO", "YES")
  ),
  notes = "Standard errors in parentheses. * p<0.1; ** p<0.05; *** p<0.01"
)

webshot::webshot("PanelB.html", "PanelB.png", zoom = 2)

