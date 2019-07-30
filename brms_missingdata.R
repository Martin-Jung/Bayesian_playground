# Missing data with brms
# https://cran.r-project.org/web/packages/brms/vignettes/brms_missings.html
library(brms)
library(tidyverse)
library(mice)

data("nhanes", package = "mice")
head(nhanes)

imp <- mice(nhanes, m = 5, print = FALSE)

# Fit model with imputed data
fit_imp1 <- brm_multiple(bmi ~ age*chl, data = imp, chains = 2)
summary(fit_imp1)

# Plot chains
plot(fit_imp1, pars = "^b")

# Marginal effects
marginal_effects(fit_imp1, "age:chl")

# Missing data imputation within model
bform <- bf(bmi | mi() ~ age * mi(chl)) +
  bf(chl | mi() ~ age) + set_rescor(FALSE)
fit_imp2 <- brm(bform, data = nhanes)

# Results are comparable
marginal_effects(fit_imp2, "age:chl", resp = "bmi")
