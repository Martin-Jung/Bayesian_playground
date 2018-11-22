library(tidyverse)
library(brms)
library(lme4)
library(mboost)
library(sjPlot)

# Load biodiversity site dataset
sites <- readRDS("~/../PhD/Projects/P5_MagnitudeBreakpoints/PREDICTS_allsites.rds")
sites <- subset(sites,Predominant_habitat != "Cannot decide")# Drop Cannot decide from the factors
sites$Predominant_habitat <- fct_collapse(sites$Predominant_habitat,"Primary vegetation" = c("Primary forest","Primary non-forest"))
sites$Predominant_habitat <- factor(sites$Predominant_habitat,
                                    levels = c("Primary vegetation","Young secondary vegetation","Intermediate secondary vegetation","Mature secondary vegetation",
                                               "Secondary vegetation (indeterminate age)","Plantation forest","Pasture","Cropland","Urban"),
                                    ordered = T)
sites$SS <- factor(sites$SS)
# ------------------------ #
# Lets fit four different models here
# First for lme4 using a "classic" predicts model and reproduce a model similar to Newbold et al. 2015

# Species richness against land use
# As simple test we on use two random intercepts, the study and a spatial block within study
fit1 <- glmer(Species_richness ~ Predominant_habitat + (1|SS) + (1|SSB),
              data = sites,family = poisson(link = "log"))
saveRDS(fit1,"fit1.rds")

# ------------------ #
# Now lets refit bayesian models in a proper bayesian setting

#student_t(5,0,10)
fit2 <- brm(Species_richness ~ Predominant_habitat + (1|SS) + (1|SSB),
            data = sites,family = poisson(link = "log"),
            prior = prior(normal(0,1), class = b) + prior(cauchy(0,2), class = sd),
            chains = 2, cores = 6, iter = 2000)
saveRDS(fit2,"fit2.rds")

# generate a summary of the results
summary(fit2)

# plot the MCMC chains as well as the posterior distributions
plot(fit2, ask = FALSE)
plot(marginal_effects(fit2), ask = FALSE)
pp_check(fit2)

# Test monotonic effects
#is land use a cont. gradient?
fit3 <- brm(Species_richness ~ mo(Predominant_habitat) + (1|SS) + (1|SSB),
            data = sites,family = poisson(link = "log"),
            prior = prior(normal(0,1), class = b) + prior(cauchy(0,2), class = sd),
            chains = 2, cores = 6, iter = 2000)
saveRDS(fit3,"fit3.rds")

plot(fit3, ask = FALSE)
plot(marginal_effects(fit3), ask = FALSE)

# Leave one out crossvalidation for both bayesian models #
LOO(fit2,fit3)

# -- #
# However using monotonic effects is not the first time i have seen this
# Cite Hofner et al. 
# They use mboost
# Lastly lets fit the model using a monotonic effect in the mboost package
bctrl = boost_control(mstop = 5000,nu = 0.01,risk = "inbag", trace = TRUE)

mod1 <- mboost(Species_richness ~ bols(Predominant_habitat,intercept = T) + brandom(SS) + brandom(SSB),
               data=sites %>% mutate(Predominant_habitat = ordered(Predominant_habitat)), # Set the habitat as ordered factor for this to work
               family = Poisson(),control = bctrl)
saveRDS(mod1,"mod1.rds")
summary(mod1)
# Plot
plot(mod1,which = "Predominant_habitat")
nd <- data.frame(Predominant_habitat = factor(levels(sites$Predominant_habitat),levels = levels(sites$Predominant_habitat),ordered = T) ) # Newdata frame
nd$fit1 <- predict.mboost(mod1,which = "Predominant_habitat",newdata = nd,type = "response")
# Reset relative
nd$fit1 <- (nd$fit1 /nd$fit1[1])-1

nd$fit3 <- predict(fit1,newdata=nd,re.form=NA,type="response")
nd$fit3 <- (nd$fit3 /nd$fit3[1])-1

mod2 <- mboost(Species_richness ~ bmono(Predominant_habitat,intercept = T,constraint = "decreasing") + brandom(SS) + brandom(SSB),
               data=sites %>% mutate(Predominant_habitat = ordered(Predominant_habitat)), # Set the habitat as ordered factor for this to work
               family = Poisson(),control = bctrl)
saveRDS(mod2,"mod2.rds")
plot(mod2,which = "Predominant_habitat")
nd$fit2 <- predict.mboost(mod2,which = "Predominant_habitat",newdata = nd,type = "response")
# Reset relative
nd$fit2 <- (nd$fit2 /nd$fit2[1])-1

plot(nd$fit2~nd$Predominant_habitat)

# -------------------------------------- #
#### Lets compare all models in terms of the coefficients ####

coef(fit1)
coef(fit2)

