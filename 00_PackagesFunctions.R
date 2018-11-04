cpu.cores <- 4
options(mc.cores = cpu.cores)

# Rstan
Sys.setenv(USE_CXX14 = 1)
library(rstan)
rstan_options(auto_write = TRUE)

library(brms)
library(tidybayes)
library(tidyverse)
