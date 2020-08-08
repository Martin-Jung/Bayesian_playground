# Playing around with INLA
# Haakon Bakka's tutorials
# https://haakonbakka.bitbucket.io/btopic109.html

# Load INLA and see if it works
library(INLA)
inla.update(testing=T)

#### Seeds example ####
data(Seeds);head(Seeds)

# Analysis data.frame
df = data.frame(y = Seeds$r, Ntrials = Seeds$n, Seeds[, 3:5])

# Set family / likelihood
family1 = "binomial"
control.family1 = list(control.link=list(model="logit"))

# Hyper parameter of binomial variable
hyper1 = list(theta = list(prior="pc.prec", param=c(1,0.01)))
formula1 = y ~ x1 + x2 + f(plate, model="iid", hyper=hyper1)

res1 = inla(formula=formula1, data=df, 
            family=family1, Ntrials=Ntrials, 
            control.family=control.family1)



# Also good:
# https://becarioprecario.bitbucket.io/spde-gitbook/index.html