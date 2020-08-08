# Return the maximum a posteriori
returnMAP <- function(lambda){
  got.logposterior <- function(lambda) {
    dgamma(lambda, 180, 6, log = TRUE) + sum(dpois(GoT$Us, lambda,
                                                   log = TRUE))
  }
  
  #Maximize log-posterior
  got.MAP <- optim(30, got.logposterior, control = list(fnscale = -1))
  got.MAP$par
}
