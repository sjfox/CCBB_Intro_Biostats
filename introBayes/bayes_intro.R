##################################################
# Introduction to Bayes' Theorem
# Introduction to Biological Statistics
# CCBB Fall 2015
##################################################
# Spencer Fox
##################################################
# Some code/ideas adapted from the MMED workshop Summer 2015
# Those materials can be found here, and I highly suggest 
# the workshop for gaining experience in epidemiological modeling
# http://mmed2015.ici3d.org/schedule.html
# or https://ici3d.org
# License: http://mmed2015.ici3d.org/license.html
##################################################
rm(list=ls())
library(boot); library(coda)

# There's a terrible new epidemic ravaging the population!
# You go out in the field during this terrible epidemic, find 100 people randomly
# and sample them for the new disease. You find that 24 of them were infected
# Your goal is to use mcmc sampling to estimate the population prevalence from the sample
size <- 100
sampPos <- 24

# First we need to specify our prior for the distribution
# We need to think about the parameter for our model, the prevalence.
# This is the probability of success in a binomial trial, meaning it ranges from 0 to 1.
# The beta distribution is a natural distribution for a prior for a probability, 
# since it goes from 0-1, and a beta(1,1) distribution is said to be flat (uninformative), 
# so shouldn't influence inference

# We use logs for convenience because we can sum rather than multiply for next steps.

# Specify function to calculate prior density
logBetaPrior <- function(prevalence, shape1 = 1, shape2 = 1)
  dbeta(prevalence, shape1=shape1, shape2=shape2, log = T)

## The likelihood is simply the probability of observing a certain # of individuals test positive given
## the number tested and a specified prevalence.
logLikelihood <- function(prevalence, data = list(size=size, sampPos=sampPos))
  dbinom(data$sampPos, size = data$size, prob = prevalence, log = T)

## A convenience function that sums the log-likelihood and the log-prior.
logLikePrior <- function(prevalence, shape1=1, shape2=1, data = list(size=size, sampPos=sampPos))
  logBetaPrior(prevalence, shape1, shape2) + logLikelihood(prevalence, data)

## Convenience functions that returned the un-logged functions above.
Prior <- function(x) exp(logBetaPrior(x))
Likelihood <- function(x) exp(logLikelihood(x))
LikePrior <- function(x) exp(logLikePrior(x))

# Plot out the prior and the likelihood as a function of hypothetical prevalences
par(mfrow = c(2,1) ## panels
    , mar = c(3,6,1,1) ## panel margins
    , bty='n' ## no box around plots
    , oma = c(1.5,0,0,0)) ## outer margins
curve(Prior(x), 0, 1, ylab = 'prior')
curve(Likelihood, 0, 1, ylab = 'likelihood')
mtext('prevalence', 1, 0, outer=T)

## This function runs a Random Walk Markov chain Monte Carlo (MCMC) Metropolis-Hastings algorithm to
## numerically estimate the posterior probability distribution of the prevalence. For problems this
## simple, the posterior probability distribution can be solved for analytically, but for most
## real models and questions numerical integration using MCMC may be the only way to calculate 
## the posterior probability distribution for parameters.

## We sample the prevalence parameter on the logistic-scale so that it is bounded by [-Inf, Inf] and not [0,1]
runMCMC <- function(iterations ## Number of mcmc iterations to run
                    , startvalue = runif(1, logit(.01), logit(.99)) ## random starting value
                    , proposerSD = .5 ## standard deviation of the gaussian proposal distribution (tuning parameter)
                    , verbose = 0){ ## for debugging
  if(verbose > 0) browser()
  chain <- array(dim = c(iterations+1, 1)) ## initialize empty iterations X 1 array
  chain[1,] <- startvalue ## set first value of chain
  
  for(ii in 1:iterations){ 
    # Symmetric proposal around current state
    proposal <- rnorm(1, chain[ii,], proposerSD) 
    
    # Calculate the MHratio -- First need to inverse logit prevalences, because we are sampling
    # using the logit scale originally
    MHratio <- exp(logLikePrior(inv.logit(proposal)) -  logLikePrior(inv.logit(chain[ii,])))
    ## If the MH-ratio is > 1, accept new value. Otherwise, accept it with probability equal to
    ## the the MH-ratio.
    if(runif(1) < MHratio){ 
      chain[ii+1,] <- proposal
    }else{ ## If rejecting it, stay at the last state.
      chain[ii+1,] <- chain[ii,]
    }
  }
  # Return the prevalence states on the correct scale
  return(inv.logit(chain)) 
}

iters <- 1000
posteriorSample <- runMCMC(iters, proposerSD = .1) ## sample 1000 times from posterior
par(mfrow=c(1,1), bty = 'n')
plot(posteriorSample, xlab='iteration', ylab = 'prevalence', main = 'posterior sample',
     type = 'l', las = 1)

hist(posteriorSample, xlab='prevalence', col = 'black',
     main = 'Posterior Prevalence',
     las = 1,
     breaks = seq(0,1,by=.01),
     xlim = c(0,1))

  
# Calculate median and 95% credible interval for prevalence
quantile(posteriorSample, probs=c(0.025,0.5,0.975))

# Calculate med and 95% CI removing burn-in period
quantile(posteriorSample[100:iters], probs=c(0.025,0.5,0.975))

# Calculate the acceptance ratio
runs <- rle(as.numeric(posteriorSample))$lengths
sum(runs)
acceptanceRatio <- 1 - sum(runs[runs>1]-1) / sum(runs)
acceptanceRatio


## Compare proposal distributions variances
## How do they influence the mixing of the chain?
par(mfrow = c(2,2), oma = c(0,0,2,0))
for(sdVal in c(.05, .1, .5, 1)) {
  posteriorSample <- runMCMC(1000, proposerSD = sdVal) 
  plot(posteriorSample, xlab='iteration', ylab = 'prevalence',
       main = bquote(sigma==.(sdVal)),
       ylim = c(0,1),
       type = 'l', las = 1)
}
mtext('posterior sample by proposer sd', side=3, line=0, outer=T)

## Same thing but with histograms
par(mfrow = c(2,2), oma = c(0,0,2,0))
for(sdVal in c(.05, .1, .5, 1)) {
  posteriorSample <- runMCMC(1000, proposerSD = sdVal) 
  hist(posteriorSample, xlab='prevalence', col = 'black',
       main = bquote(sigma==.(sdVal)),
       las = 1,
       breaks = seq(0,1,by=.01),
       xlim = c(0,1))
}
mtext('posterior sample by proposer sd', side=3, line=0, outer=T)

## Assess convergence and mixing of MCMC by running multiple chains at once
numChains <- 4
numIter <- 3000
for(ii in 1:numChains) 
  assign(paste0('posteriorSample',ii), ## assign is like "<-" but can be done with text strings to name things
         as.mcmc(runMCMC(numIter, proposerSD = .1))) ## use as.mcmc to help with Gelman-Rubin diagnostic function below

par(mfrow=c(1,1), bty = 'n')
plot(0,0, type = 'n', ## intialize
     xlim = c(1,numIter), ylim = c(0,1),
     xlab='iteration', ylab = 'prevalence', main = 'posterior sample',
     las = 1)
for(ii in 1:numChains) 
  lines(get(paste0('posteriorSample',ii)), col = rainbow(numChains)[ii]
  )

# Assess gelman-rubin diagnostic for convergence
# If <1.1 convergence is usually adequate
chainList <- as.mcmc.list(list(posteriorSample1, posteriorSample2, posteriorSample3, posteriorSample4))
class(chainList)
gelman.diag(chainList)


## How would we change MCMC sampler if we had two parameters?







##################################################
# Introduction to rjags
# Necessitates JAGS installed separately on computer
# Also need file ("linReg.jags") which will be in github repo
#  more info here: http://www.r-bloggers.com/getting-started-with-jags-rjags-and-bayesian-modelling/
##################################################
rm(list=ls())
library(rjags)
setwd("~/projects/CCBB_Intro_Biostats/introBayes/")
N <- 1000
x <- 1:N
epsilon <- rnorm(N, 0, 1)
y <- x + epsilon

# Model formulation in text file
# model {
#   for (i in 1:N){
#     y[i] ~ dnorm(y.hat[i], tau)
#     y.hat[i] <- a + b * x[i]
#   }
#   a ~ dnorm(0, .0001)
#   b ~ dnorm(0, .0001)
#   tau <- pow(sigma, -2)
#   sigma ~ dunif(0, 100)
# }

# Setup jags model for use in R
mod <- jags.model('linReg.jags',
                   data = list('x' = x,
                               'y' = y,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

# Burn in period
update(mod, 1000)

# Posterior samples
posterior <- jags.samples(mod, c('a', 'b'), 1000)

postA <- as.mcmc.list(posterior$a)
postB <- as.mcmc.list(posterior$b)

summary(postA)
summary(postB)

plot(postA)
plot(postB)

gelman.plot(postA)
gelman.diag(postA)
gelman.plot(postB)
gelman.diag(postB)


