
##################################################
# Introduction to Probability and Likelihood in R
# Introduction to Biological Statistics
# CCBB Fall 2015
# Spencer Fox
##################################################


rm(list=ls())
set.seed(808)

# Basic Probability in R - Random sampling
# Defining a sample space
coinFlip <- c("heads", "tails")

# Sample once from the created vector
sample(x = coinFlip, size = 1)

# Now sample many times, and output the result
flips <- sample(x = coinFlip, size = 100000, replace = T)
table(flips)

# Create a vector for the sample space of rolling a die
# sample from your vector and output the results as in the coin flip above


#################################################
# sample is a convenient example function, but isn't often used in practice
# most random sampling is done in R using probability distributions
# Here is a list of frequently used probability distributions and their abbreviations:

# Beta                  beta
# Binomial              binom
# Chi-square            chisq
# Exponential           exp
# Gamma                 gamma
# Negative Binomial     nbinom
# Normal                norm
# Poisson               pois
# Uniform               unif

# R has four functions available for each probability distribution (r, d, p, and q)
# r - random values from a distribution
# d - density of a distribution at a certain value
# p - cumulative probability up to a certain value (is the output)
# q - inverse cumulative probability (returns the number that has a cumulative probability of a certain value)

# These functions are accessed in the format "function"+"probability distribution abbreviation"
# For example, if we want to generate random numbers from a normal distribution: rnorm()
# if we want to generate density values for a poisson distribution: dpois()
# Each of the four functions and all of the distributions take different inputs
# look at the help pages for the functions to get a better idea of how to run them

# We will only focus on the 'r' and 'd' functions today, and will speak about 
# 'p' and 'q' functions more with hypothesis testing next week

####################################################
# Basic probability in R - Distributions
# Okay so if we don't normally use sample, what do we use?
# Well let's go back to the coinflip real quick
# a coin flip is really a bernoulli trial with 50% probability of success
# a bernoulli trial is a single binomial trial, so we can use rbinom() to sample
# Take a lot of samples and look at the result
flips <- rbinom(n=100000, size = 1, prob = .5)
# Imagine heads=0 and tails=1
table(flips)

# Okay, so now lets look at some distributions
# First generate some data from a normal distribution
plotDat <- rnorm(n = 10, mean = 0, sd = 10)

# The function hist takes in a vector of values and simply creates a histogram of the values
# adding freq=FALSE turns counts into probabilities for the y-axis
hist(plotDat, freq = FALSE)

# Now let's add a line that contains the analytical distribution to our plot to compare
# lookup ?curve if interested
# We use dnorm this time, because we are asking for the 
# density of the distribution at every value of x plotter
curve(dnorm(x, mean = 0, sd = 10), add=TRUE)

# Okay that doesn't look very normal, go back and generate many more random numbers (~10000)
# Plot it again, does it look better? (hint altering the breaks parameter for ?hist might help if not)

# So let's now assume that you're an epidemiologist tracking the flu
# during a pandemic, and assume 15% of the population is infected 
# with the flu currently. If we sample 100 people form our population how
# many do we expect to find sick? We expect our sample to follow a binomial 
# distribution with a probability of success equal to the current prevalence (0.15)

numPos <- rbinom(n = 1, size = 100, prob = 0.15)
numPos

# That value corresponds to how many you found to be positive
# but what does that distribution look like?

hist(rbinom(n=1000, size=100, prob=0.15), freq=F)
arrows(x0 = numPos, y0 = 0.8, x1 = numPos, y1 = 0, length = .2, col="red",lwd=3)

# So maybe your value looks close to the center, maybe not.
# What was the likelihood that we observed that value?
dbinom(x = numPos, size = 100, prob = .15)

# What does that mean?
# If we know that our data generating process (disease prevalence) follows
# a binomial distribution of size 100 and success probability of 0.15 then
# the likelihood of observing our numPos number is whatever that value was for you
# Is that useful? maybe, but probably not!
# usually more what we're interested in is finding the maximum likelihood 
# estimate for a parameter, because we never know the true value

############################################
# Basics of Likelihood

# Rarely do we ever know the true disease prevalence (or whatever parameter of interest you fancy)
# So we go collect some data, and see how likely those data are under a number of 
# hypothetical scenarios. For example, what was the probability of getting our number of positives
# with the true prevalence of 15%? what about if the true prevalence was 30%? 45%?
dbinom(x = numPos, size = 100, prob = .15)
dbinom(x = numPos, size = 100, prob = .3)
dbinom(x = numPos, size = 100, prob = .45)

# Ah, so those likelihoods are pretty different right?
# Well now lets look at the likelihood we would get our sample from all possible probabilities
probs <- seq(0,1,length.out = 1000)
likelihoods <- dbinom(x = numPos, size = 100, prob = probs)
plot(probs, likelihoods, type = "l", lwd=2)

# Any guesses to what that maximum likelihood estimate corresponds to?
# Graphs are great, but now let's try to get an actual estimate for the 
# influenza prevalence from our data
# First let's create a function that gives back the negative log likelihood of our model

#Sidebar:
# We use negative log-likelihood, because it simplifies calculations and historical purposes
# Don't quote me on that above statement, but I've never heard a real reason for using it other than that

# Okay so when we create functions, we need to think of three main things
# What do we want to do, what do we need to do it, and what do we want to return
# In our case, we want to return the likelihood of observing our data points given
# any hypothetical prevalence rate
negLL <- function(fitPrev, sampleData, size=100){
  - dbinom(sampleData, size = size, prob = fitPrev, log = T)
}

# Now we will use a function called optim
paramEstimate <- optim(par = 0.1, fn = negLL, sampleData = numPos, hessian=T)

paramEstimate$par
# Great, we were able to get an MLE which makes sense


######################################################
# Now take multiple (~10) samples from your population with 15% prevalence
# How would you alter the functions above to find the MLE of those multiple samples?
# Code it below
######################################################





