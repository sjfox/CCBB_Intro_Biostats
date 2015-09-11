
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

# These functions are accessed in the format "letter"+"probability distribution abbreviation"
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
# One option is to draw a uniform number between (0,1), 
# and call heads >0.5, and tails everything else
flips <- runif(n = 10000, min = 0, max = 1) > 0.5
table(flips)

# a coin flip can also be thought of as a bernoulli trial with 50% probability of success
# a bernoulli trial is a single binomial trial, so we can use rbinom() to sample
# Take a lot of samples and look at the result
flips <- rbinom(n=100000, size = 1, prob = .5)
# Imagine heads=0 and tails=1
table(flips)

# So you can see once again that there are a number of ways of doing things in R
# Now lets look at some distributions
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

##########################
# Now plot a random sampling from a poisson distribution of rate 5 in the same fashion
# Be careful, poisson distributions can only take in integer values!
# hint: use the function round() or ceiling() -- compare these two function outputs



####################################################
# Great now your experts in generating random numbers from distributions
# So now let's now assume that you're an epidemiologist tracking the flu
# during a pandemic, and assume 15% of the population is truly infected 
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
# What was the likelihood that we observed that value given the true distribution?
dbinom(x = numPos, size = 100, prob = .15)

# What does that mean?
# It means that under that model (prevalence of 0.15) we had that probability
# of sampling our number

# In this case, we knew the underlying model (we created it with 15% flu prevalence)
# usually we don't know the model, and thus have to come up with a "best guess" based on our data
# This is where likelihood comes into play, and where we look for the maximum likelihood estimate (MLE)

############################################
# Basics of Likelihood

# Rarely do we ever know the true disease prevalence (or whatever parameter of interest you fancy)
# So we go collect some data, and see how likely those data are under a number of 
# hypothetical model scenarios. In this case our model is 15% prevalence, so we can figure out
# the probability of getting our number of positives with the true prevalence of 15% this way:
dbinom(x=numPos, size=100, prob=0.15)

# But if we don't know the true prevalence, we can't just look at this one value
# Find the probability of getting our number of positives if the true prevalence was 5%, 30%, or 45%.
# How do those numbers compare, and how do they compare to our number of positives?


# Well now lets look at the likelihood we would get our sample from all possible probabilities
# First create a vector of probabilities to test
probs <- seq(0,1,length.out = 10000)

# Now calculate the likelihood of observing our data for each of those values
likelihoods <- dbinom(x = numPos, size = 100, prob = probs)

#Plot the result
plot(probs, likelihoods, type = "l", lwd=2)

# Any guesses to what that maximum likelihood estimate corresponds to?
# let's try to get an actual estimate for the influenza prevalence from our data
# Here is one crude way from what we already have to find the maximum likelihood estimate
probs[match(max(likelihoods), likelihoods)]

# Does that value make sense based on our data?
# Draw a different numPos sample, and rerun the code. What do you notice about the estimate?


###############################
# Often the MLE is not so easy to find, so we will now create a simple likelihood function
# and solve it using the optim() function, which is a much more powerful way to get MLEs
# First let's create a function that gives back the negative log likelihood of our model

# Sidebar:
# We usually minimize the negative log-likelihood (deviance) 
# rather than maximize the likelihood, for calculation simplicity

# Okay so when we create functions, we need to think of three main things
# What do we want to do, what values do we need to do that, and what do we want to return
# A general formula for creating functions is as follows: 
# function_name <- function(parameters, for, function){
#      Do things with parameters
#      Return whatever is desired result
#}

# In our case, we want to return the likelihood of observing our data points given
# any hypothetical prevalence rate
# So we know we are going to need all of the inputs for the dbinom function
# these include our sampled data, the size of our sample, and the hypothetical prevalence we're testing
# We call the hypothetical prevalence "parameters" because often you will run MLE for multiple parameters at once
# Our sampleData in the current case will just be the numPos we observed, and the size wil be a constant 100,
# because that's what we did in this case.
negLL <- function(parameters, sampleData, size=100){
  # Then just return the negative of the log-likelihood like we calculated earlier
  - dbinom(sampleData, size = size, prob = parameters[1], log = T)
}



# Now we will use a function called optim
# Normally with only 1 parameter, we would use a different function, 
# but we want to show you optim so that you can use it for more complicated models later on
# so ignore the warning for now
# Optim takes in a vector of parameters "par", a function "fn", and then has many other options
# We enter in the value of the sampleData for our function in these extra options
# We also set hessian=T, because this relates to the precision of our estimate (standard error)
paramEstimate <- optim(par = c(0.1), fn = negLL, sampleData = numPos, hessian=T)
# note that if you were optimizing with multiple parameters, you would have to give multiple starting values

paramEstimate$par
# Great, we were able to get an MLE which makes sense

# now what about the standard error of the MLE?
# this is equal to the square root of the inverse of the Hessian, 
# which are the second derivatives of the log-likelihood (in matrix form with multiple parameters).
standardError <- sqrt(1/paramEstimate$hessian)
confidenceInterval <- c(paramEstimate$par-1.96*standardError, paramEstimate$par+1.96*standardError)
confidenceInterval

# Are we very confident in our estimate or not? Why might that be?
# What does this mean in terms of how prevalent we think flu is in the population?

######################################################
# Let's say now you go out and take ~10 random samplings of 100 individuals from the population.
# How would you alter the function and optim above to find the MLE of those multiple samples?
# Take the samples, and code up the new function and optimize
# How do your estimates compare to the true prevalence (15%)?
######################################################




