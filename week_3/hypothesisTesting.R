##################################################
# Introduction to Hypothesis Testing
# Introduction to Biological Statistics
# CCBB Fall 2015
##################################################
# Spencer Fox
##################################################
# Some code/ideas adapted from the MMED workshop Summer 2015
# Those materials can be found here, and I highly suggest 
# the workshop for gaining experience in epidemiological modeling
# http://mmed2015.ici3d.org/schedule.html
# License: http://mmed2015.ici3d.org/license.html
##################################################

# Last week we spoke about generating our own data, 
# and using likelihood for parameter estimation.
# This week we will be exploring how we can use probability
# distributions, simulation, and permutation tests to investigate
# hypotheses about our data.
# First set a seed, which makes it so everyone who runs this script gets the same
# "random" results -- The number inside can be any number, like for example
# I chose the number 6 in support of Drake who is from Toronto and nicknamed it "The 6" 
rm(list=ls())
set.seed(6)

# Let's start with a familiar example:
# You're an epidemiologist tracking the flu during a pandemic, and you've sampled 
# 100 people from our population, and you've found 23 people to be sick.
# What can we say about the true prevalence? 
numPos <- 23
size <- 100

# Let's test a null hypothesis. Do our data support a null hypothesis that
# the true prevalence is 15%?
# This is the same as asking what is the probability of seeing a more extreme value
# than 20 given a true distribution of 15%?
# First we need to set our alpha value -- Our threshold for significance
alpha <- 0.05

# So let's look at our null distribution (This assumes 15% prevalence)
# What are the possible values we could have gotten positive from our sample of 100 people?
pos <- 0:100

# The probability of getting these values with 15% prevalence
hypPrev <- 0.15
prob <- dbinom(pos, size=size, prob=hypPrev)

# Which of these values are as extreme or more extreme than our value
extremeVals <- pos[pos>=numPos]

# Create a color vector for our plot and make extreme values red
color.vec <- rep("grey", size+1)
color.vec[extremeVals] <- "red"

# Plot our hypothetical distribution of samples if the true prevalence was 15%
# red bars indicate samples more extreme than ours (one tail)
barplot(prob,
        xlab = "number flu+",
        ylab = "probability",
        col = color.vec ,
        border = NA,
        main = "Distribution if 15% prevalence",
        space = 0,
        xlim = c(0, 40))

# So that's a great visual, but how do we figure out that probability?
# There are two probability functions we didn't speak about last week: p and q
# p (default) - calculates the probability of getting a value 
# smaller than or equal to our sample. Cumulative distribution function
# q (default) - calculates the value that has a cumulative 
# distribution of a certain probability. Inverse cumulative distribution function
# These functions give inverse values: 
curve(dnorm(x), from=-2, to=2)
pnorm(0)
qnorm(0.5)

# So what is the probability of seeing something more extreme than our value?
pVal <- pbinom(numPos, size = size, prob = hypPrev)
pVal <- 1- pVal
# Why do I find 1- the probability?

# We could have also done this directly:
pVal <- pbinom(numPos, size = size, prob = 0.15, lower.tail = FALSE)

pVal
# Is that our p value? Not quite, but almost! What other values would be
# more extreme than that value? 
pVal <- 2*pVal
pVal
pVal < alpha

# Our p-value is significant with our threshold, but what does this mean?
# Our data support a rejection of the null hypothesis that the true prevalence is 15%
# We could also do this a different way. We can reject the null hypothesis
# if our data our more extreme than 95% of the samples under our null, so we can find the
# values for the 95% CI and compare to our data:
hypRange <- qbinom(p = c(0.025,0.975), size = size, prob = hypPrev)
hypRange

# Before going on, let's go back, and see what happens with a hypothetical prevalence of 20%


####################################################
# So hopefully you can see we could use this method to 
# find all hypothetical prevalences our data support, so let's do that
possibleTruePrevalence <- seq(from = 0, to = 1, length.out = 10000)

findPVal <- function(hypPrev, numPos, size){
  # Our number could be above or below the mean, which
  # means we need to sample from different tails
  # depending on where it is
  if(numPos < hypPrev*size){
    # If it's less then the mean use the left tail
    pValOneSide <- pbinom(numPos, size, hypPrev, lower.tail = TRUE)
  }else{
    # if it's more than the mean use the right tail
    pValOneSide <- pbinom(numPos-1, size, hypPrev, lower.tail = FALSE)
  }
  # Return the two tail p value
  2*pValOneSide
}

pVals <- sapply(X = possibleTruePrevalence, FUN = findPVal, numPos = numPos, size=size)

# Let's look at this in a plot
plot(possibleTruePrevalence, pVals, xlab = "hypothetical prevalence (null hypothesis)", ylab = "p-value",
     lwd = 2, bty = "n", type = "l", cex.lab = 1, col = "red")
## Draw a line at the alpha = .05 cutoff.
segments(0, .05, 1, .05, lty = 2, lwd = 3)

# What true prevalences can we not reject with our data?

nonRejected <- possibleTruePrevalence[pVals > .05]
ci <- range(nonRejected)
ci

## Now plot the CI bounds
ci.l <- round(ci[1],3)
ci.u <- round(ci[2],3)
arrows(ci.l, .4, ci.l, -.03, length = .2)
arrows(ci.u, .4, ci.u, -.03, length = .2)
text(ci.l,.4, ci.l,cex = 1,pos = 3)
text(ci.u,.4, ci.u,cex = 1,pos = 3)

###########################################
# Sampling for hypothesis testing
###########################################
# So one powerful way for hypothesis testing is to set up a null distribution,
# sample from that distribution, and compare it to your data to accept/reject.
# We'll start with a basic example where we have a well-defined distribution, but
# then show how this technique can be used for any distribution that generates your data

# Let's go back to this flu prevalence example for one quick second.
# If we wanted to test whether the true prevalence was 15%, since we know
# the distribution (binomial), we could take a bunch of samples from it and then
# compare that to our data (numPos=23) in this case
binSample <- rbinom(100000, size = size, prob = 0.15)

oneSidePVal <- sum(binSample > numPos)/100000 # one tailed test
oneSidePVal < alpha/2

# Do our data support acceptance or rejection based on this result?

# This was a basic example, where we don't need to do this, because we could
# use the q or p function with binom, but what if we have a weird distribution
# that R doesn't have premade for us? As long as we can sample from it
# we can hypothesis test with our data -- See the end of this script for how to do that


###########################################
# Permutation Tests -- My favorite
###########################################
# Okay new example
# You are a fish researcher, and you're investigating the influence of 
# hormones on the weight (measured in grams) of your fish
# You set up an experiment to test whether hormones influence their weight
# These are your data from 10 fish for each treatment:
hormoneWt <- c(35.2, 38.3, 36.4, 32.1, 37.8, 32.6, 29.3, 37.3, 38.9, 40.1)
controlWt <- c(30.1, 35.4, 32.3, 28.5, 29.4, 29.1, 33.6, 34.3, 31.2, 34.8)

mean(hormoneWt)
sd(hormoneWt)
mean(controlWt)
sd(controlWt)

# So did the treatment have any effect on the fish weight?
# It's hard to tell, we could make an assumption, and say that the fish weight
# is probably normally distributed, and then run 
# a test similar to the example above right? 
# But what if we don't want to make that assumption?
# This is where permutation tests come in to play. So how should the means of the 
# hormone and control groups compare if there was no effect?
# Under a null hypothesis, how do we expect the treatment group to influence the fish weight?


# Right, so if there is not effect, then the treatment identity shouldn't influence the results, 
# so we can shuffle the identities and their should be no influence on our data.
# So what is the difference in means we saw in our data?
ourDiff <- mean(hormoneWt) - mean(controlWt)

# Let's now create a dataframe that holds our identities and values
treatmentState <- c(rep("control",10), rep("treatment", 10))
weights <- c(controlWt, hormoneWt)
fishData <- data.frame (treatmentState, weights)

permuteIDs <- function(data){
  # Function takes in two column data frame, shuffles the column
  # "treatmentState", and then calculates the mean difference
  # in weights of the two treatment classes
  data$shuffledTreatment <- sample(data$treatmentState)
  mean(data$weights[data$shuffledTreatment=="treatment"]) - 
    mean(data$weights[data$shuffledTreatment=="control"])
}

permuteIDs(fishData)

# Run that a lot of times, and look at that distribution
diffs <- vector(mode = "numeric", length = 10000)
for(i in 1:10000){
  diffs[i] <- permuteIDs(fishData) 
}

# Plot the results of the test
hist(diffs, freq = FALSE)
arrows(x0 = ourDiff, y0 = 0.8, x1 = ourDiff, y1 = 0, length = .2, col="red",lwd=3)
 
# What does this mean about the treatment group?
# Let's find a confidence interval
fishCI <- quantile(x = diffs, probs = c(0.025, 0.975))
fishCI
ourDiff > fishCI

# Let's say now we also have a water quality metric (score out of 100)
# Assume 5 fish were in each tank
fishData$waterQual <- c(rep(85, 5), rep(90, 5), rep(93, 5), rep(95, 5)) 

# Does it seem like the water quality is correlated with weights?
cor(fishData$waterQual, fishData$weights)
# Maybe, but how could we use a permutation test to test this?
# Try coding it up yourself!
















###################################################
# Example of sampling from a weird distribution
###################################################
# For example, let's say you go out and measure the heights of students at UT,
# and you're new to this whole thing and forget to measure the genders
# What might this distribution look like? would it be normally distributed?
hist(rnorm(10000, mean = 66, sd = 3), freq=FALSE)
curve(dnorm(x, mean=66, sd=3), add=T)

# Maybe, but likely you would have a bimodal distribution corresponding to males and females
# How could we make a function to sample from that distribution?
singleSample <- function(maleMean, maleSD, femaleMean, femaleSD){
  # First we have to figure out which distribution we are sampling from
  # Let's assume that 51% of our population is female, and that we have an equal chance of selecting 
  # any individual, then with what probability should we sample from our male/female populations?
  prob <- runif(1, min = 0, max = 1)
  
  if(prob > 0.51){
    # Sample from Male distribution
    return(rnorm(1, mean=maleMean, sd=maleSD) )
  } else{
    # Sample from Female distribution
    return(rnorm(1, mean=femaleMean, sd=femaleSD) )
  }
}

# Let's assume that from other studies, we already know the true mean and sd for
# the college population we're looking at 
# for sampling hypothesis testing you need to know the data generating process
singleSample(maleMean = 68, maleSD = 2, femaleMean = 62, femaleSD=2)

# Can you tell which distribution it sampled from?
# Let's take a lot of samples and look at what this looks like
samps <- vector(mode = "numeric", length = 10000)
for(i in 1:10000){
  samps[i] <- singleSample(maleMean = 68, maleSD = 2, femaleMean = 62, femaleSD=2) 
}

hist(samps, freq=FALSE, breaks = 50)
# Awesome, so now you can clearly see the two peaks
