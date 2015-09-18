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
# 100 people from our population, and you've found 25 people to be sick.
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
pnorm(0)
qnorm(0.5)

# So what is the probability of seeing something more extreme than our value?
pVal <- pbinom(numPos, size = size, prob = hypPrev)

# Why do I find 1- the probability?
pVal <- 1- pVal
# We could have also done this directly:
# pVal <- pbinom(numPos, size = size, prob = 0.15, lower.tail = FALSE)

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
hypCI <- qbinom(p = c(0.025,0.975), size = size, prob = hypPrev)
hypCI

# Before going on, let's go back, and see what happens with a hypothetical prevalence of 20%


####################################################
# So hopefully you can see we could use this method to 
# find all hypothetical prevalences our data support, so let's do that
possibleTruePrevalence <- seq(from = 0, to = 1, length.out = 10000)

findPVal <- function(hypPrev, numPos, size){
  if(numPos < hypPrev*size)
  {
    pValOneSide <- pbinom(numPos, size, hypPrev, lower.tail = TRUE)
  }else{
    pValOneSide <- pbinom(numPos-1, size, hypPrev, lower.tail = FALSE)
  }
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
# Permutation Tests
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




