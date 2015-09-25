####################################
## Example code		              ##
## Sept. 20 2013, Linear Models   ##
## Intro to Biological Statistics ##
####################################

# R version 3.0.1

# TO READ IN THE DATA:
# set your working directory, as the directory where you have put the
# data file, 'Bombus_pesticide.csv'

Bombus <- read.csv("Bombus_pesticide.csv", header=T)

## When using functions from packages that are not loaded by default,
## I use the syntax 'package::function()' to make clear which function is
## in which package.
## This is NOT necessary and generally not preferred. Normally we would
## load a package with 'library(package)' and then just use the functions
## as usual.

library(MASS)		# version 7.3-29
library(bbmle)		# version 1.0.13
library(car)		# version 2.0-18
library(multcomp)   # version 1.3-2

pairs(Bombus) # motivating example: neonicotinoids and bumble bee colony fitness, data from [Whitehorn et al. 2012 Science]

###################
## Model Fitting ##
###################
# linear model
weight_lm <- lm(max.weight ~ start.weight + start.workers + treatment, data = Bombus )

# model summary for lm
print(weight_lm) # partial summary
summary(weight_lm) # full summary
summary(weight_lm)$coef # just the table of coefficients

# some extractor functions ...
str(weight_lm) # the fitted model object has many useful components ...
# such as ...
formula(weight_lm) # formula
resid(weight_lm) # residuals
fitted(weight_lm) # fitted values
coef(weight_lm) # estimated coefficients
vcov(weight_lm) # variance-covariance matrix of predictors
model.matrix(weight_lm) # model matrix
logLik(weight_lm) # log likelihood

#####################
## Model selection ##
#####################
## illustrates update() for changing predictor variables without new call to lm()
full_model <- weight_lm 									# full model is our 'weight_lm' from above
reduced_model <- update(full_model, . ~ start.weight) 		# model with only start.weight
null_model <- update(full_model, . ~ 1) 					# model with only intercept

## model selection via AIC
# compute AIC for single model
AIC(full_model) 											# the AIC of the full model
bbmle::AICc(full_model) 									# the second-order AIC -- corrected for small sample size
# compute AIC for multiple models
bbmle::ICtab(full_model, reduced_model, null_model, type="AIC", base = T, weights = T)	# table of information criteria for multiple models

## automated model selection (generally a bad idea)
MASS::stepAIC(null_model, scope = formula(full_model) )

#################
## Diagnostics ##
#################
# three related themes:
# (1) verifying that assumptions of model are met
# (2) detecting outliers
# (3) detecting influencial observations

## residuals are the primary tool in regression diagnostics.
resid(weight_lm) 		 # we use for assessing normality
rstandard(weight_lm)	 # scaled to standard deviation
rstudent(weight_lm)		 # 'studentized' -- standardized by residual-specific variance (in this case practically identical to standardized)
# we expect these to follow a 't' distribution, not a normal distribution
# use studentized for detecting outliers, heteroskedasticity. b/c they are closer to 'theoretical' errors

# here is a normal linear model fit to a non-normal response, to see what a BAD fit looks like
reprod_lm <- lm(reproductives ~ max.weight + treatment, data = Bombus)

## assumption: normality of residuals
# quantile-quantile of regular residuals
plot(weight_lm, which = 2) 							# 'which' specifies what diagnostic plot
car::qqPlot(rstandard(weight_lm)) 					# quantile-quantile of residuals with 95% confidence
car::qqPlot(rstandard(reprod_lm))					# this is an example of non-normal residuals. Notice the deviation from the tails

# graphical summaries are the best, but also possible to use statistical test for normality
# note that with low sample size, power to detect deviations from normality is low
shapiro.test(resid(weight_lm)) 		# p is probability that data are normal -- worry if significantly low
shapiro.test(resid(reprod_lm))		# and on the non-normal response, it gives a significantly low value

## assumption: homoskedasticity
# residuals vs. fitted
plot(weight_lm, which = 1) 		# scaled residuals
plot(reprod_lm, which = 1)		# and an example of heteroskedasticity with the non-normal response model
# residuals vs. groups
plot(rstudent(weight_lm) ~ Bombus$treatment, ylab="Studentized Residuals") 	# by treatment group

## assumption: linearity
plot(weight_lm, which = 3) # look for systematic non-linear trend
plot(reprod_lm, which = 3) # such as this! For non-normal response model
car::ceresPlots(weight_lm)	# CERES plots -- look for non-linear trend with each predictor -- won't work with interactions

## collinearity
vif(weight_lm)							# variance inflation factors for our model
collinear_model <- lm(reproductives ~ max.weight + range.weight, data=Bombus) # here is a collinear model
car::vif(collinear_model) 		 		# variance inflation factors for collinear model
sqrt(car::vif(collinear_model) ) > 2 	# rule of thumb ... if "TRUE" than worry

## Outliers
# outliers are on tail of observation -- studentized residuals that are beyond 2 standard deviations (red lines in plot)
plot(fitted(weight_lm), rstudent(weight_lm) ); abline(h = c(0, -1.96, 1.96), col = c("gray", "red", "red"), lty = c(1,3,3))
is_outlier <- which(abs(rstudent(weight_lm)) > 2) # detect which observations are outliers
text(fitted(weight_lm)[is_outlier], rstudent(weight_lm)[is_outlier], labels = is_outlier, pos = 4)	# label outliers on plot
qqPlot(weight_lm)	# look for outliers in the tails of qq plot -- by default uses studentized residuals

## Influence
# hat values, aka leverage
hat_values <- hatvalues(weight_lm)
hat_values[which(hat_values > 2*mean(hat_values) )] # rule of thumb: check out hat values greater than twice the mean hat value 

# influence measures (more than you want)
influence.measures(weight_lm)
influence.measures(weight_lm)$is.inf # flagged observations for each statistic -- these are worth checking out

# Cook's distance: combines outlying-ness with influence
cooks.distance(weight_lm)
plot(weight_lm, which = 5) # absolute standardized residuals vs. leverage (dotted red line -- not apparent here -- is threshhold for worrying)
plot(reprod_lm, which = 5) # an example where you CAN see threshhold

################
## Prediction ##
################
predict(weight_lm)	# returns fitted values
to_predict = expand.grid(start.weight = c(200, 400, 600), treatment = c("Low", "High", "Control"), start.workers = 20)	# data frame with covariate levels we want to predict response for
predicted_response <- predict(weight_lm, newdata = to_predict, se.fit=T)  # the predicted response
print(predicted_response)
plot(predict(weight_lm), Bombus$max.weight); abline(a=0,b=1) # fitted vs. observed values

########################
## Hypothesis Testing ##
########################
## t- and z- tests of single parameters
summary(weight_lm)$coef # significance tests of single parameters; what is the probability that some coefficient equals 0?
test_statistic <- coef(summary(weight_lm))[,'Estimate'] / coef(summary(weight_lm))[,'Std. Error'] # calculate t- (or z-) statistic by hand
p_value_t <- (1-pt(abs(test_statistic), df=70))*2 # calculate t-statistic p-value by hand
p_value_t	# these are given by default in summary(lm)
p_value_z <- round((1-pnorm(abs(test_statistic)))*2, 5) # calculate z-statistic p-value by hand (assumes that the number of degrees freedom is infinite -- ie. variance is known)
p_value_z	# less conservative
## confidence intervals for parameters
# by hand
ci95_t <- rbind(upper=coef(summary(weight_lm))[,'Estimate'] + qt(0.975, df=70)*coef(summary(weight_lm))[,'Std. Error'],
				lower=coef(summary(weight_lm))[,'Estimate'] - qt(0.975, df=70)*coef(summary(weight_lm))[,'Std. Error'])
ci95_t
# shortcut
confint(weight_lm)

## test of all pairwise combinations, for ANOVA
# many ways to do this, but all try to take into account non-independence of tests
# here is one popular way: Tukey's Honest Significant Difference (only works on aov object)
weight_aov <- aov(max.weight ~ treatment + start.weight + start.workers, data = Bombus)
TukeyHSD(weight_aov)
# check out multcomp package for many approaches to multiple testing in more complex models
multcomp::glht(weight_lm, linfct = mcp(treatment = "Tukey")) ## for same old Tukey test

## F-tests
anova(weight_lm) # ANOVA table for Type I sum of squares
car::Anova(weight_lm, type = "3") # ANOVA table for Type III sum of squares

## likelihood ratio test
full_model <- weight_lm
reduced_model <- update(weight_lm, .~.-start.weight) # update allows easy refitting of models
anova(full_model, reduced_model)	# likelihood ratio test -- indicates that full model is significantly better

## Goodness of fit: R-squared
# extract R-squared and associated F statistic, compute P
R_sq <- summary(weight_lm)$r.squared
R_sq_F <- summary(weight_lm)$fstatistic
pf(R_sq_F[["value"]], R_sq_F[["numdf"]], R_sq_F[["dendf"]], lower = F) # p-value for F statistic -- this is given in model output
# Omnibus F-test on R2, by hand
k <- weight_lm$rank - 1                     # number of parameters additional to intercept
n <- nrow(Bombus) 							# number of observations
F_stat = (R_sq / k) / ((1-R_sq)/(n-k-1))    # F statistic for R-squared
pf(F_stat, k, n-k-1, lower = F)				# P-value for F statistic

#########################
## Data transformation ##
#########################
# AIC for log-transformed lm() is NOT comparable with vanilla lm()
norm_lm <- lm(max.weight~start.weight, data=Bombus) # vanilla, plain-old normal model
lognorm_lm <- lm(log(max.weight)~start.weight, data=Bombus) # normal model with log transformed response
logLik(norm_lm) # on scale of original (untransformed) data
logLik(lognorm_lm) # on scale of transformed data
bbmle::AICtab(lognorm_lm, norm_lm, base = TRUE) # this is NOT a valid comparison!

# calculate AIC by hand for lognormal model
lognorm_sigma <- summary(lognorm_lm)$sigma		# estimated variance (for log-normal data)
lognorm_mu <- fitted(lognorm_lm)				# expected value (on log scale) for each data point
lognorm_np <- lognorm_lm$rank					# number of parameters in model
lognorm_logLik <- sum(dlnorm(Bombus$max.weight, meanlog = lognorm_mu, sdlog = lognorm_sigma, log=T)) # likelihood of log-normal distribution
lognorm_AIC <- -2*lognorm_logLik + 2*lognorm_np # AIC

AIC(norm_lm) # AIC for normal model ...
lognorm_AIC # AIC for lognormal model ... lognormal is preferred by AIC

#############################################
## Simulation and the parametric bootstrap ##
#############################################
# can also use package 'boot' (included in base distribution)
# but for the sake of illustration I show the parametric bootstrap by hand
# this section makes heavy use of apply() (a very useful function)

set.seed(1001) 							# sets a random seed -- causes random number draws to be repeatable
sims <- simulate(weight_lm, n = 999) 	# simulate new data from model 999 times
head(sims) 								# rows are data points, columns are simulations
test_statistic = function(new.y) { 		# function to compute statistic of interest for a simulated dataset
	model <- update(reprod_lm, new.y~., data = data.frame(new.y, Bombus) )
	return(coef(model)) 				# in this case the statistic is just the model coefficients
	}
test_sims <- apply(sims, 2, function(y) test_statistic(y) ) # calculate statistic of interest for each new dataset

# we can get confidence intervals for the model coefficients ...
apply(test_sims, 1, function(.) quantile(., c(0.025, 0.975)) )

# or get a 'parametric bootstrap' p-value for the model coefficients ...
# for an example, consider the intercept (first row of simulation object)...
sum(test_sims[1,] >= 0) / length(test_sims[1,]) # we ask, what proportion of the 999 simulations were greater than 0?

# An example of diagnostics: how many outliers are expected given our model?
# Here we use simulation to check the consistancy of our observed data with our estimated model
outlier_calc <- function(new.y) {	# function to refit model and find outliers
	model <- update(reprod_lm, new.y~., data = data.frame(new.y, Bombus))
	return(sum(abs(rstudent(model) > 2)))	# returns number of observations with absolute studentized residuals greater than 2
	}
how_many_outliers <- apply(sims, 2, outlier_calc )						# calculate the number of outliers in each simulation
observed_outliers <- sum(rstudent(weight_lm) > 2)						# calculate the number of outliers in our observed data
hist(how_many_outliers); abline(v = observed_outliers, col = "red")		# distribution of number of outliers in simulations, with observed number shown by line
sum(observed_outliers > how_many_outliers) / length(how_many_outliers) 	# probability value
# in this case, the amount of outliers in our data is low with regards to what the model predicts -- could indicate overfitting

## can be used to test main effects
## the parametric bootstrap is super useful for complex linear models! (i.e. mixed-effect and hierarchical GLMs)
