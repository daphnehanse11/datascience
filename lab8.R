###############################################################
#######   GLM with One Binary Predictor (a.k.a t-test)  #######
###############################################################

###OUTLINE:
#1. NHST version of an independent t-test (interpreting output, confidence intervals, effect size)
#2. Bayesian version of an independent t-test (interpreting output, HDI's, effect size)

## Set Working Directory
setwd("~/Desktop/315/Spr24/Lab/data")

## Check to see if package is installed, install if not
if (!require("emmeans")) install.packages("emmeans")
if (!require("bayesplot")) install.packages("bayesplot")


## Load packages
library(psych)
library(car)
library(brms)
library(tidyverse)
library(bayesplot)
library(emmeans)

## Get rid of the pesky scientific notation
options(scipen = 999) 

### We're going to focus on predicting course evaluations (eval) from tenure status (0 = No, 1 = Yes)
## Generate an experimental hypothesis to guide our analysis:

## Alt 1: Tenured instructors received higher evaluations compared to non-tenured instructors 
# Why might this be?

## Alt 2: Non-tenured instructors received higher evaluations compared to tenured instructors
# Why might this be? 

#### Either case seems plausible. So we will implement a 2-sided 'test'. 
## Null: MD = 0 --> Expected difference in evaluation is 0
## Two-Sided Alternative: MD != 0 --> Expected difference in evaluation is not 0

## Load Data
tr <- read.csv("teachingratings.csv", stringsAsFactors = T)
str(tr)
summary(tr)

## Let's start by creating a factor version of tenure status
tr$tenureF <- factor(tr$tenureY, levels = c(0,1), labels = c("No", "Yes"))
## NOTE: By default the reference group (i.e., the group represented by the intercept) will be the one that comes FIRST alphabetically
levels(tr$tenureF) # 'No' will be our reference group

## Generating summary statistics of eval scores by tenure status
describeBy(tr$eval, group = tr$tenureF, IQR = T) 
#which group's average evaluation is lower? 

## looking at the association and checking for outliers with a boxplot
plot(tr$eval ~ tr$tenureF,
     main = "Evaluation by Tenure Status",
     xlab = "Tenure Status",
     ylab = "Course Evaluation",
     col = "Grey")

## Checking that there are sufficient observations in each group
table(tr$tenureF)

# So, let's get into some inferential statistics and compare these mean differences 
# using the GLM equivalent of a t-test!

################# NHST Version #####################

## Check out the distribution of our DV (evaluations)
hist(tr$eval)

## Fit the model
m1 <- glm(eval ~ tenureF, data = tr, 
          family = "gaussian", na.action = na.exclude)


######### First, let's check model assumptions:

### 1. Normality of Residuals (test statistics and p-values rely on this):
hist(residuals(m1), probability = T, ylim = c(0, 0.8)) #histogram overall
summary(residuals(m1))

# Superimpose density curve (no assumptions made about normality)
lines(density(residuals(m1), na.rm=T), lwd = 2, col = "red")
# Superimpose best fitting normal curve
curve(dnorm(x, mean = mean(residuals(m1), na.rm=T), sd = sd(residuals(m1), na.rm=T)),
      lty = 2, lwd = 2, add = TRUE, col = "blue")

# the closer the two lines are together, the 'better' the normality assumption is met 

## By Group: 
qqPlot(residuals(m1), groups = tr$tenureF) #by group

### 2. Homoscedasticity (Constant Variance):
# A violation of the constant variance assumption results in inaccurate confidence intervals 
# and p-values, even in large samples, although regression coefficient estimates will still be unbiased 
car::residualPlots(m1,
                   pch=20, col="gray",
                   fitted = T,
                   ask = F, layout = c(1,2),
                   tests = F, quadratic = F)



### 3. Linearity
# irrelevant with one binary predictor. 
# predictions are for the mean of each group which will always be a straight line


### 4. Independence
# data were drawn from a simple random sample. assumption met by default 


#### Our goal is to be as transparent as possible and contribute to replicable science. 
# So, what would we say about the residuals in terms of 
# normality around 0?
# constant variance? 


#################### Finally ready to interpret our findings! 
summary(m1)

# Intercept: Expected level of course evaluation for reference group
# Slope of Tenure: Expected mean difference in course evaluation for tenure status compared to non-tenured professors
# Dispersion parameter = Residual/Leftover/Unexplained variation in the course evaluation

# Null deviance = total variance to be explained based on the empty GLM (i.e., sample variance of outcome)
359.78/462
var(tr$eval)

# Residual deviance = residual/leftover/unexplained variation AFTER considering tenure status
339.24/461

###### Remember those linear combinations from the equations in lecture? We can get R to do this automatically for us: 
cont1 <- emmeans(m1, specs = "tenureF")
summary(cont1) # expected evaluation for each group with CI's
pairs(cont1) # expected mean difference between groups 
confint(pairs(cont1)) # CI's for expected mean difference 

######### Effect Size Calculation 

###### Model Effect Size (R-squared)
r_sq <- function(model_name) {
  r2 <- 1 - model_name$deviance/model_name$null.deviance
  return(r2)
}

r_sq(m1) # Interpret this 

###### Predictor Effect Size: Standardized Mean Difference (Cohen's d)
# How many standard deviations apart are the two group means? 
#note: there are packages that will calculate this for you (e.g., effectsize, emmeans, rstatix, etc)
#however, it's also good to know how to calculate this manually for when we get into more complex models 

# emmeans approach 
emm <- emmeans(m1, specs="tenureF")
eff_size(emm, sigma= sigma(m1), edf = df.residual(m1))


# manual approach (more general but requires you to understand what to pull out)
estD <- m1$coefficients[2]
estS <- sigma(m1) # residual/leftover/unexplained variance in SD units
abs(estD/estS) # don't report negative values of Cohen's d, take the absolute value and report this


##### Recap:
# so the expected difference in course evals between non-tenured and tenured professors is moderate, 
# but the model is not accounting for very much of the overall variance in evaluation scores
# In other words, knowing whether or not a professor is tenured is important but there are still other 
# things that impact evaluation scores

#Pause


################# Bayesian Version #####################

hist(tr$eval)

## We previously decided that the distribution of eval looks okay, but Bayesian analyses allows
# for increased flexibility in the specification of data distributions 
# It's very easy to change the distribution. We just add it in as an argument to the model

#Group 1 mean and SD: What is the expected mean of the first group in the population and how spread out would that be
#Group 2 mean and SD: What is the expected mean of the second group in the population and how spread out would that be
#We use MCMC to estimate the posterior distributions for these parameters
#Considers what these parameters are most likely to be in the *population*
#Then, we can use the posterior distributions to estimate our mean group differences, effect size, etc. 

b1 <- brm(eval ~ 0 + Intercept + tenureF, family = "skew_normal",
          chains = 4, iter = 2000, seed = 123, data = tr) 
#note: will take a minute to run. your computer isn't frozen :), that's just MCMC working hard behind the scenes


## Now, before we interpret the output, let's check for evidence of convergence
# Looking at Rhats, Effective Sample Sizes, and Trace Plots
summary(b1) #good convergence (Rhats = 1, ESS is sufficient)
plot(b1) # more evidence for convergence; trace plots look like fuzzy caterpillars


#Let's get back to the output
# 1. Intercept Estimate: Represents the posterior mean of evaluation scores for non-tenure instructors
# posterior mean for non-tenure = 3.29, SD = 0.08, HDI = (3.14, 3.45)

# 2. tenureFYes Estimate: Represents the posterior mean difference of evaluation scores between non-tenure and tenured professors
# b = -0.50, SD = 0.09, HDI (-0.67, -0.32)
# Credible evidence that tenured instructors receive lower course evaluations than non-tenured instructors
# e.g., HDI does not include zero

# 3. Est. Error is the SD of the posterior distribution

# 4. l-95% CI and u-95% CI: Represents the lower and upper boundaries of the HDI
#the 95% probability that the parameter falls within that HDI, given the model and data

# 5. sigma: Represents the SD of the residual variance (it is the square root of the residual variance that we saw from NHST)
## It is still the leftover/unexplained/residual variance BUT in SD units 

# Pause


###### Remember those linear combinations from the equations in lecture? We can get R to do this automatically for us: 

## Generate expected posterior means for both groups (for reporting in write up)
cont1 <- emmeans::emmeans(b1, specs = "tenureF")
hpd.summary(cont1, point.est = mean)

## Generate expected posterior difference between groups 
pairs(cont1)

#Visualize association
conditional_effects(b1) # visually: no overlap between HDIs suggesting credible difference


##Overall, we get much more information than we did with the NHST version, 
#because we can see the most probable mean difference (the peak of the distribution), 
#the 95% probability range in which the mean difference lies, 
#the shape of the probabilities in-between, 
# and we actually model the SD for each group as parameters within the model

######### Effect Size Calculation 

###### Model Effect Size (R-squared)
bayes_R2(b1) # proportion of variance of eval explained by the model

##### Standardized Mean Difference (Cohen's d)
# Extract posterior samples for beta(s) and sigma parameters
post_sam <- as.data.frame(posterior_samples(b1, pars = c("^b_", "sigma")))
# Compute Cohen's d for each iteration
post_sam$cohen_d <- abs(post_sam$b_tenureFYes / post_sam$sigma)
# Posterior density
bayesplot::mcmc_areas(post_sam, pars = "cohen_d", prob = .95)
# Point estimate (similar to NHST analysis)
mean(post_sam$cohen_d)

##### REGION of PRACTICAL EQUIVALENCE 
r <- rope(b1)
r
plot(r)

##### PROBABILITY OF DIRECTION
pd <- p_direction(b1)
pd
plot(pd)

##### Recap:
# so the expected difference in course evals between non-tenured and tenured professors is moderate, 
# but the model is not accounting for very much of the overall variance in evaluation scores
# In other words, knowing whether or not a professor is tenured is important but there are still other 
# things that impact evaluation scores

########### Check Model Assumptions (same as NHST)
pp_check(b1, ndraws=30) # checking predictive accuracy: it's predicting fairly well

##### Normality of Residuals
pp_check(b1, type="error_hist", ndraws=20, set.seed(293)) # overall
pp_check(b1, type="error_hist_grouped", ndraws=5, group = "tenureF", freq = T, set.seed(293)) # checking residuals by group
qqPlot(residuals(b1)[,1], groups = tr$tenureF)


##### Constant Variance
ggplot(tr, aes(x = tenureF, y = residuals(b1)[,1]))+
  geom_point(size=2)+geom_jitter(width=0.1)
