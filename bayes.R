####################################
#### LAB 6: Tale of Two Methods ####
####################################

### OUTLINE:
#1. Review GLM syntax 
#2. Introduction to brms syntax


# Set WD 
setwd("/Users/akuelz/Desktop/315/Spr24/Lab/data")

# Read in Data
library(readr)
d1 <- read_csv("~/Downloads/climateData.csv")

# Check Data loaded in correctly
str(d1)
head(d1)

# Install new packages 
if (!require("bayesplot")) install.packages("bayesplot")
if (!require("bayestestR")) install.packages("bayestestR")
if (!require("ggfortify")) install.packages("ggfortify")

# Load packages
library(tidyverse)
library(psych)
library(brms)
library(bayesplot)
library(bayestestR)
library(ggfortify)

# Get rid of the pesky scientific notation
options(scipen = 999) 


### Interested in researching what impacts consumer behaviors related to carbon emissions
# This is proBx in the data and it is ordinal treated as interval based on the average of 15 items

# Example Item: When you visit the grocery store, how often do you use reusable bags? (never to always)
# Example Item: How often do you act to conserve water when showering, cleaning clothes, dishes, water plants, or other uses? (never to always)

describe(d1$proBx, IQR = T, quant = c(.25, .75))
hist(d1$proBx)

ggplot(d1, aes(proBx)) +
  geom_histogram(aes(x=proBx, y=..density..), bins=10, fill="grey60", color="black") +
  stat_function(fun=dnorm, 
                args = list(mean=mean(d1$proBx, na.rm = T), sd=sd(d1$proBx, na.rm = T)), 
                color="blue", lty = 2, lwd = 1) +
  geom_density(color="red", lwd = 1) +
  xlab("Pro-Environmental Behavior") +
  ylab("Density") +
  coord_cartesian(xlim=c(1,5), ylim=c(0,0.8)) +
  theme_bw()
# play with bins from 10 to 50 and change ylim

############ NHST Empty GLM ###########
m0 <- glm(proBx ~ 1, data = d1, na.action = na.omit, family = "gaussian") 
#one fixed effect: intercept which represents sample mean

summary(m0)
# Dispersion = Mean Sq Error = Total Residual Variance (all the reasons people differ in their behaviors)
# This is also sample variance as there are no predictors in the model 

# Denominator degrees of freedom = (N - k), where k is the # of fixed effects 
346 - 1

# t test statistic calculated by: 
(2.74915 - 0) /  0.03574

# Critical Value for Student's t with 345 degrees of freedom:
abs(qt(p = .025, df = 345)) # divide alpha by 2 for two-tailed test 
# Notice that the cutoff converges on the cutoff for standard normal (z) because of our 'large' sample size 

# Residual SE represents the SD of the residuals (which is sample SD in this model) 
# Visualize model deviations (error terms for each individual)
plot(m0$residuals, 
     col = "black",  
     bg = "lightgreen", 
     pch = 21)
abline(h = 0, col = "grey6", lwd = 2)


variance <- sum((m0$residuals)^2)/346 # we plug in the error variance in the formula
variance #sample variance
sqrt(variance) # sample SD 

# Distribution of the errors 
hist(m0$residuals)

# Confidence Intervals
confint.lm(m0)

## Relevant output all together:
round(cbind(summary(m0)$coef, confint.lm(m0)),3)


############### Bayesian Empty GLM ##############

b0 <- brm(proBx ~ 0 + Intercept, data = d1, family = "gaussian",
          chains = 4, iter = 2000, seed = 123, sample_prior = T)

################ First check for evidence of convergence: 
plot(b0)
# We start by focusing in on the plots on the right. We want to see “fuzzy caterpillars”. 
# Each MCMC chain is a separate line and we want to see each line blending with one another.

summary(b0)
# ESS = Effective Sample Size: samples drawn sequentially will be positively correlated (autocorrelated)
# ESS captures how many independent draws contain the same amount of information as the dependent sample 
# The higher the ESS the better.
# Rhat: We ideally want this value to be 1.00 and nothing greater than 1.05
# More formally: This compares the between and within chain estimate for model parameters
# If chains have not blended well (e.g., the between and within chain estimates do not agree), Rhat will be larger than 1

########### Acknowledge the priors that have been set (by default)
prior_summary(b0)

### Intercept = flat (uniform) prior over the real #'s (-inf to +inf)
ggdistribution(dunif, seq(-100000, 100000, 10000), min = -100000, max = 100000, colour = "red")

### Sigma (residual SD, which is sample SD in this model)
## Will always be a student-t prior with 3 parameters: df, mu, sigma
# Default Sigma will always have 3 df, with a mu = 0, and a SD that it selects based on:

mad(d1$proBx)
# If MAD is less than 2.5, the SD will be 2.5
# If MAD is greater than 2.5, it will round that to a single decimal point and use that as SD

ggdistribution(dstudent_t, seq(0, 20, 0.5), df = 3, mu = 0, sigma = 2.5, colour = "blue")


##### Interpret the output
summary(b0)
plot(b0)

################ Additional Evidence from Bayesian: 

## P_direction (correlates strongly with NHST p-value)
p_direction(b0, null = 0)

## ROPE
rope(b0)

## Equivalence Test
equivalence_test(b0)


############ Relevant output all together: 
describe_posterior(b0, dispersion = T, centrality = "mean")
