################################
### Simple Linear Regression ###
################################


# Set WD 
setwd("/Users/akuelz/Desktop/315/Spr24/Lab/data")


# Check to see if package is installed, install if not
if (!require("car")) install.packages("car")


# Load packages
library(psych)
library(rties)
library(brms)
library(bayesplot)
library(bayestestR)
library(car)

# Get rid of the pesky scientific notation
options(scipen = 999) 

# Read in Data
d0 <- read.csv("LAF_labV06_subsetV02.csv", stringsAsFactors = T)
d0<-LAF_labV06_subsetV02

## Premise for today: Identify variables that are meaningful predictors of relationship conflict. 
## Our goal is to find meaningful predictors that impact relational conflict. 
str(d0)
# Let's focus on 2 potential predictors: 
# Level of relationship stress experienced in the last 7 days
# Level of general stress experienced in the last 7 days (i.e., stress caused by external factors)

d1 <- subset(d0, select = c(conflict, genstressA, relstressA))

############### Visualize and Explore Data ##################

describe(d1, IQR = T, quant = c(.25, .75))
histAll(d1)

# Visualize potential associations (checking for linearity)
plot(conflict ~ genstressA, data = d1)
abline(glm(conflict ~ genstressA, data = d1, family = "gaussian"))

plot(conflict ~ relstressA, data = d1)
abline(glm(conflict ~ relstressA, data = d1, family = "gaussian"))

# All together here 
pairs.panels(d1, lm = T)


#### Practice Centering: Technically a value of '0' is meaningful for both predictors (reflects no stress in the past 7 days...oh to be them)
# We're going to center around the mean to show as an example. Now the 0 for each predictor will reflect the sample average for that predictor

d1$relstressA_c <- d1$relstressA - mean(d1$relstressA, na.rm = T)
summary(d1$relstressA)
summary(d1$relstressA_c)

d1$genstressA_c <- d1$genstressA - mean(d1$genstressA, na.rm = T)

############### NHST Version #################

m1 <- glm(conflict ~ relstressA_c, data = d1, 
          family = "gaussian", na.action = na.omit)

######### First, let's check model assumptions:

### 1. Normality of Residuals (test statistics and p-values rely on this):
hist(resid(m1), breaks = 10) 
summary(resid(m1))
qqPlot(resid(m1))


### 2. Homoscedasticity (Constant Variance):
# A violation of the constant variance assumption results in inaccurate confidence intervals 
# and p-values, even in large samples, although regression coefficient estimates will still be unbiased 
plot(resid(m1) ~ relstressA, data = d1)
abline(h = 0)


### 3. Linearity
# We've already checked this prior when exploring the data 

### 4. Independence
# Assumption will always be met with the data in this course. 


#### Our goal is to be as transparent as possible and contribute to replicable science. 
# So, what would we say about the residuals in terms of 
# normality around 0?
# constant variance? 


#################### Finally ready to interpret our findings! 
summary(m1)
# Intercept: b = 2.01, SE = 0.07, p < .001
# Slope of Relationship Stress: b = 1.29, SE = 0.13, p < .001

# Disperion parameter (residual/unexplained/leftover variance): This is all the remaining reasons we haven't explained yet

###### Confidence Intervals 
confint.lm(m1)

###### Model Effect Size (R-squared)
r_sq <- function(model_name) {
  r2 <- 1 - model_name$deviance/model_name$null.deviance
  return(r2)
}

r_sq(m1) # Interpret this 


############ Bayesian Version ##############

b1 <- brm(conflict ~ 0 + Intercept + relstressA_c, family = "gaussian",
          chains = 4, iter = 2000, seed = 123, data = d1)

### First let's acknowledge our ignorance around the priors: 
prior_summary(b1)

### Now let's check for evidence of convergence: 
plot(b1) # looking for fuzzy caterpillars in the trace plots 
summary(b1) # with special focus on the ESS and Rhat values  

### Check Model Assumptions (including accuracy of specified likelihood distribution)
pp_check(b1, ndraws = 100) # how accurate do the draws based on the normal distribution appear?
pp_check(b1, type = "error_hist", ndraws = 12, set.seed(456)) # check normality of residuals around 0
pp_check(b1, type = "error_scatter_avg_vs_x", x = "relstressA_c", ndraws = 20, set.seed(456)) #check for constant variance 


#################### Finally ready to interpret our findings! 
summary(b1)
plot(b1)
# Intercept: b = 2.01, SD = 0.07, HDI (1.87, 2.15)
# Slope: b = 1.29, SD = 0.13, HDI (1.03, 1.56)
# Sigma = Residual/Leftover/Unaccounted for Variance in SD units


###### Model Effect Size (R-squared)
bayes_R2(b1) 

##### Probability of Direction
pd <- p_direction(b1)
pd
plot(pd)

##### Region of Practical Equivalence 
r <- rope(b1)
r # by default the rope range is calculated as 0 +- .01 * SD of Y
plot(r)


##### Most of the relevant output in one place:
describe_posterior(b1, dispersion = T, centrality = "mean")