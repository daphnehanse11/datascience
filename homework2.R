climateData <- read.csv("~/Downloads/climateData.csv", stringsAsFactors = TRUE)


# Load packages
library(psych)
library(rties)
library(brms)
library(bayesplot)
library(bayestestR)
library(car)


options(scipen = 999) 

#organizing my data
summary(climateData)

histAll(climateData)
datasub <- subset(climateData, select = c(proBx, econStrain, pce))


describe(datasub, IQR = T, quant = c(.25, .75))
histAll(datasub)


datastats<-get_summary_stats(datasub)
flextable(datastats)


# Visualize potential associations (checking for linearity)

plot(proBx ~ econStrain, data = datasub)
abline(glm(proBx ~ econStrain, data = datasub, family = "gaussian"))

plot(proBx ~ pce, data = datasub)
abline(glm(proBx ~ pce, data = datasub, family = "gaussian"))


pairs.panels(datasub, lm = T)

# Centering the variables
datasub$proBx_centered <- datasub$proBx - mean(datasub$proBx)
datasub$econStrain_centered <- datasub$econStrain - mean(datasub$econStrain)
datasub$pce_centered <- datasub$pce - mean(datasub$pce)



#models NHST
m1f <- glm(proBx_centered ~ econStrain_centered, data = datasub, 
          family = "gaussian", na.action = na.omit)
m2f <- glm(proBx_centered ~ pce_centered, data = datasub, 
           family = "gaussian", na.action = na.omit)


#1. Normality of Residuals 
hist(resid(m1f), breaks = 10) 
summary(resid(m1f))
qqPlot(resid(m1f))

hist(resid(m2f), breaks = 10) 
summary(resid(m2f))
qqPlot(resid(m2f))





#2. Homoscedasticity (Constant Variance):

plot(resid(m1f) ~ econStrain_centered, data = datasub)
abline(h = 0) 
plot(resid(m2f) ~ econStrain_centered, data = datasub)
abline(h = 0) 


summary(m1f)
confint.lm(m1f)
r_sq <- function(model_name) {
  r2 <- 1 - model_name$deviance/model_name$null.deviance
  return(r2)
}

r_sq(m1f) 

summary(m2f)
confint.lm(m2f)
r_sq(m2f) 


#Bayesian

m1b <- brm(proBx_centered ~ 0 + Intercept + econStrain_centered, family = "gaussian",
           chains = 4, iter = 2000, seed = 123, data = datasub)
m2b <- brm(proBx_centered ~ 0 + Intercept + pce_centered, family = "gaussian",
           chains = 4, iter = 2000, seed = 123, data = datasub)





prior_summary(m1b)

### Now let's check for evidence of convergence: 
plot(m1b) # looking for fuzzy caterpillars in the trace plots 
summary(m1b) # with special focus on the ESS and Rhat values  

### Check Model Assumptions (including accuracy of specified likelihood distribution)
pp_check(m1b, ndraws = 100) # how accurate do the draws based on the normal distribution appear?
pp_check(m1b, type = "error_hist", ndraws = 12, set.seed(456)) # check normality of residuals around 0
pp_check(m1b, type = "error_scatter_avg_vs_x", x = "relstressA_c", ndraws = 20, set.seed(678)) #check for constant variance 


summary(m1b)
plot(m1b)


###### Model Effect Size (R-squared)
bayes_R2(m1b) 

##### Probability of Direction
pd <- p_direction(m1b)
pd
plot(pd)

##### Region of Practical Equivalence 
r <- rope(m1b)
r 
plot(r)





#Second Model

prior_summary(m2b)

plot(m2b) 
summary(m2b) 

### Check Model Assumptions 
pp_check(m2b, ndraws = 100) 
pp_check(m2b, type = "error_hist", ndraws = 12, set.seed(456)) 
pp_check(m2b, type = "error_scatter_avg_vs_x", x = "relstressA_c", ndraws = 20, set.seed(678))  


summary(m2b)
plot(m2b)

###### Model Effect Size (R-squared)
bayes_R2(m2b) 

##### Probability of Direction
pd <- p_direction(m2b)
pd
plot(pd)

##### Region of Practical Equivalence 
r <- rope(m2b)
r 
plot(r)

