GSS <- read.csv("~/Downloads/GSS_primary.csv", stringsAsFactors = TRUE)


# Load packages
library(psych)
library(rties)
library(brms)
library(bayesplot)
library(bayestestR)
library(car)


options(scipen = 999) 

#organizing my data
summary(GSS)

histAll(GSS)

str(GSS)
describe(GSS, IQR = T, quant = c(.25, .75))
histAll(GSS)


GSS<-get_summary_stats(GSS)
flextable(GSS)


# Visualize potential associations (checking for linearity)

plot(income ~ marryY, data = GSS)
abline(glm(income ~ marryY, data = GSS, family = "gaussian"))

plot(income ~ sesF, data = GSS)
abline(glm(income ~ sesF, data = GSS, family = "gaussian"))


pairs.panels(GSS, lm = T)





#models NHST
m1f <- glm(income~ marryY, data = GSS, 
           family = "gaussian", na.action = na.omit)



#1. Normality of Residuals 
hist(resid(m1f), breaks = 10) 
summary(resid(m1f))
qqPlot(resid(m1f))






#2. Homoscedasticity (Constant Variance):
plot(resid(m1f) ~ marryY, data = GSS)
plot(resid(m1f) ~ income, data = GSS)
abline(h = 0) 



summary(m1f)
confint.lm(m1f)
r_sq <- function(model_name) {
  r2 <- 1 - model_name$deviance/model_name$null.deviance
  return(r2)
}

r_sq(m1f) 

  
#Bayesian

m1b <- brm(income~ 0 + Intercept + marryY, family = "gaussian",
           chains = 4, iter = 2000, seed = 12, data = GSS)




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







