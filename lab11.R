
########## Lab 11 (GLM) ############

#1. Initial data clean up
#2. Exploring data and preparing for model(s)
#3. Review of GLMs with one predictor (NHST Focused) 
#4. NHST Model Building and Model Comparison through our Goals
#5. Bayesian Model Building and Model Comparison through our Goals

## Set Working Directory

## Get rid of the pesky scientific notation
options(scipen = 999) 

## Install packages
if (!require("apaTables")) install.packages("apaTables")

## Load packages
library(tidyverse)
library(car)
library(psych)
library(emmeans)
library(apaTables)
library(rties)
library(brms)
library(BayesFactor)



## Load user defined functions for later use 
source("userfunctions.R")
# F_test: omnibus test used to discern the significance of a single model
# F_change: used to compare NESTED models. models must be listed in order of increasing complexity
# r_sq: Model R-squared for a single model (NHST)
# r_sqAdj: adjusted model R-squared for a single model (NHST)
# sr2: computes semi-partial eta squared for slopes in a single model (NHST)

## Read in data
d1 <- read.csv("regressionExample.csv", stringsAsFactors = TRUE)

## Today, we're focused on what relational factors are associated with relationship conflict including:
# It is well known that internal and external stress factors are associated with conflict
# We have relationship stress (internal), general stress (external), and current stress level (both)
# I want to test if current ambivalence toward the relationship is associated with conflict (over and above stress)

################# 1. Initial Data Clean-Up ################

## Check the data
str(d1)
summary(d1)
head(d1)
# do the variables appear to look okay?

## 'ambivalence' seems dichotomous, but it's recognized as an integer. Let's check the distribution:
hist(d1$ambivalence) # yep. this should be a factor 

## recode 'ambivalence' into factor 
d1$ambivY <- factor(d1$ambivalence, levels = c(0,1), labels = c("No", "Yes"))
table(d1$ambivY)

## also, stress is ordered in the wrong way. So, we should reorder it. 
levels(d1$stress)
d1$stress <- factor(d1$stress, levels=c("Low","Medium","High"))
levels(d1$stress)

## check data again to make sure things went as planned
str(d1)

## create new subset that just includes variables of interest
d2 <- subset(d1, select = c(conflict:genstressA, ambivY))

############## 2. Exploring data and preparing for model(s) ###############

# Remember goal: What relational factors are important for predicting conflict?

#### Check distribution of DV, and also check to see if anything weird for IV's (e.g., outliers, zero variance, etc)
histAll(d2) # the variable we want to use as our DV is fairly normally distributed. As I said, this is only sort of real data :)

##**RECALL: The assumptions for a GLM are largely about the residuals
# You can have a seemingly normally distributed DV that results in skewed, non-linear, heteroscedastic residuals
# You can have a skewed DV that results in sufficiently normal, homoscedastic residuals
#****KEEP YOUR BRAIN ATTACHED******

#### Visualize associations between DV (conflict) and IV's
plot(conflict ~ ambivY, data = d2) #Any problems with outliers? What is your guess about what associations we will find?
plot(conflict ~ stress, data = d2) #Any problems with outliers? What is your guess about what associations we will find?
plot(conflict ~ relstressA, data = d2) #Any problems with outliers? What is your guess about what associations we will find?
plot(conflict ~ genstressA, data = d2) #Any problems with outliers? What is your guess about what associations we will find?
# yes there are definitely more efficient ways of completing this :)

#### Generate descriptive statistics of conflict by categorical variables
ambivD <- d2 %>%
  group_by(ambivY) %>%
  get_summary_stats(conflict); ambivD 

stressD <- d2 %>%
  group_by(stress) %>%
  get_summary_stats(conflict); stressD 
# Can use code from previous weeks to save these tables as Word docs 


#### Check for relationships among continuous IV's and produce APA style table
quant <- subset(d2, select = c(conflict, relstressA, genstressA))
pairs.panels(quant)
apaTables::apa.cor.table(quant, filename = "lab11corTable.docx")


#### Center continuous variables (if needed)

summary(d2) # in our case values of 0 for general stress and relationship stress are valid observations

# I'm going to proceed with centering these variables because:
#1: It's slightly more interesting to talk about the value of conflict for persons with average genStress and relStress

#2: Centering will become crucial for moderation models (i.e., interactions) moving forward to 
# obtain accurate estimates of simple slopes

# mean center continuous variables (genStress and relStress):
d2 <- d2 %>% mutate(
  genstressA_c = genstressA - mean(genstressA, na.rm = TRUE),
  relstressA_c = relstressA - mean(relstressA, na.rm = TRUE))

summary(d2)
# the new variables marked with '_c' reflect mean centered versions of the original variables


########## Up until here, everything applies whether using NHST or Bayesian


############## 3. Review of GLMs With One Predictor ###############

# Ideally we would start with the full, hypothesized models 
# However, I want to make sure we're refreshed first. 
# So, we'll start with simpler models just for example:

## SLR review:
mSimple <- glm(conflict ~ genstressA_c, data = d2,
               family = "gaussian", na.action = na.exclude)
summary(mSimple) # Interpret the intercept and slope
r_sq(mSimple) # Interpret R-squared

## GLM equivalent of t-test review:
mttest <- glm(conflict ~ ambivY, data = d2, 
              family = "gaussian", na.action = na.exclude)
summary(mttest) # Interpret the intercept and slope
r_sq(mttest)  # Interpret R-squared

## GLM equivalent of ANOVA review:
mANOVA <- glm(conflict ~ stress, data = d2,
              family = "gaussian", na.action = na.exclude)
F_test(mANOVA) #WHY DO WE NEED THIS FOR THIS MODEL? 
summary(mANOVA) # Interpret the intercept and slope(s)
# WHAT'S THE PROBLEM HERE COMAPARED TO OTHER MODELS? WHAT DO WE NEED TO CONTROL FOR??
emm1 <- emmeans(mANOVA, specs = "stress")
pairs(emm1)
emmeans::eff_size(emm1, sigma = sigma(mANOVA), edf = df.residual(mANOVA))
r_sq(mANOVA)  


############## 4. NHST Model Building and Model Comparison through our Goals ##############

# Skip assumption checks to save time. These things are still relevant

## We are going to take a Hierarchical Model Building Approach by:
#1. starting out with a model that includes the stress predictors (factors we know are associated with conflict based on prior research)
#2. adding ambivalence to the model (our focal predictor). this is what we're bringing to the table so all other predictors now become covariates

##### conflict ~ stress predictors 
m1 <- glm(conflict ~ relstressA_c + genstressA_c + stress, data = d2,
          family = "gaussian", na.action = na.exclude)

## Goal 1: Establish if an effect exists. Does our model with these variables do better than the null model?
F_test(m1) # Results for Model 1: F(4, 124) = 18, p < .0001
r_sq(m1) # .37

# Conclusion: The general linear model including four slopes representing current stress level (High, Medium, Low), 
# relationship stress, and general stress captured a significant amount of 
# variance in conflict F(4, 124) = 18.00, p < .0001, R2 = .367. 

## Goal 2: Assuming something exists (e.g., an effect is present), 
# estimate the parameter values and our degree of uncertainty about those estimates.
Anova(m1, type = "III", test.statistic = "F") #need omnibus test because 'stress' has more than 2 groups
sr2(m1)

# Although our model is 'significant' in that we are explaining significantly more variance than the null model, 
# it appears that the only predictor that significantly contributes uniquely is relationship stress. 
# This contrasts to what we saw in the individual models....should we be concerned?!?!

### Interpretion of results so far based on omnibus test for each unique predictor effect:
# Relationship stress was found to be a significant predictor 
# of conflict [F(1,124) = 44.61, p < .0001] controlling for the effects of other stress sources.
# In contrast, the unique effects of general stress and current stress level were not significant predictors of conflict controlling for the effects of internal stress. 

## Quantify the Relationship(s) for the predictors:
summary(m1)
# Intercept reflects?
# We've already established that genstressA and stress are not significant. 
# How would we interpret the slope of relationship stress? 

sr2(m1) #semipartial eta-squared for relstress: .23

###### Overall: The general linear model including four slopes representing current stress level (High, Medium, Low), 
# relationship stress, and general stress captured a significant amount of variance in conflict F(4, 124) = 18.00, p < .0001, R2 = .367.
# The unique effects of general stress and current stress level were not significant predictors of conflict controlling for the effects of internal stress. 
# In contrast, relationship stress was found to be a significant predictor of conflict [F(1,124) = 44.61, p < .0001, semipartial eta-squared = .23] controlling for the effects of other stress sources.
# Specifically, for every one-unit increase in relationship stress, relationship conflict was expected to increase by 1.05 units (SE = .16). 


##### conflict ~ stress predictors PLUS our theory around ambivalence 
m2 <- glm(conflict ~ ambivY + relstressA_c + genstressA_c + stress, data = d2,
          family = "gaussian", na.action = na.exclude)

## Goal 1: Establish if an effect exists. Does our model with these variables do better than the null model?
F_test(m2) # Results for Model 1: F(5, 123) = 39.75, p < .0001
r_sq(m2) # .62

# Conclusion: The general linear model including five slopes representing current stress level (High, Medium, Low), 
# relationship stress, general stress, and ambivalence captured a significant amount of 
# variance in conflict F(5, 123) = 39.75, p < .0001, R2 = .62. 

## Goal 2: Assuming something exists (e.g., an effect is present), 
# estimate the parameter values and our degree of uncertainty about those estimates.
Anova(m2, type = "III", test.statistic = "F") #need omnibus test because 'stress' has more than 2 groups

# Although our model is 'significant' in that we are explaining significantly more variance than the null model, 
# it appears that the only predictors that significantly contribute uniquely are relationship stress and ambivalence. 

### Interpretion of results so far based on omnibus test for each unique predictor effect:
# Relationship stress remained a significant predictor of conflict [F(1,123) = 10.25, p = .002] controlling for the effects of other stress sources and ambivalence.
# Ambivalence toward the relationship was also a significant predictor of conflict [F(1, 123) = 80.57, p < .0001] controlling for the effects of other stress sources.
# In contrast, the unique effects of general stress and current stress level remained insignificant predictors of conflict. 


## Quantify the Relationship(s) for the predictors:
summary(m2)
# Intercept reflects?
# We've already established that genstressA and stress are not significant. 
# How would we interpret the slope of relationship stress? 

sr2(m2) #semipartial eta-squared for ambivalence: .25 and relstress: .03
emm2 <- emmeans(m2, specs = "ambivY")
emm2
emmeans::eff_size(emm2, sigma = sigma(m2), edf = df.residual(m2)) #cohen's d for mean difference

#### Overall: The general linear model including five slopes representing ambivalence toward the relationship as well as the stress sources previously examined  
# captured a significant amount of variance in conflict F(5, 123) = 39.75, p < .0001, R2 = .62. 
# Relationship stress remained a significant predictor of conflict [F(1,123) = 10.25, p = .002, semipartial eta-squared = .03]. 
# Specifically, for every 1-unit increase in relationship stress, conflict was expected to increase by .45 units (SE = .14).
# Ambivalence toward the relationship was also a significant predictor of conflict [F(1, 123) = 80.57, p < .0001, semipartial eta-squared = .25] controlling for the effects of other stress sources.
# Relationship conflict for those who felt ambivalent toward their partner was expected to be 1.19 units higher relative to those who were not ambivalent toward their relationship (Cohen's d = 2.00) 


############ Goal 4: Compare Models

# Model 1: conflict ~ stress + genstress + relstress
# Model 2: conflict ~ stress + genstress + relstress + ambivY

r_sq(m1) #.37
r_sq(m2) #.62
# By adding in ambivalence as a predictor, we have accounted for an additional 25% of the total variation in conflict
# We can use an F test to determine if that is significant. 

# These models are nested, so use F statistic. 
# **Models should be listed in terms of increasing complexity**
F_change(m1, m2) # Does the reduction in RSS result in better model fit? (i.e., is the increased complexity worth it?)

### Conclusion: The more complex model including ambivalence explained significantly more variance
# in relationship conflict compared to the model that only included stress sources (F[1,123] = 80.57, p < .0001). 


############## 5. Bayesian Model Building and Model Comparison through our Goals ##############

## Skip assumption checks and checking for convergence to save time. These things are still relevant. 

##### conflict ~ stress predictors 
b1 <- brm(conflict ~ 0 + Intercept + relstressA_c + genstressA_c + stress, data = d2,
          family = "skew_normal", chains = 4, iter = 2000, seed = 123)
saveRDS(b1, file = "b1_L11.rds")

##### conflict ~ stress predictors + ambivalence
b2 <- brm(conflict ~ 0 + Intercept + relstressA_c + genstressA_c + stress + ambivY, data = d2,
          family = "skew_normal", chains = 4, iter = 2000, seed = 123)
saveRDS(b2, file = "b2_L11.rds")

b1 <- readRDS("/Users/akuelz/Desktop/315/Spr24/Lab/data/b1_L11.rds")
b2 <- readRDS("/Users/akuelz/Desktop/315/Spr24/Lab/data/b2_L11.rds")

## Goal 1: Establish if an effect exists. Does our model with these variables do better than the null model?
lmBF(conflict ~ relstressA_c + genstressA_c + stress, data = d2) # Very strongly favored over the null
lmBF(conflict ~ relstressA_c + genstressA_c + stress + ambivY, data = d2) # Very strongly favored over the null


## Goal 2: Assuming something exists (e.g., an effect is present), 
# estimate the parameter values and our degree of uncertainty about those estimates.
summary(b1)
# Relationship stress has a credible positive effect on relationship conflict. 
# For every 1-unit increase in relationship stress, conflict is expected to increase linearly by 1.01 units [95% HDI(.71, 1.31)]
# Current stress level and general stress are not credible predictors. 

summary(b2)
# Relationship stress and ambivalence are credible predictors of conflict.
# Ambivalence in the relationship is expected to increase relationship conflict by 1.18 units [95% HDI(0.91, 1.43)]
# For every 1-unit increase in relationship stress, conflict is expected to increase linearly by .43 units [95% HDI(.14, .71)]

#### Cohen's D for standardized mean difference:
summary(b2)$fixed
summary(b2)$spec_pars

# We want to take the mean difference between the two groups and divide by sigma
1.1794266/0.6064348 # Cohen's d = 1.94
difYN <- summary(b2)$fixed[6,1]
sigma <- summary(b2)$spec_pars[1,1]
difYN/sigma

bayes_R2(b1) #.36
bayes_R2(b2) #.60
# By adding in ambivalence, we're accounting for an additional 24% of the total variation in conflict

## Goal 4: Model Comparison
b1 <- add_criterion(b1, criterion = "waic")
b2 <- add_criterion(b2, criterion = "waic")

loo_compare(b1, b2, criterion = "waic") 
## elpd_diff is the "expected log predictive density" which is the difference between the models 
# in terms of expected predictive accuracy in new data
## first line compares the model with the highest elpd to itself, so the values are always zero
## next line(s) compare the best model to the others.
# #if ratio  > approximately 2, then the second model is notably worse than the first model. If < approximately 2, the models are not distinguishable in terms of fit.

abs(28.9/7.1) # 4.07
# So the model including ambivalence is strongly favored (i.e., the model with stress predictors only is notably worse in terms of predictive accuracy)

