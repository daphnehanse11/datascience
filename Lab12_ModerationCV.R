
########## Lab 12 (Moderation & Cross-Validation) ############

#### OUTLINE:
#1. Moderation with continuous IV and binary/dichotomous moderator --> NHST
#2. Moderation with continuous IV and binary/dichotomous moderator --> Bayesian + Cross-Validation 

# Set Working Directory
setwd("/Users/akuelz/Desktop/315/Spr24/Lab/data")

# Install new packages
if (!require("interactions")) install.packages("interactions")

# Load Packages
library(tidyverse)
library(psych)
library(car)
library(interactions)
library(emmeans)
library(brms)
library(BayesFactor)
library(bayestestR)

## Load user defined functions for later use with NHST 
source("userfunctions.R")
  # F_test: omnibus test used to discern the significance of a single model
  # F_change: used to compare NESTED models. models must be listed in order of increasing complexity
  # r_sq: Model R-squared for a single model 
  # r_sqAdj: adjusted model R-squared for a single model 
  # sr2: computes semi-partial eta squared for slopes in a single model


#Turning off scientific notation
options(scipen = 999) 


## We're going to skip a lot of assumption checking today so we can focus on breaking down moderation 
  # Note: The model checks you know are all still relevant and you should do them to trust your results!! 
    # We just do not have time today..


########### EXAMPLE 1: CONTINUOUS IV (CENTERED), DICHOTOMOUS MODERATOR (NHST) ###########

#RQ: Does gender moderate the association between perceived social support and depression?

d2 <- read.csv("modExample2.csv", stringsAsFactors = T)
str(d2)

# DV: depression
# IV: socsup 
# Z: girls (0= boys, 1 = girls)

## Turn 'girls' into a factor:
d2$girls <- factor(d2$girls, levels = c(0,1), labels = c("boys", "girls"))

## Plot Associations
scatterplotMatrix(~ depression + socsup | girls, data = d2)

## Grand-mean centering continuous predictor (socsup)  
summary(d2)

d2 <- d2 %>% mutate(
  socsup_c = socsup - mean(socsup, na.rm = TRUE))

describe(d2$socsup)
describe(d2$socsup_c)


### BOYS are REFERENCE GROUP (i.e., coded 0)
table(d2$girls)
levels(d2$girls)

# Fit Model Including Main Effects
m1Main <- glm(depression ~ socsup_c + girls, data = d2)
F_test(m1Main) # is the model significant? 
summary(m1Main) # Practice interpreting these 

# Fit Model Including Interaction 
m1Int <- glm(depression ~ socsup_c + girls + socsup_c*girls, data = d2)
F_test(m1Int) # is the model significant? 
Anova(m1Int, type = "III", test.statistic = "F") # is interaction term significant overall?

r_sq(m1Main)
r_sq(m1Int)

# Compare models
F_change(m1Main, m1Int) # including moderation effect in model significantly reduces RSS
  # the moderation model explains significantly more variance
  #What if it didn't? My recommendation would be to go back to main effects model and report these results

round(cbind(summary(m1Int)$coef, confint.lm(m1Int)),3)
#Intercept = the value of Y when X and Z = 0. Said differently, the value of Y at average levels of support (X) 
  # for the group coded 0 on gender (Z) -->  Boys’ Depressive symptoms at average levels of support.
#socsup_c (X) = (conditional effect) Estimated slope of the association between X and Y when Z = 0. 
  # the association between social support and depressive symptoms for boys.
#girls (Z) = (conditional effect) Estimated slope of the association between Z and Y when X = 0. Said differently, at
  # average levels of support, depressive symptoms increase/decrease with 1-unit increase in gender.
   # Depressive symptoms increase by .55 for girls at average levels of support.
#socsup:girls = (interaction effect). The amount by which the effect of X on Y changes with every 1-unit increase in Z. 
  # For dichotomous moderator, this is the difference between the groups on the slope (of socsup_c).
  # If you take -.02958 + (-.07962), you’ll get the slope for girls.


########## That was a lot let's try breaking down what's going on:

### Start by visualizing!!!

apatheme <- theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='serif')) # setting up some of display options


fig1 <- interact_plot(m1Int, pred = socsup_c, modx = girls,
              modx.labels = c("Boys", "Girls"), 
              x.label = 'Social Support (Grand-Mean Centered)', y.label = 'Depressive Symptoms',
              colors = c('black', 'gray'), line.thickness = 0.6, 
              legend.main = 'Gender', interval = TRUE, int.width = .95) +
              coord_cartesian(ylim = c(0,6)) +
              scale_y_continuous(breaks = scales::breaks_pretty(n = 13)) + apatheme 
fig1 #can save this figure
  # remember to play around with y-axis and faceting (facet.modx = TRUE)


ggsave(filename = "ModerationExample.png",
       plot = fig1,
       device = "png",
       width = 6,
       height = 4,
       units = "in",
       dpi = 500)

## Probe Simple Slopes:
interact <- emtrends(m1Int, var = "socsup_c", ~ girls)
      # var = main predictor variable in model 
      # ~ .. = moderating variable 
test(interact) # test of whether each simple slope is significantly different from 0
# For boys:
#The association between social support and depression is insignificant (b = -0.03, SE = 0.03, p = .35)

# For girls:
# You tell me!!

interact # confidence intervals for each simple slope 



########### EXAMPLE 2. CONTINUOUS IV (CENTERED), DICHOTOMOUS MODERATOR (BAYESIAN) #############

hist(d2$depression, breaks = 15) # let's use Student t likelihood given the heavy tail

## Main Effects Model 
b1Main <- brm(depression ~ 0 + Intercept + socsup_c + girls, data = d2,
              family = "student", chains = 4, iter = 2000, seed = 321)
saveRDS(b1Main, file = "b1Main_L12.rds")

## Moderation Model 
b1Int <- brm(depression ~ 0 + Intercept + socsup_c + girls + socsup_c*girls, 
             data = d2, family = "student", chains = 4, iter = 2000, seed = 321)
saveRDS(b1Int, file = "b1Int_L12.rds")

b1Main <- readRDS("/Users/akuelz/Desktop/315/Spr24/Lab/data/b1Main_L12.rds")
b1Int <- readRDS("/Users/akuelz/Desktop/315/Spr24/Lab/data/b1Int_L12.rds")


pp_check(b1Main, ndraws = 50)
pp_check(b1Int, ndraws = 50) # in both cases, the likelihood we chose seems reasonable

## Goal 1: Establish if an effect exists. Does our model with these variables do better than the null model?
bfMain <- lmBF(depression ~ socsup_c + girls, data = d2) 
bfMain # Very strongly favored over the null
bfInt <- lmBF(depression ~ socsup_c + girls + socsup_c*girls, data = d2) 
bfInt # Very strongly favored over the null

## Goal 2: Estimate the parameter values for the intercept and each predictor and degree of uncertainty about them 
summary(b1Main) #practice interpreting
summary(b1Int)  


## PROBE Interaction: 
simSlopes <- emtrends(b1Int, ~ girls, var = "socsup_c")
summary(as.mcmc(simSlopes)) # HDI's test whether each simple slope is credibly different from 0 

### interact_plot also works for brms objects. The difference is that HDI's are plotted instead 
bfig1 <- interact_plot(b1Int, pred = socsup_c, modx = girls,
                      modx.labels = c("Boys", "Girls"), 
                      x.label = 'Social Support (Grand-Mean Centered)', y.label = 'Depressive Symptoms',
                      colors = c('black', 'gray'), line.thickness = 0.6, 
                      legend.main = 'Gender', geom = 'line', interval = TRUE, int.width = .95) +
  coord_cartesian(ylim = c(0,6)) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 13)) + apatheme 

bfig1

## Goal 3 & 4: Compare Models. How well will our model do in predicting new data?
bayes_R2(b1Main)
bayes_R2(b1Int)

## Can also use Bayes Factors to compare other models
bfInt/bfMain # slight evidence in favor of the moderation model over the main effects only model 

## Let's use Leave One Out (LOO) Cross-Validation
b1Main <- add_criterion(b1Main, criterion = "loo")
b1Int <- add_criterion(b1Int, criterion = "loo")


loo_compare(b1Main, b1Int, criterion = "loo")

# ratio of the absolute value of the difference relative to its standard error
# if > 2, second model is notably worse than the first model in terms of predictive accuracy
# if < 2, models are not distinguishable in terms of predictive accuracy 


### ROPES AND PROBABILITY OF DIRECTION 
p_d <- pd(b1Int); p_d
plot(p_d)

r <- rope(b1Int); r
plot(r)
