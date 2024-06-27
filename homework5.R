
########## Lab 12 (Moderation & Cross-Validation) ############

#### OUTLINE:
#1. Moderation with continuous IV and binary/dichotomous moderator --> NHST
#2. Moderation with continuous IV and binary/dichotomous moderator --> Bayesian + Cross-Validation 



# Install new packages
if (!require("interactions")) install.packages("interactions")

# Load Packages
library(apaTables)
library(tidyverse)
library(psych)
library(ggplot2)
library(car)
library(interactions)
library(emmeans)
library(brms)
library(BayesFactor)
library(bayestestR)
library(easystats)
library(readr)

## Load user defined functions for later use with NHST/turn off sci not
source("userfunctions.R")
options(scipen = 999) 


#load in data and check model assumptions 
data<-read.csv("HW5_data.csv", stringsAsFactors = TRUE)
str(data)
describe(data)

flextable::flextable(data)
table(data)






# DV: worry
# IV: reap 
# Z: women (0= men, 1 = women)

## Turn 'women' into a factor:
data$women <- factor(data$sex, levels = c(0,1), labels = c("men", "women"))

## Plot Associations
scatterplotMatrix(~ worry + reap | sex, data = data)

## Grand-mean centering continuous predictor (reap)  
summary(data)

data <- data %>% mutate(
  reap_c = reap - mean(reap, na.rm = TRUE))
data$sex <- factor(data$sex, labels = c("men", "women"))

describe(data$reap)
describe(data$reap_c)

ggplot(data, aes(x = worry)) + 
  geom_histogram(bins = 15, fill = "lightblue", color = "black") 
cor(data[, c("worry", "reap")])
ggplot(data, aes(x = reap, y = worry)) +
  geom_point() +


ggplot(data, aes(x = sex, y = worry)) +
  geom_boxplot()+apatheme

describe(data[, c("worry", "reap")])

table(data$sex)

### men are REFERENCE GROUP (i.e., coded 0)
table(data$sex)
levels(data$sex)



########### EXAMPLE 1: CONTINUOUS IV (CENTERED), DICHOTOMOUS MODERATOR (NHST) ###########


# Fit Model Including Main Effects
m1Main <- glm(worry ~ reap_c + sex, data = data)
F_test(m1Main) 
summary(m1Main) 

# Fit Model Including Interaction 
m1Int <- glm(worry ~ reap_c + sex + reap_c*sex, data = data)
F_test(m1Int)
Anova(m1Int, type = "III", test.statistic = "F") 

r_sq(m1Main)
r_sq(m1Int)

#check model assumptions
#easystats way
library(easystats)
check_model(m1Main)

#old fashioned way


# 1. Normality of Residual
hist(residuals(m1Main), probability = T) #histogram overall
summary(residuals(m1Main))

# Superimpose density curve
lines(density(residuals(m1Main), na.rm=T), lwd = 2, col = "red")
# Superimpose best fitting normal curve
curve(dnorm(x, mean = mean(residuals(m1Main), na.rm=T), sd = sd(residuals(m1Main), na.rm=T)),
      lty = 2, lwd = 2, add = TRUE, col = "blue")
#yeah looks pretty good

#data still too big for qqPlot by group

### 2. Homoscedasticity (Constant Variance):
car::residualPlots(m1Main,
                   pch=20, col="gray",
                   fitted = T,
                   ask = F, layout = c(1,2),
                   tests = F, quadratic = F)

#sure, seems fine

#second model
# 1. Normality of Residual
hist(residuals(m1Int), probability = T) #histogram overall
summary(residuals(m1Int))

# Superimpose density curve
lines(density(residuals(m1Int), na.rm=T), lwd = 2, col = "red")
# Superimpose best fitting normal curve
curve(dnorm(x, mean = mean(residuals(m1Int), na.rm=T), sd = sd(residuals(m2Main), na.rm=T)),
      lty = 2, lwd = 2, add = TRUE, col = "blue")
#yeah looks pretty good

#data still too big for qqPlot by group

### 2. Homoscedasticity (Constant Variance):
car::residualPlots(m1Int,
                   pch=20, col="gray",
                   fitted = T,
                   ask = F, layout = c(1,2),
                   tests = F, quadratic = F)

#not that bad







# Compare models
F_change(m1Main, m1Int) 
round(cbind(summary(m1Int)$coef, confint.lm(m1Int)),3)


#visualize the relationship

apatheme <- theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='serif')) # setting up some of display options


fig1 <- interact_plot(m1Int, pred = reap_c, modx = sex,
                      modx.labels = c("Men", "Women"), 
                      x.label = 'Reappraisal (Grand-Mean Centered)', y.label = 'Worry',
                      colors = c('black', 'gray'), line.thickness = 0.6, 
                      legend.main = 'Gender', interval = TRUE, int.width = .95) +
  coord_cartesian(ylim = c(0,6), xlim = c(-1.8, 2.055)) + 
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
interact <- emtrends(m1Int, var = "reap_c", ~ sex)
# var = main predictor variable in model 
# ~ .. = moderating variable 
test(interact) # test of whether each simple slope is significantly different from 0


interact # confidence intervals for each simple slope 



########### EXAMPLE 2. CONTINUOUS IV (CENTERED), DICHOTOMOUS MODERATOR (BAYESIAN) #############

hist(data$worry, breaks = 15) # look at them fat tails
boxplot.default(data$worry)


## Main Effects Model 
b1Main <- brm(worry ~ 0 + Intercept + reap_c + sex, data = data,
              family = "student", chains = 4, iter = 2000, seed = 321)
saveRDS(b1Main, file = "b1Main_L12.rds")

## Moderation Model 
b1Int <- brm(worry ~ 0 + Intercept + reap_c + sex + reap_c*sex, 
             data = data, family = "student", chains = 4, iter = 2000, seed = 321)
saveRDS(b1Int, file = "b1Int_L12.rds")

b1Main <- readRDS("/Users/daphnehansell/Documents/GitHub/datascience")
b1Int <- readRDS("/Users/daphnehansell/Documents/GitHub/datascience")
plot(b1Main)
plot(b2Int)

pp_check(b1Main, ndraws = 50)
pp_check(b1Int, ndraws = 50) # looks good

## Goal 1: Establish if an effect exists. Does our model with these variables do better than the null model?
bfMain <- lmBF(worry ~ reap_c + sex, data = data) 
bfMain 
bfInt <- lmBF(worry ~ reap_c + sex + reap_c*sex, data = data) 
bfInt 

plot(bfInt)
## Goal 2: Estimate the parameter values for the intercept and each predictor and degree of uncertainty about them 
summary(b1Main) 
summary(b1Int)  


## PROBE Interaction: 
simSlopes <- emtrends(b1Int, ~ sex, var = "reap_c")
summary(as.mcmc(simSlopes)) 

### interact_plot also works for brms objects. The difference is that HDI's are plotted instead 
bfig1 <- interact_plot(b1Int, pred = reap_c, modx = sex,
                       modx.labels = c("men", "women"), 
                       x.label = 'Reappraisal (Grand-Mean Centered)', y.label = 'Worry',
                       colors = c('black', 'gray'), line.thickness = 0.6, 
                       legend.main = 'Gender', geom = 'line', interval = TRUE, int.width = .95) +
  coord_cartesian(ylim = c(0,6)) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 13)) + apatheme 

bfig1

## Goal 3 & 4: Compare Models. How well will our model do in predicting new data?
bayes_R2(b1Main)
bayes_R2(b1Int)

## Can also use Bayes Factors to compare other models
bfInt/bfMain 

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
#wow. 0% in the rope. yahoo





#check model assumptions 




## Normality of Residuals
pp_check(b1Int, type="error_hist", ndraws=20, set.seed(293)) # overall
pp_check(b1Int, type="error_hist_grouped", ndraws=5, group = "pts", freq = T, set.seed(293)) 
qqPlot(residuals(b1Int)) #doing ungrouped because of the maximum issue

#same weird hump as last lab??

pp_check(b2, type="error_hist", ndraws=20, set.seed(293)) # overall
pp_check(b2, type="error_hist_grouped", ndraws=5, group = "pts", freq = T, set.seed(293)) 
qqPlot(residuals(b2)) #doing ungrouped because of the maximum issue


