if (!require("apaTables")) install.packages("apaTables")
options(scipen = 999) 
library(tidyverse)
library(car)
library(psych)
library(emmeans)
library(apaTables)
library(rties)
library(brms)
library(BayesFactor)


#I saw this package on twitter that apparently has one fucntion to check model assumptions?
library(easystats)

#Load data and functions 
natdis<-read.csv("naturalDisaster.csv", stringsAsFactors = TRUE)
source("userfunctions.R")

#First look at the data 
str(natdis)
summary(natdis)
head(natdis)

natdis_uncentered<-read.csv("naturalDisaster.csv", stringsAsFactors = TRUE)


varofint<-subset(natdis, select = c(pts, aaq, age, hurte, mindful, therap, wome, race))
histAll(varofint)

#women therapy and race should not be int. lets make them factors

natdis$therapyF <- factor(natdis$therapy, levels = c(0,1), labels = c("No", "Yes"))
natdis$genderF <- factor(natdis$women, levels = c(0,1), labels = c("Man", "Woman"))
natdis$raceF <-factor(natdis$race, levels = c(0,1,2), labels = c("White", "Black", "Asian"))

#Look at data again (pts ~ aaq + women + age + therapy + race)
histAll(natdis_uncentered)

plot(pts ~ aaq, data = natdis)
plot(pts ~ age, data = natdis)
plot(pts ~ genderF, data = natdis)
plot(pts ~ therapyF, data = natdis)
plot(pts ~ raceF, data = natdis)


genderD<- natdis %>%
  group_by(genderF) %>%
  get_summary_stats(pts); genderD 

raceD<- natdis %>%
  group_by(raceF) %>%
  get_summary_stats(pts); raceD 


#data looks normalish. should center aaq and age. maybe more when I add my own variables later

natdis<- natdis %>% mutate(
  age_c = age - mean(age, na.rm = TRUE),
  aaq_c = aaq - mean(aaq, na.rm = TRUE))


quant <- subset(natdis, select = c(pts, aaq, age, mindful, hurte))
pairs.panels(quant)
apaTables::apa.cor.table(quant, filename = "homework4cortable.docx")



#lets build model 1

m1f<- glm(pts ~ aaq_c + genderF + raceF + age_c + therapyF,  data = natdis,
          family = "gaussian", na.action = na.exclude)

#i want to try the new way of checking model assumptions
check_model(m1f)
#the chart is a bit ugly but i think this package is cool/saves time

summary(m1f)
F_test(m1f)
r_sq(m1f)
#super mega significant model


#now to add my new variables. I think mindfulness and hurricane are promising 

#first center the variables/look at plots

plot(pts ~ mindful, data = natdis)
plot(pts ~ hurte, data = natdis)


natdis<- natdis %>% mutate(
  mindful_c = mindful - mean(mindful, na.rm = TRUE),
  hurte_c = hurte - mean(hurte, na.rm = TRUE))





m2f<- glm(pts ~ aaq_c + genderF + raceF + age_c + therapyF +mindful_c +  hurte_c ,  data = natdis,
          family = "gaussian", na.action = na.exclude)

#i want to try the new way of checking model assumptions
check_model(m2f)
#the chart is a bit ugly but i think this package is cool/saves time

summary(m2f)
F_test(m2f)
r_sq(m2f)

# also significant. 




#bayes time >:)


b1 <- brm(pts ~ 0 + aaq_c + genderF + raceF + age_c + therapyF,  data = natdis,
          family = "skew_normal", chains = 4, iter = 2000, seed = 123)
saveRDS(b1, file = "b1_hw4.rds")

b2 <- brm(pts ~ 0 + aaq_c + genderF + raceF + age_c + therapyF +mindful_c +  hurte_c ,  data = natdis,
          family = "skew_normal", chains = 4, iter = 2000, seed = 123)
saveRDS(b2, file = "b2_hw4.rds")

b1 <- readRDS("/Users/daphnehansell/Documents/GitHub/datascienceb/b1_hw4.rds")
b2 <- readRDS("/Users/daphnehansell/Documents/GitHub/datascience/b2_hw4.rds")

#Establish if an effect exists
lmBF(pts ~ aaq_c + genderF + raceF + age_c + therapyF,  data = natdis)
lmBF(pts ~ aaq_c + genderF + raceF + age_c + therapyF +mindful_c +  hurte_c ,  data = natdis) 

#strong evidence for both over the null



#goal 2 nhst. check model assumptions the old fashioned way


# 1. Normality of Residual
hist(residuals(m1f), probability = T) #histogram overall
summary(residuals(m1f))

# Superimpose density curve
lines(density(residuals(m1f), na.rm=T), lwd = 2, col = "red")
# Superimpose best fitting normal curve
curve(dnorm(x, mean = mean(residuals(m1f), na.rm=T), sd = sd(residuals(m1f), na.rm=T)),
      lty = 2, lwd = 2, add = TRUE, col = "blue")

#bit skewed. weird qqplot

## By Group: 
qqPlot(residuals(m1f), groups = natdis$pts) 

#this code didn't work because there are too many variables in my big dataframe
natdis_sub <- natdis[, c("pts", "aaq_c", "genderF", "raceF", "age_c", "therapyF", "mindful_c", "hurte_c")]

qqPlot(residuals(m1f), groups = natdis_sub$pts) 

#still too big, out of ideas
qqPlot(residuals(m1f))


### 2. Homoscedasticity (Constant Variance):
car::residualPlots(m1f,
                   pch=20, col="gray",
                   fitted = T,
                   ask = F, layout = c(1,2),
                   tests = F, quadratic = F)

Anova(m1f, type = "III", test.statistic = "F") 
sr2(m1f)


#still seems like only aaq is predictive 

#second model
hist(residuals(m2f), probability = T) #histogram overall
summary(residuals(m2f))

# Superimpose density curve
lines(density(residuals(m2f), na.rm=T), lwd = 2, col = "red")
# Superimpose best fitting normal curve
curve(dnorm(x, mean = mean(residuals(m2f), na.rm=T), sd = sd(residuals(m1f), na.rm=T)),
      lty = 2, lwd = 2, add = TRUE, col = "blue")

#yeah looks pretty normal, bit skewed


### 2. Homoscedasticity (Constant Variance):
car::residualPlots(m2f,
                   pch=20, col="gray",
                   fitted = T,
                   ask = F, layout = c(1,2),
                   tests = F, quadratic = F)

Anova(m2f, type = "III", test.statistic = "F") 
sr2(m2f)

#aaq and hurte are the only significant IVs. but they seem very significant 
#both are continuous variables so no post-hoc controls required i think

#model comparisons 

F_change(m1f, m2f)
m1fr2<-r_sq(m1f)
m2fr2<-r_sq(m2f)


#model 2 does explain more variance in pts



#back to bayes

plot(b1)
plot(b2)

summary(b1)
summary(b2)


pp_check(b1, ndraws=30) 
pp_check(b2, ndraws=30) # checking predictive accuracy




## Normality of Residuals
pp_check(b1, type="error_hist", ndraws=20, set.seed(293)) # overall
pp_check(b1, type="error_hist_grouped", ndraws=5, group = "pts", freq = T, set.seed(293)) 
qqPlot(residuals(b1)) #doing ungrouped because of the maximum issue

#these residuals are kinda weird. whats with the hump

pp_check(b2, type="error_hist", ndraws=20, set.seed(293)) # overall
pp_check(b2, type="error_hist_grouped", ndraws=5, group = "pts", freq = T, set.seed(293)) 
qqPlot(residuals(b2)) #doing ungrouped because of the maximum issue

#weird hump again...

#Constant Variance
ggplot(natdis, aes(x = pts, y = residuals(b1)[,1]))+
  geom_point(size=2)+geom_jitter(width=0.1)

ggplot(natdis, aes(x = pts, y = residuals(b2)[,1]))+
  geom_point(size=2)+geom_jitter(width=0.1)




# rope
r1 <- rope(b1)
r1
plot(r1)

r2 <- rope(b2)
r2
plot(r2)

#PROBABILIY OF DIRECTION
pd1 <- p_direction(b1)
pd1
plot(pd1)

pd2 <- p_direction(b2)
pd2
plot(pd2)




#cohen's d
difYN1 <- summary(b1)$fixed[6,1]

sigma1 <- summary(b1)$spec_pars[1,1]

cd1<-difYN1/sigma1


difYN <- summary(b2)$fixed[6,1]

sigma <- summary(b2)$spec_pars[1,1]

cd<-difYN/sigma



#model comparison NHST

F_change(m1f, m2f)

#model 2 seems better 
r_sq(mf1)
r_sq(m22)



#Bayesian 

## Goal 4: Model Comparison
b1 <- add_criterion(b1, criterion = "waic")
b2 <- add_criterion(b2, criterion = "waic")

loo_compare(b1, b2, criterion = "waic") 
