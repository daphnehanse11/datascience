GSS <- read.csv("~/Downloads/GSS_primary.csv", stringsAsFactors = TRUE)


# Load packages
## Load packages
library(tidyverse)
library(car) 
library(psych) 
library(emmeans) 
library(brms)
library(BayesFactor)
library(bayestestR)
library(bayesplot)
library(coda)
library(flextable)
library(rstatix)


options(scipen = 999) 

#organizing my data
summary(GSS)

histAll(GSS)

str(GSS)
describe(GSS, IQR = T, quant = c(.25, .75))
histAll(GSS)


GSS<-get_summary_stats(GSS)
flextable(GSS)


##NHST t-test
#order the factor, unmarried is reference 

GSS$marryY <- factor(GSS$marryY, levels = c(0,1))

## Generating summary statistics of income by marriage status
describeBy(GSS$income, group = GSS$marryY, IQR = T) 
 

## looking at the association and checking for outliers with a boxplot
plot(GSS$income ~ GSS$marryY,
     main = "income by marriage Status",
     xlab = "marriage Status",
     ylab = "Income",
     col = "Grey")

## Checking that there are sufficient observations in each group
table(GSS$marryY)



## histogram
hist(GSS$income)
#not very normal

## Fit the model
m1f <- glm(income ~ marryY, data = GSS, 
          family = "gaussian", na.action = na.exclude)


#check model assumptions:

### 1. Normality of Residuals 
hist(residuals(m1f), probability = T, ylim = c(0, 0.05)) 
summary(residuals(m1f))

# Superimpose density curve
lines(density(residuals(m1f), na.rm=T), lwd = 2, col = "red")
# Superimpose best fitting normal curve
curve(dnorm(x, mean = mean(residuals(m1f), na.rm=T), sd = sd(residuals(m1f), na.rm=T)),
      lty = 2, lwd = 2, add = TRUE, col = "blue")



## By Group: 
qqPlot(residuals(m1f), groups = GSS$marryY) #by group

### 2. Homoscedasticity (Constant Variance):

car::residualPlots(m1f,
                   pch=20, col="gray",
                   fitted = T,
                   ask = F, layout = c(1,2),
                   tests = F, quadratic = F)





#interpret the findings
summary(m1f)

var(GSS$income)



###### Remember those linear combinations from the equations in lecture? We can get R to do this automatically for us: 
cont1f <- emmeans(m1f, specs = "marryY")
summary(cont1f) 
pairs(cont1f) # expected mean difference between groups 
confint(pairs(cont1f)) # CI's for expected mean difference 

######### Effect Size Calculation 

###### Model Effect Size (R-squared)
r_sq <- function(model_name) {
  r2 <- 1 - model_name$deviance/model_name$null.deviance
  return(r2)
}

r_sq(m1f) 

###### Predictor Effect Size: Standardized Mean Difference (Cohen's d)

# emmeans approach 
emm <- emmeans(m1f, specs="marryY")
eff_size(emm, sigma= sigma(m1f), edf = df.residual(m1f))





################# Bayesian Version #####################

hist(GSS$income)


b1 <- brm(income ~ 0 + Intercept + marryY, family = "skew_normal",
          chains = 4, iter = 2000, seed = 123, data = GSS) 


## Now, before we interpret the output, let's check for evidence of convergence
summary(b1) 
plot(b1)



## Generate expected posterior means for both groups (for reporting in write up)
cont1 <- emmeans::emmeans(b1, specs = "marryY")
hpd.summary(cont1, point.est = mean)

## Generate expected posterior difference between groups 
pairs(cont1)

#Visualize association
conditional_effects(b1) 



######### Effect Size Calculation 

###### Model Effect Size (R-squared)
bayes_R2(b1) # proportion of variance of income explained by the model

##### Standardized Mean Difference (Cohen's d)
#R said that the function we were using for this was deprecated so I googled how to find cohen's D, but nothing I tried worked. 

##### REGION of PRACTICAL EQUIVALENCE 
r <- rope(b1)
r
plot(r)

##### PROBABILITY OF DIRECTION
pd <- p_direction(b1)
pd
plot(pd)



########### Check Model Assumptions (same as NHST)
pp_check(b1, ndraws=30) # checking predictive accuracy: it's predicting fairly well

##### Normality of Residuals
pp_check(b1, type="error_hist", ndraws=20, set.seed(293)) # overall
pp_check(b1, type="error_hist_grouped", ndraws=5, group = "marryY", freq = T, set.seed(293)) # checking residuals by group
qqPlot(residuals(b1)[,1], groups = GSS$marryY)


##### Constant Variance
ggplot(GSS, aes(x = marryY, y = residuals(b1)[,1]))+
  geom_point(size=2)+geom_jitter(width=0.1)








##Anova time
#reorder that factor
GSS$sesF <- factor(GSS$sesF, levels = c("l", "m", "h"))
apatheme <- theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='serif'))


plot(GSS$income ~ GSS$sesF,
     main = "Income By Self-Described Socio-Economic Status",
     xlab = "SES",
     ylab = "Income",
     col = "Grey")


ggplot(data = GSS, mapping = aes(x=sesF, y=income))+
  geom_point(size=2) +
  geom_jitter(width=0.1) +apatheme

###### Visualize distribution of outcome:
hist(GSS$income, breaks = 20)

###### Check the levels of SES:
levels(GSS$sesF) 


######################### 2. NHST VERSION ####################

## Fit the model
m2f <- glm(income ~ sesF, data = GSS, 
          family = "gaussian", na.action = na.exclude)

#check model assumptions:

### 1. Normality of Residuals (test statistics and p-values rely on this):
hist(residuals(m2f), probability = T,  breaks = 20) #histogram overall
summary(residuals(m2f))

# Superimpose density curve (no assumptions made about normality)
lines(density(residuals(m2f), na.rm=T), lwd = 2, col = "red")
# Superimpose best fitting normal curve
curve(dnorm(x, mean = mean(residuals(m2f), na.rm=T), sd = sd(residuals(m2f), na.rm=T)),
      lty = 2, lwd = 2, add = TRUE, col = "blue")

# the closer the two lines are together, the 'better' the normality assumption is met 


qqPlot(residuals(m2f)) #overall
qqPlot(residuals(m2f), groups = GSS$sesF) #by group

### 2. Homoscedasticity (Constant Variance):
# A violation of the constant variance assumption results in inaccurate confidence intervals 
# and p-values, even in large samples, although regression coefficient estimates will still be unbiased 
car::residualPlots(m2f,
                   pch=20, col="gray",
                   fitted = T,
                   ask = F, layout = c(1,2),
                   tests = F, quadratic = F)





Anova(m2f, type = "III", test.statistic = "F")
# MS model: SS model / numerator DF
8075/2 
# MS residual: SS residual / denominator DF
37083/597

# F test statistic: MS model / MS residual
4037.5 / 62.11558

## We have established a significant effect of sesFram overall
# reject the null hypothesis that all groups receive equal income related test scores

## Do we know which group or groups are significantly different from one another? 
# NOW it's our job to figure out what the significant conGSSasts are AND conGSSol for multiple comparisons

##### Multiple Comparisons (showing TukeyHSD and Bonferroni)

## Create an emm object to be used for multiple comparison adjustment
emm1 <- emmeans(m2f, specs = "sesF") # specs = the groups

emm1 #model predicted income scores for each sesFram (these are the means for each group)

## Correct our Type I error rate using Tukey's Honest Signficant Differences 
tukey <- pairs(emm1) #default is Tukey
tukey
confint(tukey) 

## Correct our Type I error rate using Bonferroni (much more conservative; use this approach in the HW)
bonf <- pairs(emm1, adjust = "bonferroni")
bonf
confint(bonf)



##### Effect size (Cohen's d)
emmeans::eff_size(emm1, sigma = sigma(m2f), edf = df.residual(m2f))

##### Model Effect Size (R2)
r_sq <- function(model_name) {
  r2 <- 1 - model_name$deviance/model_name$null.deviance
  return(r2)
}

r_sq(m2f) 



######################### 3. BAYESIAN VERSION ####################

# Will use skew_normal likelihood for income related test scores to better meet assumptions

# Fit the Model
b2 <- brm(income ~ 0 + Intercept + sesF, family = "skew_normal",
          chains = 4, iter = 2000, seed = 123, data = GSS) #note: will take a minute to compile your computer isn't frozen :), that's just MCMC working hard behind the scenes

### before we interpret, let's check for evidence of convergence (look at Rhat's, ESS, and the GSSace plots)
summary(b2) # good convergence (Rhat = 1.00, ESS is close to 4000)
plot(b2) #do we have evidence that chains converged?


########### Check Model Assumptions (same as NHST)
pp_check(b2, ndraws=30) # checking predictive accuracy

##### Normality of Residuals
pp_check(b2, type="error_hist", ndraws=20, set.seed(293)) # overall
pp_check(b2, type="error_hist_grouped", ndraws=5, group = "sesF", freq = T, set.seed(293)) # checking residuals by group
qqPlot(residuals(b2)[,1], groups = GSS$sesF)


##### Constant Variance
ggplot(GSS, aes(x = sesF, y = residuals(b1)[,1]))+
  geom_point(size=2)+geom_jitter(width=0.1)



BF1 <- lmBF(income ~ sesF, data=GSS)
BF1 
# returns the Bayes Factor (odds ratio) in favor of our model over the empty (null) model
# We want this ratio to be as high as possible** (greater than one).  


## Do we know which group or groups are credibly different from one another? 

# getting posterior medians
emm2 <- emmeans(b1, "sesF")
emm2
summary(as.mcmc(emm2)) # posterior means for groups with HDI's
pairs(emm2) # posterior median differences with respective HDI's

#visualize the association
conditional_effects(b1)
# visually: no overlap between acad-gen and acad-voc HDIs suggesting large effect sizes. 
# lower HDI for general and upper HDI for vocational are closer together, suggesting small effect size
# Agrees with NHST, with academic scores highest, followed by general and then vocational


##### Effect Size Calculation

#### Model R2
bayes_R2(b2)

#### Cohen's d (standardized mean difference)
# use the user created function that is specific for a GLM model with a 3 group predictor

cohenD3 <- function(model_name) {
  dif1 <- summary(model_name)$fixed[2,1] # get dif in mean scores between reference group and group represented by B1
  dif2 <- summary(model_name)$fixed[3,1] # get dif in mean scores between reference group and group represented by B2
  dif3 <- dif2 - dif1 # get dif in mean scores between non-reference groups
  sigma <- summary(model_name)$spec_pars[1,1] # pull out sigma
  es1 <- abs(dif1/sigma) # calculate standardized mean dif between reference group and group represented by B1
  es2 <- abs(dif2/sigma) # calculate standardized mean dif between reference group and group represented by B2
  es3 <- abs(dif3/sigma) # calculate standardized mean dif between non-reference groups 
  final <- rbind(es1, es2, es3) 
  return(final)
}

cohenD3(b2)


###### ROPES
r <- rope(b1)
r
plot(r)


###### PROBABILIY OF DIRECTION
pd <- p_direction(b1)
pd
plot(pd)




