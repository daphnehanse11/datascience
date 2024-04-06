
###################### LAB 9 ANOVA ######################

### OUTLINE:
#1. Guiding hypothesis, visualizing the data, and checking assumptions
#2. NHST version of ANOVA (interpreting output, multiple comparisons, effect size)
#3. Bayesian version of ANOVA (checking convergence, interpreting output, HDI's, effect size)


## Set Working Directory
setwd("/Users/akuelz/Desktop/315/Spr24/Lab/data")


## Install packages
if (!require("BayesFactor")) install.packages("BayesFactor")
if (!require("coda")) install.packages("coda")


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

## Get rid of the pesky scientific notation
options(scipen = 999) 

## Load Data
d1 <- strings2factors(HSB_subset)
str(d1)
summary(d1)

################ 1. Guiding hypothesis and visualizing data ###############

## Null Hypothesis:
# Students receive similar STEM test scores regardless of program

## Alternative Hypothesis:
# At least one group of students receive different STEM test scores 

###### Descriptive statistics:

#create an object called 'desc' that contains descriptive statistics of stem scores BY program
desc <- d1 %>%
  group_by(prog) %>%
  get_summary_stats(stem)

desc 

# we want the usual summary statistics: sample size, min, max, mean, sd, median, iqr, Q1 and Q3
desc1 <- desc[ ,c(1, 3:5, 11:12, 6:9)]
desc1

flextable(desc1) #use flextable function to display output in viewer tab

## We again use the flextable package to save the output displayed in Viewer as a word document in our working directory
flextable(desc1) %>% 
  save_as_docx(path="L9_descriptives.docx")


###### Visualize association using box plot
plot(d1$stem ~ d1$prog,
     main = "STEM Test Scores by Highschool Program",
     xlab = "Program",
     ylab = "STEM Score",
     col = "Grey")
# describe what you see (make note of trends and outliers)

ggplot(data = d1, mapping = aes(x=prog, y=stem))+
  geom_point(size=2) +
  geom_jitter(width=0.1)

###### Visualize distribution of outcome:
hist(d1$stem, breaks = 20)

###### Check the levels of program:
levels(d1$prog) # which group will represent our intercept? 

## Code to relevel the groups if needed:
# d1$prog <- relevel(d1$prog, ref = "academic")

######################### 2. NHST VERSION ####################

## Fit the model
m1 <- glm(stem ~ prog, data = d1, 
          family = "gaussian", na.action = na.exclude)

######### First, let's check model assumptions:

### 1. Normality of Residuals (test statistics and p-values rely on this):
hist(residuals(m1), probability = T, ylim = c(0, 0.06), breaks = 20) #histogram overall
summary(residuals(m1))

# Superimpose density curve (no assumptions made about normality)
lines(density(residuals(m1), na.rm=T), lwd = 2, col = "red")
# Superimpose best fitting normal curve
curve(dnorm(x, mean = mean(residuals(m1), na.rm=T), sd = sd(residuals(m1), na.rm=T)),
      lty = 2, lwd = 2, add = TRUE, col = "blue")

# the closer the two lines are together, the 'better' the normality assumption is met 


qqPlot(residuals(m1)) #overall
qqPlot(residuals(m1), groups = d1$prog) #by group

### 2. Homoscedasticity (Constant Variance):
# A violation of the constant variance assumption results in inaccurate confidence intervals 
# and p-values, even in large samples, although regression coefficient estimates will still be unbiased 
car::residualPlots(m1,
                   pch=20, col="gray",
                   fitted = T,
                   ask = F, layout = c(1,2),
                   tests = F, quadratic = F)


### 3. Linearity
# irrelevant with one multi-categorical predictor


### 4. Independence
# data were drawn from a simple random sample. assumption met by default 


#### Our goal is to be as transparent as possible and contribute to replicable science. 
# So, what would we say about the residuals in terms of 
# normality around 0?
# constant variance? 

# It looks like we're fine. I might check to see what results looked like with person 510 in and out because they've been flagged a few times now
# If results don't change that much, we're fine. If results did change....uh oh!


#################### Finally ready to interpret our findings! 

###### First step is to establish whether there is a significant effect of program overall
Anova(m1, type = "III", test.statistic = "F")
# MS model: SS model / numerator DF
8075/2 
# MS residual: SS residual / denominator DF
37083/597

# F test statistic: MS model / MS residual
4037.5 / 62.11558

## We have established a significant effect of program overall
# F(2,597) = 65.00, p < .0001
# reject the null hypothesis that all groups receive equal STEM related test scores

## Do we know which group or groups are significantly different from one another? 
# NOW it's our job to figure out what the significant contrasts are AND control for multiple comparisons

##### Multiple Comparisons (showing TukeyHSD and Bonferroni)

## Create an emm object to be used for multiple comparison adjustment
emm1 <- emmeans(m1, specs = "prog") # specs = the groups

emm1 #model predicted stem scores for each program (these are the means for each group)

## Control our Type I error rate using Tukey's Honest Signficant Differences 
tukey <- pairs(emm1) #default is Tukey
tukey # model predicted pairwise differences in stem scores (controlling for our Type I error rate)
confint(tukey) # adjust confidence intervals for predicted pairwise differences 

## Control our Type I error rate using Bonferroni (much more conservative; use this approach in the HW)
bonf <- pairs(emm1, adjust = "bonferroni")
bonf
confint(bonf)

## All of our comparisons are significant
# Academic students received significantly higher STEM scores compared to general and vocational students
# Students on the general track received significantly higher STEM scores compared to vocational

# How do we know if our significant mean differences are meaningful?
# Does the difference make a difference? 

##### Effect size (Cohen's d)
emmeans::eff_size(emm1, sigma = sigma(m1), edf = df.residual(m1))

##### Model Effect Size (R2)
r_sq <- function(model_name) {
  r2 <- 1 - model_name$deviance/model_name$null.deviance
  return(r2)
}

r_sq(m1) 

##### plot means with 95% confidence intervals 

## create table with group means and 95% confidence intervals
d1_g <- d1 %>% dplyr::group_by(prog) %>% 
  dplyr::summarize(n = length(stem),
                   mean = mean(stem),
                   sd   = sd(stem),
                   se   = sd / sqrt(n)) %>% 
  mutate (upper = mean + 1.96*se,
          lower = mean - 1.96*se) 

d1_g

## plot group means with 95% confidence intervals
# x = grouping var, y = group mean, group = grouping var
fig1 <- ggplot(d1_g, mapping = aes(x=prog, y = mean, group = prog)) +
  geom_point() + # plots the means
  geom_errorbar(data=d1_g, # draws the CI error bars
                aes(ymin=lower, ymax=upper, color=prog), 
                width=.2, show.legend = F) + # ymin = lower CI, ymax = upper CI, color = grouping var, width = horizontal width of CI var
  scale_x_discrete(labels=c("Academic", "General", "Vocation")) +
  labs(x = "Program Type", y = "Average STEM Score") 

fig1 # can then save this using 'ggsave' function 

######################### 3. BAYESIAN VERSION ####################

# Will use skew_normal likelihood for STEM related test scores to better meet assumptions

# Fit the Model
b1 <- brm(stem ~ 0 + Intercept + prog, family = "skew_normal",
          chains = 4, iter = 2000, seed = 123, data = d1) #note: will take a minute to compile your computer isn't frozen :), that's just MCMC working hard behind the scenes

### before we interpret, let's check for evidence of convergence (look at Rhat's, ESS, and the trace plots)
summary(b1) # good convergence (Rhat = 1.00, ESS is close to 4000)
plot(b1) #do we have evidence that chains converged?


########### Check Model Assumptions (same as NHST)
pp_check(b1, ndraws=30) # checking predictive accuracy

##### Normality of Residuals
pp_check(b1, type="error_hist", ndraws=20, set.seed(293)) # overall
pp_check(b1, type="error_hist_grouped", ndraws=5, group = "prog", freq = T, set.seed(293)) # checking residuals by group
qqPlot(residuals(b1)[,1], groups = d1$prog)


##### Constant Variance
ggplot(d1, aes(x = prog, y = residuals(b1)[,1]))+
  geom_point(size=2)+geom_jitter(width=0.1)


#################### Finally ready to interpret our findings! 

###### First step is to establish whether the model with program is favored over the empty model
#A Bayes Factor is an odds ratio that results from the comparison of two different models.
# This gives us a tool for thinking about the strength of evidence in favor of one model over the other. 

BF1 <- lmBF(stem ~ prog, data=d1)
BF1 
# returns the Bayes Factor (odds ratio) in favor of our model over the empty (null) model
# We want this ratio to be as high as possible** (greater than one).  

### Our model is 149,692,604,307,939,998,564,352 more likely than the empty model
# i.e., we have VERY STRONG evidence that differences in STEM scores based on program type is credible


## Do we know which group or groups are credibly different from one another? 

# getting posterior medians
emm2 <- emmeans(b1, "prog")
emm2
summary(as.mcmc(emm2)) # posterior means for groups with HDI's
pairs(emm2) # posterior median differences with respective HDI's
# All contrasts appear credible (i.e. HDI's do not include 0)

#visualize the association
conditional_effects(b1)
# visually: no overlap between acad-gen and acad-voc HDIs suggesting large effect sizes. 
# lower HDI for general and upper HDI for vocational are closer together, suggesting small effect size
# Agrees with NHST, with academic scores highest, followed by general and then vocational


##### Effect Size Calculation

#### Model R2
bayes_R2(b1)

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

cohenD3(b1)
# .70 is effect size for comparison between academic and general students 
# these groups are .70 SDs apart
# 1.1 is effect size for comparison between academic and vocational students
# these groups are 1.1 SDs apart 
# .37 is effect size for comparison between general and vocational students
# these groups are .37 SDs apart

###### ROPES
r <- rope(b1)
r
plot(r)


###### PROBABILIY OF DIRECTION
pd <- p_direction(b1)
pd
plot(pd)