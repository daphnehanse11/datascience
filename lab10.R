
### Revisit the t-test using priors with brms

library(brms)
library(ggfortify)
library(bayestestR)

## simulate some data from 2 groups and put into a dataframe

######### Make a population with a smallish effect size and then sample a typical sized data set 

set.seed(123)
treatPop <- rnorm(n=10000, mean=8.5, sd=5) # the "true" mean for the treatment group is 8
controlPop <- rnorm(n=10000, mean=10, sd=5) # the "true" mean for the control group is 10
treatSamp <- sample(treatPop, size=20, replace=T) # in my study I get a sample of size 20 frrom each populatio
controlSamp <- sample(controlPop, size=20, replace=T)

data <- data.frame(treatSamp, controlSamp) # putting the data from the 2 samples together
data$id <- c(1:20) # create an ID variable 
colnames(data) <- c("depress1","depress2","id") # give the variables nice names
head(data)
data <- reshape(data, idvar="id", varying=c(1,2), timevar="cond", sep="", direction="long") # put the data into long format
head(data)
data$condT <- factor(data$cond, levels=c(1,2), labels=c("treat","control") ) # make the condition variable into a factor
data$condC <- relevel(data$condT, ref="control")
head(data)

## Now we're ready....

############## First we go with default priors

b1 <- brm(depress ~ 0 + Intercept + condC, family = "gaussian",
          chains = 4, iter = 2000, seed = 123, data = data) 

# Since these take awhile to run, I save the result for future use
setwd("/Users/akuelz/Desktop/315/Spr24/Lab/data")
saveRDS(b1, "S10_b1.rds") # save the model object as an RDS file

# Then we can read back in that RDS file in future sessions 
`# (note: you need to change the file path below to match your WD)
b1 <- readRDS("/Users/daphnehansell/Documents/GitHub/datascience/S10_b1.rds")

summary(b1)
plot(b1)

plot(rope(b1, ci=.95)) # chooses the range based on -0.1 * SDy, 0.1 * SDy.; setting HDI to 95%
rope(b1, ci=.95) # the treatment effect has 1.7% probability of being in the ROPE. 
# In other words, there is a 1.7% probabilty that it is so small as to be equivalent to zero.

## in reality we would want to check our residuals, but we can't do everything in one class...

###################################

## Now we investigate informative priors, let's assume based on prior research

# First find out what default priors are. Those that say nothing are improper uniform over the reals.
prior_summary(b1)

### Intercept and Slope = flat (uniform) prior over the real #'s (-inf to +inf)
ggdistribution(dunif, seq(-1000, 1000, 10), min = -1000, max = 1000, ylim = c(0,0.0005), colour = "red")

### Residual Variance (Sigma) in SD Units
# Will always be a student-t prior with 3 parameters: df, mu, sigma
# Default Sigma will always have 3 df, with a mu = 0, and a SD that it selects based on the MAD

ggdistribution(dstudent_t, seq(0, 20, 1), 
               df = 3, mu = 0, sigma = 5.4, 
               ylim = c(0,.5), colour = "blue")

################# Moving Beyond Default Priors with Small Samples #####################

## We will set up 3 priors for the condition effect (which is the only parameter in our model other than the intercept and sigma): 
# 1) strongly in favor of a large difference (let's imagine there is a lot of high quality prior research out there showing a big group difference)
# 2) weakly in favor of a small difference (there is some prior research out there but it's a little unstable)
# 2) strongly in favor of no difference (maybe this treatment is somewhat dangerous, so we only want to start using it if it REALLY reduces depression).

priorStrongDif <- set_prior("normal(-2, .75)", class="b", coef="condCtreat") 
# we are assuming a group difference of -2, which is 2 times what we created in our sample

### See what the prior we just set looks like. We can modify how strong the prior is by modifying the sd for the prior. 

# First we use a very small SD for a strong prior. 
p_strong <- ggdistribution(dnorm, seq(-7, 3, .1), # create sequence for x-axis
                           mean = -2, sd = .75, # plot the normal distribution we specified
                           ylim = c(0,1), colour = "blue")
p_strong # call on the object

#_________________________________

priorWeakDif <- set_prior("normal(-2, 2)", class="b", coef="condCtreat") 
# same difference of -2 but much less certain about this (i.e., expect there to be more variation)

# Here we set the SD larger - this is what we would do if we wanted a weakly informative prior
p_weak <- ggdistribution(dnorm, seq(-7, 3, .1), # create sequence for x-axis
                         mean = -2, sd = 2, # plot the normal distribution we specified
                         ylim = c(0,1), colour = "red", p = p_strong) # overlay onto the previous plot
p_weak #call on the object

#___________________________________________________

priorNoDif <- set_prior("normal(0,.75)", class="b", coef="condCtreat") 
# we are assuming a group difference of 0

# see what the prior we just set looks like
ggdistribution(dnorm, seq(-7, 3, .1), # create sequence for x-axis
               mean = 0, sd = .75, # plot the normal distribution we specified
               ylim = c(0,1), colour = "darkgreen", p = p_weak) # overlay onto the previous plot


################## Rerun the model with the different priors

## strong prior difference
b2 <- brm(depress ~ 0 + Intercept + condC, family = "gaussian", prior = priorStrongDif,
          chains = 4, iter = 2000, seed = 123, data = data) 
saveRDS(b2, "S10_b2.rds") # save model object for future use
b2 <- readRDS("/Users/akuelz/Desktop/315/Spr24/Lab/data/S10_b2.rds") # read back in object

# Compare default prior model with strong prior model in favor of difference:
summary(b2) 
summary(b1) # what do we conclude
plot(rope(b2, ci=.95)) # chooses the range based on -0.1 * SDy, 0.1 * SDy.; setting HDI to 95%
rope(b2, ci=.95) # the treatment effect has 0% probability of being in the ROPE. 
# In other words, there is zero probability that it is so small as to be equivalent to zero based on 95% HDI.


## weak prior dif 
b3 <- brm(depress ~ 0 + Intercept + condC, family = "gaussian", prior = priorWeakDif,
          chains = 4, iter = 2000, seed = 123, data = data) 
saveRDS(b3, "S10_b3.rds")
b3 <- readRDS("/Users/akuelz/Desktop/315/Spr24/Lab/data/S10_b3.rds")

summary(b3)
plot(rope(b3, ci=.95)) # chooses the range based on -0.1 * SDy, 0.1 * SDy.; setting HDI to 95%
rope(b3, ci=.95) # the treatment effect has .42% probability of being in the ROPE. 
# In other words, there is a .42% probability that it is so small as to be equivalent to zero.



## strong no dif prior
b4 <- brm(depress ~ 0 + Intercept + condC, family = "gaussian", prior = priorNoDif,
          chains = 4, iter = 2000, seed = 123, data = data) 
saveRDS(b4, "S10_b4.rds")
b4 <- readRDS("/Users/akuelz/Desktop/315/Spr24/Lab/data/S10_b4.rds")

summary(b4)
plot(rope(b4, ci=.95)) # chooses the range based on -0.1 * SDy, 0.1 * SDy.; setting HDI to 95%
rope(b4, ci=.95) # the treatment effect has 42% probability of being in the ROPE. 
# In other words, there is a 42% probability that it is so small as to be equivalent to zero.


## _____________________________

########## Moving Beyond Default Priors with Larger Sample Sizes ##############

# get samples of n=100 (instead of 20) and put the data back into a dataframe
set.seed(123)
treatSamp <- sample(treatPop, size=100, replace=T) 
controlSamp <- sample(controlPop, size=100, replace=T)

data <- data.frame(treatSamp, controlSamp)
data$id <- c(1:100)
colnames(data) <- c("depress1","depress2","id")
data <- reshape(data, idvar="id", varying=c(1,2), timevar="cond", sep="", direction="long")
data$condT <- factor(data$cond, levels=c(1,2), labels=c("treat","control") )
data$condC <- relevel(data$condT, ref="control")
head(data)

## strong prior difference
b5 <- brm(depress ~ 0 + Intercept + condC, family = "gaussian", prior = priorStrongDif,
          chains = 4, iter = 2000, seed = 123, data = data) 
saveRDS(b5, "S10_b5.rds")
b5 <- readRDS("/Users/akuelz/Desktop/315/Spr24/Lab/data/S10_b5.rds")


summary(b5) 
plot(rope(b5, ci=.95)) # chooses the range based on -0.1 * SDy, 0.1 * SDy.; setting HDI to 95%
rope(b5, ci=.95) # the treatment effect has 0% probability of being in the ROPE. 
# In other words, there is a zero probability that it is so small as to be equivalent to zero.


## weak prior difference 
b6 <- brm(depress ~ 0 + Intercept + condC, family = "gaussian", prior = priorWeakDif,
          chains = 4, iter = 2000, seed = 123, data = data) 
saveRDS(b6, "S10_b6.rds")
b6 <- readRDS("/Users/akuelz/Desktop/315/Spr24/Lab/data/S10_b6.rds")


summary(b6) 
plot(rope(b6, ci=.95)) # chooses the range based on -0.1 * SDy, 0.1 * SDy.; setting HDI to 95%
rope(b6, ci=.95) # the treatment effect has 0% probability of being in the ROPE. 
# In other words, there is a 0% probability that it is so small as to be equivalent to zero.


## strong no dif prior
b7 <- brm(depress ~ 0 + Intercept + condC, family = "gaussian", prior = priorNoDif,
          chains = 4, iter = 2000, seed = 123, data = data) 
saveRDS(b7, "S10_b7.rds")
b7 <- readRDS("/Users/akuelz/Desktop/315/Spr24/Lab/data/S10_b7.rds")


summary(b7)

plot(rope(b7, ci=.95)) # chooses the range based on -0.1 * SDy, 0.1 * SDy.; setting HDI to 95%
rope(b7, ci=.95) # the treatment effect has 16% probability of being in the ROPE. 
# In other words, there is a 16% probability that it is so small as to be equivalent to zero.

# how close were the estimates of the group effect to the "true" difference of 1.5 ?
describe_posterior(b1, centrality = "mean") # default priors, est = -3.47,  HDI lies on the edge of 0
describe_posterior(b2, centrality = "mean") # strong dif, small n, est = -2.27, HDI does not include 0
describe_posterior(b3, centrality = "mean") # weak dif, small n, est = -2.89, HDI does not include 0
describe_posterior(b4, centrality = "mean") # strong no-dif, small n, est = -.59,  HDI includes 0
describe_posterior(b5, centrality = "mean") # strong dif, large n, est = -1.98, HDI does not include 0
describe_posterior(b6, centrality = "mean") # weak dif, large n, est = -1.97, HDI does not include 0
describe_posterior(b7, centrality = "mean") # strong no-dif, large n, est = -.1.01, HDI lies on the edge of 0


## in summary, our most accurate answer was a strong or weakly informative prior in the right direction and lots of data.