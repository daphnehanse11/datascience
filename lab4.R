############################# Lab 5 #################################

##### Some Basics of Sampling (a.k.a. "Who gets to publish?")

## You decide to do your thesis on the effects of social media and sleep  
# Since we'll be doing confirmatory research, let's pre-register our hypothesis: 
# Is your hypothesis that:
# those who peruse social media prior to bedtime get more sleep?
# or that those who stay off social media prior to bedtime get more sleep??

## First we are going to become omnipotent (note that you don't normally get to do this step in your research)

# Create two vectors of 10,000 observations each, with different means and same SD. 
# These will provide our "population" scores of hours slept for our two groups  
# Note the "true" means for these populations will be close, but not exactly, what we specified 

set.seed(356) 
controlPop <- rnorm(n=10000, mean=8, sd=2)
hist(controlPop)
mediaPop <- rnorm(n=10000, mean=6, sd=2) 
hist(mediaPop)

mean(controlPop)
mean(mediaPop)

set.seed(NULL) # now we turn this off because we do want random sampling 

######## Study 1: We are now ready for you to sample from the population (which is what you do when you collect data for a study). 

# As a side note, we are always going to sample without replacement in the real world. 
# Sampling with replacement only gets mentioned in the book because often the math behind the stats works out simpler that way. 
# It is not relevant to actual research.

# Some of you are going to have bad luck with data collection and will have smaller sample sizes.

# If your last name starts with A - K, then you are going to use samples of size 20.
# If your last name start with L - Z, then you are going to use samples of size 500.

# So, you go and collect data for your thesis, with one sample of social media scrollers and one sample of non-scrollers. 
# You should run whichever code applies to your group, e.g., small vs. large sample size. 

# In both cases, note the very large variability in the values you get.

## Small sample, Size = 20 
controlSampS <- sample(controlPop, size=20, replace=T) 
mean(controlSampS)
hist(controlSampS)

mediaSampS <- sample(mediaPop, size=20, replace=T)
mean(mediaSampS)
hist(mediaSampS, breaks =20)

###################### Small sample folks: What sorts of means did you get? What do your histograms look like? 
#### What do you conclude about hours slept in media scrollers versus non-scrollers?

## Large sample, Size = 500
controlSampL <- sample(controlPop, size=500, replace=T) 
mean(controlSampL)
hist(controlSampL)

mediaSampL <- sample(mediaPop, size=500, replace=T)
mean(mediaSampL)
hist(mediaSampL)

###################### Large sample folks: What sorts of means did you get? What do your histograms look like? 
#### What do you conclude about hours slept in media scrollers versus non-scrollers?


## Most likely, the people with the large samples got means closer to the population means, with more normally distributed scores as well. 
# This is the "Law of Large Numbers" (e.g., estimate of the mean is getting closer to the true value) 
# and "Central Limit Theorem" (e.g., the sample distribution is becoming more normally distributed) at work 
# and why people always say larger samples are better, or more reliable, than smaller samples


######## Replication studies: Ok, the committee was satisfied and you've graduated, but now you want to publish. 
# The reviewers and editor agree that you need to conduct some replication studies. 
# So, you re-run your study 10 times with your original sample size and report the means in your paper. 

# Note: This is what NHST reasoning is based on -- as if you really were going to redo the study 10 times! 

## Small sample, Size = 20 
controlRepS <- replicate(10, mean(sample(controlPop, size=20, replace=T)), simplify=T) 
mediaRepS <- replicate(10, mean(sample(mediaPop, size=20, replace=T)), simplify=T)
controlRepS
mediaRepS

###################### Small sample folks: How spread out do your values appear to be?


## Large sample, Size = 500 
controlRepL <- replicate(10, mean(sample(controlPop, size=500, replace=T)), simplify=T) 
mediaRepL <- replicate(10, mean(sample(mediaPop, size=500, replace=T)), simplify=T)
controlRepL
mediaRepL

###################### Large sample folks: How spread out do your values appear to be?

## Darn, the editor isn't satisfied. She asks how you can tell from a bunch of means whether social media negatively affects sleep? 
# So, you try reporting the means of the means.

mean(controlRepS)
mean(mediaRepS)

###################### Small sample folks: How close are your means to the population means?

mean(controlRepL)
mean(mediaRepL)

###################### Large sample folks: How close are your means to the population means?

# She writes back, "But how do we know the difference between those two means is large compared to how much variance there is? 
# Show me histograms of the two distributions and if they don't appear to overlap very much I'll be convinced." So, you do that:

par(mfrow=c(2,1))
hist(controlRepS)
hist(mediaRepS) # Note they are hard to eyeball due to the x-axis being aligned differently
# set both x-axes to be the same
hist(controlRepS, xlim=c(3, 11), ylim=c(0,6))
hist(mediaRepS, xlim=c(3, 11), ylim=c(0,6))

###################### Small sample folks: Do your histograms appear to overlap? If they do, your paper is rejected. If they don't, yay! you get to publish :)

par(mfrow=c(2,1))
hist(controlRepL, xlim=c(4.5, 10), ylim=c(0,6))
hist(mediaRepL, xlim=c(4.5, 10), ylim=c(0,6))

###################### Large sample folks: Do your histograms appear to overlap? If they do, your paper is rejected. If they don't, yay! you get to publish :)



################ In Summary: Sampling Distributions

## NHST inference rests on "Sampling Distributions", which are the theoretical probability distributions for
# different statistics (e.g., for the mean of a sample of size n). 
## In other words, what would we expect the distribution of a statistic (e.g., the mean) to be if we could 
# repeat our experiment an infinite number of times?

# Sampling distributions in turn rest on the "Law of Large Numbers" (e.g., estimate of the mean getting closer to the true value with large samples and large number of samples) 
# and the "Central Limit Theorem" (e.g., the sample distribution becoming normally distributed with large number of samples)

# We can see this in action by replicating a very large number of samples of size n and seeing what happens to the distribution of the mean

# First with small samples. How strong is the evidence about hours slept in scrollers vs non-scrollers?
controlDistSmall <- replicate(10000, mean(sample(controlPop, size=10, replace=T)), simplify=T) 
mediaDistSmall <- replicate(10000, mean(sample(mediaPop, size=10, replace=T)), simplify=T)
par(mfrow=c(2,1))
hist(controlDistSmall, xlim=c(0, 13), ylim=c(0,3000))
hist(mediaDistSmall, xlim=c(0, 13), ylim=c(0,3000)) 

# Now with large samples. How strong is the evidence about hours slept in scrollers vs non-scrollers? What changed? e.g., did the mean change, or??
controlDistLarge <- replicate(10000, mean(sample(controlPop, size=100, replace=T)), simplify=T) 
mediaDistLarge <- replicate(10000, mean(sample(mediaPop, size=100, replace=T)), simplify=T)
par(mfrow=c(2,1))
hist(controlDistLarge, xlim=c(3, 11), ylim=c(0,3000))
hist(mediaDistLarge, xlim=c(3, 11), ylim=c(0,3000)) 

# The means aren't all that different between small/large samples
mean(controlDistSmall)
mean(controlDistLarge)
mean(mediaDistSmall)
mean(mediaDistLarge)

# The spread of the data is what really changes
sd(controlDistSmall)
sd(controlDistLarge)
sd(mediaDistSmall)
sd(mediaDistLarge)

## Taking a first step from sampling to inference:

## So, we see we get mostly sample means that are close to the true means and the bigger the samples, 
# the smaller the variance of the means; but how unlikely are the unlikely ones??
# Let's look at the histogram of the sampling distribution for large samples of non-scrollers  again. 
# How likely would it be for a single study with a sample size of 500 that sampled from the non-scroller population 
# to get a mean for hours slept of 8.1? How about for a mean of 6.1? 
hist(controlDistLarge)


## So, why do we care? We can use the sampling distributions to reason about new results we might see. 
# For example, if I got a sample of 500 sleep scores and found a mean of 8.2, how likely do you think it 
# is that my participants are non-scrollers? How about similar social media scrollers? 
# Or if the mean I got was 6.1, what would you say?  

par(mfrow=c(2,1))
hist(controlDistLarge, xlim=c(3, 11), ylim=c(0,3000))
hist(mediaDistLarge, xlim=c(3, 11), ylim=c(0,3000)) 




