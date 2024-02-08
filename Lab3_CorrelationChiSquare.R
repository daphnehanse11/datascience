##############################################
####### LAB 3: Bivariate Associations  #######
##############################################

### OUTLINE:
#1. Visualizing and describing continu-ish associations (correlations)
#2. Visualizing and describing categorical associations (chi-square)

## NOTE: We're working with 2 separate data sets today. 
   # 'dataNice' for correlation --> should already have from week 1
   # 'HSB' for chi-square --> download from the data sets folder on Moodle 

# Set wd
setwd("/Users/akuelz/Desktop/315/Spr24/Lab/data") 

# Install new packages
if (!require("gmodels")) install.packages("gmodels")
if (!require("nortest")) install.packages("nortest")

# Load packages
library(tidyverse)
library(psych)
library(gmodels)
library(rties)
library(nortest)

# Get rid of the pesky scientific notation
options(scipen = 999) 

########## 1. Visualizing and describing continu-ish data and correlations ############

# Load in data 
d0 <- read.csv("dataNice.csv", stringsAsFactors = T)

# Check data loaded in correctly
str(d0)
summary(d0) 
head(d0) 

## We're going to work with 3 continu-ish variables today: 
     #1. Relationship Satisfaction
     #2. Relationship Commitment
     #3. Relationship Conflict 

## Subset the variables
num <- subset(d0, select = c(satisfy, commitment, conflict))

## Any ideas about the associations we might expect to see among these variables? 
    # Let's come up with hypotheses that incorporate direction and strength of association    

##### Descriptive Statistics 
describe(num) # what kind of distributions do you think we will see? 

##### Visualize Distributions to check for normality
histAll(num) #describe these distributions 

### Note: There are other ways of checking for normality (normality tests), but we won't use them

# Generally for these:
      # Null Hypothesis: Data were drawn from a normal distribution
      # If significant, then data are not 'normal'
      # Problems with this: turns off your brain with a simple no/go decision (look at your data!!)
      # If not enough data, there's no power so everything is normal
      # If a lot of data, even small deviations from normality will become significant

  # Shapiro-Wilk test for normality 
    shapiro.test(num$commitment) 
  
  # Kolmogorov-Smirnov test with Lilliefors Correction 
    lillie.test(num$commitment)

  # Anderson Darling test
    ad.test(num$commitment)

      
####### Visualize Associations 
    #(checking for linearity and to get a sense of the association)
plot(commitment ~ conflict, data = num) #comments on direction, strength, and type of association?
plot(commitment ~ satisfy, data = num) #comments on direction, strength, and type of association?
plot(conflict ~ satisfy, data = num) #comments on direction, strength, and type of association?

## Another way to do this (with a smaller # of variables)
pairs(num)

######## Correlation Time! 
cor(num, method = "pearson")
  # describe direction and strength

######## R^2 (variance shared between two variables)
cor(num, method = "pearson")^2 *100

## Useful way to visualize all things at once 
  # (best for small data sets with less than 6 or so variables)
  # all variables should be numeric/integer
pairs.panels(num, lm = T)


######## Let's make an APA formatted scatter plot for the association between commitment and conflict

apatheme <- theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='serif')) # setting up some of display options


s1 <- ggplot(data = num, mapping = aes(x = commitment, y = conflict)) + # specify variables and where R can find them
  geom_point(colour = "black", fill = "grey60", size = 2, 
             shape = 21, alpha = .60, position = position_jitter()) + # add the observations as shaded circles 
  geom_smooth(method = "lm", se = TRUE, fullrange = TRUE, 
              colour = "black", fill = "grey60") +  # add the linear line through the observations
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +  # scale x axis to go from 1 to 10 in increments of 1
  scale_y_continuous(breaks = seq(0, 3, by = 0.5)) + # scale y axis to go from 0 to 3 in increments of 0.5
  labs(x = "Relationship Commitment",
       y = "Relationship Conflict") + # add X and Y labels
  apatheme # add the formatting 

s1 # we must call on the object that we created to see it 

## Save the figure as a png:
ggsave(filename = "conflictByCommitmentL3.png",
       plot = s1,
       device = "png",
       width = 6,
       height = 4,
       units = "in",
       dpi = 500)


########## 2. Visualizing and describing categorical data and chi-square ############

## Read in data (comes from the High School and Beyond study that began in the 80's)
dcs <- read.csv("HSB.csv", stringsAsFactors = T)

## Check data loaded in correctly 
str(dcs)
head(dcs)

## We're going to work with: 
    # socioeconomic status (low, middle, high)
    # program type (academic, general, vocational)

## Hypothesis: There is an association between a student's SES and the type of program that choose to pursue in high school 
      # Why might we think this? 


##### Check the levels (i.e., groupings) of each variable:
levels(dcs$prog) 
levels(dcs$ses) # is there something weird about the ordering here? 
    # R often thinks it's being helpful by ordering your levels/groups by alphabetical order...which is a nuisance here 

## Fix the ordering of levels (Note: not doing this wouldn't change the results...but it will look strange in figures)
dcs$ses2 <- factor(dcs$ses, levels = c('low', 'middle', 'high'))
table(dcs$ses, dcs$ses2) #check our coding work

##### Observed Frequencies 
addmargins(table(dcs$ses2, dcs$prog, useNA = "ifany")) # frequencies 
prop.table(table(dcs$ses2, dcs$prog, useNA = "ifany")) #proportions 
prop.table(table(dcs$ses2, dcs$prog, useNA = "ifany")) * 100 # percentages

#### Visualize Breakdown of Levels 
plot(prog ~ ses2, data = dcs) # this is called a mosaic plot (a stacked bar chart)
        # easiest way to interpret is by looking at the 'thickness' of one group across others
        # for example: the 'thickness' for academic programs varies based on SES 

###### A better visualization (and the one to use for HW 1)
  # Use a bar chart to display either the proportion or percentage of each grouping

# We'll start out by plotting the proportions then the percentages: 
bar1 <- ggplot(data = dcs, mapping = aes(x=prog, fill=ses2)) + 
            geom_bar(aes(y = (..count..)/sum(..count..)), position = "dodge") +
            #scale_y_continuous(labels=scales::percent, limits = c(0, 0.25)) +
            scale_x_discrete(labels=c("Academic", "General", "Vocation")) +
            labs(x = "Program Type", y = "Proportion", fill = "Socioeconomic Status") +
            scale_fill_manual(labels=c("Low", "Middle", "High"), values=c("grey","grey60", "black")) +
            apatheme

bar1 #remember we must call on the object to display it 

## Would then save the figure as a png in the same manner that we did for the scatterplot


###### Chi-Square

CrossTable(dcs$prog, dcs$ses2, 
           prop.chisq = FALSE, chisq = TRUE, 
           expected = TRUE, sresid = TRUE, format = "SPSS", 
           dnn = c("Program Pursued", "Socioeconomic Status"))


########## Let's break this down piece by piece:

#### Assumption Check:
# Minimum Expected Frequency = 33

#### Chi-square output (Ignore for Now)

#### Standardized -Residuals
# residuals < -1.96 or > 1.96 are different than would be expected if there were no association between the variables
# The frequency in cells with large positive residuals are higher than would be expected by chance, 
# and the frequency in cells with large negative residuals are lower than would be expected by chance.

# Respondents who pursued academic programs were less likely to come from low SES backgrounds 
    # and more likely to come from high SES backgrounds 

# What about for general programs? 
# What about for vocational programs? 
