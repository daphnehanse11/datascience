###############################################
### LAB 2: Describing and Visualizing Data  ###
###############################################

#### OUTLINE:
  #1. Looking at our data + practice with spotting mistakes and recoding variables
  #2. Descriptive Statistics for Categorical variables (+presentation style tables)
  #3. Descriptive Statistics for Quantitative variables (+presentation style tables)
  #4. Introduction to ggplot2 package for creating figures
  #5. Visualizing data for exploration


#### Install New Packages 
if (!require("flextable")) install.packages("flextable")
if (!require("table1")) install.packages("table1")
if (!require("rstatix")) install.packages("rstatix")
if (!require("tidyverse")) install.packages("tidyverse")

#### Load Packages
library(psych)
library(rties)
library(flextable)
library(table1)
library(tidyverse)
library(rstatix)

## What is the first thing you should do when you open RStudio and want to work with data? Set our working directory!

setwd("/Users/akuelz/Desktop/315/Spr24/Lab/data") # this is my path.. your path WILL BE different 
#OR - 1) Session > 2)Set Working Directory > 3) Choose Directory
getwd()

#### Load in Data
d0 <- read.csv("Garcia.csv", stringsAsFactors = T)

## *** Two simple things we should do after loading in a dataset ***:
  str(d0) #str stands for structure. think of it like a table of contents
  # num stands for numeric (vectors are numeric when elements contain decimals) - descriptive statistics can be computed
  # int stands for integer (similar to numeric but only whole #'s) - descriptive statistics can be computed but it may not make sense
  # factor - this is R's version of a categorical variable 

  summary(d0) # summary provides common summary statistics 
  # summary statistics reported for continuous variables
  # frequencies reported for categorical/factor variables

  # another way to look at summary statistics using describe in the psych package:
  describe(d0, quant = c(.25, .75), IQR = T) 

## the min and max values for two of these variables appears to be capped at either (0,2) or (0,1)
  # these may not be truly quantitative or "continu-ish"  
  #let's check out their distributions using histograms 
histAll(d0) # what are your thoughts on 'protest'? what about anger? 

# these "numbers" appear to be labels only
  # let's look at the information on the dataset to see what the labels correspond to
  # open description of data

## There are two variables (protest and anger) that are not truly continuous based on the way they were measured
  # We have some 'cleaning up' to do since both are currently recognized as integer variables (i.e., quantitative variables)
  # Although we know what these variables should be, R currently does not. Time to catch R up to speed! 


########## Recode 'protest' into it's appropriate form: Categorical Variable (i.e., Factor in R)

## Base R Way: 
d0$protestF <- factor(d0$protest, levels = c(0,1,2), 
                      labels = c("No Protest", "Individual Protest", "Collective Protest"))

## Most functions in R share a similar structure:
   # Comprised of an object or objects that you want to perform the function on 
   # Mandatory and optional arguments. These are followed by commas inside the parentheses 
## Here we have two additional arguments:
   # Levels: These are the "numbers" that were meant to serve as labels/placeholders for groupings within the categorical variable
   # Labels: This is where we assign the actual labels to each "number". These must be in the order that the levels show up. 

## check to make sure our values line up with the original 'protest' variable:
table(d0$protest, useNA = "ifany") # remember that 0, 1, and 2 are not real numbers. these are labels 
table(d0$protest, d0$protestF, useNA = "ifany") # yes! we're excellent coders so everything went as planned :) 


## Tidyverse Approach: 

d0 <- d0 %>% mutate(
  protest_f=case_when(protest == 0 ~ "No protest",
                      protest == 1 ~ "Individual Protest",
                      protest == 2 ~ "Collective Protest"),
  protest_f = factor(protest_f, levels=c("No protest",  "Individual Protest", "Collective Protest")))

# Let's break down this line of code as it includes some elements we may not be familiar with
  # Starting with 'mutate' --> this is a tidyverse function for recoding variables 
  # protest_f --> this is the new factor variable we are creating 
  # case_when --> this is a tool within mutate that allows us to use if/then logic 
     # (e.g., if the original variable protest is equal to 0, the new factor variable protest_f will equal "No protest")
  # notice within the case_when parentheses, the tilda symbol "~" represents the logic of "then"
  # also notice the comma after every line inside the case_when parentheses, excluding the final line


########## Recode 'anger' into a Categorical Variable (i.e., Factor in R)

## Let's practice doing this using the data description:
d0$angerY <- factor(d0$anger, levels = c(0,1), 
                      labels = c("No Anger", "Anger"))

# Check our coding work
table(d0$anger, d0$angerY)

########### Describing Data: Categorical Variables ###########

## Now that we have accurately coded our categorical variables, let's start working to describe the frequencies 

#what percentage were in the 'No Protest' group?
table(d0$protestF, useNA = "ifany") # frequencies
prop.table(table(d0$protestF,useNA="ifany")) # proportions
prop.table(table(d0$protestF,useNA="ifany")) * 100 # percentages 


## Create a table that includes our correctly identified categorical variables using the table1 function
   # We choose to save this an object 'freq' so that we can export the resulting table into a Word document for later use
freq <- table1(~ angerY + protestF, data = d0)
freq # Notice that we needed to call on the defined object in order to see what was stored within 

## Now we use the flextable package to save the html output from the table1 function to a word document in our working directory
   # note that this uses the pipe %>% 
t1flex(freq) %>% 
  save_as_docx(path="L2_frequencies.docx")

## Later we will open this document and manipulate it to look "better" 


######### Describing Data: Contui-ish/Quantitative Variables #########

# summary and describe functions are the quickest/easiest for exploration
summary(d0)
describe(d0)

## For presentation purposes we will use a different function that works nicely with the flextable package

## Generate descriptive statistics using 'get_summary_stats' function in the 'rstatix' package
get_summary_stats(d0) # statistics only calculated for variables identified as numeric/integer
  # oops..we still need to get rid of the original versions of 'protest' and 'anger'

num <- subset(d0,  select = c(sexism, liking, respappr)) # base R way

num_tv <- d0 %>% dplyr::select(sexism, liking, respappr) # tidyverse version using the '%>%

## Use get_summary_statistics on this new dataframe with appropriate variables
  # Then use flextable function to display results 
  desc <- get_summary_stats(num) # statistics only calculated for variables identified as numeric/integer
  flextable(desc) 

## For reporting (and HW..hint), we want sample size, mean, sd, median, IQR, Q1, Q3, min and max
  # i.e., remove 'mad', 'se', and 'ci' from the object
## We will also take this time to rearrange the columns such that statistics follow a more cohesive structure
desc1 <- desc[ ,c(1:4, 10:11, 5:8)]
flextable(desc1)

## We again use the flextable package to save the output displayed in Viewer as a word document in our working directory
flextable(desc1) %>% 
  save_as_docx(path="L2_descriptives.docx")

## Later we will open this document and manipulate it to look "better" 


######## VISUALIZING DATA (Plotting) ########

#### Introduction to ggplot (Grammar of Graphics) #####

## ggplot stands for the "Grammar of Graphics," drawing from the idea that you can construct 
  # any visual with a group of rudimentary parts.

## This will be an example of creating a slightly fancy scatterplot.

# Step 1: Specify the data
ggplot(data = d0)

# Step 2: Specify the aesthetic mapping
ggplot(data = d0, 
       mapping = aes(x = respappr,
                     y = liking))

# Step 3: Specify the geometric object
ggplot(data = d0, 
       mapping = aes(x = respappr,
                     y = liking)) +
  geom_point(colour = "black", 
             fill = "grey60", 
             size = 2, 
             shape = 21, 
             alpha = .40, 
             position = position_jitter()) 
  # geom_point adds the observed data points 
  # colour = color of shape 'borders'
  # fill = specify color of shape 'insides'
  # size = size of shapes
  # shape = specify actual shape
  # alpha = the shading inside of the shape
  # position_jitter = helpful when we have multiple data points lying on top of each other

# Step 4: Add a regression line
ggplot(data = d0, 
       mapping = aes(x = respappr,
                     y = liking)) +
  geom_point(colour = "black", 
             fill = "grey60", 
             size = 2, 
             shape = 21, 
             alpha = .60, 
             position = position_jitter()) + 
  geom_smooth(method = "lm", 
              se = TRUE, 
              fullrange = TRUE, 
              colour = "black", 
              fill = "grey60") 

# Step 5: Add in some labels and fix scaling
ggplot(data = d0, 
       mapping = aes(x = respappr,
                     y = liking)) +
  geom_point(colour = "black", 
             fill = "grey60", 
             size = 2, 
             shape = 21, 
             alpha = .60, 
             position = position_jitter()) + 
  geom_smooth(method = "lm", 
              se = TRUE, 
              fullrange = TRUE, 
              colour = "black", 
              fill = "grey60") +
  scale_x_continuous(breaks = seq(1, 7, by = .5)) +  
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  labs(x = "Perceived Appropriateness",
       y = "Target Likeability",
       caption = "Data from the sexism (protest) study of Garcia et al., 2010",
       title = "Target Likeability by Perceived Response Appropriateness")

# Step 6: Deal with background lines and other formatting

apatheme <- theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='serif')) # setting up some of display options


ggplot(data = d0, 
       mapping = aes(x = respappr,
                     y = liking)) +
  geom_point(colour = "black", 
             fill = "grey60", 
             size = 2, 
             shape = 21, 
             alpha = .60, 
             position = position_jitter()) + 
  geom_smooth(method = "lm", 
              se = TRUE, 
              fullrange = TRUE, 
              colour = "black", 
              fill = "grey60") + 
  scale_x_continuous(breaks = seq(1, 7, by = .5)) +  
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  labs(x = "Perceived Appropriateness",
       y = "Target Likeability",
       caption = "Data from the sexism (protest) study of Garcia et al., 2010",
       title = "Target Likeability by Perceived Response Appropriateness") +
  apatheme


# Step 7: Store as an object so we can export from R

scatterH1 <- ggplot(data = d0, 
                    mapping = aes(x = respappr,
                                  y = liking)) +
  geom_point(colour = "black", 
             fill = "grey60", 
             size = 2, 
             shape = 21, 
             alpha = .60, 
             position = position_jitter()) + 
  geom_smooth(method = "lm", 
              se = TRUE, 
              fullrange = TRUE, 
              colour = "black", 
              fill = "grey60") + 
  scale_x_continuous(breaks = seq(1, 7, by = .5)) +  
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  labs(x = "Perceived Appropriateness",
       y = "Target Likeability") +
  apatheme

## Call on the object
scatterH1

## Save the object as a .png 
ggsave(filename = "scatter_plot.png",
       plot = scatterH1,
       device = "png",
       width = 6,
       height = 4,
       units = "in",
       dpi = 500)


#### Visualizing (for Exploration) ####

## Histograms are always good to look at (which we did prior)
histAll(d0)
hist(d0$liking)

## Plot  is a workhorse function. # It will return: 
  # boxplots, scatterplots, barcharts, mosaic plots 
  # Depends on the type of variable(s) that you're working with 

## Barchart Example
plot(d0$protestF, 
     xlab = "Protest Condition", 
     ylab = "Frequency") # labels are completely optional
  #barchart is displayed because protestF is recognized as a factor

## Scatterplot Example 
plot(liking ~ respappr, data = d0)
  #scatterplot is displayed because both variables are recognized as quantitative 

## Boxplot Example
boxplot(d0$respappr) # single variable example 
plot(respappr ~ protestF, data = d0) 
  # boxplot is displayed because "predictor" is recognized as a factor 

## Mosaic Plot Example
plot(protestF ~ angerY, data = d0)
  #mosaic is displayed because both variables are recognized as factors 


## Note: We can make all of these + more in ggplot
  # We will see some examples next week 
