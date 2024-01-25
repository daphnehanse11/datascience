#######################################
### LAB 1: Introduction to RStudio  ###
#######################################

##### *Potentially new terminology that you will hear today and then again, again, again*:
   # 1. R script (aka this whole document)
   # 2. Packages
   # 3. Functions
   # 4. Global Environment 
   # 5. Working Directory

####### Outline:
   # 1. Get R and RStudio installed 
   # 2. Behold the RStudio environment (each window, packages, functions, etc.)
   # 3. Set our working directory
   # 4. Load in some data
   # 5. Play with functions, Manipulate Data, Visualize Data
   # 6. Make a new script (copy, paste, find and replace)


####### Behold RStudio #######

# What do you see?

##### Four spaces/windows

#### Top Left (i.e., the "Brain"): ####
# The executive functioning piece that sends orders to all other windows (more on this in a second)
# This is where all of your R scripts will reside for a given session 
# This is where you will write and 'run' code (more on this in a second)
# Also where you will document what you're doing by leaving commments 

## Opening our first R script (aka the 'conductor' of our session today):
# File -> Open File -> Navigate to "Lab1_Intro.R" -> Click 'Open'
# Observe what happened

## What are comments? This is one! 
# anything followed by a pound sign/hashtag 
# incredibly useful for both own sanity as well as others
# leaving notes/comments of what you did and why you did it
# R ignores things followed by a # and only pays attention to the other parts

## Lines of Code/Comments: 
# Here we are at line 46
# Each line is separately interpreted by R but can be 'grouped' together as a 'chunk' as well 

## Running Lines of Code (i.e., the 'Brain' makes contact with other windows)
46*2 
# Command+Enter (Mac) or Control+Enter(Windows)
# Alternatively you can select the 'Run' icon 
   # Observe what happened 

#____________________________________________________________________

#### Bottom Left (the Console i.e., "glorified calculator") ####

# When you 'run' code, the 'Brain' communicates to the console and output will be displayed there 
# This also means that any errors caused by bugs in code will show up in the console as well. Pay attention to your output :)

# Note: We won't use the Terminal in this course

#____________________________________________________________________

#### Bottom Right (the 'grab bag') ####

# Many things live here. One of the  important tabs that we will use is:

#### Plots Tab: Anything that we plot will show up in the plot tab
  # Example:
  plot(dnorm(seq(1,100,.01), mean = 50, sd = 45))

#### Packages: This tab requires some further explanation

##### Introduction to packages (installing and loading) 

# Two kinds of packages that we will use:

#1. Packages that come with base R (no installation necessary) 
# sort of like preinstalled apps on your phone but more useful :)

#2. Packages that you need to install (only once) and then load (every time)
# these packages are more like library books, you can check them out as many times as you want but eventually have to return them 
  # again, this is because R suffers from memory loss between sessions 

# Within packages, are things called functions (e.g., mean). 
  # These are what we will use to perform statistical analyses
  # Anything that is followed by () is a function. Examples:

mean() #observe the error 
hist()
str()
summary()

# Example of a base R function (no installation necessary)
?mean
  # Question marks can be used to get more information (i.e., documentation about a function)

#### Installing Packages: 
install.packages("psych", dependencies = T)

#### Loading Packages:
# When you need to use R to use a specific skill, you need to 'enable' it with the knowledge of a package. 
library(psych)
  # Notice that the psych package is now "checked off" in the packages tab meaning it is ready to be used

# Get more information on the package we installed:
help(package="psych") # we know help is a function because it is followed by parentheses 
  
# Help Tab:
  # When we ask for more information on a function, package, or dataset, the documentation will show up here 
  # You can also search for things within the help tab itself 

# Viewer Tab: 
  # We will only use this for specific applications (e.g., next week when we make a frequency table)

#### Top Right (i.e., the memory) ####

### Environment = any data, objects, etc. created or imported by you during a session 
# If the object is not in the environment, R doesn't know about it and cannot use it until you remind R 
# More on the environment in a minute! 

# Note: we likely won't use the other tabs 

#____________________________________________________________________

####### Working Directory #######

## In general, this is the location on your computer where you want to import, export, save, etc things to from R

## We can think of this as R's working memory for any session. 
# R has frequent memory loss and must be reminded where things live on your hard drive
# Anything you save, or import will happen in the working directory you choose.  
# It is best to have everything you need to work on a project in one folder.
# I prefer to 'hard code' the wd into my R scripts because it takes too much of my own working memory to remember where things are 

## How to Set:
# Session -> Set Working Directory -> Choose Directory-> Navigate to Folder Where Data is Stored -> Select 'Open'
# Now copy that blue path starting with 'setwd' and paste it into your R script for future use

## An example of a path:
#setwd("~/Desktop/315/Spr24/Lab/data")
  # we know that setwd is a function because it is followed by parentheses

# Check that this worked:
getwd()

#____________________________________________________________________

####### Loading in Datasets #######

# but first! notice the switch in subsections in the R script from Working Directory to Loading in Datasets 
   # this is because of the way I've used the pound signs
   # can be helpful when navigating through lengthy R scripts full of code

# We will primarily work with .csv files by purposeful design but R can handle pretty much any type of data
  # We will see this later on 

## Importing the data for today: 
d <- read.csv("dataNice.csv", stringsAsFactors = T)

#Alright what happened here? 
#The "<-" stores whatever argument(s) you specify into the object that you define 
 # Think of it like an arrow where the stuff on the right is point to the object on the left 

#Wait- what? What is an object?
#An object can be a number of things floating around in the R environment. 
#It will exist there in space in time for the duration of your R session. 

# Right now we only have one object called 'd' which happens to be our dataframe

# Let's play with this object

#____________________________________________________________________

####### Play with Functions, Manipulate and Visualize Data #######

## These are some of my most commonly used base functions after loading in a dataset: 

str(d) # structure --> provides a list of the variables in the data, along with how R recognizes them, and the first few observations for each 

#### Quick Aside:
# R understands categorical variables as 'Factors' that have levels. These levels are the groups (e.g., Yes/No)
# R understands quantitative variables as either integers (i.e., whole number variables) or numeric variables 


summary(d) # quick way to get a 5-number summary for all quantitative variables and frequencies for categorical variables
     # this is a base R function that can always be used. R will always remember this function

describe(d) # this is a function that comes from the 'psych' package. to use it we need to 'check it out' like a library book
             # note here that we get some additional descriptive measures that can be helpful 
             # and flags all categorical/factor variables with a *, these descriptives should be ignored


# We can also look at the descriptive statistics for only certain variables at a time: 
summary(d$bmi)
  # We can think of the dollar sign as a way of reaching inside the object 'd' and 
      # pulling out only the information for bmi 



## Let's visualize the distributions of some of these variables using histograms: 
hist(d) # hmm..any ideas on why this does not work? 
hist(d$bmi) 


## We can use a function in a package that I co-wrote to plot histograms for all variables at once:
install.packages("rties", dependencies = T)
library(rties)
histAll(d)
  # note the arrows are used to move back and forth
  # zoom can be used to enlarge what you see
  # export can be used to save the plot displayed on your hard drive 
  # broom (both in the plot tab and in the environment tab) is used to erase/delete everything there. only do this if you mean it :) 

# what do we think about the distribution of love? 
# what about commitment? 
# what about ambivalence? 


table(d$ambivalence) 
  # Yes. This variable is binary and should be recognized as a factor but isn't right now

# Example of how to do that (don't worry we will come back to these things in much more detail later):
d$ambivY <- factor(d$ambivalence, levels = c(0,1), labels = c("No", "Yes"))
table(d$ambivalence, d$ambivY) #check our work 

# notice that we made a change to the object 'd'. 
   # If we wanted to save this to our hard drive such that the changes would be there for next time, we have to export the data onto our hard drive 
   # The function below is exactly how to do that
   write.csv(d, file = "informativeName.csv", row.names = F)
          
 

#### The Subset Function  (will see it many many many more times)
newObject <- subset(dataFrame, condition, select = c(optionalSelection))

# first argument (i.e., dataFrame) is dataset that you want to subset from
# second argument is a condition or conditions that you create. this is optional. only cases that satisfy these conditions will be stored into the new object
# third argument is used to select only specific variables that you want to bring forward to the new object. 
    # this is also optional, but can be really helpful when trying to reduce the number of variables that you want to work with 

# Let's practice:
d2 <- subset(d, d$bmi < 25, select = c(bmi, ageYears))
  # notice that rather than overwriting 'd', we made a new object called 'd2'
  # this is important! and allows you to go back to the original data in case a bug occurs later on 
  # always, always, always try and do this where you can 

# What is c? It's a function (we know this because it is followed by parentheses)
    #It stands for concatenate. 
    #Think of c as glue, where it "glues" elements or objects together
    #Another example using c:
data <- c(1, 7, seq(20, 30, .5), 100)
data #notice that we call on the object to see what's inside of it 


# Indexing into objects using their positions: 
data[5] #fifth position in 'data'
data[c(2,5)] # second and fifth position in 'data'
data[3:5] #third through fifth position in 'data' 
   # This is pretty abstract. We will have more concrete example of 'why' we would do this later on 

#an example of user created functions!
add2things <- function(n,m) {
  return(n+m)
}

add2things(2,8)

### () Functions
### [] Positions
### {} Code


####### Make a new script, saving script, find and replace, other misc #######




