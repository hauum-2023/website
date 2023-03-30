## HEADER ####
## Who: Ed H
## What: Bootcamp page 7
## Last edited: 2023-03-28
####


## CONTENTS ####
## 00 Setup
## 01 Read in data
## 02 summary() and aggregate()
## 03 hist()
## 04 boxplot()


## 00 Setup ####

# Load necessary libraries
# install.packages("openxlsx")

## 01 Read in data ####

# Download the 7-chickwts.xlsx file, read it into a data 
# object in R called "chicks", 
# and convert the "feed" variable to a factor if necessary.

# Do not neglect looking inside the "raw" data file
# Is it as you expect?  Is the data dictionary present and clear?


library(openxlsx)

# this is MY directory - your will be different
setwd(r'(C:\Dropbox\git-hauum-2023\website\files\bootcamp07)')

# Read file
chicks <- read.xlsx("7-chickwts.xlsx")

# for web files this works too!
chicks <- read.xlsx("https://rstats-bootcamp.github.io/website/data/7-chickwts.xlsx")


# Convert feed to factor if needed
class(chicks$feed) # Character
chicks$feed <- factor(chicks$feed)
class(chicks$feed) # Factor

## 02 summary() and aggregate() ####

# Try this:

# Summarize the whole dataset
# summary() provides summary statistics for numeric variables and counts
summary(chicks)

# we might want to look at summary for different levels of feed
?summary
summary(object = chicks$weight[which(chicks$feed == "casein")])
summary(object = chicks$weight[which(chicks$feed == "horsebean")])
# etc. - this method is easy but inelegant?

# aggregate()
?aggregate

# mean
aggregate(x = chicks$weight, by = list(chicks$feed), FUN = mean)

# standard deviation
aggregate(x = chicks$weight, by = list(chicks$feed), FUN = sd)

# You can make your own function for the FUN argument
# standard error of mean, SEM = standard deviation / square root of sample size
aggregate(x = chicks$weight, by = list(chicks$feed), 
          FUN = function(x){ sd(x)/sqrt(length(x)) })

# You can apply several functions and name them!
aggregate(x = chicks$weight, by = list(feed = chicks$feed), 
          FUN = function(x){ c(mean = mean(x), 
                               sd = sd(x),  
                               SEM = sd(x)/sqrt(length(x)))})


## 03 hist() ####

# The least you can do
help(hist)
hist(x = chicks$weight)

##~~~~~~~~~~~~~~~
# Argument main
hist(x = chicks$weight,
     main = "Distribution of chick weights (all feeds)")

#~~~~~~~~~~~~~
# x axis title
hist(x = chicks$weight,
     main = "Distribution of chick weights (all feeds)",
     xlab = "Chick weight (grams)")

#~~~~~~~~~~~~~~~~~
# Add vertical line for mean weight
hist(x = chicks$weight,
     main = "Distribution of chick weights (all feeds)",
     xlab = "Chick weight (grams)")

help(abline)
abline(v = mean(chicks$weight), 
       col = "green", lty = 2, lwd = 3)


## 04 boxplot() ####
# Try a boxplot

help(boxplot)
boxplot(x = chicks$weight)

# I have seen worse graphs, but I can't remember when.
# Flash challenge: Improve the graph

# weight as a function of feed
boxplot(formula = weight ~ feed,
        data = chicks)
# This is probably a good representation of our hypothesis
# Flash challenge: Improve the graph...
