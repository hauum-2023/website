## HEADER ####
## Who: <YOUR NAME>
## What: <YOUR TOPIC e.g. Bootcamp page xx)
## Last edited: <DATE TODAY in yyyy-mm-dd format)
####


## CONTENTS ####
## 00 Setup
## 01 2.1 2 sample t-test 
## 02 2.2 Compare 1 sample to known mean 
## 03 2.3 Paired samples
## 04 3.2 Gaussian assumption
## 05 3.3 Properly testing Gaussian
## 06 5.1 t-test for 2 independent samples


## 00 Setup ####

setwd()

## 01 2.1 2 sample t-test  ####

## 2 sample t-test long format data
density <- c(rep('high', 7), rep('low', 7))
height <- c(2,3,4,3,4,3,2,
            6,8,6,9,7,8,7)

(long.data <- data.frame(density,height))


## 2 sample t-test wide format data

(wide.data <- data.frame(high.ht = c(2,3,4,3,4,3,2),
                         low.ht = c(6,8,6,9,7,8,7)))

# Boxplot
boxplot(height ~ density, data = long.data,
        main = "2 independent samples")

# Optional: add raw data points
# jitter() nudges the x-axis placement so that the points do not overlap
set.seed(42)
points(x = jitter(c(1,1,1,1,1,1,1,
                    2,2,2,2,2,2,2), 
                  amount = .2),
       y = long.data$height,
       col = "red", pch = 16, cex = .8) # Mere vanity


## 02 2.2 Compare 1 sample to known mean ####

# The data
mysam <- c(2.06, 1.77, 1.9, 1.94, 1.91, 1.83, 
           2.08, 1.84, 2.15, 1.84, 
           2.05, 2.19, 1.64, 1.81, 1.83)

boxplot(mysam, 
        main = "Is your sample population different from the dashed line?")

points(x = jitter(rep(1,15), amount = .1),
       y = mysam,
       col = "red", pch = 16, cex = .8) # Mere vanity

abline(h = 2.0,
       col = "blue", lty = 2, lwd = 2)  # Mere vanity

## 03 2.3 Paired samples ####

# Biochar application, measure N before and after

# Data 
# (the code are kind of ugly, but run it to "make" biochar)
biochar <- structure(list(
  plot = c("A", "B", "C", "D", "E", "F", "G", "H", "I", 
           "J", "K", "L", "M", "N", "O"), 
  N.first = c(13.4, 16.7, 17.9, 18.5, 18.6, 18.6, 18.7, 
              20.5, 20.6, 21.5, 24.2, 24.5, 25, 27.1, 28.1), 
  N.second = c(16, 16.7, 18.7, 18.7, 22.1, 22.7, 23.1, 
               23.1, 23.2, 23.5, 25.4, 25.9, 27.6, 28, 29.7)), 
  class = "data.frame", 
  row.names = c(NA, -15L))

biochar

# boxplot() would work, but hides pairwise relationship
# Try this:

plot(x = jitter(c(rep(1,15), rep(2,15)),amount = .02),
     y = c(biochar$N.first, biochar$N.second),
     xaxt = "n", xlim = c(0.5, 2.5),
     cex = .8, col = "blue", pch = 16,  # Mere vanity
     xlab = "Biochar treatment",
     ylab = "Soil N",
     main = "Do the lines tend to increase?")

mtext(side = 1, at = 1:2, text = c("before", "after"), line = 1)

# Get crazy: add horizontal lines to visualize the plot pairs
for(i in 1:15){
  lines(x = c(1.05,1.95),
        y = c(biochar$N.first[i], biochar$N.second[i]),
        lty = 2, lwd = 1, col = "red") # Mere vanity
}        

## 04 3.2 Gaussian assumption ####

# First make some data as an example.

# This is height in cm data measured from 30 females and 30 males
# NB because we simulate this data, we literally specify the 
# samples are indeed from DIFFERENT, GAUSSIAN populations.
# Obviously in a real sampling situation you would not know this,
# hence we will test it formally.

set.seed(1)
height <- c(rnorm(30,185,10),
            rnorm(30,150,10))
sex <- c('m','m','m','m','m','m','m','m','m','m',
         'm','m','m','m','m','m','m','m','m','m',
         'm','m','m','m','m','m','m','m','m','m',
         'f','f','f','f','f','f','f','f','f','f',
         'f','f','f','f','f','f','f','f','f','f',
         'f','f','f','f','f','f','f','f','f','f')

# Package the variables we made into a data frame
# and remove the variables outside the dataframe to 
# keep our global environment tidy (not required, but satisfying!)

data <- data.frame(height,sex)
rm(height, sex)

# plot raw data
hist(x = data$height, 
     main = 'Height ignoring sex',
     xlab = 'Height (cm)',
     freq = F)

# Draw expected Gaussian


# Draw overall mean and the means
# for each level of sex

abline(v = c(mean(data$height),                 # mean overall
             mean(data$height[data$sex=='f']),  # mean f
             mean(data$height[data$sex=='m'])), # mean m
       lty = c(1,2,2),                          # line types for each
       lwd = 3,                                 # line width for all 3
       col = c("black", "red", "blue"))         # colour for each

# Draw expected Gaussian density curve
mv <- data$height                               # generalizes following code

xcoord <- seq(min(mv),                          # make x coordinates
              max(mv),
              length = length(mv))
ycoord <- dnorm(x = xcoord,                     # make y coordinates
                mean = mean(mv),
                sd = sd(mv))

lines(x = xcoord, y = ycoord,                  # draw curve
      col = 'darkgreen', lwd = 3)

# for clarity here, add on some labels...
text(x = 155, y = .01, labels = "f\nmean", col = "red")
text(x = 182, y = .011, labels = "m\nmean", col = "blue")
text(x = 172.1, y = .015, labels = "grand\nmean")
text(x = 140, y = .012, labels = "Gaussian\ndensity", col = "darkgreen")


## q-q norm
qqnorm(data$height,
       main = "Q-Q Gaussian line for our Height data")
qqline(data$height)


## Shapiro test
shapiro.test(data$height)


## 05 3.3 Properly testing Gaussian ####
# step 1 graph a hist() of the continuous variable
# separately for each factor level

par(mfrow = c(2,1))                                # set so hist's "stack"

# draw males hist
hist(data$height[data$sex == 'm'],                 # select males
     xlim = c(min(data$height), max(data$height)), # set limit for ALL data
     col = "blue", freq = F,
     xlab = "Height (cm)", main = "Males") 

mv <- data$height[data$sex == 'm']
xcoord <- seq(min(mv),                          # make x coordinates
              max(mv),
              length = length(mv))
ycoord <- dnorm(x = xcoord,                     # make y coordinates
                mean = mean(mv),
                sd = sd(mv))

lines(x = xcoord, y = ycoord,                  # draw curve
      col = 'lightblue', lwd = 3)

# draw females hist
hist(data$height[data$sex == 'f'],                 # select females
     xlim = c(min(data$height), max(data$height)), # set limit for ALL data
     col = "red", freq = F,
     xlab = "Height (cm)", main = "Females")

mv <- data$height[data$sex == 'f']
xcoord <- seq(min(mv),                          # make x coordinates
              max(mv),
              
              length = length(mv))
ycoord <- dnorm(x = xcoord,                     # make y coordinates
                mean = mean(mv),
                sd = sd(mv))
lines(x = xcoord, y = ycoord,                   # draw curve
      col = 'pink', lwd = 3)

# set default layout back
par(mfrow = c(1,1)) 

## QQ Plots

par(mfrow = c(1,2))

# males
qqnorm(data$height[data$sex == 'm'],
       main = "Q-Q Gaussian line for male height")
qqline(data$height[data$sex == 'm'])

# females
qqnorm(data$height[data$sex == 'f'],
       main = "Q-Q Gaussian line for female height")
qqline(data$height[data$sex == 'f'])

par(mfrow = c(1,1))

## Shapiro Tests

shapiro.test(data$height[data$sex == 'm'])

shapiro.test(data$height[data$sex == 'f'])


## 06 5.1 t-test for 2 independent samples ####
# 2-sample t-test
# Try this
# data
density <- c("high","high","high","high","high","high","high",
             "low","low","low","low","low","low","low")
height <- c(2.1,3.5,4.3,3.2,4.5,3.7,2.7, 
            6.1,8,6.9,9.1,7.5,8,7.4)
(treegrowth <- data.frame(density,height))

# There is not much data to compare to the Gaussian distribution
library(car) # for qqPlot()

hist(treegrowth$height[treegrowth$density == "low"])

qqPlot(treegrowth$height[treegrowth$density == "low"])

hist(treegrowth$height[treegrowth$density == "high"])

qqPlot(treegrowth$height[treegrowth$density == "high"])

# The histograms are a little "wooly", but there are no huge 
# deviations from the expectation of Gaussian and the 
# q-q plots look ok: proceed

# ?t.test

# NB 1 - the x argument can be a formula
# x = height ~ density
# or we can set our samples to x and y respectively
# x = height[low], y = height[high]

t.test(formula = height ~ density, 
       data = treegrowth)