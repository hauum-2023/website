## HEADER ####
## Who: Ed H
## What: Mem_data example
## Last edited: 2023-03-28
####


## CONTENTS ####
## 01 Start script
## 02 setwd()
## 03 Read in the data
## 04 Graph 01 hist() Zn
## 05 Graph 02 boxplot() Zn ~ Area

## 01 Start script ####
# install.packages('open.xlsx')

library(openxlsx)

## 02 setwd() ####
# download data file and put in a new folder 
# create the the folder in a "good location"

getwd()

# this is my own folder location - yours will be different!!
# C:\Dropbox\git-hauum-2023\website\files\Mem_data
# setwd(r'(C:\Dropbox\git-hauum-2023\website\files\Mem_data)')

## 03 Read in the data ####
# Read file
cycads <- read.xlsx("BOT A.xlsx")

head(cycads)

## 04 Graph 01 hist() Zn ####

# no effort graph
hist(cycads$Zn)

# better graph
x <- hist(cycads$Zn,
     main = '',
     ylab = '%',
     xlab = 'Zinc (mg/Kg)',
     col = 'goldenrod',
     freq = F)

# graph with %

barplot(x$counts / sum(x$counts),
        main = '',
        ylab = '%',
        xlab = 'Zinc (mg/Kg)',
        col = 'goldenrod')

## 05 Graph 02 boxplot() Zn ~ Area ####

# no effort graph
boxplot(Zn ~ Area, data = cycads)

# better graph
boxplot(Zn ~ Area, data = cycads,
        ylab = 'Zinc (mg/Kg)',
        col = c('dodgerblue', 'gray70'))


        