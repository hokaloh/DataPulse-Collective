# preprocessing tutorial link 
# https://www.section.io/engineering-education/data-preprocessing-in-r/


#clear the environment
rm(list = ls())
rm(Dataset)

#preprocessing
#NOTE: you need to include the dataset in the correct application path
getwd() # It is used to find the application path
#1. creating a dummy dataset
install.packages('caTools')
library(caTools)


# Directory 
setwd("C:/Users/syahi/Documents/Degree UTHM/DOC/Data Mining/DataPulse-Collective/Datasets") # change directory, "/"
getwd() # Show current directory


#2. work with an existing dataset
Dataset = read.csv('dataset.csv')
View(Dataset)
length(Dataset)

#  check missing values,
#sum(!complete.cases(Dataset))
#sum(is.na(Dataset))
which(is.na(Dataset),arr.ind=TRUE)



meanData <- mean(Dataset$Age, na.rm = TRUE)

Dataset$Age = ifelse(is.na(Dataset$Age),
                     ave(Dataset$Age, FUN = function (x)mean(x, na.rm = TRUE)), # 
                     Dataset$Age)
Dataset$Salary = ifelse(is.na(Dataset$Salary),
                        ave(Dataset$Salary, FUN = function (x)mean(x, na.rm = TRUE)),
                        Dataset$Salary)
Dataset$Country = factor(Dataset$Country, 
                         levels = c('France','Spain','Germany'), 
                         labels = c(1.0, 2.0 , 3.0 ))
Dataset$Purchased = factor(Dataset$Purchased,
                           levels = c('No', 'Yes'),
                           labels = c(0, 1))
View(Dataset)
summary(Dataset)

#split dataset to training and testing sets
#123 is set as the random number value
set.seed(123)
# This function returns true if observation goes to the Training set (80%) and false if observation goes to the test set (20%).
split = sample.split(Dataset$Purchased, SplitRatio = 0.8)
training_set = subset(Dataset, split == TRUE)
test_set = subset(Dataset, split == FALSE)

length(test_set)
training_set
test_set
training_set[, 2:3] = scale(training_set[, 2:3])
View(training_set)
test_set[, 2:3] = scale(test_set[, 2:3])
View(test_set)