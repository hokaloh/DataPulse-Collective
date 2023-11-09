# preprocessing tutorial link 
# https://www.section.io/engineering-education/data-preprocessing-in-r/


#clear the environment
rm(list = ls())

#preprocessing
#NOTE: you need to include the dataset in the correct application path
getwd() # It is used to find the application path
setwd("C:/Users/syahi/Documents/Degree UTHM/Data Mining/Rcode/Datasets")
#1. creating a dummy dataset
install.packages('caTools')
library(caTools)
Dataset = load(file='RW9.rda')
#creating variables that take on random values
set.seed(0)
#Create a dataset from random variables
Dataset<- data.frame(x1=rnorm(500),x2=rnorm(500),x3=rnorm(500),x4=rnorm(500),x5=rnorm(500),x6=rnorm(500),x7=rnorm(500),x8=rnorm(500),x9=rnorm(500),x10=rnorm(500),x11=rnorm(500),x12=rnorm(500),x13=rnorm(500),x14=rnorm(500),x15=rnorm(500),
                     x16=rnorm(500),x17=rnorm(500),x18=rnorm(500),x19=rnorm(500),x20=rnorm(500),x21=rnorm(500),x22=rnorm(500),x23=rnorm(500),x24=rnorm(500),x25=rnorm(500),x26=rnorm(500),x27=rnorm(500),x28=rnorm(500),x29=rnorm(500),x30=rnorm(500))
View(steps)
View(Dataset)
#save the dataset in a csv file
write.csv(Dataset,file="RW9.csv")
View(Dataset)

#2. work with an existing dataset
Dataset = read.csv('dataset.csv')
View(Dataset)
Dataset$Age = ifelse(is.na(Dataset$Age),
                     ave(Dataset$Age, FUN = function (x)mean(x, na.rm = TRUE)),
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
d
#split dataset to training and testing sets
#123 is set as the random number value
set.seed(123)
# This function returns true if observation goes to the Training set (80%) and false if observation goes to the test set (20%).
split = sample.split(Dataset$Purchased, SplitRatio = 0.8)
training_set = subset(Dataset, split == TRUE)
test_set = subset(Dataset, split == FALSE)
training_set
test_set
training_set[, 2:3] = scale(training_set[, 2:3])
View(training_set)
test_set[, 2:3] = scale(test_set[, 2:3])
View(test_set)