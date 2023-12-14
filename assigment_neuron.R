# Clear Environment
rm(list = ls()) # All 
rm(model) # Specific Variable

# Load necessary libraries and the .env file
#library(dotenv)
#dotenv::load_dot_env()
#fileLocate <- Sys.getenv("LocFileDatasets")

# Directory 
setwd("C:/Users/syahi/Documents/Degree UTHM/DOC/Data Mining/DataPulse-Collective/Datasets") # change directory, "/"
getwd() # Show current directory

#loadLibrary
install.packages('tidyverse')
install.packages('caTools')
install.packages('caret')
library(caTools)
library(tidyverse) # will dw list all package
library(dplyr) 
library(caret)
library(tidyverse)
library(neuralnet)



## Data Understanding ## 
# Data Collecting # 
data <- read_csv('Diabetes.csv',show_col_types = FALSE)
tail(data)
nrow(data)

# split reduce data
split_data = sample.split(data$gender, SplitRatio = 0.5)
data = subset(data, split == TRUE) # 50%


which(is.na(data),arr.ind=TRUE) # check missing value by row colomn

first_model <- model

# Explore Data
view(model)
nrow(data)
summarise(data)
glimpse(data)
summary(data)

## Data Preparing ## 

# Data Cleaning 
data$gender <- c(Female = 0, Male = 1, Other = 2)[data$gender] # change categorical to numerical 
data$age = ifelse(
  is.na(data$age),
  ave(data$age, 
      FUN = function (x)mean(x, na.rm = TRUE)), # change empty element to mean of age
  data$age
)
data <- data[!is.na(data$hypertension ), ]
data <- data[!is.na(data$heart_disease ), ]
data$smoking_history <- c(never=0,current=1,former=2,ever=3, `not current`=4,`No Info`=5)[data$smoking_history]
data$bmi = ifelse(
  is.na(data$bmi),
  ave(data$bmi, 
      FUN = function (x)mean(x, na.rm = TRUE)),
  data$bmi
)
data$HbA1c_level = ifelse(
  is.na(data$HbA1c_level),
  ave(data$HbA1c_level, 
      FUN = function (x)mean(x, na.rm = TRUE)), 
  data$HbA1c_level
)
data$blood_glucose_level = ifelse(
  is.na(data$blood_glucose_level),
  ave(data$blood_glucose_level, 
      FUN = function (x)mean(x, na.rm = TRUE)), 
  data$blood_glucose_level
)
data <- data[!is.na(data$diabetes ), ]



# Split Data

split = sample.split(data$gender, SplitRatio = 0.8)
train_set = subset(data, split == TRUE) # 80%
test_set = subset(data, split == FALSE) # 20%


nrow(train_set)
nrow(test_set)
summary(mixmax_Data)


# Data Normalization #
typeof(data)


mixmax <- function(x){
  (x - min(x)) / (max(x) - min(x)) * ((1.0-0)+0)
}

normalize <- c('mixmax', 'standard')

for (x in normalize){
  if (x == "mixmax"){
    train_set_mixmax <- lapply(train_set, mixmax)
    test_set_mixmax <- lapply(test_set, mixmax)
  } else {
    train_set_standard <- scale(train_set)
    test_set_standard <- scale(test_set)
  }
}


# Modeling

## Neuron Network 
view(train_set_standard)

set.seed(42)


model = neuralnet(
  diabetes~gender+age+heart_disease+smoking_history,
  data=test_set_mixmax,
  hidden=c(3,4),
  linear.output = FALSE,
  stepmax = 1000000
)

# Plot the ANN architecture
plot(model,rep = "best")

pred <- predict(model, test_data)



## K-Nearest Neighbors 

knn <- function(x){
  
  #Euclidean Method 
  euclidean_distance <- function(exist, new){
    sqrt((new[0] - exist[0])**2 + (new[1] - exist[1])**2)
  }
  
  
  
}



options(max.print=200000)
view(numerical_data)



new_data <- data.frame(
  gender = factor("Male"),
  age = 40,
  heart_disease = factor("Yes"),
  smoking_history = factor("Never")
)

# Predicting diabetes using the trained model
predictions <- compute(model, new_data)
