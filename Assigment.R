# Clear Environment
rm(list = ls()) # All 
rm(test_set_x) # Specific Variable

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



## Data Understanding ## 
# Data Collecting # 
data <- read_csv('Diabetes.csv',show_col_types = FALSE)

which(is.na(data),arr.ind=TRUE) # check missing value by row colomn

# Explore Data
view(data)
nrow(data)
summarise(data)
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


nrow(training_set)
nrow(test_set)
summary(mixmax_Data)


# Data Normalization #
typeof(data)


mixmax <- function(x){
  (x - min(x)) / (max(x) - min(x)) * ((1.0-0)+0)
}

#mixmax_Data <- lapply(data, mixmax)
#standardization_Data <- scale(data)


normalize <- c('mixmax', 'standard')

for (x in normalize){
  paste0("train_set_",x) <- ifelse(x=='mixmax',
                        lapply(train_set, mixmax),
                        scale(train_set))
  paste0("test_set_",x) <- ifelse(x=='mixmax',
                       lapply(train_set, mixmax),
                       scale(train_set))
}

# for (x in normalize){
#   train_set_x[[x]] <- if (x == 'mixmax') {
#     lapply(train_set, mixmax)
#   } else {
#     scale(test_set)
#   }
#   
#   test_set_x[[x]] <- if (x == 'mixmax') {
#     lapply(test_set, mixmax)
#   } else {
#     scale(test_set)
#   }
# }


# Modeling

## K-Nearest Neighbors 

knn <- function(x){
  
  #Euclidean Method 
  euclidean_distance <- function(exist, new){
    sqrt((new[0] - exist[0])**2 + (new[1] - exist[1])**2)
  }
  
  
  
}



options(max.print=200000)
view(numerical_data)
