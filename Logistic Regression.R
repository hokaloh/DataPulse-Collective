rm(list = ls())
setwd("C:/Users/rivie/OneDrive/Universiti/UTHM/Semester 1/BIT33603 Perlombongan Data")
getwd() # Show current directory

library(caTools)
library(tidyverse)
library(dplyr)
library(caret)
library(tidyverse)

data <- read_csv('diabetes_prediction_dataset.csv', show_col_types = FALSE)

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
data$bmi <- ifelse(data$bmi < 25, 0, 
                   ifelse(data$bmi >= 25 & data$bmi <= 30, 1, 2))


data$HbA1c_level = ifelse(
  is.na(data$HbA1c_level),
  ave(data$HbA1c_level, 
      FUN = function (x)mean(x, na.rm = TRUE)), 
  data$HbA1c_level
)
data$HbA1c_level <- ifelse(data$HbA1c_level < 5.7, 0, 
                           ifelse(data$HbA1c_level >= 5.7 & data$HbA1c_level <= 6.4, 1, 2))

data$blood_glucose_level = ifelse(
  is.na(data$blood_glucose_level),
  ave(data$blood_glucose_level, 
      FUN = function (x)mean(x, na.rm = TRUE)), 
  data$blood_glucose_level
)
data$blood_glucose_level <- ifelse(data$blood_glucose_level < 180, 0, 1)

data <- data[!is.na(data$diabetes ), ]

# Split Data
split = sample.split(data$gender, SplitRatio = 0.30) #train
train_set = subset(data, split == FALSE) # 30%
test_set = subset(data, split == TRUE) # 70%

# Machine learning with progress tracking
cat("Training Logistic Regression Model...\n")
start_time <- Sys.time()

# machine learning - Logistic Regression
model <- glm(diabetes ~ gender + age + bmi + HbA1c_level + blood_glucose_level, 
             data = train_set, 
             family = binomial)

end_time <- Sys.time()
cat("Training completed in ", round(difftime(end_time, start_time, units = "mins"), 2), " minutes.\n")

# Prediction on the Test Set
cat("Predicting on Test Set...\n")
start_time <- Sys.time()

pred_prob <- predict(model, test_set, type = "response")
pred <- ifelse(pred_prob > 0.5, 1, 0)

end_time <- Sys.time()
cat("Prediction completed in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.\n")

# Create a confusion matrix
conf_matrix <- table(test_set$diabetes, pred)

# Print the confusion matrix
print(conf_matrix)

# Calculate evaluation metrics
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- diag(conf_matrix) / rowSums(conf_matrix)
recall <- diag(conf_matrix) / colSums(conf_matrix)
f_score <- 2 * (precision * recall) / (precision + recall)
error_rate <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)

# Print evaluation metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision (per class):", paste(precision, collapse = ", "), "\n")
cat("Recall (per class):", paste(recall, collapse = ", "), "\n")
cat("F-score (per class):", paste(f_score, collapse = ", "), "\n")
cat("Error Rate:", error_rate, "\n")