# ANN -----------------------------------------------------------------------------------------------------------------
rm(list = ls())
setwd("D:/Documents/sem 1/BIT 33603 data mining/work/assignment") # change directory, "/"
getwd() # Show current directory

library(caTools)
library(tidyverse) # will dw list all package
library(dplyr)
library(caret)
library(tidyverse)
library(neuralnet)

data <- read_csv('Diabetes.csv',show_col_types = FALSE)

which(is.na(data),arr.ind=TRUE) # check missing value by row colomn

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

split = sample.split(data$gender, SplitRatio = 0.5)
train_set = subset(data, split == FALSE) # 70%
test_set = subset(data, split == TRUE) # 30%

# machine learning
model = neuralnet(
  diabetes~gender+age+bmi+HbA1c_level+blood_glucose_level,
  data=train_set,
  hidden=c(5),
  linear.output = FALSE,
  stepmax=1000000
)

# Plot the ANN architecture
plot(model,rep = "best")

# Evaluation of the model
pred <- predict(model, test_set)

labels <- c("1", "0")  
prediction_label <- data.frame(max.col(pred)) %>%
  mutate(pred=labels[max.col.pred.]) %>%
  select(2) %>%
  unlist()

# Create a confusion matrix
conf_matrix <- table(test_set$diabetes, prediction_label)

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


# decision tree -----------------------------------------------------------------------------------------------------------------
rm(list = ls())
setwd("D:/Documents/sem 1/BIT 33603 data mining/work/assignment") # change directory, "/"
getwd() # Show current directory

library(caTools)
library(tidyverse) # will dw list all package
library(dplyr)
library(caret)
library(tidyverse)
library(rpart)
library(rpart.plot)

data <- read_csv('Diabetes.csv',show_col_types = FALSE)

which(is.na(data),arr.ind=TRUE) # check missing value by row colomn

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

split = sample.split(data$gender, SplitRatio = 0.3)
train_set = subset(data, split == FALSE) # 70%
test_set = subset(data, split == TRUE) # 30%

tree_model <- rpart(diabetes ~ gender+age+bmi+HbA1c_level+blood_glucose_level, data = train_set, method = "class")

# Make predictions on the testing data
predictions <- predict(tree_model, newdata = test_set, type = "class")

# Create a confusion matrix
conf_matrix <- table(test_set$diabetes, predictions)
#View(conf_matrix)

# Print the confusion matrix
print(conf_matrix)

# Calculate the error rate
error_rate <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Error Rate:", error_rate, "\n")

# Calculate evaluation metrics
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- diag(conf_matrix) / rowSums(conf_matrix)
recall <- diag(conf_matrix) / colSums(conf_matrix)
f_score <- 2 * (precision * recall) / (precision + recall)

# Print evaluation metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision (per class):", paste(precision, collapse = ", "), "\n")
cat("Recall (per class):", paste(recall, collapse = ", "), "\n")
cat("F-score (per class):", paste(f_score, collapse = ", "), "\n")

# Plot the decision tree
rpart.plot(tree_model, type = 2, extra = 101, fallen.leaves = TRUE)

# naive bayes -----------------------------------------------------------------------------------------------
rm(list = ls())
setwd("D:/Documents/sem 1/BIT 33603 data mining/work/assignment") # change directory, "/"
getwd() # Show current directory

library(caTools)
library(tidyverse) # will dw list all package
library(dplyr)
library(caret)
library(tidyverse)
library(e1071)

data <- read_csv('Diabetes.csv',show_col_types = FALSE)

which(is.na(data),arr.ind=TRUE) # check missing value by row colomn

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

split = sample.split(data$gender, SplitRatio = 0.3)
train_set = subset(data, split == FALSE) # 70%
test_set = subset(data, split == TRUE) # 30%

nb_model <- naiveBayes(diabetes ~ gender+age+bmi+HbA1c_level+blood_glucose_level, data = train_set)

# Make predictions on the test set
predictions <- predict(nb_model, newdata = test_set)

# Evaluate the model
conf_matrix <- table(predictions, test_set$diabetes)

# Print the confusion matrix
print(conf_matrix)

# Calculate evaluation metrics
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- diag(conf_matrix) / rowSums(conf_matrix)
recall <- diag(conf_matrix) / colSums(conf_matrix)
f_score <- 2 * (precision * recall) / (precision + recall)

# Print evaluation metrics
error_rate <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Error Rate:", error_rate, "\n")
cat("Accuracy:", accuracy, "\n")
cat("Precision (per class):", paste(precision, collapse = ", "), "\n")
cat("Recall (per class):", paste(recall, collapse = ", "), "\n")
cat("F-score (per class):", paste(f_score, collapse = ", "), "\n")
