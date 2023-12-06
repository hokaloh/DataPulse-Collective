# Clear Environment
rm(list = ls()) # All 
rm(VariableName) # Specific Variable

# Load necessary libraries and the .env file
#library(dotenv)
#dotenv::load_dot_env()
#fileLocate <- Sys.getenv("LocFileDatasets")

# Directory 
setwd("C:/Users/syahi/Documents/Degree UTHM/DOC/Data Mining/DataPulse-Collective/Datasets") # change directory, "/"
getwd() # Show current directory

#loadLibrary
install.packages('tidyverse')
library(tidyverse) # will dw list all package
library(dplyr) 

#LoadFile
data <- read_csv('Heart_Disease_Prediction.csv',show_col_types = FALSE)
view(data)

# Data Understanding 