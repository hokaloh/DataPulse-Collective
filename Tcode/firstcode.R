# Directory 
getwd() # Show current directory
setwd("C:/Users/syahi/Documents/Degree UTHM/DOC/Data Mining/DataPulse-Collective") # change directory, "/"

# Clear Environment
rm(list = ls()) # All 
rm(VariableName) # Specific Variable

# Vector is called c()
c(10,20,30) # numerical
c(1.1, 2.2, 3.5) # numerical
c(FALSE, TRUE, FALSE) # Boolean
c("Darth Vader", "Luke Skywalker", "Han Solo") #character

# Assign variable with 3 method like "<-", "->", "=" and "assign()"
a = 2*3-6/2
"code4life" -> b
c <- c(1.1, 2.2, 3.5)
assign("d", c(10, 20, 30))
assign("e", c(10, 20, 30, 40, 50))
f = d+e # d will recycled vector add first e to same e vector  
print(f)
mean(d)

# max and min number 
min(d)
max(d)

# length is determine element like number and mode() is datatype
length(c)
mode(c)
vec_length <- length(c(1, 2, 3, 4, 5))
mean(vec_length)
print(vec_length)

data("swiss")
head(swiss,n=3)


# Loop and length character
x2 <- c("Mohammad", "Syahir", "Asri")
x3 <- numeric()
for(i in 1:10){
  #print(paste("the name", i, "consists of", nchar(i), "character"))
  x3 <- c(x3, i^2)
}

for (i in 1:5){
  for (j in 1:3){
    x4 <- paste(x4, LETTERS[i],"_", letters[j])
  }
}



# Create two vectors of different lengths.
vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)

# Take these vectors as input to the array.
arrayA <- array(c(vector1,vector2))
print(arrayA)
result <- array(c(vector1,vector2),dim = c(3,3,1))
print(result)


# Create two vectors of different lengths.
vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)
column.names <- c("COL1","COL2","COL3", "COL4")
row.names <- c("ROW1","ROW2","ROW3", "ROW4")
matrix.names <- c("Matrix1","Matrix2")

# Take these vectors as input to the array.
result <- array(c(vector1,vector2),dim = c(4,4,2),dimnames = list(row.names,column.names,                                                     matrix.names))
print(result)

# Specific element in array
# (length,width,MATRIX)
print(result[,,2])

view(airtravel)

#Set Directory 
setwd("C:/Users/syahi/Documents/Degree UTHM/Data Mining/Rcode")
getwd()

#library
install.packages('tidyverse')
library(tidyverse)

# Files
read_data <- read.csv("airtravel.csv")
View(read_data)

cat("coloumns: ", ncol(read_data))
min(read_data$X1960)

# 
column_summary <- read_data %>%
  summarise(across(starts_with("X"), list(min = min, max = max)))
glimpse(column_summary)


row_summary <- read_data %>%
  mutate(min = pmin(X1958, X1959, X1960),
         max = pmax(X1958, X1959, X1960)) %>%
  select(-X1958, -X1959,-X1960, -Month) %>%
  list(min= Media)

View(row_summary)

# 2/11/2023
view(mtcars)
?mtcars

Number <- table(mtcars$cyl,mtcars$gear)
view(Number)
glimpse(Number)
barplot(Number,main='Automobile cylinder number grouped by number of gears', 
        col=c('red','orange', 'steelblue'),legend=rownames(Number),xlab='Number of Gears',
        ylab='count')
hist(airquality$Solar.R,na.rm = TRUE,col='yellow',main='Maximum Daily Temperature',
     xlab='Temperature (degrees Fahrenheit)')
with(subset(airquality,Month==5),plot(Wind,Ozone,col='steelblue',pch=20,cex=1.5)) 
title('Wind and Temperature in NYC in September of 1973')

airquality %>% 
  group_by(Day) %>% 
  summarise(mean_wind = mean(Wind)) %>% 
  ggplot() + 
  geom_area(aes(x = Day, y = mean_wind)) + 
  labs(title = "Area Chart of Average Wind per Day",
       subtitle = "using airquality data",
       y = "Mean Wind")
