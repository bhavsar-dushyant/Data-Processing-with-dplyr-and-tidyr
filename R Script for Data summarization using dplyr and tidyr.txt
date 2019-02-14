#########################################################################################################
## Author: Dushyant Bhavsar
##Script for: Data Process, Filter and agreegate to find insights using package - dplyr and tidyr
#########################################################################################################
# 1. Package Import
#########################################################################################################

library(data.table)
library(readr)
library(xlsx)
library(dplyr)
library(tidyr)


data <- read.csv("Testfile.csv")

View(data)
data$header

# Code to separate selected column (header in below) into 2 or more columns
data2 <- data %>% separate(header,c("Year","Make"),extra='drop')

View(data2)

# change text case of given column
data2$Make <- toupper(data2$Make)

#Group by on particular categorical column and summarise - mean,count, etc
make_data <- data2 %>% group_by(Make) %>% summarise(count=n())
View(make_data[order(make_data$count,decreasing = TRUE),]) ######--------Answer to Question 1-------######

# Filter table based on value and then group by further and summarise 
owner_count <- data2 %>%  filter(Make == "FORD") %>% group_by(byOwner) %>% summarise(count=n())
View (owner_count)  ######--------Answer to Question 2-------######

#Check for NA's in given column
nrow(data2)
sum(is.na(data2$condition))

#Replace value in given column based using ifelse
data2$condition = ifelse(data2$odometer < 100000 & data2$year > 2010,"new","old")
View(data2)

#aggregate
condition_count <- data2 %>%  group_by(condition) %>% summarise(count=n()) 
View (condition_count)  ######--------Answer to Question 3-------######

#text Case change
data2$type <- toupper(data2$type)

# filtering and aggregate
type_mean_price <- data2 %>% 
  group_by(type) %>%
  summarise(MeanPrice = mean(price, na.rm=TRUE))

View(type_mean_price[order(type_mean_price$MeanPrice,decreasing = TRUE),])  ######--------Answer to Question 4-------######
