# Final-Project
The final project in STA215 is about data collected from Wizards of Waverly Place and analysis of data collected. 

#Set working directory
setwd("H:/sta215")

data <- read.csv("raw_data.csv")

#Load packages
library(readr)
library(dplyr)
library(haven)
library(psych)
library(ggplot2)

#Descriptive Statistics
mean(data$podcast)
mean(data$creatures)
mean(data$allusions)
mean(data$guest)
mean(data$rating)
mean(data$seconds)

sd(data$podcast)
sd(data$creatures)
sd(data$allusions)
sd(data$guest)
sd(data$rating)
sd(data$seconds)

table(data$podcast, data$creatures)
table(data$allusions, data$guest)
table(data$rating, data$seconds)

describe(data$podcast)
describe(data$creatures)
describe(data$allusions)
describe(data$guest)
describe(data$rating)
describe(data$seconds)

summary(data$podcast)
summary(data$creatures)
summary(data$allusions)
summary(data$guest)
summary(data$rating)
summary(data$seconds)

# Contigency Table
table(data$podcast , data$creatures)

# BOX PLOT
ggplot(data, aes(x = guest, y = allusions)) +
  geom_boxplot() +
  labs(title = "Box Plot of Allusions and Guests",
       x = "Guest",
       y = "Allusions") +
  theme_minimal()

#Scatter Plot
ggplot(data, aes(x = rating, y = seconds)) +
  geom_point(color = "blue", size = 3) +
  ggtitle("Scatter Plot for Ratings and Seconds for Search Results") +
  xlab("Ratings") +
  ylab("Seconds") +
  theme_minimal() 

#Create Residuals
linear_relationship <- lm(data$rating ~ seconds, data = data)
summary(linear_relationship)

plot(data$rating, residuals(linear_relationship))
abline(h = 0)
