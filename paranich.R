# Set working directory
setwd("H:/sta215")

data <- read.csv("raw_data.csv")

# Load packages
library(readr)
library(dplyr)
library(haven)
library(psych)
library(ggplot2)

# Descriptive Statistics
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

table(data$podcast) 
table(data$creatures)
table(data$allusions)
table(data$guest)
table(data$rating)
table(data$seconds)

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

#Chi-Squared Test (Podcast & Creatures)
chisq.test(data$podcast , data$creatures)
result <- chisq.test(data$podcast , data$creatures)
print(result)

# BOX PLOT
ggplot(data, aes(x = guest, y = allusions)) +
  geom_boxplot() +
  labs(title = "Box Plot of Allusions and Guests",
       x = "Guest",
       y = "Allusions") +
  theme_minimal()

#ANOVA test
aov(seconds ~ rating, data = data)
anova_results <- aov(seconds ~ rating, data = data)
summary(anova_results)

#Calculate Mean lines
mean_x <- mean(data$rating) 
mean_y <- mean(data$seconds) 

#Scatter Plot
ggplot(data, aes(x = rating, y = seconds)) +
  geom_point(color = "blue", size = 3) +
  ggtitle("Scatter Plot for Ratings and Seconds for Search Results") +
  xlab("Ratings") +
  ylab("Seconds") +
  theme_minimal() +
  geom_vline(xintercept = mean_x, color = "red", lwd = 2, lty = 2) +
  geom_hline(yintercept = mean_y, color = "purple", lwd = 2, lty = 2)
  geom_smooth(method = "lm", color = "green", lwd =2)

#Calculate the Correlation Coefficient
cor(data$rating, data$seconds, method = "pearson")
cor.test(data$rating, data$seconds)

#Linear Regression
model <- lm(seconds ~ rating, data = data)
summary(model)

# Create Residuals
linear_relationship <- lm(data$rating ~ seconds, data = data)
summary(linear_relationship)

plot(data$rating, residuals(linear_relationship))
abline(h = 0)
