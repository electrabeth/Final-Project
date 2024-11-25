# Final-Project
All 110 episodes of Wizards of Waverly Place were examined using 28 variables. The variables examined in this code are “podcast,” “creatures,” “allusions,” “guest,” “rating,” and “seconds.” “Podcast” is a qualitative dummy variable which examines if the episode had a corresponding podcast episode, 1 = yes and 0=no. “Creatures” is a qualitative dummy variable which examines if there was any type of magical creature in the episode, excluding the main characters, and is measured as 1 = yes and 0 = no. “Allusions” is a discrete quantitative variable which measures if there is a widely known reference in the episode, measured as 1, 2, 3, etc…. “Guest” is a discrete quantitative variable that examines if there is a guest star, co-star, or recurring character, measured as 1, 2, 3…. “Rating” is a continuous quantitative variable which measures the rating in stars such as 4.9, 5.0, 5.1, etc…. Finally “seconds” is a continuous quantitative variable which examines how long google took to produce search results from the variable “search” and is measures as 0.1, 0.2, 0.3, etc….

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

# Scatter Plot
ggplot(data, aes(x = rating, y = seconds)) +
  geom_point(color = "blue", size = 3) +
  ggtitle("Scatter Plot for Ratings and Seconds for Search Results") +
  xlab("Ratings") +
  ylab("Seconds") +
  theme_minimal() 

# Create Residuals
linear_relationship <- lm(data$rating ~ seconds, data = data)
summary(linear_relationship)

plot(data$rating, residuals(linear_relationship))
abline(h = 0)
