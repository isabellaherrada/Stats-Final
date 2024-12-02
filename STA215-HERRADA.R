# Set working directory
setwd("H:/sta215")

#Load Packages
library(readr)
library(haven)2
library(dplyr)
library(psych)
library(ggplot2)
library(tidyverse)

#Load population data 
data <- read.csv("raw_data.csv")

#table 1
table(data$tone)
table(data$funny)
describe(data)
sd(data$writers)

#Table 2
table(data$tone, data$funny)

#Chi squared test table(data$tone, data$funny)
chisq.test(data$funny, data$tone)

#Box Plot
ggplot(data, aes(x = as.factor(tone), y = writers)) +
  geom_boxplot() + 

  labs(
    title = "Distribution of Number of Writers by Tone of Episode", 
    x = "Tone of Episode", 
    y = "Number of Writers"
  ) +
  theme_minimal()

#Scatter Plot 
 lm_model <- lm(cast ~ writers, data = data)
  ggplot(data, aes(x = writers, y = cast)) +
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Linear regression line
    labs(
      title = "Distribution of Number of Writers by Number of People Casted in the Episode",
      x = "Number of Writers", 
      y = "Number of People Casted"
    ) +
    theme_minimal()
  summary(lm_model)
  

  lm_model <- lm(cast ~ writers, data = data)
  mean_writers <- mean(data$writers, na.rm = TRUE)
  mean_cast <- mean(data$cast, na.rm = TRUE)
  ggplot(data, aes(x = writers, y = cast)) +
    geom_point() +  # Scatter plot
    geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Linear regression line
    geom_vline(xintercept = mean_writers, linetype = "dashed", color = "red") +  # Mean line for X (writers)
    geom_hline(yintercept = mean_cast, linetype = "dashed", color = "red") +  # Mean line for Y (cast)
    labs(
      title = "Distribution of Number of Writers by Number of People Casted in the Episode",
      x = "Number of Writers", 
      y = "Number of People Casted"
    ) +
    theme_minimal()

# Figure 3 Residual Plot
  ggplot(data, aes(x = writers, y = residuals(lm_model))) +
    geom_point() +  # Add scatter points for residuals
    geom_hline(yintercept = 0, color = "red") +  # Add horizontal line at y = 0
    labs(
      title = "Distribution of Number of Writers by Number of People Casted in the Episode", 
      x = "Number of Writers",
      y = "Residuals"
    ) +
    theme_minimal()

