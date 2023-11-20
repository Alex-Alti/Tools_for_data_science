# Alex Altiere
# Assignment 4


# Load packages 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stats)

## 1
 
# Import dataset and assign to caffeine
caffeine <- read.delim("https://raw.githubusercontent.com/ndphillips/ThePiratesGuideToR/master/data/caffeinestudy.txt")

## 2
# Find mean age for each gender and print
mean_age_gender <- tapply(caffeine$age, caffeine$gender, function(x) c(Mean = mean(x)))
print(mean_age_gender)

# Find mean age for each drink and print
mean_age_drink <- tapply(caffeine$age, caffeine$drink, function(x) c(Mean = mean(x)))
print(mean_age_drink)


# Find mean age for combined level and print
mean_age_combined <- tapply(caffeine$age, list(caffeine$gender, caffeine$drink), mean)
print(mean_age_combined)

# Find the median score by age and print
median_score_age <- tapply(caffeine$score, caffeine$age, median)
print(median_score_age)

## 3
# Create a filter for mens data
men_data <- caffeine %>%
  filter(gender == "male")
# Find max score for each age in men and print
max_score_age_men <- tapply(men_data$score, men_data$age, max)
print(max_score_age_men)

## 4
# Create dataframe for each level of drint and print
drink_stats <- aggregate(score ~ drink + cups, data = caffeine, 
                              FUN = function(x) c(Mean = mean(x), Median = median(x), 
                                                  Max = max(x), StdDev = sd(x)))
print(drink_stats)

## 5
# Create plot of contents in previous question to compare cups consumed and print

drink_stats_plot <- ggplot(drink_stats, aes(x = drink, y = score[, "Mean"], fill = factor(cups))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Drink Effect on Score",
       x = "Drink",
       y = "Mean Score") +
  theme_minimal() +
  scale_fill_discrete(name = "Cups")

print(drink_stats_plot)

## 6
# Create filtered table with females over 20
females_above_20 <- caffeine %>%
  filter(gender == "female", age > 20) 
# Create table of women over 20 stats with extra col and print result
table_females_above_20 <- aggregate(score ~ drink + cups, data = females_above_20, 
                                    FUN = function(x) c(Mean = mean(x), Median = median(x), 
                                                        Max = max(x), StdDev = sd(x), Count = length(x)))
print(table_females_above_20)

## 7
# Plot results to previous question
table_females_above_20_plot <- ggplot(table_females_above_20, aes(x = factor(cups), y = score[, "Mean"], fill = drink)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Statistics for Females Above Age 20",
       x = "Cups",
       y = "Mean Score") +
  scale_fill_brewer(palette = "Set1") +  # Use different colors for each "drink"
  theme_minimal()

print(table_females_above_20_plot)

##8
 # Question 1 - Find cor between test scores and type of drink
caffeine$drink_numeric <- as.integer(factor(caffeine$drink, levels = c("coffee", "greentea")))

cor_type_drink_numeric <- cor(caffeine$score, caffeine$drink_numeric, use = "complete.obs")

print(cor_type_drink_numeric)

# Question 2: Find cor between test scores and amount of drink
cor_amount_drink <- cor(caffeine$score, as.numeric(caffeine$cups))

print(cor_amount_drink)

# Question 3: Find cor between test scores and age
cor_score_age <- cor(caffeine$score, caffeine$age)

print(cor_score_age)

# Question 4: Find cor between test score and gender
caffeine$gender_numeric <- ifelse(caffeine$gender == "female", 0, 1)

cor_gender_numeric <- cor(caffeine$score, caffeine$gender_numeric, use = "complete.obs")

print(cor_gender_numeric)

# Question 5: Conclusions from study
# From this study, we can conclude the consumption of caffeine has an effect on cognition
# There is a strong positive correlation between higher test scores and elevated caffiene consumption
# There is a weak negative correlation between age and test scores
# Additionally, there is a weak postive correlation between test score and gender
# Fially, there is a weak to moderate negative correlation between test scores and type of caffeine

## Bonus question 9
# 1: Create scatterplot of age compared to different types of drink
scatterplot_drink <- ggplot(caffeine, aes(x = age, y = score, color = drink)) +
  geom_point() +
  labs(title = "Performance vs. Age by Drink Type",
       x = "Age",
       y = "Test Score") +
  theme_minimal()

print(scatterplot_drink)

# This scatterplot helps visualize the relationship between age, test scores, and the type of drink (Coffee or Green Tea).
# Further, it can be used to see if there are any noticeable patterns or differences in test scores based on the type of drink and the age of participants.

# 2: Create a scatterplot comparing test scores to age for different amounts of drink
scatterplot_cups <- ggplot(caffeine, aes(x = age, y = score, color = cups)) +
  geom_point() +
  labs(title = "Performance vs. Age by Amount of Drink",
       x = "Age",
       y = "Test Score") +
  theme_minimal()

print(scatterplot_cups)

# This scatterplot helps visialize the relationship between age, test scores, and the amount of drink consumed (1 cup or 5 cups).
# Further, this scatterplot also allows us to see if there are any patterns or differences in test scores based on the amount of drink and the age of participant.

# 3: Create scatterplot comparing test scores to age for different genders
scatterplot_gender <- ggplot(caffeine, aes(x = age, y = score, color = gender)) +
  geom_point() +
  labs(title = "Performance vs. Age by Gender",
       x = "Age",
       y = "Performance Score") +
  theme_minimal()

print(scatterplot_gender)

# This scatterplot is used to explore the relationship between age, test scores, and gender (Male or Female).
# Additionally, this allows us to examine whether there are any differences in test scores based on gender and the age of participants.

# 4: Conclusions based on the above scatterplots
# See attached "lab notebook"
