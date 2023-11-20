library(magrittr)
library(tidyverse)
library(dplyr)
# Assignment 2
# Alex Altiere

# Import dataset 1 using readr
library(readr)
salaries <- read_csv("2017_NBA.csv")

# Problem 1
 #  Question 1
highest_paid_player <- max(salaries$Salary)# Find highest paid player

highest_paid_player_info <- which.max(salaries$Salary) #Find the row of the highest paid player

salaries [highest_paid_player_info, ] # Confirm row is correct

Filtered_table <- salaries %>% 
  select(Player, Salary, NBA_DraftNumber, Tm, MP, 'TS%') # Create filtered table to display stats as prompted

print(Filtered_table[70,]) #Highest paid player/ stats

 # Question 2 - Min, max, and mean of all players
print(min(salaries$Salary)) # Minimum salary

print(max(salaries$Salary)) # Max Salary

print(mean(salaries$Salary)) #Average salary

 # Question 3
USA_players <- salaries %>%
  filter(NBA_Country == "USA") # Table of only USA players

print(min(USA_players$Age)) # Minimum age

print(max(USA_players$Age)) # Max age

print(mean(USA_players$Age)) #Average age

 # Question 4
Non_USA_Players <- salaries %>%
  filter(NBA_Country != "USA") # Table of non-USA players

print(min(Non_USA_Players$Age)) # Minimum age

print(max(Non_USA_Players$Age)) # Max age

print(mean(Non_USA_Players$Age)) # Average age


# Problem 2
Team1 <- USA_players %>%
  filter(Tm == "HOU") # Filter for team1

Team2 <- USA_players %>%
  filter(Tm == "DAL") # Filter for team2

 # Question 1
print(mean(Team1$Salary)) # Team1 average salary

print(mean(Team1$`TS%`)) # Team1 average ts%

 # Question 2
print(mean(Team2$Salary)) # Team2 average salary

print(mean(Team2$`TS%`)) # Team 2 average ts%

 # Question 3
Houston <- "Team 1" # Assign int to calcs for table for team 1

team1_avg_age <- mean(Team1$Age) # Average age of team 1

team1_avg_salary <- mean(Team1$Salary) # Average salary of team 1

team1_avg_ts <- mean(Team1$`TS%`) # Average TS% of team 1

Dallas <- "Team 2" # Assign int to calcs for table for team 2

team2_avg_age <- mean(Team2$Age) # Average age of team 2

team2_avg_salary <- mean(Team2$Salary) # Average salary of team 2

team2_avg_ts <- mean(Team2$`TS%`) # Average TS% of team 2

# Create a data frame to display the comparison table of teams
comparison_table <- data.frame(
  Teams = c(Houston, Dallas),
  `Average Age` = c(team1_avg_age, team2_avg_age),
  `Average Salary` = c(team1_avg_salary, team2_avg_salary),
  `Average TS%` = c(team1_avg_ts, team2_avg_ts)
)

# Print table comparing teams
print(comparison_table)


# Problem 3
Flights13 <- nycflights13::flights # Assign nycflights to flights
 # Question 1
Flights_to_Florida <- Flights13 %>%
  filter(dest == "FLL") # Find FLL arrivals

print(count(Flights_to_Florida)) # Number of arrivals into FLL

 # Question 2
JFK_to_FLL <- Flights13 %>%
  filter(Flights13$origin == "JFK" & Flights13$dest == "FLL") # Find JFK to FLL flights

print(count(JFK_to_FLL)) # Number of flights from JFK to FLL

 # Question 3
LGA_to_FLL <- Flights13 %>%
  filter(Flights13$origin == "LGA" & Flights13$dest == "FLL") #  Find LGA to FLL flights

print(count(LGA_to_FLL)) # Number of flights from LGA to FLL

 # Question 4
EWR_to_FLL <- Flights13 %>%
  filter(Flights13$origin == "EWR" & Flights13$dest == "FLL") # Find EWR to FLL flights

print(count(EWR_to_FLL)) # Number of flights from EWR to FLL







