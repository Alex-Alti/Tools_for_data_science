# Load packages
library(tidyverse)
library(magrittr)
library(dplyr)

# Load dataset
library(nycflights13)

# Assign dataset to int
flightdata <- nycflights13::flights
# Part 1

# Question 1
unique(flightdata$origin) #Find airports in New York
n_distinct(flightdata$origin) # Count number of New York airports

departure_count <- table(flightdata$origin) # Create table of departure airports
# Find busiest NY airport
busiest_airport <- names(departure_count[departure_count == max(departure_count)]) 


# Question 2
unique(flightdata$dest) # Find unique destinations
n_distinct(flightdata$dest) # Count number of destinations

destination_counts <- table(flightdata$dest) # Create table of dest
# Find most popular dest
popular_destination <- names(destination_counts[destination_counts == max(destination_counts)])

# Question 3
lga_july_4th_depart <- flightdata %>%
  filter(month == 7 & day == 4 & origin == "LGA" ) # Create table meeting criteria
num_lga_july_4_flight <- nrow(lga_july_4th_depart) # Find number of lga departures on july 4th

# Question 4
flightdata$date <- as.Date(paste(flightdata$year, flightdata$month, flightdata$day, sep = "-")) # Create table to combine dates

busiest_day <- as.Date(names(which.max(table(flightdata$date)))) # FInd busiest day

# Question 5
busiest_month <- names(which.max(table(flightdata$month))) # Find the busiest month of the year

# Question 6
longest_flight <- flightdata[which.max(flightdata$air_time),] # Find the longest flight

# Question 7
shortest_flight <- flightdata[which.min(flightdata$air_time),] # Find the shortest flight

# Question 8
carrier_count <- table(flightdata$carrier) # Count number of flights per carrier

busiest_carrier <- names(carrier_count[carrier_count == max(carrier_count)]) # Find the busiest carrier

# Question 9
average_arrival_delay <- tapply(flightdata$arr_delay[is.finite(flightdata$arr_delay)], flightdata$dest[is.finite(flightdata$arr_delay)], mean) # Find the average arrival delay for each destination

shortest_average_delay <- names(average_arrival_delay[average_arrival_delay == min(average_arrival_delay)]) # Find destination with shortest average delay

# Question 10
#Difference in air time time between longest and shortest flight
flight_difference <- longest_flight$air_time - shortest_flight$air_time
# Part 2

library(ggplot2)# Load ggplot

# Plot 1
ggplot(flightdata, aes(x = factor(month), fill = origin)) +
  geom_bar() + # Using bar as it looks more organized when compared to line
  labs(title = "total number of departures per month", x = "Month", y = "Number of Departures" ) # Create plot of total departures per month

# Plot 2
jfk_flights <- subset(flightdata, origin == "JFK" & is.finite(dep_delay)) # Filter to find flights from JFK

jfk_average_departure_delay <- jfk_flights %>%
  group_by(month) %>%
  summarize(mean_dep_delay = mean(dep_delay)) # Group data by month and find mean departure delay

ggplot(jfk_average_departure_delay, aes(x = factor(month), y = mean_dep_delay, group = 1)) +
  geom_line() +
  labs(title = "Average departure delay for flights departing JFK per moonth", x = "Month", y = "Average Departure Delay") # Create line plot of average departure delays for JFK by month

# Plot 3
ggplot(flightdata, aes(x = carrier)) +
  geom_bar() +
  labs(title = "Total Number of Flights per Carrier", x = "Carrier", y = "NUmber of Flights") # Create plot of total number of flights per carrier

# Plot 4
top_five_carriers <- head(carrier_count, 5) # Ident top five busiest carriers using value from question 8

top_five_carrier_names <- names(top_five_carriers) # Name top five busiest carriers

top_five_carrier_data <- subset(flightdata, carrier %in% top_five_carrier_names) # Create data containing top five data to make plot

ggplot(top_five_carrier_data, aes(x = carrier, y = dep_delay)) +
  geom_boxplot() +
  labs(title = "Statistical Distribution of Departure Delays for the 5 Busiest Carriers", x = "Carrier", y = "Departure Delay") # Create box plot of departure delays for top five busiest carriers

# Plot 5
Delay_filtered_flights <- subset(flightdata, dep_delay > 120) # Filter data to only include delays greater than two hours

ggplot(Delay_filtered_flights, aes(x = factor(month), fill = origin)) +
  geom_bar() +
  labs(title = "Total number of Flights with Departure Delay Greater than 2 hours per Month", x = "Month", y = "Number of Flights") # Create plot of total number of flights with departure delays greater than two hours per month

# Part 3

# Bonus Question 1
valid_flights <- subset(flightdata, is.finite(dep_delay) & is.finite(arr_delay)) # Filter out non-finite values

valid_flights$gain <- valid_flights$dep_delay - valid_flights$arr_delay # Create new column by calculating gain (dep delay - arr delay)

average_gain_by_carrier <- tapply(valid_flights$gain, valid_flights$carrier, mean) # calculate average gain by carrier


shortest_avg_gain_carrier <- names(average_gain_by_carrier[which.min(average_gain_by_carrier)]) # Find the carrier with the lowest average delay per flight

# Bonus Question 2
longest_avg_gain_carrier <- names(average_gain_by_carrier[which.max(average_gain_by_carrier)]) # Find the carrier with the highest average delay per flight

carrier_avg_gain_data <- data.frame(
  Carrier = names(average_gain_by_carrier),
  Average_Gain = average_gain_by_carrier
) # Validate gain values were saved properly

# Bonus Question 3
jfk_filter_gain <- valid_flights %>%
  filter(origin == "JFK") # Filter for only JFK flights 

average_dep_delay_per_day_jfk <- jfk_filter_gain %>%
  group_by(year, month, day) %>%
  summarize(mean_dep_delay = mean(dep_delay, na.rm = TRUE)) # Group and calculate average departure delay by day for JFK departures

worst_day_at_jfk <- average_dep_delay_per_day_jfk[which.max(average_dep_delay_per_day_jfk$mean_dep_delay), ] # Find worst day of the year to catch a flight from JFK

# Bonus Question 4
jfk_filter_gain$total_flight_time <- jfk_filter_gain$air_time +
  jfk_filter_gain$dep_delay + jfk_filter_gain$arr_delay # Calculate total flight time

threshold_10_percent <- 0.10 * jfk_filter_gain$total_flight_time # Calculate 10% threshold of total flight time

percentage_delay_lt_10_percent <- sum(jfk_filter_gain$dep_delay < threshold_10_percent) / nrow(jfk_filter_gain) * 100 # Calculate the percentage of flights with delay less than 10%

cat("Percentage of JFK flights with delay < 10% of total flight time:", percentage_delay_lt_10_percent, "%\n") # Print result of calculation

# Bonus Question 5
may_to_sept_flights <- subset(flightdata, month >= 5 & month <= 9) # Filter for flights between May and September

delayed_flights <- subset(may_to_sept_flights, dep_delay > 120) # Filter for fights delayed greater than 2 hours

delayed_flights_count_per_carrier <- table(delayed_flights$carrier) # Find number of flights delayed greater than 2 hours for each airline

shortest_delayed_flights_carrier <- names(delayed_flights_count_per_carrier[which.min(delayed_flights_count_per_carrier)]) # Find airline with shortest number of delayed flights
