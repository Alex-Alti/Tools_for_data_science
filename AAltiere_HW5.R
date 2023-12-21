# Alex Altiere
# Assignment 5

# Load packages
library(tidyverse)
library(magrittr)
library(dplyr)
library(readxl)

# Load dataset
library(nycflights13)


# Assign dataset to int
flightdata <- nycflights13::flights

# Part 1 - Create filtered table and export to excel
who1 <- who %>% 
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  )

who2 <- who1 %>% # Format
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))

who3 <- who2 %>% # First pass
  separate(key, c("new", "type", "sexage"), sep = "_")

who3 %>% 
  count(new)

who4 <- who3 %>% # Drop iso2 and 3 
  select(-new, -iso2, -iso3)

who5 <- who4 %>% # Split by sex and age
  separate(sexage, c("sex", "age"), sep = 1)

# Save to excel
write.csv(who5, "filteredtbdata.csv", row.names = FALSE) # Write final filtered table to CSV

filteredtbdata <- read.csv("filteredtbdata.csv") # Import this data

write_excel_csv(filteredtbdata, "tidy_tbdata.xlsx") # Write to excel CSV


# Part 2

# Question 1
departure_count <- table(flightdata$origin) # Create table of departure airports

# Find busiest NY airport
busiest_airport <- names(departure_count[departure_count == max(departure_count)])

# Print busiest airport in NY
print(busiest_airport)

# Question 2
unique(flightdata$dest) # Find # of unique destinations

Number_of_dest <- n_distinct(flightdata$dest) # Count number of destinations

print(Number_of_dest) # print number of dest

destination_counts <- table(flightdata$dest) # Create table of dest

# Find most popular dest
popular_destination <- names(destination_counts[destination_counts == max(destination_counts)])

print(popular_destination) # Print most popular dest

# Question 3
lga_thanksgiving_depart <- flightdata %>%
  filter(month == 11 & day == 28 & origin == "LGA" ) # Create table meeting criteria

lga_thanksgiving_depart <- nrow(lga_thanksgiving_depart) # Find number of lga departures on thanksgiving

print(lga_thanksgiving_depart) # Print number of LGA thanksgiving departure

# Question 4
flightdata$date <- as.Date(paste(flightdata$year, flightdata$month, flightdata$day, sep = "-")) # Create table to combine dates
jfkday <- flightdata %>%
  filter(origin == "JFK")

busiest_day <- as.Date(names(which.max(table(jfkday$date)))) # FInd busiest day

print(busiest_day) # Print busiest day at JFK

# Question 5
lgamonth <- flightdata %>%
  filter(origin == "LGA") # Filter for only LGA flights

busiest_month <- names(which.max(table(lgamonth$month))) # Find the busiest month of the year

print(busiest_month) # Print busiest month for LGA

# Question 6
longest_flight <- flightdata %>% # Find longest flight in June
  filter(month == 6) %>%
  slice(which.max(air_time))

print(longest_flight) # Print the longest flight in June

# Question 7
Shortest_flight <- flightdata %>% # Find the shortest flight during May
  filter(month == 5) %>%
  slice(which.min(air_time))

print(Shortest_flight) # Print the shortest flight during May

# Question 8
carrier_count <- table(flightdata$carrier) # Count number of flights per carrier

least_carrier <- names(carrier_count[carrier_count == min(carrier_count)]) # Find the slowest carrier

print(least_carrier) # Print the carrier with the smallest number of flights

# Question 9
valid_flights_jfk <- subset(jfkday, is.finite(dep_delay) & is.finite(arr_delay)) # Filter out non-finite values

valid_flights_jfk$gain <- valid_flights_jfk$dep_delay - valid_flights_jfk$arr_delay # Create new column by calculating gain (dep delay - arr delay)

average_gain_by_carrier_dep_jfk <- tapply(valid_flights_jfk$gain, valid_flights_jfk$carrier, mean) # calculate average gain by carrier dep jfk

shortest_avg_gain_carrier <- names(average_gain_by_carrier_dep_jfk[which.min(average_gain_by_carrier_dep_jfk)]) # Find the carrier with the lowest average delay per flight

print(shortest_avg_gain_carrier) # Print carrtier with shortest average delay dep jfk

# Question 10
valid_flights_lga <- subset(lgamonth, is.finite(dep_delay) & is.finite(arr_delay)) # Filter out non-finite values as above

valid_flights_lga$gain <- valid_flights_lga$dep_delay - valid_flights_lga$arr_delay # Create new column by calculating gain (dep delay - arr delay)

average_gain_by_carrier_dep_lga <- tapply(valid_flights_lga$gain, valid_flights_lga$carrier, mean) # calculate average gain by carrier dep lga

longest_avg_gain_carrier <- names(average_gain_by_carrier_dep_lga[which.max(average_gain_by_carrier_dep_lga)]) # Find the carrier with the longest average delay per flight

print(longest_avg_gain_carrier) # Print carrier with longest average delay departing lga

# Question 11
average_dep_delay_per_day_lga <- valid_flights_lga %>%
  group_by(year, month, day) %>%
  summarize(mean_dep_delay = mean(dep_delay, na.rm = TRUE)) # Group and calculate average departure delay by day for lga departures

worst_day_at_lga <- average_dep_delay_per_day_lga[which.max(average_dep_delay_per_day_lga$mean_dep_delay), ] # Find worst day of the year to catch a flight from lga

print(worst_day_at_lga) # Print worst day to catch flight from lga

# Question 12
valid_flights_jfk$total_flight_time <- valid_flights_jfk$air_time +
  valid_flights_jfk$dep_delay + valid_flights_jfk$arr_delay # Calculate total flight time

threshold_5_percent <- 0.05 * valid_flights_jfk$total_flight_time # Calculate 5% threshold of total flight time

percentage_delay_lt_5_percent <- sum(valid_flights_jfk$dep_delay < threshold_5_percent) / nrow(valid_flights_jfk) * 100 # Calculate the percentage of flights with delay less than 5%

cat("Percentage of JFK flights with delay < 5% of total flight time:", percentage_delay_lt_5_percent, "%\n") # Print result of calc

# Question 13
oct_to_dec_flights <- subset(flightdata, month >= 10 & month <= 12) # Filter for flights between October and December

delayed_flights <- subset(oct_to_dec_flights, dep_delay > 180) # Filter for fights delayed greater than 3 hours

delayed_flights_count_per_carrier <- table(delayed_flights$carrier) # Find number of flights delayed greater than 3 hours for each airline

shortest_delayed_flights_carrier <- names(delayed_flights_count_per_carrier[which.min(delayed_flights_count_per_carrier)]) # Find airline with shortest number of delayed flights

print(shortest_delayed_flights_carrier) # Print carrier with shortest number of flights delayed by more than 3 hours

# Part 3

# Plot 1
ggplot(flightdata, aes(x = factor(month), fill = origin)) +
  geom_bar() + # Using bar as it looks more organized when compared to line
  labs(title = "total number of departures per month", x = "Month", y = "Number of Departures" ) # Create plot of total departures per month

# Plot 2
jfk_flights <- subset(flightdata, origin == "JFK" & is.finite(dep_delay)) # Filter to find flights from JFK which contain finite int

jfk_average_departure_delay <- jfk_flights %>%
  group_by(month) %>%
  summarize(mean_dep_delay = mean(dep_delay)) # Group data by month and find mean departure delay

ggplot(jfk_average_departure_delay, aes(x = factor(month), y = mean_dep_delay, group = 1)) +
  geom_line() +
  labs(title = "Average departure delay for flights departing JFK per moonth", x = "Month", y = "Average Departure Delay") # Create line plot of average departure delays for JFK by month

# Plot 3
ggplot(jfk_flights, aes(x = carrier)) +
  geom_bar() +
  labs(title = "Total Number of Flights per Carrier from JFK", x = "Carrier", y = "NUmber of Flights") # Create plot of total number of flights per carrier departing jfk

# Plot 4
top_five_carriers <- head(carrier_count, 5) # Ident top five busiest carriers referencing value from question 8

top_five_carrier_names <- names(top_five_carriers) # Name top five busiest carriers

top_five_carrier_data <- subset(flightdata, carrier %in% top_five_carrier_names) # Create data containing top five data to make plot

ggplot(top_five_carrier_data, aes(x = carrier, y = dep_delay)) +
  geom_boxplot() +
  labs(title = "Statistical Distribution of Departure Delays for the 5 Busiest Carriers", x = "Carrier", y = "Departure Delay") # Create box plot of departure delays for top five busiest carriers

# Plot 5
ggplot(lgamonth, aes(x = factor(month), fill = origin)) +
  geom_bar() + # Using bar as it looks more organized when compared to line
  labs(title = "total departures per month from LGA", x = "Month", y = "Number of Departures" ) # Create plot of total departures per month from lga

# Part 4

# Question 1
largest_cases_2012 <- who5 %>% # Find country with largest number of cases in 2012
  filter(year == 2012) %>%
  group_by(country) %>%
  summarize(total_cases = sum(cases)) %>%
  arrange(desc(total_cases)) %>%
  slice(1)

print(largest_cases_2012) # Print country with largest number of cases in 2012

smallest_cases_2012 <- who5 %>% # Find country with smallest number of cases in 2012
  filter(year == 2012) %>%
  group_by(country) %>%
  summarize(total_cases = sum(cases)) %>%
  arrange(total_cases) %>%
  slice(1)

print(smallest_cases_2012) # Print country with smallest number of cases in 2012

# Question 2
cases_gender_australia <- who5 %>% # Create plot of number of cases per gender in Australia
  filter(country == "Australia") %>%
  ggplot(aes(x = as.factor(year), y = cases, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of TB Cases per Gender in Australia Over Time",
       x = "Year", y = "Number of Cases", fill = "Gender")

print(cases_gender_australia) # Print plot of cases in AUS per gender

# Question 3
cases_gender_afghanistan <- who5 %>% # Plot the total number of cases in Afghanistan per gender between 2000-2013
  filter(country == "Afghanistan", year >= 2000, year <= 2013) %>%
  group_by(year, sex) %>%
  summarize(total_cases = sum(cases)) %>%
  group_by(sex) %>%
  mutate(cumulative_cases = cumsum(total_cases)) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(year), y = cumulative_cases, color = sex, group = sex)) +
  geom_line() +
  labs(title = "Cumulative Total Number of TB Cases per Gender in Afghanistan (2000-2013)",
       x = "Year", y = "Cumulative Number of Cases", color = "Gender")


print(cases_gender_afghanistan) # Print plot with total cases per gender during period above

# Question 4 
north_american_countries <- c("United States of America", "Canada", "Mexico") # Define 3 largest countries in NA

cases_age_north_america <- who5 %>% # Create pie containing percent of cases per age group in NA
  filter(country %in% north_american_countries) %>%
  group_by(age) %>%
  summarize(total_cases = sum(cases)) %>%
  mutate(percentage = total_cases / sum(total_cases) * 100) %>%
  ggplot(aes(x = "", y = total_cases, fill = age)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  coord_polar("y") +
  labs(title = "Percentage of TB Cases per Age Group in North American Countries",
       fill = "Age Group")

print(cases_age_north_america) # Print chart of percent of cases per age group in NA

# Question 5
south_american_countries <- c("Brazil", "Colombia", "Argentina") # Define 3 largest countries in SA

cases_age_south_america <- who5 %>% # Create pie containing percent of cases per age group in SA
  filter(country %in% south_american_countries) %>%
  group_by(age) %>%
  summarize(total_cases = sum(cases)) %>%
  mutate(percentage = total_cases / sum(total_cases) * 100) %>%
  ggplot(aes(x = "", y = total_cases, fill = age)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  coord_polar("y") +
  labs(title = "Percentage of TB Cases per Age Group in South American Countries",
       fill = "Age Group")

print(cases_age_south_america) #  Print chart of percent of cases per age group in SA

# Question 6
european_countries <- c("Russian Federation", "Germany", "United Kingdom of Great Britain and Northern Ireland") # Define 3 largest countries in EU

cases_age_europe <- who5 %>% # Create pie containing percent of cases per age group in EU 
  filter(country %in% european_countries) %>%
  group_by(age) %>%
  summarize(total_cases = sum(cases)) %>%
  mutate(percentage = total_cases / sum(total_cases) * 100) %>%
  ggplot(aes(x = "", y = total_cases, fill = age)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  coord_polar("y") +
  labs(title = "Percentage of TB Cases per Age Group in European Countries",
       fill = "Age Group")

print(cases_age_europe) # Print chart of percent of cases per age group in EU

# Question 7
african_countries <- c("Nigeria", "Ethiopia", "Egypt") # Define 3 largest countries in Africa

cases_age_africa <- who5 %>% # Create pie containing percent of cases per age group in Africa
  filter(country %in% african_countries) %>%
  group_by(age) %>%
  summarize(total_cases = sum(cases)) %>%
  mutate(percentage = total_cases / sum(total_cases) * 100) %>%
  ggplot(aes(x = "", y = total_cases, fill = age)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  coord_polar("y") +
  labs(title = "Percentage of TB Cases per Age Group in African Countries",
       fill = "Age Group")

print(cases_age_africa) # Print chart of percent of cases per age group in Africa

# Question 8
asian_countries <- c("China", "India", "Indonesia") # Define 3 largest countries in Asia

cases_age_asia <- who5 %>% # Create pie containing percent of cases per age group in Asia
  filter(country %in% asian_countries) %>%
  group_by(age) %>%
  summarize(total_cases = sum(cases)) %>%
  mutate(percentage = total_cases / sum(total_cases) * 100) %>%
  ggplot(aes(x = "", y = total_cases, fill = age)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  coord_polar("y") +
  labs(title = "Percentage of TB Cases per Age Group in Asian Countries",
       fill = "Age Group")

print(cases_age_asia) # Print chart of percent of cases per age group in Asia

# Question 9
cases_age_worldwide <- who5 %>% # Create plot of total number of cases worldwide per age group
  group_by(year, age) %>%
  summarize(total_cases = sum(cases)) %>%
  ggplot(aes(x = year, y = total_cases, color = age, group = age)) +
  geom_line() +
  labs(title = "Total Number of TB Cases Worldwide Over Time, per Age Group",
       x = "Year", y = "Number of Cases", color = "Age Group")

print(cases_age_worldwide) # Print plot of total cases worldwide per age

# Question 10
reduction_by_country <- who5 %>% # Find country with the sharpest reduction in cases
  filter(year %in% c(1997, 2010)) %>%
  group_by(country) %>%
  summarize(total_cases_1997 = sum(cases[year == 1997]),
            total_cases_2010 = sum(cases[year == 2010])) %>%
  mutate(reduction_percentage = ((total_cases_2010 - total_cases_1997) / total_cases_1997) * 100) %>%
  arrange(desc(reduction_percentage)) %>%
  slice(1)

print(reduction_by_country) # Print country with the sharpest reduction in cases

