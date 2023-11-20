library(tidyverse)
library(ggplot2) # Load packages

data(mpg) # Load dataset

# Create new column for average city/highway MPG
mpg$ave_mpg <- (mpg$cty + mpg$hwy) / 2 

# Create new table with 5 selected columns
new_mpg <- mpg %>%
  select(manufacturer, model, displ, year, ave_mpg,class) # Instructions do not call for class, but this is required in order to answer all following questions using this new table

# Question 1
best_SUV <- new_mpg %>%
  filter(class == "suv") %>%
  arrange(desc(ave_mpg)) %>%
  head(5) %>%
  select(manufacturer, model, ave_mpg) # Create table containing suv with higest MPG

# Print Question 1 answer
print(best_SUV) # Print list of top five suv MPG

# Question 2
best_Pickup <- new_mpg %>%
  filter(class == "pickup") %>%
  arrange(desc(ave_mpg)) %>%
  head(5) %>%
  select(manufacturer, model, ave_mpg) # Create table containing pickup with higest MPG

# Print question 2 answer
print(best_Pickup) # Print list of top five pickup MPG

# Question 3
ggplot(new_mpg, aes(x = displ, y = ave_mpg, label = class)) +
  geom_point() +
  geom_text(nudge_x = 0.2, nudge_y = 0.2) +
  labs(title = "Average Fuel Efficiency vs. Engine Size by Vehicle Class", x = "Engine Size (displ)", y = "Average MPG") # Create scatter plot comparing average MPG to engine displacement


# Part 2

flights_nyc_2013 <- nycflights13::flights # Load and assign NYC flights to specified char

flights_DC_holiday <- flights_nyc_2013 %>%
  filter(
    dest %in% c("DCA", "IAD", "BWI") & 
      month %in% c(2, 11) & 
      day %in% c(14, 28)
  ) # Create table containing only arrivals to washington and on two days

# Question 1
airport_rank <- flights_DC_holiday %>%
  group_by(dest) %>%
  summarize(arrival_count = n()) %>%
  arrange(desc(arrival_count)) # Group airports and count number of arrivals each

# Print answer to question 1
busiest_airport <- airport_rank[1, ]$dest # Assign busiest airport to value

print(busiest_airport) # Print busiest airport

# Question 2
carrier_rank <- flights_DC_holiday %>%
  group_by(carrier) %>%
  summarize(arrival_count = n(), avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(arrival_count)) %>%
  head(5) # find top five carrier and create table containing them as well as average delay

worst_delay <- carrier_rank %>%
  arrange(avg_delay) %>%
  tail(1) # Using table with top five, find the carrier with the highest average delay

print(worst_delay) # Print the worst carrier to answer question 2

# Question 3
ggplot(flights_DC_holiday, aes(x = dest)) +
  geom_bar(fill = "blue") +
  facet_wrap(~as.factor(month), ncol = 2) +
  labs(
    title = "Flight Arrivals at Washington DC Airports on Holidays",
    x = "Airport",
    y = "Number of Arrivals"
  ) # Create geom bar to compare number of arrivals by airline for the two holidays

