library(dplyr)
library(ggplot2)
library(lubridate)
library(maps)
library(nycflights13)


nycflights13 <- nycflights13::flights
planes <- nycflights13::planes
# Load dataset



# Step 1
NYC_flights_2013 <- nycflights13 %>%
  filter(dest %in% c("DAB", "FLL", "RSW", "JAX", "EYW", "MIA", "MCO", "PNS", "PIE", "SRQ", "TPA", "PBI", "PFN"),
         (month == 12 & day >= 25) | (month == 1 & day <= 5))

# Find age and add column

# Step 2
avg_delay <- NYC_flights_2013 %>%
  group_by(carrier) %>%
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(desc(avg_delay))

# Step 3
airport_ranking <- NYC_flights_2013 %>%
  group_by(dest) %>%
  summarise(num_flights = n()) %>%
  arrange(desc(num_flights))

# Step 4
ggplot(head(airport_ranking, 5), aes(x = dest, y = num_flights, fill = dest)) +
  geom_bar(stat = "identity") +
  labs(title = "Five Busiest Florida Airports During Christmas",
       x = "Airport",
       y = "Number of Flights") +
  theme_minimal()

# Step 5
top_carriers <- head(avg_delay, 5)

# Step 6: Create a scatter plot to visualize the relationship between delays and carriers
ggplot(NYC_flights_2013, aes(x = carrier, y = dep_delay)) +
  geom_point(aes(color = carrier)) +
  geom_hline(yintercept = mean(NYC_flights_2013$dep_delay, na.rm = TRUE), linetype = "dashed") +
  labs(title = "Relationship Between Flight Delays and Carriers",
       x = "Carrier",
       y = "Departure Delay") +
  theme_minimal()

# Step 7
airport_arrivals <- NYC_flights_2013 %>%
  group_by(origin) %>%
  summarise(num_flights = n())

# Step 8
ggplot(airport_arrivals, aes(x = origin, y = num_flights, fill = origin)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Flight Arrivals from Florida to NYC Airports",
       x = "NYC Airport",
       y = "Number of Flights") +
  theme_minimal()

# Step 9
airport_comparison <- planes %>%
  group_by(tailnum) %>%
  summarise(median_year = median(year, na.rm = TRUE))

NYC_flights_2013 <- flights %>% 
  left_join(airport_comparison, by = "tailnum") %>%
  mutate(age = 2013 - median_year)


# Step 10
ggplot(NYC_flights_2013, aes(x = origin, fill = origin)) +
  geom_bar(stat = "count") +
  labs(title = "Comparison of NYC Airports for Florida Flights and Plane Age",
       x = "NYC Airport",
       y = "Number of Flights",
       fill = "Origin") +
  theme_minimal() +
  geom_line(aes(x = origin, y = age * 10), color = "black", size = 1, 
            position = position_nudge(x = -0.2)) +  # Adjust the position
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "Average Plane Age (in years)"))
