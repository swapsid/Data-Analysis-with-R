#Importing the required libraries
library(dplyr)
library(ggplot2)
library(nycflights13)
library(lubridate)

#Step 1: Creating required tibble from flights and printing the tibble
my_flights <- flights %>%  select(time_hour, origin, carrier, dep_delay)
print(my_flights)

#Step 2: Creating additional columns from time_hour column and printing the tibble
my_flights <- my_flights %>% mutate(month = month(time_hour), month_name = month(time_hour, label = TRUE), 
                   day = mday(time_hour), hour = hour(time_hour))
print(my_flights)

#Step 3: Performing join operation on my_flights and airlines to create new tibble
my_data <- my_flights %>% left_join(airlines)
print(my_data)

#Step 4: Removing any incomplete records using complete_cases
my_data <- my_data[complete.cases(my_data),]
print(my_data)

#Step 5: Finding the top six airlines in terms of flight frequency from the cleaned dataset
top_6 <- my_data %>% group_by(name) %>% summarise(n=n())  %>% arrange(desc(n)) %>% select(name, TotalFights = n) %>% head()
print(top_6)

#Step 6: Using filtering join to filter top 6 airlines from the cleaned data set.
final_data <- semi_join(my_data, top_6, by = "name")
print(final_data)

#Step 7: Calculating the 95% intervals for departure times for each month of the year using quantile function
quant_month_95 <- final_data %>% group_by(month_name) %>% summarise(Q2.5 = quantile(dep_delay, probs = 0.025, na.rm = T), Q97.5 = quantile(dep_delay, probs = 0.975, na.rm = T))
print(quant_month_95)

#Step 8: Calculating the 95% intervals for departure times for each carrier using quantile function
quant_carrier_95 <- final_data %>% group_by(name) %>% summarise(Q2.5 = quantile(dep_delay, probs = 0.025, na.rm = T), Q97.5 = quantile(dep_delay, probs = 0.975, na.rm = T))
print(quant_carrier_95)

#Step 9: Calculating the 95% intervals for departure times for each airport using quantile function
quant_airport_95 <- final_data %>% group_by(origin) %>% summarise(Q2.5 = quantile(dep_delay, probs = 0.025, na.rm = T), Q97.5 = quantile(dep_delay, probs = 0.975, na.rm = T))
print(quant_airport_95)

#Step 10: Plotting count of flights for each month of top 6 airline carriers
final_data %>% group_by(name,month_name) %>% summarise(n = n()) %>% ggplot(aes(x=month_name, y = n,colour=name,group=name)) +geom_line()
