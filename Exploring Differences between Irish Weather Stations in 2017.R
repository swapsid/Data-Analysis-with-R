#Loading the required libraries for the assignment

library(dplyr)
library(ggplot2)
library(aimsir17)

#Step 1: Creating tibble for storing annual summary of each weather station and printing the first 25 observations
annual <- observations %>% group_by(station) %>% summarise(TotalRain = sum(rain, na.rm = TRUE), AvrWind = mean(wdsp, na.rm=TRUE), AvrTemp = mean(temp, na.rm = TRUE))
print(annual, n=25)

#Step 2: Creating ranking for each observation in annual and printing the first 25 observations
annual <- annual %>% 
          mutate(Rank_Rain = rank(desc(TotalRain)), Rank_Wind = rank(desc(AvrWind)), Rank_Temp = rank(desc(AvrTemp)),Avr_Rank = (Rank_Rain+Rank_Wind+Rank_Temp)/3)
print(annual, n = 25)

#Step 3: Creating target variable with Maximum and Minimum Avr_Rank and printing the variable
target <- filter(annual, (Avr_Rank==max(Avr_Rank)) | (Avr_Rank==min(Avr_Rank)))
print(target)

#Step 4: Fetching the station names of target tibble and assigning to target
target <- pull(target, station)

#Step 5: Creating tibble from observations with target to get the  records for the highest and lowest ranked stations
my_obs <- observations %>% filter(station %in% target)
print(my_obs)

#Step 6: Plotting the graph showing the distribution of temperature 
ggplot(my_obs, aes(temp, color=station)) + geom_freqpoly() + ggtitle('Temperature Profile for Weather Stations')

#Step 7: Plotting the graph showing the distribution of windspeed
ggplot(my_obs, aes(station, wdsp, color=station),) + geom_boxplot() + ggtitle('Windspeed Profile for Weather Stations')

#Step 8: Plotting the graph showing the distribution of rainfall
ggplot(my_obs %>% group_by(station,day) %>% summarise(rain = sum(rain, na.rm = TRUE)) %>% filter(station %in% target), aes(station, rain, color=station)) + geom_boxplot() + ggtitle('Rainfall Profile for Weather Stations')

#Step 9: Creating the a summary tibble of monthly statistics for Mullingar and Valentia Observatory
monthly <- observations %>%  filter(station %in% target) %>% group_by(station,month) %>% summarise(Rain = sum(rain, na.rm=TRUE), AvrWind = mean(wdsp, na.rm = TRUE), AvrTemp = mean(temp, na.rm = TRUE))                        
print(monthly, n = 24)

#Step 10: Plotting the graph of rainfall for each month for both the target stations
ggplot(monthly, aes(x= month, y=Rain, color=station)) + geom_point() + geom_line() + ggtitle('Monthly Rainfall Profile for Weather Stations') + scale_x_continuous(n.breaks = 10)

#Step 11: Plotting the graph of temperature for each month for both the target stations
ggplot(monthly, aes(x= month, y=AvrTemp, color=station)) + geom_point() + geom_line() + ggtitle('Monthly Temperature Profile for Weather Stations') + scale_x_continuous(n.breaks = 10)

#Step 12: Plotting the graph of windspeed for each month for both the target stations
ggplot(monthly, aes(x= month, y=AvrWind, color=station)) + geom_point() + geom_line() + ggtitle('Montly Windspeed Profile for Weather stations') + scale_x_continuous(n.breaks = 10)
