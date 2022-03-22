#Importing the required libraries 
library(aimsir17)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)

#Calculating the average daily windspeed for all stations
daily_wind <- observations %>% group_by(station, month, day) %>% summarize(AvrWind = mean(wdsp, na.rm = TRUE))
daily_wind

#Calculating the average daily wind power generated nationally
daily_gen <- eirgrid17 %>% group_by(month, day) %>% summarize(AvrGen = mean(IEWindGeneration, na.rm = TRUE))
daily_gen


#Joining the two tibbles and printing the resulting tibble
ds <- inner_join(daily_wind, daily_gen, by = c("month", "day")) %>% ungroup()
ds

#Removing any incomplete cases from the dataset and printing clean dataset 
summary(ds)
ds_clean <- ds[complete.cases(ds),]
ds_clean

#Plotting AvrWind and AvrGen along with a linear model for each station.
ggplot(ds_clean) + geom_point(mapping = aes(x = AvrWind, y = AvrGen)) + geom_smooth(mapping = aes(x = AvrWind, y = AvrGen), method='lm', formula= y~x)+ facet_wrap(~station)

#Creating a nested tibble for each station
ds_n <- ds_clean %>% group_by(station) %>% nest()
ds_n

#Adding new columns to he dataset using mutate and map_* functions from purrr.
ds_n <- ds_n %>% mutate(EnerMod = map(data, ~lm(AvrGen ~ AvrWind, data =.))) %>% 
        mutate(Beta_0 = map_dbl(EnerMod, ~.$coefficients[1])) %>%
        mutate(Beta_1 = map_dbl(EnerMod, ~.$coefficients[2])) %>%
        mutate(RSqaured = map_dbl(EnerMod, ~summary(.)$r.squared)) %>%
        arrange(desc(RSqaured))
ds_n
