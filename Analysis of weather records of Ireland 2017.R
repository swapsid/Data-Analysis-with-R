# Importing the required libraries
library(aimsir17)
library(tibble)

# Creating the required atomic vectors
dates <-  unique(observations$date)
temp_MH <- observations[observations$station =="MACE HEAD",'temp', drop = TRUE]
temp_DA <- observations[observations$station =="DUBLIN AIRPORT",'temp', drop = TRUE]
temp_SI <- observations[observations$station =="SherkinIsland",'temp', drop = TRUE]

str(dates)
str(temp_MH)
str(temp_DA)
str(temp_SI)

# Ploting dates and respective temperatures using Base R function plot()
plot(dates,temp_MH,col = 1)
points(dates,temp_DA,col = 2)
points(dates,temp_SI, col= 3)

# Creating tibble using the atomic vectors created
data_set <- tibble(Date = dates, MaceHead = temp_MH, DublinAirport = temp_DA, SherkinIsland = temp_SI)

# Creating copy of tibble excluding the date
input_data <- subset(data_set, select = -c(Date))

# Add a new column to data_set that contains the name of the station that has the minimum temperature for each observation.
data_set$MinTemp <- apply(input_data, 1, function(x){names(which.min(x))})

#Converting the output to factor
data_set$MinTemp  <- as.factor(data_set$MinTemp)

# Add a new column to data_set that contains the name of the station that has the maximum temperature for each observation.
data_set$MaxTemp <- apply(input_data, 1, function(x){names(which.max(x))})

#Converting the output to factor
data_set$MinTemp  <- as.factor(data_set$MinTemp)



#Finding the percentage of times each station was either the minimum or maximum observation.
min_stations <- (table(data_set$MinTemp)/sum(table(data_set$MinTemp)))*100
max_stations <- (table(data_set$MaxTemp)/sum(table(data_set$MaxTemp)))*100

print(min_stations)
print(max_stations)

#Summary of the overall dataset
print(summary(data_set))

