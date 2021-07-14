#### In this Analyze Phasse, we can look into the codeing part which is perfomred in R.

#### Guiding questions

    - How should you organize your data to perform analysis on it?
    - Has your data been properly formatted?
    - What surprises did you discover in the data?
    - What trends or relationships did you find in the data?
    - How will these insights help answer your business questions?

#### Key tasks

    1. Aggregate your data so it’s useful and accessible.
    2. Organize and format your data.
    3. Perform calculations
    4. Identify trends and relationships.

#### Deliverable

    - Here is a summary of the analysis of the Bike Share data from Jan, 2021 to March, 2021
    - The data cleaning has been performed in RStudio.
    - This analysis is for case study 1 from the Google Data Analytics Certificate (Cyclistic).

    

#### Enviroment setup in R:

    - Install required packages and library for data cleaning, transformation, and visualization.


```{r Install Packages, include=FALSE}

# Install require packages 

install.packages("tidyverse")
install.packages("lubridate")
install.packages("hms")

# Load Respective packages

library(tidyverse) #helps to clean/wrangling data
library(ggplot2) #for visulization
library(lubridate) #for data function
library(dplyr) #for data manipulation

# Load data

data2<-read_csv("c:/cycledata/cycle.csv") #upload data


#Inspect for any incongruencies data

colnames(data2)  #List of column names
nrow(data2)  #How many rows are in data frame
dim(data2)  #Dimensions of the data frame?
head(data2)  #See the first 6 rows of data frame.  Also tail(qs_raw)
str(data2)  #See list of columns and data types (numeric, character, etc)
summary(data2) #summary of data, only for numeric
str(data2) #shows list if col and its data type

# Add columns that list the date, month, day, and year of each ride

data2$date <- as.Date(data2$started_at) #The default format is yyyy-mm-dd
data2$month <- format(as.Date(data2s$date), "%m")
data2$day <- format(as.Date(data2$date), "%d")
data2$year <- format(as.Date(data2$date), "%Y")
data2$day_of_week <- format(as.Date(data2$date), "%A")


# Add a "ride_length" calculation to all_trips (in seconds)

data2$ride_length<-difftime(data2$end_time,data2$start_time)


# Inspect the structure of the columns
str(data2)
 
 
#Use the below code, if there is any negative value are in ride_length

new_data2 <- data2[!(data2$start_station_name == "HQ QR" | data2$ride_length<0),]


# Descriptive analysis on ride_length (all figures in seconds)

mean(new_data2$ride_length) #straight average (total ride length / rides)
median(new_data2$ride_length) #midpoint number in the ascending array of ride lengths
max(new_data2$ride_length) #longest ride
min(new_data2$ride_length) #shortest ride

#Summary for our refernce

summary(new_data2$ride_length)


# Compare members and casual users

aggregate(new_data2$ride_length~ new_data2$member_casual, FUN = mean)
aggregate(new_data2$ride_length ~new_data2$member_casual, FUN = median)
aggregate(new_data2$ride_length ~new_data2$member_casual, FUN = max)
aggregate(new_data2$ride_length ~new_data2$member_casual, FUN = min)


# The average ride time by each day for members vs casual users

aggregate(new_data2$ride_length ~ new_data2$member_casual + new_data2$day_of_week, FUN = mean)

#Let we order the weekdays and again run the code

new_data2$day_of_week <- ordered(new_data2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(new_data2$ride_length ~ new_data2$member_casual + new_data2$day_of_week, FUN = mean)


# Analyze ridership data by type and weekday

new_data2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 	# calculates the average duration
  arrange(member_casual, weekday)		#sort

# Analyze ridership data by type and weekday

new_data2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")+
  labs(title="Number Of Rides in Weekday of Members and Causal")

# Visualization for average duration


new_data2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")+
  labs(title="Average duration in Weekday of member and causal",subtitle="Time in secs")

#Save result in new variable

counts <- aggregate(data2$ride_length ~ new_data2$member_casual + new_data2$day_of_week, FUN = mean)
#use write.csv and export it
write.csv(counts, file = 'c:/cycledata/avg_ride_length.csv')
  
