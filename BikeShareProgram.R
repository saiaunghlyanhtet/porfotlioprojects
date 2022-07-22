library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)
library(janitor)
library(data.table)
library(tidyr)
getwd()
setwd("C:/Users/Hp/Documents/Google Data Analytics Capstone Project")

#load datasets
trip_data202107 <- read.csv("202107-divvy-tripdata.csv")
trip_data202108 <- read.csv("202108-divvy-tripdata.csv")
trip_data202109 <- read.csv("202109-divvy-tripdata.csv")
trip_data202110 <- read.csv("202110-divvy-tripdata.csv")
trip_data202111 <- read.csv("202111-divvy-tripdata.csv")
trip_data202112 <- read.csv("202112-divvy-tripdata.csv")
trip_data202201 <- read.csv("202201-divvy-tripdata.csv")
trip_data202202 <- read.csv("202202-divvy-tripdata.csv")
trip_data202203 <- read.csv("202203-divvy-tripdata.csv")
trip_data202204 <- read.csv("202204-divvy-tripdata.csv")
trip_data202205 <- read.csv("202205-divvy-tripdata.csv")
trip_data202206 <- read.csv("202206-divvy-tripdata.csv")

#checking columnames
colnames(trip_data202107)
colnames(trip_data202108)
colnames(trip_data202109)
colnames(trip_data202110)
colnames(trip_data202111)
colnames(trip_data202112)
colnames(trip_data202201)
colnames(trip_data202202)
colnames(trip_data202203)
colnames(trip_data202204)
colnames(trip_data202205)
colnames(trip_data202206)

#checking data structures and data types
str(trip_data202107)
str(trip_data202108)
str(trip_data202109)
str(trip_data202110)
str(trip_data202111)
str(trip_data202112)
str(trip_data202201)
str(trip_data202202)
str(trip_data202203)
str(trip_data202204)
str(trip_data202205)
str(trip_data202206)

#bindig datasets into a single data frame
all_trips <- bind_rows(trip_data202107, trip_data202108, trip_data202109, trip_data202110,
                       trip_data202111, trip_data202112, trip_data202201, trip_data202202,
                       trip_data202203, trip_data202204, trip_data202205, trip_data202206)
str(all_trips)

#chaning start time and end time to ymd_hms format
all_trips[['started_at']] <- ymd_hms(all_trips[['started_at']])
all_trips[['ended_at']] <- ymd_hms(all_trips[['ended_at']])

str(all_trips)

all_trips <- all_trips %>%
  select(-c(start_lat:end_lng))
glimpse(all_trips)


all_trips <- all_trips %>%
  rename(ride_type = rideable_type, 
         start_time = started_at,
         end_time = ended_at,
         customer_type = member_casual)
glimpse(all_trips)

all_trips$day_of_the_week <- format(as.Date(all_trips$start_time),'%a')
all_trips$month <- format(as.Date(all_trips$start_time),'%b_%y')
all_trips$time <- format(all_trips$start_time, format = "%H:%M")
all_trips$time <- as.POSIXct(all_trips$time, format = "%H:%M")

all_trips$trip_duration <- (as.double(difftime(all_trips$end_time, all_trips$start_time)))/60
glimpse(all_trips)

# checking for trip lengths less than 0
nrow(subset(all_trips,trip_duration < 0))

#checking for testrides that were made by company for quality checks
nrow(subset(all_trips, start_station_name %like% "TEST"))
nrow(subset(all_trips, start_station_name %like% "test"))
nrow(subset(all_trips, start_station_name %like% "Test"))

# remove negative trip durations 
all_trips_v2 <- all_trips[!(all_trips$trip_duration < 0),]

#remove test rides
all_trips_v2<- all_trips_v2[!((all_trips_v2$start_station_name %like% "TEST" | all_trips_v2$start_station_name %like% "test" | all_trips_v2$start_station_name %like% "Test")),]


#check dataframe
glimpse(all_trips_v2)

# checking count of distinct values
sum_cus <- table(all_trips_v2$customer_type)
sum_cus

#aggregating total trip duration by customer type
setNames(aggregate(trip_duration ~ customer_type, all_trips_v2, sum), c("customer_type", "total_trip_duration(mins)"))

sum_dur <- summary(all_trips_v2$trip_duration)
sum_dur

all_trips_v2 %>% group_by(customer_type) %>%
  summarise(min_trip_duration = min(trip_duration), max_trip_duration = max(trip_duration),
   median_trip_duration = median(trip_duration), mean_trip_duration = mean(trip_duration))

all_trips_v2$day_of_the_week <- ordered(all_trips_v2$day_of_the_week, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
all_trips_v2$month <- ordered(all_trips_v2$month, levels=c("Jul_21", "Aug_21", "Sep_21", "Oct_21", "Nov_21", "Dec_21", 
                                                           "Jan_22","Feb_22", "Mar_22", "Apr_22", "May_22", "Jun_22"))

#Summary of Number of Rides and Average Duration by Customer Type and Day of the Week                                                  
sum_nride_adur_cus_day <- all_trips_v2 %>% 
  group_by(customer_type, day_of_the_week) %>%  
  summarise(number_of_rides = n(),average_duration_mins = mean(trip_duration)) %>% 
  arrange(customer_type, desc(number_of_rides))  
sum_nride_adur_cus_day
write.csv(sum_nride_adur_cus_day, "No of Rides and Avg Dur Day.csv", row.names = F)

#Analysis of Trips by Customer Type Vs Day of the Week
all_trips_v2 %>% group_by(customer_type, day_of_the_week) %>%
  summarise(number_of_rides = n()) %>%
  arrange(customer_type, day_of_the_week) %>%
  ggplot(aes(x = day_of_the_week, y = number_of_rides, fill = customer_type)) +
  labs(title = "Total trips by Customer Type and Day of the Week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))



#Average trips by customer type and month
unique(all_trips$month)

#Summary of Number of Rides and Average Duration by Customer Type and Month                                                 
sum_nride_adur_cus_month <- all_trips_v2 %>% 
  group_by(customer_type, month) %>%  
  summarise(number_of_rides = n(),`average_duration_(mins)` = mean(trip_duration)) %>% 
  arrange(customer_type,desc(number_of_rides))
sum_nride_adur_cus_month
write.csv(sum_nride_adur_cus_month, "No of Ride and Avg Dur Month.csv", row.names = F)


#Analysis of Trips by Customer Type and Month
all_trips_v2 %>%  
  group_by(customer_type, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(customer_type, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = customer_type)) +
  labs(title ="Total trips by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#Analysis of average trip duration by customer type on each day
all_trips_v2 %>%  
  group_by(customer_type, day_of_the_week) %>% 
  summarise(average_trip_duration = mean(trip_duration)) %>%
  ggplot(aes(x = day_of_the_week, y = average_trip_duration, fill = customer_type)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Day of the week")

#Analysis of average trip duration by customer type in each month
all_trips_v2 %>%  
  group_by(customer_type, month) %>% 
  summarise(average_trip_duration = mean(trip_duration)) %>%
  ggplot(aes(x = month, y = average_trip_duration, fill = customer_type)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30))

#Analysis of Bike Demand Per Day
all_trips_v2 %>%  
  group_by(customer_type, time) %>% 
  summarise(number_of_trips = n()) %>%
  ggplot(aes(x = time, y = number_of_trips, color = customer_type, group = customer_type)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 hour", minor_breaks = NULL,
                   date_labels = "%H:%M", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title ="Demand over 24 hours of a day", x = "Time of the day")

#Analysis of ride type and number of trips by customer type
all_trips_v2 %>%
group_by(ride_type, customer_type) %>%
  summarise(number_of_trips = n()) %>%  
  ggplot(aes(x= ride_type, y=number_of_trips, fill= customer_type))+
  geom_bar(stat='identity') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title ="Ride type Vs. Number of trips")

#Clean data
all_trips_cleandata <- aggregate(all_trips_v2$trip_duration ~ all_trips_v2$customer_type + all_trips_v2$day_of_the_week, FUN = mean)
write.csv(all_trips_cleandata, "All Trips Clean Data.csv", row.names = F)
