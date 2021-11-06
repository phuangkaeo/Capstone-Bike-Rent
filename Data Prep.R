#Install package
install.packages("lubridate")

#Load Library
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory


#=====================
# STEP 1: COLLECT DATA
#=====================

df_202010 <- read_csv("~//R Tutorials//Capstone - Bike Rent//Raw Data//202010-divvy-tripdata.csv")
df_202011 <- read_csv("~//R Tutorials//Capstone - Bike Rent//Raw Data//202011-divvy-tripdata.csv")
df_202012 <- read_csv("~//R Tutorials//Capstone - Bike Rent//Raw Data//202012-divvy-tripdata.csv")
df_202101 <- read_csv("~//R Tutorials//Capstone - Bike Rent//Raw Data//202101-divvy-tripdata.csv")
df_202102 <- read_csv("~//R Tutorials//Capstone - Bike Rent//Raw Data//202102-divvy-tripdata.csv")
df_202103 <- read_csv("~//R Tutorials//Capstone - Bike Rent//Raw Data//202103-divvy-tripdata.csv")
df_202104 <- read_csv("~//R Tutorials//Capstone - Bike Rent//Raw Data//202104-divvy-tripdata.csv")
df_202105 <- read_csv("~//R Tutorials//Capstone - Bike Rent//Raw Data//202105-divvy-tripdata.csv")
df_202106 <- read_csv("~//R Tutorials//Capstone - Bike Rent//Raw Data//202106-divvy-tripdata.csv")
df_202107 <- read_csv("~//R Tutorials//Capstone - Bike Rent//Raw Data//202107-divvy-tripdata.csv")
df_202108 <- read_csv("~//R Tutorials//Capstone - Bike Rent//Raw Data//202108-divvy-tripdata.csv")
df_202109 <- read_csv("~//R Tutorials//Capstone - Bike Rent//Raw Data//202109-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file
colnames(df_202010)
colnames(df_202011)
colnames(df_202012)
colnames(df_202101)
colnames(df_202102)
colnames(df_202103)
colnames(df_202104)
colnames(df_202105)
colnames(df_202106)
colnames(df_202107)
colnames(df_202108)
colnames(df_202109)

# Rename columns  to make them consisent with q1_2020 
#(as this will be the supposed going-forward table design for Divvy)

#(q4_2019 <- rename(q4_2019
#                   ,ride_id = trip_id
#                   ,rideable_type = bikeid 
#                   ,started_at = start_time  
#                  ,ended_at = end_time  
#                 ,start_station_name = from_station_name 
#                ,start_station_id = from_station_id 
#               ,end_station_name = to_station_name 
#              ,end_station_id = to_station_id 
#                  ,member_casual = usertype))
#

# Inspect the dataframes and look for incongruencies
str(df_202010)
str(df_202011)
str(df_202012)
str(df_202101)
str(df_202102)
str(df_202103)
str(df_202104)
str(df_202105)
str(df_202106)
str(df_202107)
str(df_202108)
str(df_202109)

# Convert ride_id and rideable_type to character so that they can stack correctly
df_202010 <-  mutate(df_202010, ride_id = as.character(ride_id),
                     rideable_type = as.character(rideable_type),
                     start_station_id = as.character(start_station_id),
                     end_station_id = as.character(end_station_id))

df_202011 <-  mutate(df_202011, ride_id = as.character(ride_id),
                     rideable_type = as.character(rideable_type),
                     start_station_id = as.character(start_station_id),
                     end_station_id = as.character(end_station_id))

df_202012 <-  mutate(df_202012, ride_id = as.character(ride_id),
                     rideable_type = as.character(rideable_type),
                     start_station_id = as.character(start_station_id),
                     end_station_id = as.character(end_station_id))

df_202101 <-  mutate(df_202101, ride_id = as.character(ride_id),
                     rideable_type = as.character(rideable_type),
                     start_station_id = as.character(start_station_id),
                     end_station_id = as.character(end_station_id)) 

df_202102 <-  mutate(df_202102, ride_id = as.character(ride_id),
                     rideable_type = as.character(rideable_type),
                     start_station_id = as.character(start_station_id),
                     end_station_id = as.character(end_station_id)) 

df_202103 <-  mutate(df_202103, ride_id = as.character(ride_id),
                     rideable_type = as.character(rideable_type),
                     start_station_id = as.character(start_station_id),
                     end_station_id = as.character(end_station_id))

df_202104 <-  mutate(df_202104, ride_id = as.character(ride_id),
                     rideable_type = as.character(rideable_type),
                     start_station_id = as.character(start_station_id),
                     end_station_id = as.character(end_station_id)) 

df_202105 <-  mutate(df_202105, ride_id = as.character(ride_id),
                     rideable_type = as.character(rideable_type),
                     start_station_id = as.character(start_station_id),
                     end_station_id = as.character(end_station_id)) 

df_202106 <-  mutate(df_202106, ride_id = as.character(ride_id),
                     rideable_type = as.character(rideable_type),
                     start_station_id = as.character(start_station_id),
                     end_station_id = as.character(end_station_id)) 

df_202107 <-  mutate(df_202107, ride_id = as.character(ride_id),
                     rideable_type = as.character(rideable_type),
                     start_station_id = as.character(start_station_id),
                     end_station_id = as.character(end_station_id)) 

df_202108 <-  mutate(df_202108, ride_id = as.character(ride_id),
                     rideable_type = as.character(rideable_type),
                     start_station_id = as.character(start_station_id),
                     end_station_id = as.character(end_station_id)) 

df_202109 <-  mutate(df_202109, ride_id = as.character(ride_id),
                     rideable_type = as.character(rideable_type),
                     start_station_id = as.character(start_station_id),
                     end_station_id = as.character(end_station_id)) 
                   
# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(df_202010,df_202011,df_202012,
                       df_202101,df_202102,df_202103,
                       df_202104,df_202105,df_202106,
                       df_202107,df_202108,df_202109)

# Remove lat, long fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

distinct(all_trips, member_casual)
# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$hour <- hour(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$time <- format(as.Date(all_trips$date), "%H:%M:%S")

# Add a "ride_length" calculation to all_trips (in seconds)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]


# Drop NA rows
all_trips_v2 <- na.omit(all_trips_v2)

# Remove Duplicate Data
all_trips_v2 <- distinct(all_trips_v2)

str(all_trips_v2)

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, 
                                    levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN = mean)


# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%   #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)	

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

            
# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
# N.B.: This file location is for a Mac. If you are working on a PC, change the file location accordingly (most likely "C:\Users\YOUR_USERNAME\Desktop\...") to export the data. You can read more here: https://datatofish.com/export-dataframe-to-csv-in-r/
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts,"~//R Tutorials//Capstone - Bike Rent//avg_ride_length.csv", row.names = FALSE)
write.csv(all_trips_v2,"~//R Tutorials//Capstone - Bike Rent//all_trip.csv", row.names = FALSE)


