# Google Data Analytics Capstone by Deniz Firat

# Load needed packages
library("tidyverse")
library("janitor")
library("lubridate")
library("hms")

# Import data sets (Source: https://divvy-tripdata.s3.amazonaws.com/index.html)
jul_2021 <- read.csv("202107-divvy-tripdata.csv")
aug_2021 <- read.csv("202108-divvy-tripdata.csv")
sep_2021 <- read.csv("202109-divvy-tripdata.csv")
oct_2021 <- read.csv("202110-divvy-tripdata.csv")
nov_2021 <- read.csv("202111-divvy-tripdata.csv")
dec_2021 <- read.csv("202112-divvy-tripdata.csv")
jan_2022 <- read.csv("202201-divvy-tripdata.csv")
feb_2022 <- read.csv("202202-divvy-tripdata.csv")
mar_2022 <- read.csv("202203-divvy-tripdata.csv")
apr_2022 <- read.csv("202204-divvy-tripdata.csv")
may_2022 <- read.csv("202205-divvy-tripdata.csv")
jun_2022 <- read.csv("202206-divvy-tripdata.csv")

# Check if data sets can be merged
compare_df_cols_same(
  jul_2021,
  aug_2021,
  sep_2021,
  oct_2021,
  nov_2021,
  dec_2021,
  jan_2022,
  feb_2022,
  mar_2022,
  apr_2022,
  may_2022,
  jun_2022,
  bind_method = c("bind_rows", "rbind"),
  verbose = TRUE
)

# Merge all data sets
cyclistic  <- rbind (
  jul_2021,
  aug_2021,
  sep_2021,
  oct_2021,
  nov_2021,
  dec_2021,
  jan_2022,
  feb_2022,
  mar_2022,
  apr_2022,
  may_2022,
  jun_2022
)

# Inspect new data set
summary(cyclistic)

# PROCESS

# Remove geographical indication contained in columns 9 to 12
cyclistic <- cyclistic[ -c(9:12) ]

# Check for duplicates
length(unique(cyclistic$ride_id)) == nrow(cyclistic)
#use "cyclistic <- distinct(cyclistic)" to remove duplicates

# Remove rows contating NA and NULL values
cyclistic[cyclistic == '' ] <- NA
cyclistic<- na.omit(cyclistic)

# Check for misspelled strings/categories
unique(cyclistic$rideable_type)
unique(cyclistic$member_casual)

# Convert character to datetime
cyclistic$started_at <- as.POSIXct(cyclistic$started_at, format="%Y-%m-%d %H:%M:%S")
cyclistic$ended_at <- as.POSIXct(cyclistic$ended_at, format="%Y-%m-%d %H:%M:%S")

# Calculate ride length
cyclistic$ride_length <- difftime(cyclistic$ended_at,cyclistic$started_at, units = "mins")
cyclistic$ride_length <- round(cyclistic$ride_length, digits = 2)

# Remove negative and/or zero ride lengths
cyclistic <- subset(cyclistic, ride_length > 0)

# Create new date columns
cyclistic$month <- format(as.Date(cyclistic$started_at), "%m")
cyclistic$day <- format(as.Date(cyclistic$started_at), "%d")
cyclistic$year <- format(as.Date(cyclistic$started_at), "%Y")
cyclistic$weekday <- format(as.Date(cyclistic$started_at), "%A")
cyclistic$time <- as_hms((cyclistic$started_at))

# ANALYZE

# Count member type
cyclistic %>%
  group_by(member_casual) %>% 
  count(member_casual)

# Count bike type
cyclistic %>%
  group_by(rideable_type) %>% 
  count(rideable_type)

# Total rides by month
cyclistic %>%
  count(month)

# Total rides by member type and month
cyclistic %>%
  group_by(member_casual) %>% 
  count(month) %>% 
  print(n = 24)

# Total rides by member, bike type and month
cyclistic %>%
  group_by(member_casual, rideable_type) %>% 
  count(month) %>% 
  print(n = 60)

# Total rides by weekday
cyclistic %>%
  count(weekday)

# Total rides by member type and weekday
cyclistic %>%
  group_by(member_casual) %>% 
  count(weekday)

# Total rides by member, bike type and weekday
cyclistic %>%
  group_by(member_casual, rideable_type) %>% 
  count(weekday) %>% 
  print(n = 35)

# Total rides by day
cyclistic %>%
  count(day)

# Total rides by member type and day
cyclistic %>%
  group_by(member_casual) %>% 
  count(day) %>% 
  print(n = 62)

# Total rides by member, bike type and day
cyclistic %>%
  group_by(member_casual, rideable_type) %>% 
  count(day) %>% 
  print(n = 155)

# Average ride length
cyclistic %>% 
  summarise(Mean = mean(ride_length))

# Average ride length by member type
cyclistic %>% 
  group_by(member_casual) %>% 
  summarise(Mean = mean(ride_length))

# Average ride length by bike type
cyclistic %>% 
  group_by(rideable_type) %>% 
  summarise(Mean = mean(ride_length))

# Average ride length by member type and month
cyclistic %>% 
  group_by(member_casual, month) %>% 
  summarise(Mean = mean(ride_length)) %>%
  print(n = 24)

# Average ride length by member type and weekday
cyclistic %>% 
  group_by(member_casual, weekday) %>% 
  summarise(Mean = mean(ride_length)) %>%
  print(n = 14)

# Average ride length by member type and day
cyclistic %>% 
  group_by(member_casual, day) %>% 
  summarise(Mean = mean(ride_length)) %>%
  print(n = 62)

# Average ride length by member and bike type
cyclistic %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(Mean = mean(ride_length))

# Average ride length by member, bike type and month
cyclistic %>% 
  group_by(member_casual, rideable_type, month) %>% 
  summarise(Mean = mean(ride_length)) %>%
  print(n = 60)

# Average ride length by member, bike type and weekday
cyclistic %>% 
  group_by(member_casual, rideable_type, weekday) %>% 
  summarise(Mean = mean(ride_length)) %>%
  print(n = 35)

# Average ride length by member, bike type and day
cyclistic %>% 
  group_by(member_casual, rideable_type, day) %>% 
  summarise(Mean = mean(ride_length)) %>%
  print(n = 155)

# Min ride length by member type
cyclistic %>% 
  group_by(member_casual) %>% 
  summarise(Min = min(ride_length))

# Min ride length by member and bike type
cyclistic %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(Min = min(ride_length))

# Max ride length by member type
cyclistic %>% 
  group_by(member_casual) %>% 
  summarise(Max = max(ride_length))

# Max ride length by member and bike type
cyclistic %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(Max = max(ride_length))

# Save new data set
write.csv(cyclistic,"cyclistic.csv")