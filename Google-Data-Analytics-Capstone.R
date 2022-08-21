# Google Data Analytics Capstone by Deniz Firat

# Load needed packages
library("tidyverse")
library("janitor")
library("lubridate")
library("hms")


# Import data sets [(Source)](https://divvy-tripdata.s3.amazonaws.com/index.html)
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
compare_df_cols_same(jul_2021, aug_2021, sep_2021, oct_2021, nov_2021, dec_2021, jan_2022, feb_2022, mar_2022, apr_2022, may_2022, jun_2022, bind_method = c("bind_rows", "rbind"), verbose = TRUE)


# Merge all data sets
cyclistic  <- rbind (jul_2021, aug_2021, sep_2021, oct_2021, nov_2021, dec_2021, jan_2022, feb_2022, mar_2022, apr_2022, may_2022, jun_2022)


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
cyclistic$started_at <- as.POSIXct(cyclistic$started_at, format = "%Y-%m-%d %H:%M:%S")
cyclistic$ended_at <- as.POSIXct(cyclistic$ended_at, format = "%Y-%m-%d %H:%M:%S")


# Calculate ride length
cyclistic$ride_length <- difftime(cyclistic$ended_at, cyclistic$started_at, units = "mins")
cyclistic$ride_length <- round(cyclistic$ride_length, digits = 2)


# Remove negative and/or zero ride lengths
cyclistic <- subset(cyclistic, ride_length > 0)


# Create new date columns
cyclistic$month <- format(as.Date(cyclistic$started_at), "%m")
cyclistic$day <- format(as.Date(cyclistic$started_at), "%d")
cyclistic$year <- format(as.Date(cyclistic$started_at), "%Y")
cyclistic$weekday <- format(as.Date(cyclistic$started_at), "%A")
cyclistic$time <- as_hms((cyclistic$started_at))
cyclistic$hour <- as_hms(floor_date(cyclistic$started_at, unit = "hour"))


# Order weekday
cyclistic$weekday <- ordered(cyclistic$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


# Save new data set
write.csv(cyclistic, "cyclistic.csv")


## ANALYZE

# Rides by User Type
cyclistic %>%
  count(member_casual)


# Rides by Bike Type
cyclistic %>%
  group_by(rideable_type, member_casual) %>% 
  count(rideable_type)


# Rides by Hour
cyclistic %>%
  group_by(member_casual) %>% 
  count(hour)


# Rides by Weekday
cyclistic %>%
  group_by(member_casual) %>% 
  count(weekday)


# Rides by Month
cyclistic %>%
  group_by(member_casual) %>% 
  count(month)


# Average Ride Length by User Type
cyclistic %>% 
  group_by(member_casual) %>% 
  summarise(Mean = mean(ride_length))


# Average Ride Length by Bike Type
cyclistic %>% 
  group_by(rideable_type, member_casual) %>% 
  summarise(Mean = mean(ride_length))


# Average Ride Length by Hour
cyclistic %>% 
  group_by(member_casual, hour) %>% 
  summarise(Mean = mean(ride_length))


# Average Ride Length by Weekday
cyclistic %>% 
  group_by(member_casual, weekday) %>% 
  summarise(Mean = mean(ride_length))


# Average Ride Length by Month
cyclistic %>% 
  group_by(member_casual, month) %>% 
  summarise(Mean = mean(ride_length))


## Visualize

# Rides by User Type
rides_casual <- NROW(filter(cyclistic, member_casual == "casual"))
rides_member <- NROW(filter(cyclistic, member_casual == "member"))

rides_by_user_type <- c(rides_casual, rides_member)
percent_rides_by_user_type <- round(100 * rides_by_user_type / sum(rides_by_user_type), 1)

pie(percent_rides_by_user_type, labels = paste0(percent_rides_by_user_type, "%"), main = "Rides by User Type", radius = 1, clockwise = TRUE, col = c("#c85200", "#1170aa"), cex = 0.8)

legend("topright", c("Casual", "Member"), cex = 0.65, fill = c("#c85200", "#1170aa"))


# Rides by Bike Type
classic_bike_casual <- NROW(filter(cyclistic, rideable_type == "classic_bike" & member_casual == "casual"))
classic_bike_member <- NROW(filter(cyclistic, rideable_type == "classic_bike" & member_casual == "member"))
docked_bike_casual <- NROW(filter(cyclistic, rideable_type == "docked_bike" & member_casual == "casual"))
docked_bike_member <- NROW(filter(cyclistic, rideable_type == "docked_bike" & member_casual == "member"))
electric_bike_casual <- NROW(filter(cyclistic, rideable_type == "electric_bike" & member_casual == "casual"))
electric_bike_member <- NROW(filter(cyclistic, rideable_type == "electric_bike" & member_casual == "member"))

rides_by_bike_type_x <- c("Classic Bike", "Electric Bike", "Docked Bike")

rides_by_bike_type <- matrix(c(classic_bike_member, classic_bike_casual, electric_bike_member, electric_bike_casual, docked_bike_member, docked_bike_casual), nrow = 2, ncol = 3)

barplot(rides_by_bike_type, space = 1, main = "Rides by Bike Type", names.arg = rides_by_bike_type_x, col = c("#1170aa", "#c85200"), cex.names = 0.8, axes = FALSE)

rides_by_bike_type_y_axis <- c(0, 1000000, 2000000, 3000000)
axis(2, at = rides_by_bike_type_y_axis, las = 1, labels = sprintf("%1.fM", rides_by_bike_type_y_axis/(1000000)), cex.axis = 0.7)

legend("topright", c("Casual", "Member"), cex = 0.65, fill = c("#c85200", "#1170aa"))


# Rides by Hour
rides_by_hour <- cyclistic %>%
  group_by(member_casual) %>% 
  count(hour)
rides_by_hour_casual <- rides_by_hour[rides_by_hour$member_casual == 'casual', ]
rides_by_hour_member <- rides_by_hour[rides_by_hour$member_casual == 'member', ]

rides_by_hour_x <- c(0:23)
rides_by_hour_y <- c(rides_by_hour_casual$n)
rides_by_hour_y2 <- c(rides_by_hour_member$n)

plot(rides_by_hour_x, rides_by_hour_y2, type = "l", lwd = 2, main = "Rides by Hour", col = c("#1170aa"), xlab = "", ylab = "", axes = FALSE)
lines(rides_by_hour_x, rides_by_hour_y, type = "l", lwd = 2, col = c("#c85200"))

rides_by_hour_y_axis <- c(0, 100000, 200000, 300000)
axis(1, at = c(0:23), cex.axis = 0.65)
axis(2, at = rides_by_hour_y_axis, las = 1, labels = sprintf("%1.fK", rides_by_hour_y_axis/(1000)), cex.axis = 0.7)

legend("topright", c("Casual", "Member"), cex = 0.65, fill = c("#c85200", "#1170aa"))


# Rides by Weekday
rides_by_weekday <- cyclistic %>%
  group_by(member_casual) %>% 
  count(weekday)
rides_by_weekday_casual <- rides_by_weekday[rides_by_weekday$member_casual == 'casual', ]
rides_by_weekday_member <- rides_by_weekday[rides_by_weekday$member_casual == 'member', ]

rides_by_weekday_x <- c(1:7)
rides_by_weekday_y <- c(rides_by_weekday_casual$n)
rides_by_weekday_y2 <- c(rides_by_weekday_member$n)

rides_by_weekday_y_axis <- c(0, 250000, 350000, 450000)

plot(rides_by_weekday_x, rides_by_weekday_y, type = "l", lwd = 2, main = "Rides by Weekday", col = c("#c85200"), xlab = "", ylab = "", axes = FALSE)
lines(rides_by_weekday_x, rides_by_weekday_y2, type = "l", lwd = 2, col = c("#1170aa"))

axis(1, at = 1:7, labels = rides_by_weekday_casual$weekday, cex.axis = 0.7)
axis(2, at = rides_by_weekday_y_axis, las = 1, labels = sprintf("%1.fK", rides_by_weekday_y_axis/(1000)), cex.axis = 0.7)

legend("topright", c("Casual", "Member"), cex = 0.65, fill = c("#c85200", "#1170aa"))


# Rides by Month
rides_by_month <- cyclistic %>%
  group_by(member_casual) %>% 
  count(month)
rides_by_month_casual <- rides_by_month[rides_by_month$member_casual == 'casual', ]
rides_by_month_member <- rides_by_month[rides_by_month$member_casual == 'member', ]

rides_by_month_x <- c(01:12)
rides_by_month_y <- c(rides_by_month_casual$n)
rides_by_month_y2 <- c(rides_by_month_member$n)

new_y_axis <- c(0, 100000, 200000, 300000, 400000)
rides_by_month_x_axis <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

plot(rides_by_month_x, rides_by_month_y, type = "l", lwd = 2, main = "Rides by Month", col = c("#c85200"), xlab = "", ylab = "", axes = FALSE)
axis(1, at = rides_by_month_x, labels = rides_by_month_x_axis, cex.axis = 0.7)
axis(2, at = new_y_axis, las = 1, labels = sprintf("%1.fK", new_y_axis/(1000)), cex.axis = 0.7)
lines(rides_by_month_x, rides_by_month_y2, type = "l", lwd = 2, col = c("#1170aa"))
legend("topright", c("Casual", "Member"), cex = 0.65, fill = c("#c85200", "#1170aa"))


# Average Ride Length by User Type
average_ride_length <- cyclistic %>% 
  group_by(member_casual) %>% 
  summarise(Mean = mean(ride_length))
average_ride_length$Mean <- round(as.numeric(as.period(average_ride_length$Mean, unit = "mins"))/60, 2)

average_ride_length_x <- c("Casual", "Member")
average_ride_length_y <- c(average_ride_length$Mean)

barplot(average_ride_length_y, xlab = "Minutes", cex.lab = 0.8, space = 1, horiz = TRUE, main = "Average Ride Length by User Type", col = c("#c85200", "#1170aa"), cex.names = 0.8, axes = FALSE)

average_ride_length_axis <- c(0, 5, 10, 15, 20, 25, 30)
axis(1, at = average_ride_length_axis, cex.axis = 0.7)

legend("topright", c("Casual", "Member"), cex = 0.65, fill = c("#c85200", "#1170aa"))


# Average Ride Length by Bike Type
average_ride_length_by_bike_type <- cyclistic %>% 
  group_by(member_casual) %>% 
  summarise(Mean = mean(ride_length))

average_ride_length_by_bike_type_matrix <- matrix(c(	
  0, 63.38972, 13.10241, 24.99549, 11.32548, 18.58991), nrow = 2, ncol = 3)

barplot(average_ride_length_by_bike_type_matrix, space = 1, main = "Average Ride Length by Bike Type", names.arg = c("Docked Bike", "Classic Bike", "Electric Bike"), col = c("#1170aa", "#c85200"), cex.names = 0.8, axes = FALSE)

average_ride_length_by_bike_type_y_axis <- c(0, 10, 20, 30, 40, 50, 60, 70)
axis(2, at = average_ride_length_by_bike_type_y_axis, las = 1, cex.axis = 0.7)

legend("topright", c("Casual", "Member"), cex = 0.65, fill = c("#c85200", "#1170aa"))


# Average Ride Length by Hour
average_ride_length_by_hour <- cyclistic %>% 
  group_by(member_casual, hour) %>% 
  summarise(Mean = mean(ride_length))

average_ride_length_by_hour_casual <- average_ride_length_by_hour[average_ride_length_by_hour$member_casual == 'casual', ]
average_ride_length_by_hour_member <- average_ride_length_by_hour[average_ride_length_by_hour$member_casual == 'member', ]

average_ride_length_by_hour_x <- c(0:23)

average_ride_length_by_hour_y <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40)
average_ride_length_by_hour_y1 <- round(as.numeric(as.period(average_ride_length_by_hour_casual$Mean, unit = "mins"))/60, 2)
average_ride_length_by_hour_y2 <- round(as.numeric(as.period(average_ride_length_by_hour_member$Mean, unit = "mins"))/60, 2)

plot(average_ride_length_by_hour_x, average_ride_length_by_hour_y, type = "l", lwd = 2, main = "Average Ride Length by Hour", col = c("white"), xlab = "", ylab = "", axes = FALSE)
lines(average_ride_length_by_hour_x, average_ride_length_by_hour_y1, type = "l", lwd = 2, col = c("#c85200"))
lines(average_ride_length_by_hour_x, average_ride_length_by_hour_y2, type = "l", lwd = 2, col = c("#1170aa"))

axis(1, at = c(0:23), cex.axis = 0.65)
axis(2, at = c(0, 10, 20, 30, 40), las = 1, cex.axis = 0.7)

legend("topright", c("Casual", "Member"), cex = 0.65, fill = c("#c85200", "#1170aa"))


# Average Ride Length by Weekday
average_ride_length_by_weekday <- cyclistic %>% 
  group_by(member_casual, weekday) %>% 
  summarise(Mean = mean(ride_length))

average_ride_length_by_weekday_casual <- average_ride_length_by_weekday[average_ride_length_by_weekday$member_casual == 'casual', ]
average_ride_length_by_weekday_member <- average_ride_length_by_weekday[average_ride_length_by_weekday$member_casual == 'member', ]

average_ride_length_by_weekday_x <- c(1:7)

average_ride_length_by_weekday_y <- c(0, 0, 0, 0, 40, 40, 40)
average_ride_length_by_weekday_y1 <- round(as.numeric(as.period(average_ride_length_by_weekday_casual$Mean, unit = "mins"))/60, 2)
average_ride_length_by_weekday_y2 <- round(as.numeric(as.period(average_ride_length_by_weekday_member$Mean, unit = "mins"))/60, 2)

plot(average_ride_length_by_weekday_x, average_ride_length_by_weekday_y, type = "l", lwd = 2, main = "Average Ride Length by weekday", col = c("white"), xlab = "", ylab = "", axes = FALSE)
lines(average_ride_length_by_weekday_x, average_ride_length_by_weekday_y1, type = "l", lwd = 2, col = c("#c85200"))
lines(average_ride_length_by_weekday_x, average_ride_length_by_weekday_y2, type = "l", lwd = 2, col = c("#1170aa"))

axis(1, at = 1:7, labels = average_ride_length_by_weekday_casual$weekday, cex.axis = 0.7)
axis(2, at = c(0, 10, 20, 30, 40), las = 1, cex.axis = 0.7)

legend("topright", c("Casual", "Member"), cex = 0.65, fill = c("#c85200", "#1170aa"))


# Average Ride Length by Month
average_ride_length_by_month <- cyclistic %>% 
  group_by(member_casual, month) %>% 
  summarise(Mean = mean(ride_length))

average_ride_length_by_month_casual <- average_ride_length_by_month[average_ride_length_by_month$member_casual == 'casual', ]
average_ride_length_by_month_member <- average_ride_length_by_month[average_ride_length_by_month$member_casual == 'member', ]

average_ride_length_by_month_x <- c(1:12)
average_ride_length_by_month_x_axis <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

average_ride_length_by_month_y <- c(0, 0, 0, 0, 0, 0, 40, 40, 40, 40, 40, 40)
average_ride_length_by_month_y1 <- round(as.numeric(as.period(average_ride_length_by_month_casual$Mean, unit = "mins"))/60, 2)
average_ride_length_by_month_y2 <- round(as.numeric(as.period(average_ride_length_by_month_member$Mean, unit = "mins"))/60, 2)

plot(average_ride_length_by_month_x, average_ride_length_by_month_y, type = "l", lwd = 2, main = "Average Ride Length by month", col = c("white"), xlab = "", ylab = "", axes = FALSE)
lines(average_ride_length_by_month_x, average_ride_length_by_month_y1, type = "l", lwd = 2, col = c("#c85200"))
lines(average_ride_length_by_month_x, average_ride_length_by_month_y2, type = "l", lwd = 2, col = c("#1170aa"))

axis(1, at = average_ride_length_by_month_x, labels = average_ride_length_by_month_x_axis, cex.axis = 0.7)
axis(2, at = c(0, 10, 20, 30, 40), las = 1, cex.axis = 0.7)

legend("topright", c("Casual", "Member"), cex = 0.65, fill = c("#c85200", "#1170aa"))
