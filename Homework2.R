prof <- read.csv('profs.csv')
library(ggplot2)

# Problem 1: Beauty, or not, in the classroom
# PART A
hist(prof$eval,main='Histogram of Course Evaluation Score',xlab='Course Evaluation Score (1-5)',ylab = 'Frequency',col='Blue')
# PART B
boxplot(eval ~ native,
        data = prof,
        main = "Boxplot of Course Evaluation Score by Native English Speaker Status",
        xlab = "Native English Speaking Status",
        ylab = "Course Evaluation Score (1-5)",
        col = c("lightblue", "lightgreen"),
        names = c("Non-Native", "Native"))
# PART C
ggplot(prof, aes(x = eval)) + geom_histogram(binwidth = 0.5, color = "black", alpha = 0.6) + facet_wrap(~gender, nrow = 2) +
  labs(title = "Distribution of Course Evaluation by Gender",
       x = "Course Evaluation Scores (1-5)",
       y = "Frequency") +
  theme_minimal()
# PART D
plot(prof$beauty, prof$eval, 
     xlab = "Scatterplot Between Physical Attractiveness and Course Evaluation Scores", 
     ylab = "Physical Attractiveness Rating", 
     main = "Course Evaluation Scores",
     col = "blue", pch = 20)  
abline(lm(eval ~ beauty, data = prof), col = "red", lwd = 2)

# Problem 2 : bike sharing
bike <- read.csv('bikeshare.csv')
avg_hourly <- aggregate(total ~ hr, data = bike, FUN = mean)
plot(avg_hourly$hr, avg_hourly$total, type = "l", col = "blue",main = "Average Hourly Bike Rentals",  
     xlab = "Hour of the Day", ylab = "Average Bike Rentals", pch = 20)

bike$workingday <- factor(bike$workingday, levels = c(0, 1), labels = c("Non-working Day", "Working Day"))
ggplot(bike, aes(x = hr, y = total, a = 1, color = workingday)) +
  geom_line(stat = "summary", fun = "mean") +  
  labs(x = "Hour of the Day", y = "Average Bike Rentals", 
       title = "Average Hourly Bike Rentals (Working Day vs. Non-working Day)") +
  facet_wrap(~ workingday) +  
  theme_minimal()

bike_9am <- filter(bike, hr == 9)
bike_9am <- add_row(bike_9am, weathersit = 4, total = 0, workingday = "Non-working Day")
bike_9am <- add_row(bike_9am, weathersit = 4, total = 0, workingday = "Working Day")
ggplot(bike_9am, aes(x = weathersit, y = total)) +
  stat_summary(fun = mean, geom = "bar", fill = "blue", color = "black") +
  facet_wrap(~ workingday) +  
  labs(x = "Weather Situation", y = "Average Bike Rentals", 
       title = "Average Bike Rentals at 9 AM by Weather Situation (Working Day vs. Non-working Day)") +
  theme_minimal()  

# Problem 3 - Capital Metro UT Ridership
capmetro_UT <- read.csv('capmetro_UT.csv')
capmetro_UT <- mutate(capmetro_UT,
                      day_of_week = factor(day_of_week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
                      month = factor(month, levels = c("Sep", "Oct", "Nov")))
avg_boardings <- aggregate(boarding ~ hour_of_day + day_of_week + month, data = capmetro_UT, FUN = mean)
ggplot(avg_boardings, aes(x = hour_of_day, y = boarding, color = month)) +
  geom_line() +
  facet_wrap(~ day_of_week) +
  labs(x = "Hour of the Day", y = "Average Boardings", 
       title = "Average Boardings by Hour, Day of Week, and Month") +
  theme_minimal()

ggplot(capmetro_UT, aes(x = temperature, y = boarding, color = weekend)) +
  geom_point() +  
  labs(x = "Temperature in F", y = "Number of Boardings", 
       title = "Boardings vs. Temperature, Faceted by Hour of the Day") +
  facet_wrap(~ hour_of_day) +  
  theme_minimal()

# Problem 4 - Wrangling the Billboard Top 100
# PART A
billboard <- read.csv("billboard.csv")
a <- aggregate(week_position ~ performer + song, data = billboard, FUN = length)
colnames(a)[3] <- "count"
sorted_data <- a[order(-a$count), ]
top <- head(sorted_data, 10)
print(top)

# PART B
filtered <- billboard[billboard$year > 1958 & billboard$year < 2021, ]
song_counts <- aggregate(week_position ~ year + performer + song, data = filtered, FUN = length)
colnames(song_counts)[4] <- "weeks_on"
unique_songs <- aggregate(song ~ year, data = song_counts, FUN = function(x) length(unique(x)))
colnames(unique_songs) <- c("year", "unique_songs")
plot(unique_songs$year, unique_songs$unique_songs, 
     type = "l", col = "blue", lwd = 2, 
     xlab = "Year", ylab = "Number of Unique Songs",
     main = "Musical Diversity of Billboard Top 100 Over Time")

# PART C
song_weeks <- aggregate(week_position ~ performer + song, data = billboard, FUN = length)
ten_week <- song_weeks[song_weeks$week_position >= 10, ]
artist <- aggregate(song ~ performer, data = ten_week, FUN = length)
colnames(artist) <- c("performer", "ten_week")
artist_hits <- artist[artist$ten_week >= 30, ]
artist_hits <- artist_hits[order(-artist_hits$ten_week), ]



