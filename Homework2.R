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
        # light blue for non native, light green for native
        col = c("lightblue", "lightgreen"),
        names = c("Non-Native", "Native"))
# PART C
#histogram with binwidth, border color, transparency
#seperate by gender, 2 rows, male and female
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
     # dot color, pch is how filled in the dots are
     col = "blue", pch = 20)  
# add best file line which is red between the two vars
abline(lm(eval ~ beauty, data = prof), col = "red", lwd = 2)

# Problem 2 : bike sharing
bike <- read.csv('bikeshare.csv')
# aggregate based on hour of day, find mean for each hour
avg_hourly <- aggregate(total ~ hr, data = bike, FUN = mean)
# same as scatterplot format excpet type = l specifies lineplot
plot(avg_hourly$hr, avg_hourly$total, type = "l", col = "blue",main = "Average Hourly Bike Rentals",  
     xlab = "Hour of the Day", ylab = "Average Bike Rentals", pch = 20)

#instead of 0, 1 for whether it is a working day, make into a categorical variable of non working day or working day
bike$workingday <- factor(bike$workingday, levels = c(0, 1), labels = c("Non-working Day", "Working Day"))
# line plot with summary stats, mean for aggrregation
ggplot(bike, aes(x = hr, y = total, a = 1, color = workingday)) +
  geom_line(stat = "summary", fun = "mean") + 
  labs(x = "Hour of the Day", y = "Average Bike Rentals", 
       title = "Average Hourly Bike Rentals (Working Day vs. Non-working Day)") +
  # facet by working day
  facet_wrap(~ workingday) +  
  theme_minimal()

# make a new subdataset where it only has data where the hour is 9
bike_9am <- filter(bike, hr == 9)
# add two rows for placeholder with these traits, need to tell the viewer that there is a 4th option for weathersit
# two because one of non working day and working day graph
bike_9am <- add_row(bike_9am, weathersit = 4, total = 0, workingday = "Non-working Day")
bike_9am <- add_row(bike_9am, weathersit = 4, total = 0, workingday = "Working Day")
ggplot(bike_9am, aes(x = weathersit, y = total)) +
  # stat summary for mean as bar graph
  stat_summary(fun = mean, geom = "bar", fill = "blue", color = "black") +
  # facet by whether it is a working day
  facet_wrap(~ workingday) +  
  labs(x = "Weather Situation", y = "Average Bike Rentals", 
       title = "Average Bike Rentals at 9 AM by Weather Situation (Working Day vs. Non-working Day)") +
  theme_minimal()  

# Problem 3 - Capital Metro UT Ridership
capmetro_UT <- read.csv('capmetro_UT.csv')
# day of week and month usually based on alphabetical order
# change so it is in actual day of week order and month order
capmetro_UT <- mutate(capmetro_UT,
                      day_of_week = factor(day_of_week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
                      month = factor(month, levels = c("Sep", "Oct", "Nov")))
# find average boardings based on same hour of day, day of week, and month
avg_boardings <- aggregate(boarding ~ hour_of_day + day_of_week + month, data = capmetro_UT, FUN = mean)
ggplot(avg_boardings, aes(x = hour_of_day, y = boarding, color = month)) +
  #line for each month
  geom_line() +
  #subset of plots by day of week
  facet_wrap(~ day_of_week) +
  labs(x = "Hour of the Day", y = "Average Boardings", 
       title = "Average Boardings by Hour, Day of Week, and Month") +
  theme_minimal()

ggplot(capmetro_UT, aes(x = temperature, y = boarding, color = weekend)) +
  # scatterplot of temp vs boarding, color the dots based on whether it is weekend or weekday
  geom_point() +  
  labs(x = "Temperature in F", y = "Number of Boardings", 
       title = "Boardings vs. Temperature, Faceted by Hour of the Day") +
  # create subsets based on what time of day it is
  facet_wrap(~ hour_of_day) +  
  theme_minimal()

# Problem 4 - Wrangling the Billboard Top 100
# PART A
billboard <- read.csv("billboard.csv")
#count num of weeks song by each performer is on top 100
a <- aggregate(week_position ~ performer + song, data = billboard, FUN = length)
# rename column so reader knows its the count
colnames(a)[3] <- "count"
#sort in descending order
sorted_data <- a[order(-a$count), ]
#print the top 10 in an organized table
b <- head(sorted_data, 10)
kable(b, col.names = c("Performer", "Song", "Count"), 
      caption = "Top 10 Songs Since 1958")

# PART B
# remove data that is from lower than 1958 and above 2021, be4t 1957 aand 2020
filtered <- billboard[billboard$year > 1958 & billboard$year < 2021, ]
# aggregate count nu mof weeks song by each performer each yr 
song_counts <- aggregate(week_position ~ year + performer + song, data = filtered, FUN = length)
#rename
colnames(song_counts)[4] <- "weeks_on"
# find out how many of them are unique for each year
unique_songs <- aggregate(song ~ year, data = song_counts, FUN = function(x) length(unique(x)))
#change names of columns again
colnames(unique_songs) <- c("year", "unique_songs")
#line plot
plot(unique_songs$year, unique_songs$unique_songs, 
     type = "l", col = "blue", lwd = 2, 
     xlab = "Year", ylab = "Number of Unique Songs",
     main = "Musical Diversity of Billboard Top 100 Over Time")

# PART C
# number of weeks based on performer and song
song_weeks <- aggregate(week_position ~ performer + song, data = billboard, FUN = length)
# filter by if that week position more than 10 times
ten_week <- song_weeks[song_weeks$week_position >= 10, ]
# find out the number of songs per performer that was on the top 100 for more than 10 times
artist <- aggregate(song ~ performer, data = ten_week, FUN = length)
# rename columns
colnames(artist) <- c("performer", "ten_week")
# filter with only the performers with the number of songs greater than 30 on the top 100 for more than 10 times 
artist_hits <- artist[artist$ten_week >= 30, ]
# descending order
artist_hits <- artist_hits[order(-artist_hits$ten_week), ]
#bar plot reg format, label the x axis with performer names
barplot(artist_hits$ten_week, names.arg = artist_hits$performer, main = "Barplot of Artists with the Most Ten-Week Hits", xlab = "Artist", ylab = "Number of Ten-Week Hits", col = "lightblue",border = "red"
)



