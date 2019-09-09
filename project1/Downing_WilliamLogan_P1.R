library(leaflet)
library(tidyverse)
##### Section 1.1 #####

# Read in the file
fpath <- 'C:\\Users\\Downi\\Documents\\GitHub\\CE-597_GDA\\project1\\pu2014.csv'
data <- read.csv(fpath)

# Subset to columns of interest
data <- data[,c('epoch', 'weekday', 'hour', 'month', 'day', 'year',
             'longitude', 'latitude', 'text', 'lang', 'source', 'user_id')]

# check for any missing data
summary(data)

sapply(data, function(x) sum(is.na(x))) # Note: This is an anonymous function that passes in the data as the x argument

# Convert Epoch time to date time and save to dataframe
data$datetime <- as.POSIXct(data$ep, origin="1970-01-01")

##### Section 1.2 #####

# Get count of unique users
unique_users <- unique(data$user_id)
unique_users.count <- length(unique_users)

# Count number of tweets by each user and sort in descending order & visualize
tweet_count <- as.data.frame(table(data$user_id)) # table produces a table of counts for each user
names(tweet_count) <- c('user_id', 'freq')
tweet_count <- tweet_count[order(tweet_count$freq, decreasing=TRUE),]

plot(tweet_count$freq, col='blue', ylab='Frequency of Tweets', xlab='Index',
     main='Tweet Frequency on a User by User Basis')

# Viualize, small number of users generate the most tweets

# The previous plot sort of accomplishes this already. Zoom in on the head and tail
# to emphasize this.

par(mfrow=c(1,2))
plot(head(tweet_count$freq, n=50L), col='red', ylab='Frequency of Tweets', xlab='Index',
     main='Top 50 Tweet Frequency')
plot(tail(tweet_count$freq, n=50L), col='blue', ylab='Frequency of Tweets', xlab='Index',
     main='Bottom 50 Tweet Frequency')

# Reset the grahical parameter for future use
par(mfrow=c(1,1))

##### Section 1.3 #####

# Summarize the number of tweets over weekends (S&S)and workdays(M-F); visualize it
weekday_tweets = as.data.frame(table(data$weekday))
names(weekday_tweets) = c('day', 'freq')

# Order the days of the week
weekday_tweets$day <- factor(weekday_tweets$day, levels= c("Sun", "Mon", 
                                                           "Tue", "Wed", 
                                                           "Thu", "Fri", 
                                                           "Sat"))

print(weekday_tweets)

qplot(weekday_tweets$day, weekday_tweets$freq, xlab='Day of Week', ylab='Frequency of Tweets',
      main='Frequency of Tweets by Day') + theme(plot.title = element_text(hjust = 0.5))


# Summarize the number of tweets over time of the day (0-24h); visualize it
hour_tweets = as.data.frame(table(data$hour))
names(hour_tweets) <- c('hour', 'freq')
qplot(hour_tweets$hour, hour_tweets$freq, xlab='Hour of Day', ylab='Frequency of Tweets',
      main='Frequency of Tweets by Hour') + theme(plot.title = element_text(hjust = 0.5))


# Summarize the (average) frequency of tweeting (time interval between two adjacent tweets); 
# visualize it

  #order the datetime, ascending
data <- data[ order(data$datetime, decreasing=FALSE),]
time_diff <- as.data.frame(as.numeric(diff.Date(data$datetime)))
names(time_diff) <- c('time_tweets')

summary(time_diff)
par(mfrow=c(1,2))
plot(time_diff$time_tweets, xlab='Index', ylab='Time Difference in Minutes',
     main='Time between tweets') # This plot doesn't work well at all.

boxplot(time_diff$time_tweets, outline=F, ylab='Time Between Tweets',
        main='Summary')


##### Exploratory #####
# Lets build a map showing the spatial extent of the data
m <- leaflet(data=cbind(data$longitude, data$latitude)) %>% 
  addTiles() %>% addCircles() %>% addScaleBar() %>% add
m



# Addendum
# Following code is from suggestion of fellow classmate.
daynum <- as.numeric(strftime(data$datetime, format='%j'))
tmp <- as.data.frame(table(daynum))
plot(tmp)

# Notice all of the missing data! Something is wrong with the data.
