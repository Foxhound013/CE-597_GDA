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
mtext('Top and Bottom 50 Tweet Frequency', outer = TRUE)

# Reset the grahical parameter for future use
par(mfrow=c(1,1))

##### Section 1.3 #####

# Summarize the number of tweets over weekends (S&S)and workdays(M-F); visualize it
weekday_tweets = as.data.frame(table(data$weekday))





# Summarize the number of tweets over time of the day (0-24h); visualize it
hour_tweets = as.data.frame(table(data$hour))
names(hour_tweets) <- c('hour', 'freq')
plot(hour_tweets) #! Pretty it up





# Summarize the (average) frequency of tweeting (time interval between two adjacent tweets); 
# visualize it








