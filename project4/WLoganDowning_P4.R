library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(rgeos)


# 1. Given a building shapefile of Purdue campus, make an R program to calculate 
#   (you need to implement the formulas, 4 pts)
#
#   1) the length (perimeter), area and centroid for each building
#   2) add the above results as 4 columns associate with each building
#   3) do the same with the built-in functions in R by adding another 4 columns
#   4) evaluate/summarize the differences (histogram, mean, standard deviation etc)

raw_data <- st_read('./data/raw/buildings_shpfile', layer='buildings')

# drop any invalid polygons
data <- raw_data[which(st_is_valid(raw_data)),]
# drop empty polygons
data <- data[which(!st_is_empty(data)),]
# drop any buildings that have been razed
data <- data[is.na(data$YEAR_RAZED),]
# drop entities that are NA, these tend to be overlapping shapes
# or what seems to be errors in the data
data <- data[!is.na(data$Entity),]

# drop out the non-useful columns
data <- data[,c('Entity', 'Level', 'Layer', 'Elevation', 'BLDG_ABBR', 'BUILDING_N',
                'OWNER', 'GIS_AREA', 'PERIMETER', 'MAPID', 'Shape_Leng', 'Shape_Area',
                'YEAR_BUILT', 'geometry')]

# check the crs
st_crs(data)
# transverse mercator with NAD83 datum

# quickly get a view of the data
tmap_mode('view')
tm_shape(raw_data) + tm_polygons()

tm_shape(data) + tm_polygons()

# Perimeter Calculation
# Formula: Sum( sqrt( (yi+1 - yi)^2 + (xi+1 - xi)^2) )

# accepts a dataframe but expects a geometry column
ld_perimeter <- function(data) {
  perimeters <- list()
  
  for (i in data$geometry) {
    # check polygon type
    if (st_is(i,'POLYGON')) {
      y <- diff(i[[1]][,2])
      x <- diff(i[[1]][,1])
      
      perimeters <- append(perimeters, sum(sqrt(y^2+x^2)))
    } else if (st_is(i,'MULTIPOLYGON')){
      multi_perim <- 0
      for (j in 1:length(i)) {
        y <- diff(i[[j]][[1]][,2])
        x <- diff(i[[j]][[1]][,1])
        
        multi_perim <- multi_perim + sum(sqrt(y^2+x^2))
      }
      #sum the multipolygon
      perimeters <- append(perimeters, multi_perim)
    }
  }
  return(unlist(perimeters))
}

data$ld_perim <- ld_perimeter(data)
summary(data$ld_perim-data$PERIMETER)
# my method doesn't seem to be perfect. There are some strange edge cases but the 1st and 3rd
# quartiles are fine. (compared to the inbuilt perimeters)


# Area Calculation
# Formula: 1/2 * sum( (xi+1-xi)*(yi+yi+1) )

ld_area <- function(data) {
  area <- list()
  
  for (i in data$geometry) {
    # check polygon type
    if (st_is(i,'POLYGON')) {
      y <- head(i[[1]][,2], -1) + tail(i[[1]][,2], -1)
      x <- diff(i[[1]][,1])
      
      area <- append(area, .5 * sum(y*x))
    } else if (st_is(i,'MULTIPOLYGON')) {
      multi_area <- 0
      for (j in 1:length(i)) {
        y <- head(i[[j]][[1]][,2], -1) + tail(i[[j]][[1]][,2], -1)
        x <- diff(i[[j]][[1]][,1])
        multi_area <- multi_area + .5 * sum(y*x)
      }
      
      area <- append(area, multi_area)
    }
  }
  return(unlist(area))
}

data$ld_area <- ld_area(data)
summary(data$ld_area - data$Shape_Area)
# everything appears to be good, there are some discrepancies but overall it's dead on.

# Centroid Calculation
# Formulas:
#   x_bar = 1/(6A) * Sum( (yi+1 - yi) * (xi^2 + xi*xi+1 + xi+1^2) )
#   y_bar = 1/(6A) * Sum( (xi - xi+1) * (yi^2 + yi*yi+1 + yi+1^2) )

# this function will return two columns of x and y centroid values
# area should be calculated prior to running this function
# requires a geometry column and area column. Area must be defined when called
ld_centroid <- function(data, area) {
  data <- data$geometry
  
  # Initialize a container to store centroid data
  centroids <- data.frame(x_bar = rep(NA, length(data)),
                          y_bar = rep(NA, length(data))) 
  
  for (i in 1:length(data)) {
    # verify polygon type
    if (st_is(data[[i]], 'POLYGON')) {
      #x_bar
      y <- diff(data[[i]][[1]][,2])
      x <- head(data[[i]][[1]][,1], -1)^2 + 
        head(data[[i]][[1]][,1], -1)*tail(data[[i]][[1]][,1], -1) +
        tail(data[[i]][[1]][,1], -1)^2
      
      x_bar <- (1/(6*area[i]) * sum(y*x))*-1
      
      #y_bar
      x <- head(data[[i]][[1]][,1], -1) - tail(data[[i]][[1]][,1], -1)
      y <- head(data[[i]][[1]][,2], -1)^2 +
        head(data[[i]][[1]][,2], -1)*tail(data[[i]][[1]][,2], -1) +
        tail(data[[i]][[1]][,2], -1)^2
      
      y_bar <- (1/(6*area[i]) * sum(y*x))*-1
      
      centroids[i, ] <- rbind(x_bar, y_bar)
    } else if (st_is(data[[i]], 'MULTIPOLYGON')) {
      x_list <- rep(list(NULL),length(data[[i]]))
      y_list <- rep(list(NULL),length(data[[i]]))
      next()
      #   for (j in 1:length(data[[i]])) {
      #     #x_bar
      #     y <- diff(data[[i]][[j]][[1]][,2])
      #     x <- head(data[[i]][[j]][[1]][,1], -1)^2 + 
      #       head(data[[i]][[j]][[1]][,1], -1)*tail(data[[i]][[j]][[1]][,1], -1) +
      #       tail(data[[i]][[j]][[1]][,1], -1)^2
      #     
      #     x_bar <- (1/(6*area[i]) * sum(y*x))*-1# this bit isn't technically right.
      #     # to fix it, it would require tweaking the area function to return the individual
      #     # areas for each polygon
      # 
      #     #y_bar
      #     x <- head(data[[i]][[j]][[1]][,1], -1) - tail(data[[i]][[j]][[1]][,1], -1)
      #     y <- head(data[[i]][[j]][[1]][,2], -1)^2 +
      #       head(data[[i]][[j]][[1]][,2], -1)*tail(data[[i]][[j]][[1]][,2], -1) +
      #       tail(data[[i]][[j]][[1]][,2], -1)^2
      #     
      #     y_bar <- (1/(6*area[i]) * sum(y*x))*-1 # this bit isn't technically right.
      #     # to fix it, it would require tweaking the area function to return the individual
      #     # areas for each polygon
      #     
      #     # now, you need to store each multipoly's center to a list, for future storage in 
      #     # the data frame.
      #     x_list[[j]] <- append(x_list[[j]], x_bar)
      #     y_list[[j]] <- append(y_list[[j]], y_bar)
      #   }
      #   x_list <- sapply(x_list, cbind)
      #   y_list <- sapply(y_list, cbind)
      #   
      #   print(centroids[890:898,])
      #   centroids[[1]][i][[1]] <- x_list
      #   centroids[[2]][i][[1]] <- y_list
    }
  }
  return(centroids)
}

centroid_data <- ld_centroid(data, data$ld_area)
data$ld_centroid_x <- centroid_data$x_bar
data$ld_centroid_y <- centroid_data$y_bar

# calculate perimeter with inbuilt function
data$builtIn_perimeter <- st_length(st_cast(data, 'MULTILINESTRING'))
data$builtIn_area <- st_area(data)
pts <- data.frame(st_geometry(data) %>% st_centroid %>% st_coordinates)
data$builtIn_centroidX <- pts$X
data$builtIn_centroidY <- pts$Y

# round the answers
data <- data %>% mutate_at(vars('ld_perim', 'ld_area', 'builtIn_perimeter', 'builtIn_area'), 
                           .funs=list(function(x) round(x,3)))
data <- data %>% mutate_at(vars('ld_centroid_x', 'ld_centroid_y', 'builtIn_centroidX', 'builtIn_centroidY'), 
                           .funs=list(function(x) round(x,0)))

# comparing my calculations vs R's built in calculations
summary(data$ld_perim - as.numeric(data$builtIn_perimeter))
summary(data$ld_area - as.numeric(data$builtIn_area))
summary(data$ld_centroid_x - data$builtIn_centroidX)
summary(data$ld_centroid_y - data$builtIn_centroidY)


# 2. Given the WL tweets data of year 2014, make an R program to (use built-in functions, 6pts)
#
#   1) Find the closest tweets for each building, add the sum (the number of tweets) to that
#     building
#   2) For the tweets data frame( layer), associate the closest building ID to each tweet
#   3) Find the "busiest" buildings in WL (campus)
#   4) Evaluate/discuss the 'busiest' buildings in terms of time period (e.g. day or night etc.)
#   5) Show your results with appropriate maps
#     - Note - if the distance of a tweet to the closest building is too large, you may want
#       to discard that tweet from this building
#     - For spatial operation command, check
#       https://r-spatial.github.io/sf/reference/index.html; there is an "Articles" pull down
#       button. Also look at https://github.com/rstudio/cheatsheets/blob/master/sf.pdf
#     - Option - you may download OSM buildings of WL (see my early notes) and use these
#       verse the one I provided (which has only Purdue buildings)

# st nearest feature should be helfpul

# load the tweet data
tweets <- read.csv('./data/raw/pu2014.csv', header=T, stringsAsFactors=F)
tweets <- tweets[,c('epoch', 'longitude', 'latitude', 'user_id', 'source', 'hashtags')]

# tweet_counts <- data.frame(table(tweets$user_id))
# colnames(tweet_counts) <- c('user_id', 'frequency')

sf_tweets <- st_as_sf(tweets, coords=c('longitude', 'latitude'), crs=4326)
sf_tweets <- st_transform(sf_tweets, crs=st_crs(data))

# quick look at all of the data together
#tm_shape(data) + tm_polygons() + tm_shape(sf_tweets) + tm_dots(col='red')

# 1) Find the closest tweets for each building, add the sum (the number of tweets) to that building
# this works but it is super slow
#tmp <- st_join(sf_tweets, data[,'BUILDING_N'], join=st_is_within_distance, dist=20)

# buffer the buildings, then you can find tweets that are within those new polygons
# some tweets will likely be assigned multiple times but this is an uncertainty
# that I'm willing to accept
bldBuff <- st_buffer(data, 30)
BldTweets <- st_join(sf_tweets, bldBuff, join=st_within)
BldTweets <- BldTweets[!is.na(BldTweets$BUILDING_N),] # drop out values not in buildings
BldTweetsCount <- data.frame(table(BldTweets$BUILDING_N))
colnames(BldTweetsCount) <- c('BUILDING_N', 'tweetFrequency')

BldTweets <- merge(data, BldTweetsCount, by='BUILDING_N')

# Visualize the tweets by building
tm_shape(BldTweets) + tm_polygons() +
  tm_shape(BldTweets) + tm_dots(title='Tweet Count', col='tweetFrequency') + 
  tm_scale_bar()
  

# 2: For the tweets data frame( layer), associate the closest building ID to each tweet
bldAssociation <- st_nearest_feature(sf_tweets, data)
sf_tweets$buildingRow <- bldAssociation
sf_tweets$buildingName <- data$BUILDING_N[bldAssociation]

# Visualization
# pick a few buildings close to one another
tm_shape(data) + tm_polygons()
# Purdue Memorial Union, Stewart Center, Richard Benbridge Wetherill Lab Of Chemistry,

# The rec Center will be used for the visualization example
bldSubset <- data[which(data$BUILDING_N=='Purdue Memorial Union' | 
                               data$BUILDING_N=='Stewart Center' | 
                               data$BUILDING_N=='Richard Benbridge Wetherill Lab Of Chemistry'),]

bldTweetSubset <- sf_tweets[which(sf_tweets$buildingName=='Purdue Memorial Union' | 
                                    sf_tweets$buildingName=='Stewart Center' | 
                                    sf_tweets$buildingName=='Richard Benbridge Wetherill Lab Of Chemistry'),]
bldTweetSubset$buildingName <- droplevels(bldTweetSubset$buildingName )

tm_shape(bldSubset) + tm_polygons() + 
  tm_shape(bldTweetSubset) + tm_dots(title='Building', size=.01, col='buildingName') +
  tm_scale_bar()


# 3: Find the "busiest" buildings in WL (campus)
topBlds <- BldTweets[order(BldTweets$tweetFrequency, decreasing=T),]
topBlds <- head(topBlds, 6)
topBlds$BUILDING_N <- droplevels(topBlds$BUILDING_N)
# The rec center is duplicated due to construction, drop it
topBlds <- topBlds[which(topBlds$Layer != 'BLDG Building Under Construction'),]
# reset row index
rownames(topBlds) <- NULL

topBldTweets <- sf_tweets[sf_tweets$buildingName %in% topBlds$BUILDING_N,]
topBldTweets$buildingName <- droplevels(topBldTweets$buildingName)

tm_shape(topBlds) + tm_polygons(col='blue',border.col='black') + 
  tm_shape(topBldTweets) + tm_dots(size=0.006, col='buildingName') +
  tm_view(view.legend.position=c('left','top')) + tm_scale_bar()

# 4: Evaluate/discuss the 'busiest' buildings in terms of time period (e.g. day or night etc.)

# should be able to leverage the epoch data from sf_tweets


# 5: Show your results with appropriate maps
