##### Imports #####
library(sf)
library(sp)
library(rgdal)
library(tidyverse)
library(tmap)
library(tmaptools)
library(OpenStreetMap) # You'll need java 64 bit to run this.

##### Section 1.1 #####
# 1.1. Data preparation - sp and sf object creation
# • Read in the given .xls file
# • Create an sp object and an sf object to store the input data
# • Create a shapefile from the input data
# • Assign the coord. system as WGS 84 (no map projection) to all associates data files
# • Create and assign the bounding box to all data files

fpath <- './data/raw/pu2014.csv'
data <- read.csv(fpath, header=T, stringsAsFactors=F)
data$datetime <- as.POSIXct(data$epoch, origin="1970-01-01")

# Create your sp object.
coords = cbind(data$longitude, data$latitude)
latlon = CRS("+proj=longlat +datum=WGS84")
sp = SpatialPointsDataFrame(coords, data, proj4string=latlon)

# Create your sf object.
sf = st_as_sf(data, coords=c('longitude', 'latitude'), crs=4326) # cbind coords doesn't work here.
# Note: You could have passed it the sp object as well.

# Set the bounding box for these datasets to be slightly bigger than default
sf_bbox = st_bbox(sf)
sp_bbox = sp::bbox(sp)

sf_bbox; sp_bbox;

new_sf_bb = c('xmin'=-87, 'ymin'=40.4, 'xmax'=-86.9, 'ymax'=40.49)
attr(new_sf_bb, "class") = "bbox"
attr(sf, "bbox") = new_sf_bb

new_sp_bb = matrix(new_sf_bb, nrow=2, ncol=2)
attr(sp, "bbox") = new_sp_bb

sf_bbox = st_bbox(sf)
sp_bbox = sp::bbox(sp)

sf_bbox; sp_bbox;


##### Section 1.2 #####
# 1.2. Spatial object I/O and conversion
# • Write the shapefile to your working folder
# • Read the shapefile back from your working folder
# • Convert the sf and sp to a shapefile & one other geo-data format & read them in
# • Create an sf and sp from these read-in’s if needed

# Create a shapefile from the input data. First verify files don't exist.
fpath_sp = './data/processed/sp/sp_out.shp'
if (file.exists(fpath_sp)) {
  file.remove(fpath_sp)
  gsub('.shp', '.dbf', fpath_sp) %>% file.remove()
  gsub('.shp', '.shx', fpath_sp) %>% file.remove()
  gsub('.shp', '.prj', fpath_sp) %>% file.remove()
}

fpath_sf = './data/processed/sf/sf_out.shp'
if (file.exists(fpath_sf)) {
  file.remove(fpath_sf)
  gsub('.shp', '.dbf', fpath_sf) %>% file.remove()
  gsub('.shp', '.shx', fpath_sf) %>% file.remove()
  gsub('.shp', '.prj', fpath_sf) %>% file.remove()
}

## Do so from an sp object
rgdal::writeOGR(sp, dsn='./data/processed/sp', layer='sp_out', driver='ESRI Shapefile')

## Do so from an sf object
sf::st_write(sf, dsn='./data/processed/sf', layer='sf_out', driver='ESRI Shapefile')
# Need clarification on what is wanted here.


# Read your data back from the files just created.
sf_tweets <- st_read(dsn='./data/processed/sf', layer='sf_out')
sp_tweets <- readOGR(dsn='./data/processed/sp', layer='sp_out')

# Let's convert my sf and sp objects to geojson
writeOGR(sp_tweets, "./data/processed/sp/sp_geojson", layer="sp_tweets", driver="GeoJSON")
st_write(sf_tweets, "./data/processed/sf/sf_geojson", layer="sf_tweets", driver="GeoJSON")

# read them in, don't save to a var just to save space
st_read(dsn='./data/processed/sf/sf_geojson')
readOGR(dsn='./data/processed/sp/sp_geojson')

##### Section 1.3 #####
# 1.3. Spatial object’s attribute manipulation (use the sf file from 1.1)
# • Convert the epoch (Unix time) to day-month-year, day of the week, and local time
# • Add these derived info to the sf
# • Subset the columns/info you will use and keep the rest as a new sf file
# • Stay with this sf file onwards

# I had completed this earlier but repeated here for sake of the question.
sf_tweets$datetime <- as.POSIXct(sf_tweets$epoch, origin="1970-01-01", tz="EST")
sf_tweets$date <- format(sf_tweets$datetime, format='%d-%m-%Y')
sf_tweets$dayOfWeek <- format(sf_tweets$datetime, format='%w')
sf_tweets$localTime <- format(sf_tweets$datetime, format='%X')

# subset the data, it looks like a few of the columns got messed up when making the sh file
sf_sub = sf_tweets[,c('user_d', 'usr_sc_', 'geometry', 'epoch', 'datetime', 'date', 'dayOfWeek', 'localTime')]


##### Section 1.4 #####
# 1.4. Mapping the overall tweets distribution
# • Show/map the overall tweet locations atop OSM and/or GoogleMap (ttm function of
#                                                                   tmap)
# • Color-code the maps based on month, day and/or hour
# • Color-code the map based on selected individual users
# • Prepare clean versions of above maps for discuss/presentation

tmap_mode('view')

tm_shape(sf_sub) + tm_dots(col='blue') +
  tm_layout(title='Spatial Distribution of Tweets - West Lafayette')

# color map based on month
# add month to the sf dataframe
sf_sub$month = strftime(sf_sub$datetime, format='%B')
sf_sub$month = factor(sf_sub$month, levels=c('January', 'February', 'March', 'April',
                      'May', 'June', 'July', 'August', 'September', 'October',
                      'November', 'December'))

tm_shape(sf_sub) + tm_dots('month', title='Month') + 
  tm_layout(title='Spatial Distribution of Tweets by Month - West Lafayette')



# Day of Week
sf_sub$dayOfWeek = strftime(sf_sub$datetime, format='%A') %>%
  factor(levels=c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday',
                   'Friday', 'Saturday'))

tm_shape(sf_sub) + tm_dots('dayOfWeek', title='Day of Week') +
  tm_layout(title='Spatial Distribution of Tweets by Day of Week - West Lafayette')


# Hour
sf_sub$hour = strftime(sf_sub$datetime, format='%H') %>%
  factor(levels=c('00', '01', '02', '03', '04', '05', '06', '07', '08',
                  '09', '10', '11', '12', '13', '14', '15', '16', '17',
                  '18', '19', '20', '21', '22', '23'))
# break the hours up into manageable chunks
sf_sub$hours_cut = cut(as.numeric(sf_sub$hour), breaks = seq(0,24,6), 
                       labels=c('Midnight-6AM', '6AM-12PM', '12PM-6PM', '6PM-Midnight'))

tm_shape(sf_sub) + tm_dots('hours_cut', title='Hour of the Day')+
  tm_layout(title='Spatial Distribution of Tweets by Hour of the Day - West Lafayette')


# color code map for top 5 tweeters?
tweet_total <- data.frame(table(sf_sub$user_d))
names(tweet_total) <- c('user_id', 'freq')
tweet_total <- tweet_total[order(tweet_total$freq, decreasing=T),]
top3 <- as.numeric(as.character(tweet_total[seq(1,3,1), 'user_id']))

top3 <- sapply(top3, function(x) which(sf_sub$user_d == x))
top3 <- sf_sub[unlist(top3),]
top3$user_d <- as.factor(top3$user_d)
top3$usr_sc_ <- as.factor(as.character(top3$usr_sc_))


tm_shape(top3) + tm_dots('usr_sc_', title='Top 3 Tweeters by Screen Name') +
  tm_layout(title='Top 3 Tweeters in Dataset')


##### Section 1.5 #####
# 1.5. Mapping your story of interest (AOI, individuals)
# • Find out/show the hot spots over time in WL and/or on campus
# • Find out/show the spatial-temporal distribution/trajectory of top or selected 3
# individuals
# • Make sure your maps are clean/clear

# This is how you can get the open streeet map backgournd m = read_osm(sf_sub)
# m=read_osm(st_bbox(sf_tweets))
# tmap_mode('plot')
# tm_shape(m) + tm_rgb() + tm_shape(top3) + 
#   tm_dots('usr_sc_', palette=c('cyan', 'red', 'blue'), size=.1, clustering=T)
# Could I do this map but over time slices
tm_shape(top3) + 
  tm_dots('usr_sc_', palette=c('cyan', 'red', 'blue'), size=.1, clustering=T)
  
# for the spatial-temporal distro, how would I represent these 3 individuals movements?
# maybe you can calculate distance covered at each time step by each user
# do this as a line graph for the report but
# include a link to animation of tweets every hour. Minute scale would be better but unrealistic.

# Kernel density estimation is how you'll do this one.
# Maybe try using dplyr to group by in your sf object

# try plotting a single time slice, maybe there's a way to average all positions
# and set size of circle based on time slice
# maybe make animation?
tm_shape()







##### Section 1.6 #####
# 1.6. Mapping/estimate mobility (max 2 additional points)
# • Summarize and visualize the daily moving distances of selected (or all) individuals
# • Creative visualization 

# estimate euclidean distance traveled by user id?

# create a map animation that will allow roll through the time window plotting user locations
# through time.









