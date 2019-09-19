##### Imports #####
library(sf)
library(sp)
library(rgdal)
library(tidyverse)
library(tmap)
library(tmaptools)
library(OpenStreetMap) # You'll need java 64 bit to run this.
library(lattice)

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
#recover the lat long data for future use.
tmp_c <- data.frame(st_coordinates(sf_sub$geometry))
sf_sub$lat <- tmp_c$Y
sf_sub$lon <- tmp_c$X


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

# How about making the cluster map from tmap view into a facet grid with 4 maps.
# group by hour and cluster on tmap method

tm_shape(sf_sub) + tm_dots('hours_cut',clustering=T, style='kmeans', legend.show=F, ) + 
  tm_shape(sf_sub) + tm_dots('hours_cut', size=0.02, title='Hour Ranges', alpha=.5) +
  tm_layout(title='Clustered Tweets across Time Ranges')


#for second part, use group by to group by ids in top3, then calculate distance between
#time steps
# This is a pretty good and workable solution. It gives distance traveled in meters
# and shows how far out these users moved over time.
travel_sf <- top3 %>% group_by(usr_sc_) %>% 
  mutate(lead=geometry[row_number()+1],
         traveled=st_distance(geometry, lead, by_element=T, default=0))

# Log transform not used due to 0s present in the data
lattice::xyplot(traveled~datetime |factor(usr_sc_), data=travel_sf, xlab='Date', 
                ylab='Distance Traveled (m)', pch=16, cex='0.8', jitter=T, 
                col=alpha('deepskyblue3',0.4),
                main="Spatial-Temporal Distribution for the Top 3 Tweeters",
                layout=c(1,3))

# This is an auxillary plot that outputs an 8 page pdf with categories group by user and hour
trellis.device(device="pdf", filename="SpaceTime_Top3ByHour.pdf")
pdf("SpaceTime_Top3ByHour.pdf")
lattice::xyplot(traveled~datetime |factor(usr_sc_)*factor(hour), data=travel_sf, xlab='Date', 
                ylab='Distance Traveled (m)', pch=16, cex='0.8', jitter=T, 
                col=alpha('deepskyblue3',0.4),
                main="Spatial-Temporal Distribution for the Top 3 Tweeters",
                layout=c(3,3,8))
dev.off()

# This is how you can get the open streeet map backgournd m = read_osm(sf_sub)
# m=read_osm(st_bbox(sf_tweets))
# tmap_mode('plot')
# tm_shape(m) + tm_rgb() + tm_shape(top3) + 
#   tm_dots('usr_sc_', palette=c('cyan', 'red', 'blue'), size=.1, clustering=T)

##### Section 1.6 #####
# 1.6. Mapping/estimate mobility (max 2 additional points)
# • Summarize and visualize the daily moving distances of selected (or all) individuals
# • Creative visualization 


# Want to count tweets for each hour and calculate avg. location of tweets by hour.
avg_lat <- sf_sub %>% group_by(hour=hour) %>% 
  summarize(avg_lat=mean(lat))
avg_lon <- sf_sub %>% group_by(hour=hour) %>%
  summarize(avg_lon=mean(lon))

avg_location <- data.frame(avg_lat$hour, avg_lat$avg_lat, avg_lon$avg_lon)
names(avg_location) <- c('hour', 'avg_lat', 'avg_lon')

avg_location <- st_as_sf(avg_location, coords=c('avg_lon', 'avg_lat'), crs=4326)

# Now if you figure out the counts for each hour, you can scale your circles by that count.
countsByHour <- sf_sub %>% group_by(hour=hour) %>%
  count
avg_location$count <- countsByHour$n

# bin the hours into 4 groups for convenience.
avg_location$hours_cut = cut(as.numeric(avg_location$hour), breaks = seq(0,24,6), 
                     labels=c('Midnight-6AM', '6AM-12PM', '12PM-6PM', '6PM-Midnight'))
# scale the counts down to between 0 and 1
avg_location$scale <- ((avg_location$count - min(avg_location$count))/ (max(avg_location$count) - min(avg_location$count)))

tmap_mode('view')
tm_shape(avg_location) + tm_dots('hours_cut', size='scale') +
  tm_layout(title='Average Tweet Locations by Hour Range')


