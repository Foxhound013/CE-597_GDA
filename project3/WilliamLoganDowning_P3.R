##### Imports #####
library(tmap)
library(rgdal)
library(sf)
library(osmdata)
library(units)
library(tidyverse)

##### 1.1. Data preparation #####
#   - Read in the shapefile (note it has over 267K polygons) as an sf
#   - Check its CRS and project it to WGS 84 UTM 16N if needed
#   - Calculate its size(area) of each census block (in square kilometers) and add it to the
#     sf as a new column
#   - The new sf file will be your working file

# Load the block data and clip it first.
blocks <- sf::st_read('./data/raw/tabblock2010_18_pophu', layer='tabblock2010_18_pophu')
#blocks_proj <- sf::st_transform(blocks, crs=32616)
# subset blocks to west lafayette.
wl <- osmdata::getbb('west lafayette, usa')
# need to create a polygon from the bbox.
coords <- matrix(c(wl[1,1], wl[2,1], wl[1,1], wl[2,2], wl[1,2], wl[2,2],
                   wl[1,2], wl[2,1], wl[1,1], wl[2,1]), ncol=2,
                 byrow=T)
geoobj <- st_sfc(st_polygon(list(coords)))
wl_poly <- st_sf(geoobj, crs=st_crs(blocks))

wl_blocks <- st_intersection(blocks, wl_poly)
wl_blocks <- sf::st_transform(wl_blocks, crs=32616)
sf::st_crs(wl_blocks)
wl_blocks$blockArea <- sf::st_area(wl_blocks) %>% set_units(km^2) # set the blockArea


tract <- sf::st_read('./data/raw/Tract_2010Census_DP1_IN/utm', layer='tract_in_selected_utm')
sf::st_crs(tract)
# epsg code found at https://spatialreference.org/ref/epsg/wgs-84-utm-zone-16n/
tract_proj <- sf::st_transform(tract, crs=32616) 

tract_proj$blockArea <- sf::st_area(tract_proj) %>% set_units(km^2)
tract_proj <- tract_proj[,c("GEOID10", "NAMELSAD10", "ALAND10", "AWATER10",
                         "INTPTLAT10", "INTPTLON10", "DP0010001", "blockArea")]
class(tract_proj)

#sf::st_write(tract_proj, './data/processed/tract_proj/', 
#             layer='tracts', driver='ESRI Shapefile', 
#             delete_layer=TRUE)
#sf::st_write(data_proj, './data/processed/Tract_2010Census_DP1_IN',
#            layer='Tract_2010Census_DP1_IN_projected', driver='ESRI Shapefile',
#            delete_layer=TRUE)

##### 1.2. Thematic mapping for population density #####
#   - Design a map layout that can produce a map as aesthetic as possible as commercial
#     (ArcGIS) or professional (e.g. QGIS) tools
#   - Some necessary map components should include:
#     o Boarder line (frame); scale bar; title; north pointer;
#     o Legend; map maker (e.g., your name); data source (e.g., Census 2010);
#     o Map projection;
#     o Other proper map components at your choice
#   - Based on the above map design, use 6 R built-in classification methods to produce a
#     population density map. Among these methods you should include Jenks, kmeans,
#     hclust, bclust; keep the number of classes to be the same (usually not more than 6)
#     for all methods.

# Clustering Mapping Methods
tmap_mode('plot')

k <- tm_shape(tract_proj) + tm_polygons('DP0010001', style='kmeans', title='Block Area (Km^2)') +
  tm_compass(position=c('right', 'bottom'), text.size=1) + 
  tm_scale_bar(position=c('left', 'bottom'), text.size=1.5) +
  tm_layout(frame=T, panel.show=T, main.title.position='center', main.title.size=1,
            legend.position=c('right', 'bottom'), legend.text.size=1,
            legend.title.size=1.4, legend.bg.color='white', legend.bg.alpha=.6,
            inner.margins = c(.17, 0, 0, 0), 
            panel.labels='Population Clustered by Kmeans')

j <- tm_shape(tract_proj) + tm_polygons('DP0010001', style='jenks', title='Block Area (Km^2)') + 
  tm_compass(position=c('right', 'bottom'), text.size=1) + 
  tm_scale_bar(position=c('left', 'bottom'), text.size=1.5) +
  tm_layout(frame=T, panel.show=T, main.title.position='center', main.title.size=1,
            legend.position=c('right', 'bottom'), legend.text.size=1,
            legend.title.size=1.4, legend.bg.color='white', legend.bg.alpha=.6,
            inner.margins = c(.17, 0, 0, 0),
            panel.labels='Population Clustered by Jenks')

h <- tm_shape(tract_proj) + tm_polygons('DP0010001', style='hclust', title='Block Area (Km^2)') +
  tm_compass(position=c('right', 'bottom'), text.size=1) + 
  tm_scale_bar(position=c('left', 'bottom'), text.size=1.5) +
  tm_layout(frame=T, panel.show=T, main.title.position='center', main.title.size=1,
            legend.position=c('right', 'bottom'), legend.text.size=1,
            legend.title.size=1.4, legend.bg.color='white', legend.bg.alpha=.6,
            inner.margins = c(.17, 0, 0, 0),
            panel.labels='Population Clustered by Hclust')

b <- tm_shape(tract_proj) + tm_polygons('DP0010001', style='bclust', title='Block Area (Km^2)') + 
  tm_compass(position=c('right', 'bottom'), text.size=1) + 
  tm_scale_bar(position=c('left', 'bottom'), text.size=1.5) +
  tm_layout(frame=T, panel.show=T, main.title.position='center', main.title.size=1,
            legend.position=c('right', 'bottom'), legend.text.size=1,
            legend.title.size=1.4, legend.bg.color='white', legend.bg.alpha=.6,
            inner.margins = c(.17, 0, 0, 0),
            panel.labels='Population Clustered by Bclust')

manual <- tm_shape(tract_proj) + 
  tm_polygons('DP0010001', style='fixed', 
              breaks=c(0,3000,6000,9000,15000,19000),
              title='Block Area (Km^2)') +
  tm_compass(position=c('right', 'bottom'), text.size=1) + 
  tm_scale_bar(position=c('left', 'bottom'), text.size=1.5) +
  tm_layout(frame=T, panel.show=T, main.title.position='center', main.title.size=1,
            legend.position=c('right', 'bottom'), legend.text.size=1,
            legend.title.size=1.4, legend.bg.color='white', legend.bg.alpha=.6,
            inner.margins = c(.17, 0, 0, 0),
            panel.labels='Population Clustered by Hclust')

q <- tm_shape(tract_proj) + tm_polygons('DP0010001', style='quantile', title='Block Area (Km^2)') +
  tm_compass(position=c('right', 'bottom'), text.size=1) + 
  tm_scale_bar(position=c('left', 'bottom'), text.size=1.5) +
  tm_layout(frame=T, panel.show=T, main.title.position='center', main.title.size=1,
            legend.position=c('right', 'bottom'), legend.text.size=1,
            legend.title.size=1.4, legend.bg.color='white', legend.bg.alpha=.6,
            inner.margins = c(.17, 0, 0, 0),
            panel.labels='Population Clustered by Quantiles')

f <- tm_shape(tract_proj) + tm_polygons('DP0010001', style='fisher', title='Block Area (Km^2)') + 
  tm_compass(position=c('right', 'bottom'), text.size=1) + 
  tm_scale_bar(position=c('left', 'bottom'), text.size=1.5) +
   tm_layout(frame=T, panel.show=T,
            main.title.position='center', main.title.size=1,
            legend.position=c('right', 'bottom'), legend.text.size=1,
            legend.title.size=1.4, legend.bg.color='white', legend.bg.alpha=.6,
            inner.margins = c(.17, 0, 0, 0),
            panel.labels='Population Clustered by Fisher')

# png('./figures/tracts_builtInClustering2.png', width=1600, height=800)
# tmap_arrange(k,j,h,b, nrow=1, ncol=4, outer.margins = c(0,0,0,0)) # this seemed to work well
# dev.off()

png('./figures/builtInClustering1.png', width=713, height=545)
tmap_arrange(k,j, nrow=1)
dev.off()
png('./figures/builtInClustering2.png', width=713, height=545)
tmap_arrange(h,b, nrow=1)
dev.off()
png('./figures/builtInClustering3.png', width=713, height=545)
tmap_arrange(q,f, nrow=1)
dev.off()

# Implementing clustering on the west lafayette blocks now, but for block area
# this was for curiousity's sake.
wl_k <- tm_shape(wl_blocks) + tm_polygons('blockArea', style='kmeans', title='Block Area (Km^2)') +
  tm_compass(text.size=1.2) + tm_scale_bar(text.size=1.2, ) + 
  tm_layout(main.title='Population Clustered by Kmeans', main.title.size=1, 
            main.title.position='center', frame=FALSE,
            legend.bg.color='white',  legend.text.size=1.2,
            legend.bg.alpha=.6)

wl_j <- tm_shape(wl_blocks) + tm_polygons('blockArea', style='jenks', title='Block Area (Km^2)') +
  tm_layout(main.title='Population Clustered by Jenks', main.title.size=1, 
            main.title.position='center', frame=FALSE,
            legend.bg.color='white', legend.text.size=1.2,
            legend.bg.alpha=.6)

wl_h <- tm_shape(wl_blocks) + tm_polygons('blockArea', style='hclust', title='Block Area (Km^2)') +
  tm_compass(text.size=1.2) + tm_scale_bar(text.size=1.2, ) + 
  tm_layout(main.title='Population Clustered by Hclust', main.title.size=1, 
            main.title.position='center', frame=FALSE,
            legend.bg.color='white', legend.text.size=1.2,
            legend.bg.alpha=.6)

wl_b <- tm_shape(wl_blocks) + tm_polygons('blockArea', style='bclust', title='Block Area (Km^2)') +
  tm_layout(main.title='Population Clustered by Bclust', main.title.size=1, 
            main.title.position='center', frame=FALSE,
            legend.bg.color='white', legend.text.size=1.2,
            legend.bg.alpha=.6)

png('./figures/blocks1_builtInClustering.png', width=1000, height=800)
tmap_arrange(wl_k, wl_j, nrow=1, ncol=2)
dev.off()

png('./figures/blocks2_builtInClustering.png', width=1000, height=800)
tmap_arrange(wl_h, wl_b, nrow=1, ncol=2)
dev.off()

##### 1.3. Data classification #####
#   - Implement your kmeans clustering method to classify the population density into k
#     (e.g. 5) classes
#   - Discuss your initialization method and termination method for your implementation
#   - Implement the Jenks criterion to terminate your kmeans method
#   - Produce a thematic map(s) based on the results of your kmeans

# From Wikipedia - Searched Jenks Criterion
# The Jenks optimization method, also called the Jenks natural breaks classification method, 
# is a data clustering method designed to determine the best arrangement of values into 
# different classes. This is done by seeking to minimize each class’s average deviation from 
# the class mean, while maximizing each class’s deviation from the means of the other groups. 
# In other words, the method seeks to reduce the variance within classes and maximize the 
# variance between classes.

# A brilliantly helpful resource on the Jenks Algorithm
# https://medium.com/analytics-vidhya/jenks-natural-breaks-best-range-finder-algorithm-8d1907192051


# the data value to be provided should be the item you want classes for
my_kmeans <- function(data, k) {
  # first sort the data and get only the unique values
  # you probably don't want the unique values
  # unique values only will mess with the variance probably.
  sorted_vals <- sort(data, decreasing=F)
  # choose k elements at random, this will only happen once to initialize
  centers <- sample(unique(sorted_vals), k) # use unique to guarantee no overlap
  F_val.old <- -9999 # initializing the old F value to determine when to terminate the loop
  rank.old <- 0 # initializing a container for previous iteration rankings.
  
  # calculate the overall variance, this won't change.
  betweenClass.var = var(sorted_vals)
  
  # In my testing, 100 iterations seemed quick and suffient
  # using a high number of iterations to help ensure stabilized solution
  for (j in seq(1, 400)) {
    
  
    # need the distances for each k class
    # line below will get the distance from centers and each value
    center_dists <- t(sapply(sorted_vals, function(x) abs(centers-x)))
    
    # find all values for each center that are less than evertyhing else
    rank <- matrix(apply(center_dists, 1, which.min),ncol=1)
    rank <- cbind(sorted_vals, rank)
    colnames(rank) <- c('val', 'class')
    
    # now, each row has a class from 1 to k assigned to it.
    # group by the class and calculate variances
    # can replace length with k
    inClass.var = list()
    
    for (i in 1:length(centers)) {
      # grab all class values of a single type
      inClass <- data.frame(rank) %>% dplyr::filter(class == i)
      # find the variance in class
      inClass.var[[i]] = var(inClass$val)
      
      # adjust the class center
      centers[i] = mean(inClass$val)
    }
    
    inClass.var <- sum(unlist(inClass.var))
    F_val <-  betweenClass.var/inClass.var
    
    # Save the largest version of F and the rankings.
    if (F_val > F_val.old) {
      F_val.old <- F_val
      rank.old <- rank
    }
  
  }
  # rank.old should have the maximal F rankings
  # break into a 2 column set with min and max values for each
  classBreaks <- data.frame(rank.old) %>% group_by(class) %>% 
    summarize(min_val=min(val), max_val=max(val))
  return(classBreaks)
}

# my method seems fairly dirty but it does seem to work
# will need to verify it's validity with ArcGIS
my_breaks <- my_kmeans(tract_proj$DP0010001, 5)
my_breaks <- as.vector(list(sort(c(my_breaks$min_val, max(my_breaks$max_val))))[[1]])

# Tm will automatically fill in the gaps between the breaks
# since my algorithm will not ensure that each segment butts up against each other.
png('./figures/myKmeans.png')
tm_shape(tract_proj) + 
  tm_polygons('DP0010001', style='fixed', breaks=my_breaks, title='Population') +
  tm_compass(position=c('right', 'bottom'), text.size=1) + 
  tm_scale_bar(position=c('left', 'bottom'), text.size=.8) +
  tm_layout(frame=T, panel.show=T, panel.labels='Population Clustered by my Kmeans',
            legend.bg.color='white', legend.text.size=.8,
            legend.bg.alpha=.6, inner.margins = c(.17, .2, 0, .2))
dev.off()


##### 1.4. Evaluation and discussion #####
#   - Produce a density map from Jenks (Natural Break) method using QGIS or ArcGIS
#   - Discuss the differences of the break points created from all these classification
# methods (including the one from ArcGIS/QGIS)
#   - Discuss the maps created from different classification methods
#   - Note you may want to plot a histogram of population density for your discussion.


# No code for this section, it will all be in the discussion and in ArcGIS
# Finished ArcGIS portion, the rest of the work will be found in the report.



##### 1.5. Optional (max 2 additional pts; not count towards the page limit) #####
#   - Aggregate the census blocks to census tracts to create a new shp and sf
#   - Produce a population density map for census tracts with your kmeans method. 
#   - Determine how one might work with a much larger dataset (i.e. the original)