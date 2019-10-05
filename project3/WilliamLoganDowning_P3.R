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

# get the population density
tract_proj$density <- tract_proj$DP0010001/tract_proj$blockArea

# write to file
sf::st_write(tract_proj, dsn='./data/processed', layer='projectedTracts.shp',
             driver='ESRI Shapefile', delete_layer=TRUE)

# Clustering Mapping Methods
#tmap_mode('plot')

k <- tm_shape(tract_proj) + 
  tm_polygons('density', style='kmeans', title='Population per Km^2', border.alpha=.2) +
  tm_compass(position=c('right', 'bottom'), text.size=1) + 
  tm_scale_bar(position=c('left', 'bottom'), text.size=1.5) +
  tm_layout(frame=T, panel.show=T, main.title.position='center', main.title.size=1,
            legend.position=c('right', 'bottom'), legend.text.size=1,
            legend.title.size=1.4, legend.bg.color='white', legend.bg.alpha=.6,
            inner.margins = c(.17, 0, 0, 0), 
            panel.labels='Population Density Clustered by Kmeans')

j <- tm_shape(tract_proj) + 
  tm_polygons('density', style='jenks', title='Population per Km^2', border.alpha=.2) + 
  tm_compass(position=c('right', 'bottom'), text.size=1) + 
  tm_scale_bar(position=c('left', 'bottom'), text.size=1.5) +
  tm_layout(frame=T, panel.show=T, main.title.position='center', main.title.size=1,
            legend.position=c('right', 'bottom'), legend.text.size=1,
            legend.title.size=1.4, legend.bg.color='white', legend.bg.alpha=.6,
            inner.margins = c(.17, 0, 0, 0),
            panel.labels='Population Density Clustered by Jenks')

h <- tm_shape(tract_proj) + 
  tm_polygons('density', style='hclust', title='Population per Km^2', border.alpha=.2) +
  tm_compass(position=c('right', 'bottom'), text.size=1) + 
  tm_scale_bar(position=c('left', 'bottom'), text.size=1.5) +
  tm_layout(frame=T, panel.show=T, main.title.position='center', main.title.size=1,
            legend.position=c('right', 'bottom'), legend.text.size=1,
            legend.title.size=1.4, legend.bg.color='white', legend.bg.alpha=.6,
            inner.margins = c(.17, 0, 0, 0),
            panel.labels='Population Density Clustered by Hclust')

b <- tm_shape(tract_proj) + 
  tm_polygons('density', style='bclust', title='Population per Km^2', border.alpha=.2) + 
  tm_compass(position=c('right', 'bottom'), text.size=1) + 
  tm_scale_bar(position=c('left', 'bottom'), text.size=1.5) +
  tm_layout(frame=T, panel.show=T, main.title.position='center', main.title.size=1,
            legend.position=c('right', 'bottom'), legend.text.size=1,
            legend.title.size=1.4, legend.bg.color='white', legend.bg.alpha=.6,
            inner.margins = c(.17, 0, 0, 0),
            panel.labels='Population Density Clustered by Bclust')


q <- tm_shape(tract_proj) + 
  tm_polygons('density', style='quantile', title='Population per Km^2', border.alpha=.2) +
  tm_compass(position=c('right', 'bottom'), text.size=1) + 
  tm_scale_bar(position=c('left', 'bottom'), text.size=1.5) +
  tm_layout(frame=T, panel.show=T, main.title.position='center', main.title.size=1,
            legend.position=c('right', 'bottom'), legend.text.size=1,
            legend.title.size=1.4, legend.bg.color='white', legend.bg.alpha=.6,
            inner.margins = c(.17, 0, 0, 0),
            panel.labels='Population Density Clustered by Quantiles')

f <- tm_shape(tract_proj) + 
  tm_polygons('density', style='fisher', title='Population per Km^2', border.alpha=.2) + 
  tm_compass(position=c('right', 'bottom'), text.size=1) + 
  tm_scale_bar(position=c('left', 'bottom'), text.size=1.5) +
   tm_layout(frame=T, panel.show=T,
            main.title.position='center', main.title.size=1,
            legend.position=c('right', 'bottom'), legend.text.size=1,
            legend.title.size=1.4, legend.bg.color='white', legend.bg.alpha=.6,
            inner.margins = c(.17, 0, 0, 0),
            panel.labels='Population Density Clustered by Fisher')


png('./figures/builtInClustering1.png', width=713, height=545)
tmap_arrange(k,j, nrow=1)
dev.off()
png('./figures/builtInClustering2.png', width=713, height=545)
tmap_arrange(h,b, nrow=1)
dev.off()
png('./figures/builtInClustering3.png', width=713, height=545)
tmap_arrange(q,f, nrow=1)
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
# different classes. This is done by seeking to minimize each class average deviation from 
# the class mean, while maximizing each class deviation from the means of the other groups. 
# In other words, the method seeks to reduce the variance within classes and maximize the 
# variance between classes.

# A brilliantly helpful resource on the Jenks Algorithm
# https://medium.com/analytics-vidhya/jenks-natural-breaks-best-range-finder-algorithm-8d1907192051


# the data value to be provided should be the item you want classes for
my_kmeans <- function(data, k) {
  data <- as.numeric(data) # sanitize the data in the event that it has units
  # first sort the data and get only the unique values
  # you probably don't want the unique values
  # unique values only will mess with the variance probably.
  sorted_vals <- sort(data, decreasing=F)
  # choose k elements at random, this will only happen once to initialize
  centers <- sample(unique(sorted_vals), k) # use unique to guarantee no overlap
  F_val.old <- -9999 # initializing the old F value to determine when to terminate the loop
  F.all <- vector() # put in place to print an iterations graphic
  rank.old <- 0 # initializing a container for previous iteration rankings.
  
  # In my testing, 100 iterations seemed quick and suffient
  for (j in seq(1, 100)) {
    
  
    # need the distances for each k class
    # line below will get the distance from centers and each value
    center_dists <- t(sapply(sorted_vals, function(x) abs(centers-x)))
    
    # find all values for each center that are less than evertyhing else
    rank <- matrix(apply(center_dists, 1, which.min),ncol=1)
    rank <- cbind(sorted_vals, rank)
    colnames(rank) <- c('val', 'class')
    
    # now, each row has a class from 1 to k assigned to it.
    # group by the class and calculate variances
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
    betweenClass.var = var(centers)
    F_val <-  betweenClass.var/inClass.var
    
    F.all <- append(F.all, F_val)
    
    # Save the largest version of F and the rankings.
    # i.e. the jenk's criterion
    if (F_val > F_val.old) {
      iteration_max <- j
      F_val.old <- F_val
      rank.old <- rank
    }
  
  }
  # rank.old should have the maximal F rankings
  # break into a 2 column set with min and max values for each
  classBreaks <- data.frame(rank.old) %>% group_by(class) %>% 
    summarize(min_val=min(val), max_val=max(val))
  
  classBreaks <- as.vector(list(sort(c(classBreaks$min_val, max(classBreaks$max_val))))[[1]])
  
  png('./figures/F_graph.png')
  plot(F.all, xlab='Iterations', ylab='F Value', main='Variation of F for each Iteration',
       pch=16, col='deepskyblue3')
  points(iteration_max, F_val.old, pch=16, col='red')
  dev.off()
  
  return(classBreaks)
}

# my method seems fairly dirty but it does seem to work
# will need to verify it's validity with ArcGIS
my_breaks <- my_kmeans(tract_proj$density, 5)


# Tm will automatically fill in the gaps between the breaks
# since my algorithm will not ensure that each segment butts up against each other.
png('./figures/myKmeans.png')
tm_shape(tract_proj) + 
  tm_polygons('density', style='fixed', breaks=my_breaks, 
              title='Population per Km^2', border.alpha=.2 ) +
  tm_compass(position=c('right', 'bottom'), text.size=1) + 
  tm_scale_bar(position=c('left', 'bottom'), text.size=.8) +
  tm_layout(frame=T, panel.show=T, panel.labels='Population Density Clustered by my Kmeans',
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