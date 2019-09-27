##### Imports #####
library(tmap)
library(rgdal)
library(sf)
library(units)
library(tidyverse)

##### 1.1. Data preparation #####
#   - Read in the shapefile (note it has over 267K polygons) as an sf
#   - Check its CRS and project it to WGS 84 UTM 16N if needed
#   - Calculate its size(area) of each census block (in square kilometers) and add it to the
#     sf as a new column
#   - The new sf file will be your working file

#blocks <- sf::st_read('./data/raw/tabblock2010_18_pophu', layer='tabblock2010_18_pophu')
data <- sf::st_read('./data/raw/Tract_2010Census_DP1_IN/utm', layer='tract_in_selected_utm')
sf::st_crs(data)
# epsg code found at https://spatialreference.org/ref/epsg/wgs-84-utm-zone-16n/
data_proj <- sf::st_transform(data, crs=32616) 

data_proj$blockArea <- sf::st_area(data_proj) %>% set_units(km^2)

#sf::st_write(data_proj, './data/processed/tabblock2010_18_pophu_projected', 
#             layer='tabblock2010_18_pophu_projected', driver='ESRI Shapefile', 
#             delete_layer=TRUE)
sf::st_write(data_proj, './data/processed/Tract_2010Census_DP1_IN',
            layer='Tract_2010Census_DP1_IN_projected', driver='ESRI Shapefile',
            delete_layer=TRUE)

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

# First up you're going to need to get the population data from the blocks and add it to
# the tract data.

data_proj <- data_proj[,c("GEOID10", "NAMELSAD10", "ALAND10", "AWATER10", 
                          "INTPTLAT10", "INTPTLON10", "DP0010001", "blockArea")]

# Clustering Mapping Methods
# Finish tweaking the map layouts

tmap_mode('plot')
k <- tm_shape(data_proj) + tm_polygons('blockArea', style='kmeans') +
  tm_compass() + tm_scale() + tm_layout(frame=F)
j <- tm_shape(data_proj) + tm_polygons('blockArea', style='jenks') + tm_layout(frame=F)
h <- tm_shape(data_proj) + tm_polygons('blockArea', style='hclust') + tm_layout(frame=F)
b <- tm_shape(data_proj) + tm_polygons('blockArea', style='bclust') + tm_layout(frame=F)
pdf('test.pdf')
tmap_arrange(k,j,h,b, nrow=1, ncol=4) # this seemed to work well
dev.off()


##### 1.3. Data classification #####
#   - Implement your kmeans clustering method to classify the population density into k
#     (e.g. 5) classes
#   - Discuss your initialization method and termination method for your implementation
#   - Implement the Jenks criterion to terminate your kmeans method
#   - Produce a thematic map(s) based on the results of your kmeans






##### 1.4. Evaluation and discussion #####
#   - Produce a density map from Jenks (Natural Break) method using QGIS or ArcGIS
#   - Discuss the differences of the break points created from all these classification
# methods (including the one from ArcGIS/QGIS)
#   - Discuss the maps created from different classification methods
#   - Note you may want to plot a histogram of population density for your discussion.






##### 1.5. Optional (max 2 additional pts; not count towards the page limit) #####
#   - Aggregate the census blocks to census tracts to create a new shp and sf
#   - Produce a population density map for census tracts with your kmeans method. 
#   - Determine how one might work with a much larger dataset (i.e. the original)