library(sp)
library(rgdal)
library(tmap)
library(tidyverse)

rgdal::ogrListLayers("./data/raw/buildings")
buildings <- rgdal::readOGR('./data/raw/buildings_shpfile')

layers <- rgdal::ogrListLayers("./data/raw/geocodings")
#multiple layers, need to run apply
geocodings <- lapply(layers, function(i) readOGR('./data/raw/geocodings', layer=i))
names(geocodings) <- layers

rgdal::ogrListLayers('./data/raw/streets')
streets <- rgdal::readOGR('./data/raw/streets')


tm_shape(buildings) + tm_polygons() +
  tm_shape(streets) + tm_lines()
