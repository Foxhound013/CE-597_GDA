##### Imports #####
library(sf)
library(sp)
library(rgdal)
library(tidyverse)

##### Section 1.1 #####
# 1.1. Data preparation - sp and sf object creation
# • Read in the given .xls file
# • Create an sp object and an sf object to store the input data
# • Create a shapefile from the input data
# • Assign the coord. system as WGS 84 (no map projection) to all associates data files
# • Create and assign the bounding box to all data files

data <- read.csv('./data/raw/pu2014.csv', header=T, stringsAsFactors=F)
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

# Create a shapefile from the input data. First verify files don't exist.
fpath_sp = './data/processed/sp_out.shp'
if (file.exists(fpath_sp)) {
  file.remove(fpath_sp)
  gsub('.shp', '.dbf', fpath_sp) %>% file.remove()
  gsub('.shp', '.shx', fpath_sp) %>% file.remove()
  gsub('.shp', '.prj', fpath_sp) %>% file.remove()
}

fpath_sf = './data/processed/sf_out.shp'
if (file.exists(fpath_sf)) {
  file.remove(fpath_sf)
  gsub('.shp', '.dbf', fpath_sf) %>% file.remove()
  gsub('.shp', '.shx', fpath_sf) %>% file.remove()
  gsub('.shp', '.prj', fpath_sf) %>% file.remove()
}
## Do so from an sp object
rgdal::writeOGR(sp, dsn=fpath_sp, layer='tweets', driver='ESRI Shapefile')

## Do so from an sf object
sf::st_write(sf, dsn='./data/processed/sf_out.shp', layer='tweets', driver='ESRI Shapefile')

# The coordinate system and bbox have been assigned to the data prior to writing.



##### Section 1.2 #####
# 1.2. Spatial object I/O and conversion
# • Write the shapefile to your working folder
# • Read the shapefile back from your working folder
# • Convert the sf and sp to a shapefile & one other geo-data format & read them in
# • Create an sf and sp from these read-in’s if needed

# writing the shapeful was completed in Section 1.1 above.

# Need clarification on what is wanted here.










##### Section 1.3 #####
# 1.3. Spatial object’s attribute manipulation (use the sf file from 1.1)
# • Convert the epoch (Unix time) to day-month-year, day of the week, and local time
# • Add these derived info to the sf
# • Subset the columns/info you will use and keep the rest as a new sf file
# • Stay with this sf file onwards


# Convert Epoch time: Taken care of from the start. May want to rethink
# this one and just do that conversion at this point.











##### Section 1.4 #####
# 1.4. Mapping the overall tweets distribution
# • Show/map the overall tweet locations atop OSM and/or GoogleMap (ttm function of
#                                                                   tmap)
# • Color-code the maps based on month, day and/or hour
# • Color-code the map based on selected individual users
# • Prepare clean versions of above maps for discuss/presentation













##### Section 1.5 #####
# 1.5. Mapping your story of interest (AOI, individuals)
# • Find out/show the hot spots over time in WL and/or on campus
# • Find out/show the spatial-temporal distribution/trajectory of top or selected 3
# individuals
# • Make sure your maps are clean/clear












##### Section 1.6 #####
# 1.6. Mapping/estimate mobility (max 2 additional points)
# • Summarize and visualize the daily moving distances of selected (or all) individuals
# • Creative visualization 











