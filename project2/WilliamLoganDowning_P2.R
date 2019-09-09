##### Imports #####
library(sf)
library(sp)
library(tidyverse)

##### Section 1.1 #####
# 1.1. Data preparation - sp and sf object creation
# • Read in the given .xls file
# • Create an sp object and an sf object to store the input data
# • Create a shapefile from the input data
# • Assign the coord. system as WGS 84 (no map projection) to all associates data files
# • Create and assign the bounding box to all data files












##### Section 1.2 #####
# 1.2. Spatial object I/O and conversion
# • Write the shapefile to your working folder
# • Read the shapefile back from your working folder
# • Convert the sf and sp to a shapefile & one other geo-data format & read them in
# • Create an sf and sp from these read-in’s if needed












##### Section 1.3 #####
# 1.3. Spatial object’s attribute manipulation (use the sf file from 1.1)
# • Convert the epoch (Unix time) to day-month-year, day of the week, and local time
# • Add these derived info to the sf
# • Subset the columns/info you will use and keep the rest as a new sf file
# • Stay with this sf file onwards














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











