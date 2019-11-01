library(raster)
library(sf)
library(tmap); tmap_mode('view')
library(mapview)
library(lattice)
library(dplyr)
library(exactextractr)
library(rasterVis)

# 1. Data
#   1) DEM over entire Indiana (given)
#   2) County boundaries of IN (to download)
#   3) Flood plain of IN (to download)
#   4) Land cover maps of the entire country (entire US, raster; to download)

# Data has been download, this section will load these data sources
dem <- raster('./data/raw/IN_DEM_UTM/indiana_utm84')
dem; st_crs(dem); image(dem);
# UTM Zone 16 Projection with meters as it's unit

counties <- st_read('./data/raw/IN_Counties')
st_crs(counties)

flood <- st_read('./data/raw/Floodplains_FIRM', layer='Floodplains_FIRM_IDNR_IN')
st_crs(flood)

nlcd <- raster('./data/raw/NLCD_2016_LandCover/NLCD_2016_Land_Cover_L48_20190424.img')
nlcd; st_crs(nlcd); image(nlcd) # CONUS

# 2. Terrain analysis (2 pts)
#   1) Map the elevation
#   2) Calculate and map the slope
#   3) Find/show any relationship between elevation (x-direction) and slope (y-direction); you
#     may try several binning (in both x and y) for this relationships

tmap_mode('view')
tm_shape(dem) + tm_raster(alpha=0.7, title='Elevation') + tm_scale_bar() + 
  tmap_options(max.raster=c(plot = 1000000, view = 1000000))

slope <- terrain(dem, opt='slope')
mapview(slope)

# coerce the rasters to vectors
dem.vect <- as.data.frame(dem, xy=T) %>% na.omit

slope.vect <- as.data.frame(slope, xy=T) %>% na.omit

dem_slope <- cbind(sample(slope.vect$slope, 250000), sample(dem.vect$indiana_utm84, 250000)) %>%
  as.data.frame
colnames(dem_slope) <- c('slope', 'elevation')

png('./figures/slopeVelevation.png')
xyplot(slope~elevation, data=dem_slope, pch=16, col='deepskyblue3', alpha=0.4,
       main='Slope Vs. Elevation for Indiana - Random 250K Samples', ylab='Slope (Degrees)',
       xlab='Elevation (m)')
dev.off()


# 3. Evaluate the ‘zone’ data (2 pts)
#   1) Prepare and map the county map
#   2) Prepare and map the land cover data
#   3) Prepare and map the flood maps
#   4) Crop, clip or mask them when needed; make sure they are ready for next tasks.


#1)
counties <- st_transform(counties, crs=st_crs(dem))
dem <- crop(dem, counties)
mapview(dem)
tm_shape(counties) + tm_polygons() + tm_scale_bar()


#2) The nlcd data is way to big to try to map prior to subsetting, you'll need to clip it with other data first
start <- Sys.time(); nlcd <- crop(nlcd, st_transform(counties, crs=st_crs(nlcd))); end <- Sys.time();
end-start
# Warning, projection takes a very long time
start <- Sys.time(); nlcd <- projectRaster(nlcd, crs=st_crs(dem)$proj4string, method='ngb', res=c(30,30)); end <- Sys.time();
end-start;

nlcd <- ratify(nlcd)
nlcd@data@attributes[[1]]$NLCD.class <- c('Open Water', 'Developed, Open Space', 'Developed, Low Intensity', 'Developed, Medium Intensity', 'Developed, High Intensity', 'Barren Land (Rock/Sand/Clay)', 'Deciduous Forest', 'Evergreen Forest', 'Mixed Forest', 'Shurb/Scrub', 'Grassland/Herbaceous', 'Pasture/Hay', 'Cultivated Crops', 'Woody Wetlands', 'Emergent Herbaceous Wetlands')

mapview(nlcd)
myPallette = rev(c('#64B3D5', '#C8E6F8', '#CA9146', '#FBF65D', '#FDE9AA', '#DCCA8F', '#D4E7B0', 
                   '#38814E', '#85C77E', '#D2CDC0', '#B50000', '#FF0000', '#E29E8C', '#E8D1D1',
                   '#5475A8'))
rasterVis::levelplot(nlcd, col.regions=myPallette, main='NLCD Raster for Indiana',
                     xlab='Longitude (m)', ylab='Latitude (m)')

#3) flood data takes forever to plot
flood <- lwgeom::st_make_valid(flood) # not all of the geometries are valid
flood <- st_transform(flood, crs=st_crs(dem))



##############################
# ISSUE GETTING MAPPING FOR FLOOD


#mapview(flood$geometry)


# given the time to download this data, I'm saving the rasters to read in if necessary later
# writeRaster(dem, filename='./data/processed/dem.grd', overwrite=T)
# writeRaster(nlcd, filename='./data/processed/nlcd.grd', overwrite=T)
# st_write(counties, dsn='./data/processed/counties', layer='counties', driver='ESRI Shapefile')
# st_write(flood, dsn='./data/processed/flood', layer='flood', driver='ESRI Shapefile')

# 4. Zonal analysis (6 pts)
#   1) Determine the mean, standard deviation of elevation and slope for each county
#   2) Determine the mean, std. dev. of elevation and slope for land cover classes
#   3) Determine the mean, standard deviation of elevation and slope for flood zones

# Load transformed data from .RData file
load('./data/processed/projectedData.RData')

#1)
elevMean.cnty <- exact_extract(dem, counties, fun='mean')
elevStd.cnty <- exact_extract(dem, counties, function(value, cov_frac) sd(value*cov_frac))

slopeMean.cnty <- exact_extract(slope, counties, fun='mean')
slopeStd.cnty <- exact_extract(slope, counties, function(value, cov_frac) sd(value*cov_frac))

counties$elevMean <- elevMean.cnty
counties$elevStd <- elevStd.cnty
counties$slopeMean <- slopeMean.cnty
counties$slopeStd <- slopeStd.cnty

tmap_mode('plot')

tm_shape(counties) + tm_fill('elevMean', title='Elevation (m)') + tm_borders() + 
  tm_scale_bar() + tm_layout()

tm_shape(counties) + tm_fill('elevStd', title='Elevation (m)') + tm_borders() + 
  tm_scale_bar() + tm_layout()

tm_shape(counties) + tm_fill('slopeMean', title='Slope (Degrees)') + tm_borders() + 
  tm_scale_bar() + tm_layout()

tm_shape(counties) + tm_fill('slopeStd', title='Slope (Degrees)') + tm_borders() + 
  tm_scale_bar() + tm_layout()

#2) the data has to be coerced to match up
nlcd.dem <- crop(nlcd, dem)
extent(nlcd.dem) <- extent(dem)
elevMean.nlcd <- zonal(dem, nlcd.dem, fun='mean', method='simple')
elevStd.nlcd <- zonal(dem, nlcd.dem, fun=sd, method='simple')

nlcd.slope <- crop(nlcd,slope)
slope.nlcd <- crop(slope, nlcd.slope)
extent(slope.nlcd) <- extent(nlcd.slope)
slopeMean.nlcd <- zonal(slope.nlcd, nlcd.slope, fun='mean', method='simple')
slopeStd.nlcd <- zonal(slope.nlcd, nlcd.slope, fun=sd, method='simple')

# Data is fine being tabular.
# save the data up to this point
load('./data/processed/projected&processedData.Rdata')

#3) ### Not quite sure what to do with these, the data values are entirely too high.
elevMean.fld <- exact_extract(dem, flood, fun='mean')
elevStd.fld <- exact_extract(dem, flood, function(value, cov_frac) sd(value*cov_frac))

slopeMean.fld <- exact_extract(slope, flood, fun='mean')
slopeStd.fld <- exact_extract(slope, flood, function(value, cov_frac) sd(value*cov_frac))
# 5. Bonus (3 pts)
#   1) Evaluate the county’s risk based on its population and its floodplain
#   2) Reclassify/classify elevations (or slopes) using one of the data classifiers we discussed
#   3) Implement your own slope calculator and evaluate it



