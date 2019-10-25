library(raster)
library(sf)
library(tmap); tmap_mode('view')
library(mapview)
library(lattice)

# 1. Data
#   1) DEM over entire Indiana (given)
#   2) County boundaries of IN (to download)
#   3) Flood plain of IN (to download)
#   4) Land covey maps of the entire country (entire US, raster; to download)

# Data has been download, this section will load these data sources
dem <- raster('./data/raw/IN_DEM_UTM/indiana_utm84')
dem; st_crs(dem); image(dem);
# UTM Zone 16 Projection with meters as it's unit

counties <- st_read('./data/raw/IN_Counties')
st_crs(counties)

flood <- st_read('./data/raw/Floodplains_FIRM', layer='Floodplains_FIRM_IDNR_IN')
st_crs(flood)

nlcd <- brick('./data/raw/NLCD_2016_LandCover/NLCD_2016_Land_Cover_L48_20190424.img')
nlcd; st_crs(nlcd); image(nlcd) # CONUS
nlcd.data.att <- nlcd@data@attributes # capture the data attributes
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

hist(dem, main='Elevation', ylab='Frequency', xlab='Elevation (meters)')
hist(slope, main='Slope', ylab='Frequency', xlab='Slope (degrees)')
xyplot(slope[1000,]~dem[1000,]) # 1 row cross section
xyplot(slope[,1000]~dem[,1000]) # 1 column cross section




# 3. Evaluate the ‘zone’ data (2 pts)
#   1) Prepare and map the county map
#   2) Prepare and map the land cover data
#   3) Prepare and map the flood maps
#   4) Crop, clip or mask them when needed; make sure they are ready for next tasks.

#1)
counties <- st_transform(counties, crs=st_crs(dem))
tm_shape(counties) + tm_polygons() + tm_scale_bar()


#2) The nlcd data is way to big to try to map prior to subsetting, you'll need to clip it with other data first
start <- Sys.time(); nlcd <- crop(nlcd, st_transform(counties, crs=st_crs(nlcd))); end <- Sys.time();
end-start
# Warning, projection takes roughly 16 minutes.
start <- Sys.time(); nlcd <- projectRaster(nlcd, crs=st_crs(dem)$proj4string, method='ngb'); end <- Sys.time();
end-start;

nlcd <- ratify(nlcd)
nlcd@data@attributes[[1]]$NLCD.class <- c('Open Water', 'Developed, Open Space', 'Developed, Low Intensity', 'Developed, Medium Intensity', 'Developed, High Intensity', 'Barren Land (Rock/Sand/Clay)', 'Deciduous Forest', 'Evergreen Forest', 'Mixed Forest', 'Shurb/Scrub', 'Grassland/Herbaceous', 'Pasture/Hay', 'Cultivated Crops', 'Woody Wetlands', 'Emergent Herbaceous Wetlands')

mapview(nlcd)


#3) flood data takes forever to plot
flood <- lwgeom::st_make_valid(flood) # not all of the geometries are valid
flood <- st_transform(flood, crs=st_crs(dem))

##############################
# ISSUE GETTING MAPPING FOR FLOOD


#mapview(flood$geometry)


# 4. Zonal analysis (6 pts)
#   1) Determine the mean, standard deviation of elevation and slope for each county
#   2) Determine the mean, std. dev. of elevation and slope for land cover classes
#   3) Determine the mean, standard deviation of elevation and slope for flood zones

#1)
elevMean.cnty <- raster::extract(dem, counties, method='simple', fun=mean, na.rm=T)
# works but very slow (40 seconds for 1 county)
start <- Sys.time()
slopeMean.cnty <- raster::extract(slope, counties[1,], method='simple', fun='mean')
end <- Sys.time(); end-start

#2)
tmp <- nlcd
extent(tmp) <- extent(dem)
elevSlope.nlcd <- zonal(dem, tmp, fun='mean', na.rm=T)

# 5. Bonus (3 pts)
#   1) Evaluate the county’s risk based on its population and its floodplain
#   2) Reclassify/classify elevations (or slopes) using one of the data classifiers we discussed
#   3) Implement your own slope calculator and evaluate it
