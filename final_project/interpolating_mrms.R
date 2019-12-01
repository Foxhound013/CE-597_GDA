library(ncdf4)
library(dplyr)
library(akima)
library(lattice)

fpath <- './data/raw/6_out743.nc'
#fpath <- 'C:/Users/Downi/Desktop/scratch/smaller_sub.nc'
mrms <- nc_open(fpath)

lon <- ncvar_get(mrms, 'longitude')
nlon <- dim(lon)
lat <- ncvar_get(mrms, 'latitude')
nlat <- dim(lat)

# manage the time componenet and check for missigness
time <- ncvar_get(mrms, 'time') # defines the number of loops for interpolation
tunits <- ncatt_get(mrms,'time','units')

timeSeq <- data.frame(tstamp=as.POSIXct(time, tz='utc', origin='1970-01-01 00:00:00'))
# verify integrity
start.t = as.POSIXct('2018-06-01 00:00:00', tz='UTC', origin='1970-01-01 00:00:00')
end.t = as.POSIXct('2018-06-30 23:58:00', tz='UTC', origin='1970-01-01 00:00:00')
dateSeq = data.frame(tstamp=seq.POSIXt(from=start.t, to=end.t, by='2 min'))

# two time steps are missing, we'll back fill these later
# the time steps will be in the order from timeSeq.

# variable information
varName <- 'PrecipRate_0mabovemeansealevel'
precipRate.units <- ncatt_get(mrms,varName, 'units')
fillvalue <- ncatt_get(mrms,varName,"_FillValue")

# get the road data and prep it.
roads <- read.csv('./data/processed/uniqueSegs_I65.csv')
roads <- roads %>% filter(direction=='N' & position >= 238 & position <= 307)

# can loop through the time by specifying where to start in the netcdf
pb <- txtProgressBar(min = 1, max = length(timeSeq$tstamp), style = 3) 
final.interp <- data.frame()
i <- 1

while (i <= length(timeSeq$tstamp)) {
  precipRate <- ncvar_get(mrms,varName, start=c(1,1,i), count=c(411,451,1))
  precip.data <- expand.grid(lon=lon, lat=lat)
  precip.data$precip <- as.vector(precipRate)
  
  precip.data <- precip.data %>% filter(lat > 39.754079 & lat < 40.678555 & 
                                          lon < -85.915814 & lon > -87.455752)
  
  # interpolate, create data frame, and join with road data.
  precip.interp <- interpp(x=precip.data$lon, 
                           y=precip.data$lat, 
                           z=precip.data$precip, 
                           xo=roads$lon, 
                           yo=roads$lat,
                           linear=T)
  
  precip.interp <- data.frame(tstamp=timeSeq$tstamp[i],
                              lon=precip.interp$x,
                              lat=precip.interp$y,
                              precip=precip.interp$z) %>% 
    left_join(roads, by=c('lon', 'lat'))
  
  final.interp <- bind_rows(final.interp, precip.interp)
  
  i <- i + 1
  setTxtProgressBar(pb, i)
  
}

nc.close(mrms)
write.csv(x=final.interp, file='./data/processed/interp_precip.csv', row.names=F)
