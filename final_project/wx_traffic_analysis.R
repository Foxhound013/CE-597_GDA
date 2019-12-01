library(lattice)
library(data.table)
library(dplyr)
library(tidyr)

##### Load & Prep Data #####
precip <- fread('./data/processed/interp_precip.csv')
precip$tstamp <- as.POSIXct(precip$tstamp, tz='utc', origin='1970-01-01 00:00:00')

traffic <- fread('./data/raw/June_I-65_2018.csv',
                 col.names=c('xdid', 'tstamp', 'speed', 'score', 'lat', 'lon', 'position',
                             'roadname', 'direction', 'bearing', 'startmm', 'endmm'))

traffic <- traffic %>% filter(direction=='N' & position >= 238 & position <= 307)
traffic$tstamp <- as.POSIXct(traffic$tstamp, tz='utc', origin='1970-01-01 00:00:00')

# The traffic data is set to 1 minute resolution, scale down to 2 minute resolution
# in order to match the precip data
setDT(traffic)
traffic <- traffic[,list(speed=mean(speed)), 
                   by=list(xdid=xdid, 
                           position=position, 
                           tstamp=as.POSIXct(cut(tstamp, '2 min'),tz='UTC'),
                           lat=lat,
                           lon=lon,
                           roadname=roadname,
                           direction=direction,
                           bearing=bearing)]

# Fix missigness in the precip data and join the datasets
traffic <- traffic %>% left_join(precip, by=c('tstamp', 'lon', 'lat', 'position', 'xdid', 'direction'))

tmp <- traffic[is.na(traffic$precip),]
##### Data Characterization  #####

# 