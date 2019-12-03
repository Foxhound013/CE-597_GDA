library(data.table)
library(dplyr)
library(sf)
library(tmap)

traffic <- fread('./data/raw/June_I-65_2018.csv',
                 col.names=c('xdid', 'tstamp', 'speed', 'score', 'lat', 'lon', 'position',
                             'roadname', 'direction', 'bearing', 'startmm', 'endmm'))

traffic <- traffic %>% filter(direction=='N' & position >= 238 & position <= 307)
traffic$tstamp <- as.POSIXct(traffic$tstamp, tz='utc', origin='1970-01-01 00:00:00')

# The traffic data is set to 1 minute resolution, scale down to 2 minute resolution
# in order to match the precip data
traffic.scored <- traffic
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

traffic.sf <- traffic %>% select(position, lon, lat) %>% unique() %>% st_as_sf(coords=c('lon','lat'))

tmap_mode('view')
tm_shape(traffic.sf) + tm_dots() + tm_scale_bar()


# produce some metrics about confidence scores present in the dataset.
conf.counts <- traffic.scored %>% summarize(lowConfCounts=sum(ifelse(score==10,1,0)),
                                            medConfCounts=sum(ifelse(score==20,1,0)),
                                            highConfCounts=sum(ifelse(score==30,1,0)))
round((conf.counts/nrow(traffic.scored) * 100),2)
