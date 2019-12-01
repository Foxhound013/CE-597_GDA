library(data.table)
library(sf)
library(tmap); tmap_mode('view')
library(dplyr)
library(tidyr)

##### Load the Data & Prep #####
fpath <- './data/raw/June_I-65_2018.csv'
traffic <- fread(fpath,col.names=c('xdid', 'tstamp', 'speed', 'score', 'lat', 'lon', 'position',
                                   'roadname', 'direction', 'bearing', 'startmm', 'endmm'))
glimpse(traffic)

uniqueSegs <- traffic[,c('xdid','position','lon','lat', 'direction')] %>% unique
write.csv(uniqueSegs, './data/processed/uniqueSegs_I65.csv', row.names=F)

uniqueSegs.n.sf <- traffic[,c('xdid','position','lon','lat', 'direction')] %>% 
  filter(direction == 'N') %>% unique %>% 
  st_as_sf(coords=c('lon','lat'))

tm_shape(uniqueSegs.n.sf) + tm_dots('position', style='cont')
