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

tmp <- traffic[is.na(traffic$precip),] # check location of missigness

# impute missing values
traffic <- traffic %>% group_by(position) %>% do(zoo::na.locf(.))
traffic <- traffic[order(traffic$tstamp),]

tmp <- traffic[is.na(traffic$precip),] # check location of missigness

# truncate the precip data
traffic$precip <- round(traffic$precip, 2)


##### Data Characterization  #####

pdf('./figures/precipVspeeds.pdf')
xyplot(precip~speed | factor(position), data=traffic, pch=16, col='deepskyblue3', alpha=0.5,
       layout=c(1,1))
dev.off()

pdf('./figures/precipVspeeds_noZeros.pdf')
xyplot(precip~speed | factor(position), data=traffic[which(traffic$precip > 0),], 
       pch=16, col='deepskyblue3', alpha=0.5, layout=c(1,1))
dev.off()

# develop a column to mark civil twilight hours
# sunrise: 10am sunset: 12:30am next day, these are civil twilight hours (approx.)

# could simply label the hours
traffic$hours <- lubridate::hour(traffic$tstamp)
traffic$mins <- lubridate::minute(traffic$tstamp)

traffic$light <- NA
traffic[which(((traffic$hours>=0 & traffic$mins>=30) | (traffic$hours > 0 & traffic$mins >=0)) & 
                (traffic$hours <10) ),'light'] <- F
traffic[which((traffic$hours == 10 & traffic$mins == 00)),'light'] <- F
traffic[is.na(traffic$light),'light'] <- T


pdf('./figures/precipVspeed_civilTwilight.pdf')
xyplot(precip~speed | factor(light), data=traffic, pch=16, col='deepskyblue3', alpha=0.5,
       layout=c(1,2))
dev.off()

# is there a day of week component that should be fleshed out?
traffic$day <- weekdays(traffic$tstamp) %>% factor(levels=c('Sunday', 'Monday', 'Tuesday',
                                                            'Wednesday', 'Thursday', 'Friday',
                                                            'Saturday'))


pdf('./figures/precipVspeed_dayofweek.pdf')
xyplot(precip~speed | factor(day), data=traffic, pch=16, col='deepskyblue3', alpha=0.5,
       layout=c(7,1), par.strip.text=list(cex=.8))
dev.off()

# sector of day (morning, afternoon, evening, night)?

# a more appropriate x and y scale may also be useful in fleshing out this relationship some


# what happens if we look at any event where rain occurs and measure the max speed drop
# over the next 30 minutes.
# this would produce a precip value and max speed drop for precip.

tmp <- traffic %>% group_by(position) %>% 
  mutate(fourteenMinLag=speed - dplyr::lag(speed,n=7,default=NA),
         thirtyMinLag=speed - dplyr::lag(speed,n=15,default=NA))

tmp2 <- tmp %>% filter(position==250)

