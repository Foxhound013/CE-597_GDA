library(lattice)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(sf)
library(tmap)

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

# a more appropriate x and y scale may also be useful in fleshing out this relationship some




# create 30 minute window around rain events.
traffic <- traffic %>% group_by(position) %>% arrange(tstamp) # guarantees time stamp order

traffic.events <- traffic %>% group_by(position) %>% 
  transmute(tstamp=tstamp,
            speed=speed,
            precip=precip,
            light=light,
            day=day,
            event=ifelse((precip > 0 & speed < 60), yes=T, no=F))

# fig.key <- list(space='bottom', columns=2,
#                 text = list(c("Night", "Day")),
#                 lines = list(type = c("p", "p"), col = c("blue", "red"), pch = c(16,16))
#                 )

png('./figures/precipvspeeds_events.png', width=1080, height=760)
xyplot(precip~speed | factor(ifelse(event,'Traffic Slow Down','Usual Traffic')) * 
         factor(ifelse(light, 'Night', 'Day')), 
       data=traffic.events, pch=16, alpha=0.5, jitter=T,
       layout=c(1,4), xlab='Speed (MPH)', ylab='Precipitation (mm/hr)', grid=T,
       xlim=c(0,90), ylim=c(0,150), aspect='fill',
       scales=list(x=list(at=seq(0,90,5)), y=list(at=seq(0,150,30))))
dev.off()
#col=c('blue','red')

# can produce a heat map with this
events.summary <- traffic.events %>% group_by(position) %>% 
  summarize(sumEventsNight=sum(ifelse((light==F & event==T), yes=1, no=0), na.rm=T),
            meanSpd_EventsNight=mean(ifelse((light==F & event==T), yes=speed, no=NA), na.rm=T),
            varSpd_EventsNight=var(ifelse((light==F & event==T), yes=speed, no=NA), na.rm=T),
            sdSpd_EventsNight=sd(ifelse((light==F & event==T), yes=speed, no=NA), na.rm=T),
            
            meanPrecip_EventsNight=mean(ifelse((light==F & event==T), yes=precip, no=NA), na.rm=T),
            varPrecip_EventsNight=var(ifelse((light==F & event==T), yes=precip, no=NA), na.rm=T),
            sdPrecip_EventsNight=sd(ifelse((light==F & event==T), yes=precip, no=NA), na.rm=T),
            
            sumEventsDay=sum(ifelse((light==T & event==T), yes=1, no=0), na.rm=T),
            meanSpd_EventsDay=mean(ifelse((light==T & event==T), yes=speed, no=NA), na.rm=T),
            varSpd_EventsDay=var(ifelse((light==T & event==T), yes=speed, no=NA), na.rm=T),
            sdSpd_EventsDay=sd(ifelse((light==T & event==T), yes=speed, no=NA), na.rm=T),
            
            meanPrecip_EventsDay=mean(ifelse((light==T & event==T), yes=precip, no=NA), na.rm=T),
            varPrecip_EventsDay=var(ifelse((light==T & event==T), yes=precip, no=NA), na.rm=T),
            sdPrecip_EventsDay=sd(ifelse((light==T & event==T), yes=precip, no=NA), na.rm=T),)

# verify validity
tmp <- traffic.events %>% filter(position==241) %>%  filter(light==T & event==T)
sum(tmp$event)
var(tmp$speed)
mean(tmp$speed)
sd(tmp$speed)

plot(events.summary$position, events.summary$sumEventsNight, col='blue', pch=16)
points(events.summary$position, events.summary$sumEventsDay, col='red', pch=16)

plot(events.summary$position, events.summary$meanSpd_EventsNight, col='blue', pch=16)
points(events.summary$position, events.summary$meanSpd_EventsDay, col='red', pch=16)

plot(events.summary$position, events.summary$meanPrecip_EventsNight, col='blue', pch=16)
points(events.summary$position, events.summary$meanPrecip_EventsDay, col='red', pch=16)

plot(events.summary$position, events.summary$sdPrecip_EventsNight, col='blue', pch=16)
points(events.summary$position, events.summary$sdPrecip_EventsDay, col='red', pch=16)

# interestingly, this reveals there is a spatial component to these
uniqueSegs <- traffic %>% select(position, lon, lat) %>% unique()
tmp <- events.summary %>% left_join(uniqueSegs, by='position') %>% st_as_sf(coords=c('lon','lat'))

tmap_mode('view')
tm_shape(tmp) + tm_dots('sumEventsNight')
tm_shape(tmp) + tm_dots('sumEventsDay')

densityplot(~sumEventsNight, data=events.summary)
densityplot(~sumEventsDay, data=events.summary)

hist(traffic.e.counts$sumEventsDay)
if(tmp$light) sum()








# what happens if we look at any event where rain occurs and measure the max speed drop
# over the next 30 minutes.
# this would produce a precip value and max speed drop for precip.

# may be a good idea to calculate lagged speeds and their precipitation values
# as a seperate table

speedStats <- traffic %>% group_by(position) %>% 
  transmute(tstamp=tstamp,
            speed=speed,
            precip=precip,
            light=light,
            twoMinLag=speed - dplyr::lag(speed,n=1,default=NA),
            twoMinPrecip=precip - dplyr::lag(precip,n=1,default=NA),
            fourMinLag=speed - dplyr::lag(speed,n=2,default=NA),
            sixMinLag=speed - dplyr::lag(speed,n=3,default=NA),
            eightMinLag=speed - dplyr::lag(speed,n=4,default=NA),
            tenMinLag=speed - dplyr::lag(speed,n=5,default=NA),
            twelveMinLag=speed - dplyr::lag(speed,n=6,default=NA),
            fourteenMinLag=speed - dplyr::lag(speed,n=7,default=NA),
            sixteenMinLag=speed - dplyr::lag(speed,n=8,default=NA),
            eightteenMinLag=speed - dplyr::lag(speed,n=9,default=NA),
            twentyMinLag=speed - dplyr::lag(speed,n=10,default=NA),
            twentytwoMinLag=speed - dplyr::lag(speed,n=11,default=NA),
            twentyfourMinLag=speed - dplyr::lag(speed,n=12,default=NA),
            twentysixMinLag=speed - dplyr::lag(speed,n=13,default=NA),
            twentyeightMinLag=speed - dplyr::lag(speed,n=14,default=NA),
            thirtyMinLag=speed - dplyr::lag(speed,n=15,default=NA),
            thirtyMinPrecip=dplyr::lag(precip,n=15,default=NA),)

# could calculate a corresponding precip value. That is to say, if we're looking at lagged
# speeds of twenty mins, the precip from twenty minutes ago can be reflected to the same row
# so as to match up the values.


tmp <- speedStats %>% filter(position==250)
tmp <- tmp %>% mutate(spd_p_diff=speed-precip)

tmp2 <- traffic %>% filter(precip == 0) %>% transmute(avgSpeed=mean(speed), light=light)
densityplot(~avgSpeed | factor(light), data=tmp2)

xyplot(precip~fourMinLag, data=tmp, pch=16, alpha=0.5)



# the ridgeline plot may be good if I can melt the dataframe and get a lag as a categorical
# variable.
tmp2 <- reshape2::melt(tmp, id=c('position', 'tstamp', 'speed', 'precip', 'light'))

ggplot(tmp2, aes(x=value, y=variable)) + 
  geom_density_ridges2(rel_min_height=0.001, scale=1) +
  facet_wrap(~light)


pdf('./figures/precipVfourMinLag.pdf')
xyplot(precip~fourMinLag | factor(light), data=speedStats, pch=16, alpha=0.5, grid=T,
       auto.key=T, layout=c(1,2))
dev.off()

pdf('./figures/desnityPlot.pdf')
densityplot(~fourMinLag, data=speedStats, layout=c(1,1), alpha=0.5,
            plot.points=F, groups=light, auto.key=T, grid=T, aspect=1)
dev.off()


pdf('./figures/precipV2MinLag.pdf')
xyplot(precip~twoMinLag | factor(position), data=speedStats, pch=16, alpha=0.5, grid=T,
       layout=c(2,2,1), groups=light, auto.key=T)
dev.off()
densityplot(~twentyMinLag,data=tmp)

lattice::scat
# with the data layed out like above, we can roll through the time steps and have a quick look
# up of the speed drop at any time interval within 30 minutes.

