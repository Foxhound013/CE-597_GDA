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

traffic <- traffic %>% group_by(position) %>% arrange(tstamp) # guarantees time stamp order

# we need a definition of normal and non normal traffic flow.
traffic.norm <- traffic %>% group_by(position) %>% 
  transmute(tstamp=tstamp,
            speed=speed,
            precip=precip,
            light=light,
            )



traffic.events <- traffic %>% group_by(position) %>% 
  transmute(tstamp=tstamp,
            speed=speed,
            precip=precip,
            light=light,
            day=day,
            event=ifelse((precip > 0 & speed < 60), yes=T, no=F))

densityplot(~speed | factor(light), data=traffic.events, plot.points=F,
            layout=c(1,2))

# fig.key <- list(space='bottom', columns=2,
#                 text = list(c("Night", "Day")),
#                 lines = list(type = c("p", "p"), col = c("blue", "red"), pch = c(16,16))
#                 )

png('./figures/precipvspeeds_events.png', width=1080, height=760)
xyplot(precip~speed | factor(ifelse(event,'Traffic Slow Down','Usual Traffic')) * 
         factor(ifelse(light, 'Night', 'Day')), 
       data=tmp, pch=16, alpha=0.5, jitter=T,
       layout=c(1,4), xlab='Speed (MPH)', ylab='Precipitation (mm/hr)', grid=T,
       xlim=c(0,90), ylim=c(0,150), aspect='fill',
       scales=list(x=list(at=seq(0,90,5)), y=list(at=seq(0,150,30))))
dev.off()
#col=c('blue','red')

nonevents.summary <- traffic.events %>% group_by(position) %>%
  summarize(sumNight=sum(ifelse((light==F & event==F), yes=1, no=0), na.rm=T),
            meanSpd_Night=mean(ifelse((light==F & event==F), yes=speed, no=NA), na.rm=T),
            varSpd_Night=var(ifelse((light==F & event==F), yes=speed, no=NA), na.rm=T),
            sdSpd_Night=sd(ifelse((light==F & event==F), yes=speed, no=NA), na.rm=T),
            
            meanPrecip_Night=mean(ifelse((light==F & event==F), yes=precip, no=NA), na.rm=T),
            varPrecip_Night=var(ifelse((light==F & event==F), yes=precip, no=NA), na.rm=T),
            sdPrecip_Night=sd(ifelse((light==F & event==F), yes=precip, no=NA), na.rm=T),
            
            sumDay=sum(ifelse((light==T & event==F), yes=1, no=0), na.rm=T),
            meanSpd_Day=mean(ifelse((light==T & event==F), yes=speed, no=NA), na.rm=T),
            varSpd_Day=var(ifelse((light==T & event==F), yes=speed, no=NA), na.rm=T),
            sdSpd_Day=sd(ifelse((light==T & event==F), yes=speed, no=NA), na.rm=T),
            
            meanPrecip_Day=mean(ifelse((light==T & event==F), yes=precip, no=NA), na.rm=T),
            varPrecip_Day=var(ifelse((light==T & event==F), yes=precip, no=NA), na.rm=T),
            sdPrecip_Day=sd(ifelse((light==T & event==F), yes=precip, no=NA), na.rm=T))


isolated_normMean <- nonevents.summary %>% select('position', 'meanSpd_Day', 'meanSpd_Night') %>% 
  reshape2::melt(id=c('position'))
isolated_normMean$variable <- ifelse((isolated_normMean$variable == 'meanSpd_Day'), 
                                     'Mean Speed: Day', 'Mean Speed Night')
isolated_normMean$variable <- as.factor(isolated_normMean$variable)
bwplot(variable~value, data=isolated_normMean, main='Mean Speed Distribution Under "Normal" Conditions',
       xlab='Speed (MPH)', xlim=c(60,70), scales=list(x=list(at=seq(60,70,2))))

sum(nonevents.summary$sumNight)/(21600*69) * 100 +
sum(nonevents.summary$sumDay)/(21600*69) * 100

# can produce a heat map with this
events.summary <- traffic.events %>% group_by(position) %>% 
  summarize(sumNight=sum(ifelse((light==F & event==T), yes=1, no=0), na.rm=T),
            meanSpd_Night=mean(ifelse((light==F & event==T), yes=speed, no=NA), na.rm=T),
            varSpd_Night=var(ifelse((light==F & event==T), yes=speed, no=NA), na.rm=T),
            sdSpd_Night=sd(ifelse((light==F & event==T), yes=speed, no=NA), na.rm=T),
            
            meanPrecip_Night=mean(ifelse((light==F & event==T), yes=precip, no=NA), na.rm=T),
            varPrecip_Night=var(ifelse((light==F & event==T), yes=precip, no=NA), na.rm=T),
            sdPrecip_EventsNight=sd(ifelse((light==F & event==T), yes=precip, no=NA), na.rm=T),
            
            sumDay=sum(ifelse((light==T & event==T), yes=1, no=0), na.rm=T),
            meanSpd_Day=mean(ifelse((light==T & event==T), yes=speed, no=NA), na.rm=T),
            varSpd_Day=var(ifelse((light==T & event==T), yes=speed, no=NA), na.rm=T),
            sdSpd_Day=sd(ifelse((light==T & event==T), yes=speed, no=NA), na.rm=T),
            
            meanPrecip_Day=mean(ifelse((light==T & event==T), yes=precip, no=NA), na.rm=T),
            varPrecip_Day=var(ifelse((light==T & event==T), yes=precip, no=NA), na.rm=T),
            sdPrecip_Day=sd(ifelse((light==T & event==T), yes=precip, no=NA), na.rm=T),)


isolated_eventMean <- events.summary %>% select('position', 'meanSpd_Day', 'meanSpd_Night') %>% 
  reshape2::melt(id=c('position'))
isolated_eventMean$variable <- ifelse((isolated_eventMean$variable == 'meanSpd_Day'), 
                                     'Mean Speed: Day', 'Mean Speed Night')
isolated_eventMean$variable <- as.factor(isolated_eventMean$variable)
bwplot(variable~value, data=isolated_eventMean, main='Mean Speed Distribution Under "Event" Conditions',
       xlab='Speed (MPH)', xlim=c(20,70), scales=list(x=list(at=seq(20,70,10))))

sum(events.summary$sumNight)/(21600*69) * 100 
sum(events.summary$sumDay)/(21600*69) * 100

# what is the difference? Negative indicates var in question was higher during an event.
normVevent <- round((nonevents.summary - events.summary), 2)
normVevent$position <- events.summary$position

tmp.events <- events.summary %>% select(position, meanSpd_Night, meanSpd_Day)
tmp.nonEvents <- nonevents.summary %>% select(position, meanSpd_Night, meanSpd_Day)

eventVnon_percent <- 100 - (tmp.events/tmp.nonEvents) * 100
eventVnon_percent$position <- tmp.events$position

eventPrecip <- events.summary %>% select(position, meanPrecip_Night, meanPrecip_Day)
eventVnon_percent <- eventVnon_percent %>% left_join(eventPrecip, by='position')


png('./figures/percentReduction_Night.png', width=680, height=520)
xyplot(meanPrecip_Night~meanSpd_Night, data=eventVnon_percent, pch=16,
       main='Percent Speed Reduction from Normal at Night',
       xlab='Percent Speed Reduction From Normal', ylab='Mean Precipitation (mm/hr)',
       xlim=c(0,70), ylim=c(0,70),
       scales=list(x=list(at=seq(0,70,5)), y=list(at=seq(0,70,10))))
dev.off()
densityplot(~meanSpd_Night, data=eventVnon_percent, pch=16,
            main='Density Estimation of Speed Reduction from Normal at Night',
            xlab='Percent Speed Reduction From Normal',
            xlim=c(0,80), ylim=c(0,.1),
            scales=list(x=list(at=seq(0,80,5)), y=list(at=seq(0,.1,0.01))))

png('./figures/percentReduction_Day.png', width=680, height=520)
xyplot(meanPrecip_Day~meanSpd_Day, data=eventVnon_percent, pch=16,
       main='Percent Speed Reduction from Normal during Day',
       xlab='Percent Speed Reduction From Normal', ylab='Mean Precipitation (mm/hr)',
       xlim=c(0,70), ylim=c(0,70),
       scales=list(x=list(at=seq(0,70,5)), y=list(at=seq(0,70,10))))
dev.off()
densityplot(~meanSpd_Day, data=eventVnon_percent, pch=16,
            main='Density Estimation of Speed Reduction from Normal during Day',
            xlab='Percent Speed Reduction From Normal',
            xlim=c(0,80), ylim=c(0,.1),
            scales=list(x=list(at=seq(0,80,5)), y=list(at=seq(0,.1,0.01))))


#
plot(events.summary$position, events.summary$sumNight, col='blue', pch=16)
points(events.summary$position, events.summary$sumDay, col='red', pch=16)

plot(events.summary$position, events.summary$meanSpd_Night, col='blue', pch=16)
points(events.summary$position, events.summary$meanSpd_Day, col='red', pch=16)

plot(events.summary$position, events.summary$meanPrecip_Night, col='blue', pch=16)
points(events.summary$position, events.summary$meanPrecip_Day, col='red', pch=16)

plot(events.summary$position, events.summary$sdPrecip_Night, col='blue', pch=16)
points(events.summary$position, events.summary$sdPrecip_Day, col='red', pch=16)



# interestingly, this reveals there is a spatial component to these
uniqueSegs <- traffic %>% select(position, lon, lat) %>% unique()
events.sf <- events.summary %>% left_join(uniqueSegs, by='position') %>% 
  st_as_sf(coords=c('lon','lat'))

tmap_mode('view')
tm_shape(events.sf) + tm_dots('sumNight', title='Sum of Night Events',
                              style='cont', breaks=c(0,20,40,60,80),
                              palette='-magma') + tm_scale_bar()
  
tm_shape(events.sf) + tm_dots('sumDay', title='Sum of Day Events',
                              style='cont',breaks=c(0,20,40,60,80),
                              palette='-magma') + tm_scale_bar()
seq(0,80,20)









