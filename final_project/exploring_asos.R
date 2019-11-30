library(dplyr)
library(tidyr)
library(sf)
library(data.table)
library(lattice)

col.classes <- c('factor', 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
               'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'character',
               'character', 'character', 'numeric', 'numeric', 'numeric', 'character',
               'character' , 'numeric', 'character')
col.names <- c('station', 'tstamp', 'lon', 'lat', 'tmpf', 'dwpf', 'rh', 'drct', 'spd', 'mslp',
               'precip_1hr_mm', 'vis', 'gust_mph', 'skyc1', 'skyc2', 'skyc3', 'skyl1', 'skyl2',
               'skyl3', 'wxcodes', 'peak_wind_gust_mph', 'peak_wind_drct', 'peak_wind_time')
asos <- fread('./data/raw/indiana_asos_all_june.txt', header=T, sep=',', 
              na.strings=c('null'), colClasses=col.classes, col.names=col.names)

asos$tstamp <- as.POSIXct(asos$tstamp, tz='utc', origin='1970-01-01 00:00:00')
asos$peak_wind_time <- as.POSIXct(asos$peak_wind_time, tz='utc', origin='1970-01-01 00:00:00')

na_counts <- asos %>% group_by(station) %>% 
  summarize(tstamp=length(unique(tstamp)),
            tmpf=sum(is.na(tmpf))/length(tmpf) * 100,
            dwpf=sum(is.na(dwpf))/length(dwpf) * 100,
            rh=sum(is.na(rh))/length(rh) * 100,
            drct=sum(is.na(drct))/length(drct) * 100,
            spd=sum(is.na(spd))/length(spd) * 100,
            mslp=sum(is.na(mslp))/length(mslp) * 100,
            precip_1hr_mm=sum(is.na(precip_1hr_mm))/length(precip_1hr_mm) * 100,
            vis=sum(is.na(vis))/length(vis) * 100,
            gust_mph=sum(is.na(gust_mph))/length(gust_mph) * 100,
            skyc1=sum(is.na(skyc1))/length(skyc1) * 100,
            skyc2=sum(is.na(skyc2))/length(skyc2) * 100,
            skyc3=sum(is.na(skyc3))/length(skyc3) * 100,
            skyl1=sum(is.na(skyl1))/length(skyl1) * 100,
            skyl2=sum(is.na(skyl2))/length(skyl2) * 100,
            skyl3=sum(is.na(skyl3))/length(skyl3) * 100,
            wxcodes=sum(is.na(wxcodes))/length(wxcodes) * 100,
            peak_wind_gust_mph=sum(is.na(peak_wind_gust_mph))/length(peak_wind_gust_mph) * 100,
            peak_wind_drct=sum(is.na(peak_wind_drct))/length(peak_wind_drct) * 100,
            peak_wind_time=sum(is.na(peak_wind_time))/length(peak_wind_time) * 100)

highTimeRes <- na_counts[na_counts$tstamp > (4*24*30),]
asos.sub <- asos[,c('station', 'tstamp', 'lon', 'lat', 'spd', 'drct', 'vis')] 
asos.sub <- asos.sub %>% filter(asos.sub$station == highTimeRes$station)

xyplot(spd~tstamp | factor(station), data=asos.sub, type='l', layout=c(1,4))

start.t = as.POSIXct('2018-06-01 00:00:00', tz='UTC', origin='1970-01-01 00:00:00')
end.t = as.POSIXct('2018-06-30 23:45:00', tz='UTC', origin='1970-01-01 00:00:00')
dateSeq = data.frame(tstamp=seq.POSIXt(from=start.t, to=end.t, by='15 min'))

tmp <- dateSeq %>% left_join(asos.sub, by='tstamp')

tmp <- asos.sub %>% filter(station=='LAF')
tmp2 <- tmp %>% right_join(dateSeq, by='tstamp')
tmp2$station <- rep('LAF', times=length(tmp2$station))
xyplot(spd~tstamp | factor(station), data=tmp2, type='l', layout=c(1,1))
plot(tmp2$tstamp, tmp2$vis, type='l', col='red')


pdf('./figures/station_tmps.pdf')
xyplot(spd~tstamp | factor(station), data=tmp2, type='l', layout=c(1,4))
dev.off()

