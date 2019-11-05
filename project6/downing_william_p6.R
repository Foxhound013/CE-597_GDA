library(sf)
library(dplyr)
library(spdep)
library(ggplot2)

# 1. Data
#   1) Indiana County Census data

counties <- st_read('./data/raw/census_counties', layer='Census_County_TIGER00_IN')
st_crs(counties)

# 2. Spatial connectivity evaluation (1 pt)
#   1) Plot the connection of counties under two different neighborhood definitions
#   2) Evaluate the plots and the link summary
#   3) Select Rook (Queen= False) connection for subsequent analysis unless otherwise stated

cnxn.q <- poly2nb(counties, queen=T)
cnxn.r <- poly2nb(counties, queen=F)
xy <- st_centroid(counties$geometry)

options(scipen=6)
png('./figures/queen&rook_cnxns.png')
plot(counties$geometry, axes=T, main='Counties with Connections by Queen & Rook',
     ylab='Latitude (m)', xlab='Longitude (m)')
plot(cnxn.q, xy, add=T, col='red')
plot(cnxn.r, xy, add=T, col='blue')
dev.off()

# 3. Weight matrix evaluation (3 pts)
#   1) Form three weight matrices by using any three styles from the W, S, U, B, C options
#   2) Summarize and evaluate the Moran’s I statistics (function moran) for POP2000
#     (attribute) respectively for these three weight matrices
#   3) Show relevant plots and maps to support your discussions; discuss the spatial patterns
#     you would conclude for POP2000’s distribution under the weight matrices

# using cnxnx.r unless otherwise told to from this point forward.
weight.r.ww <- nb2listw(cnxn.r, style='W') # default: row standardized. Sums over all links to n
weight.r.wb <- nb2listw(cnxn.r, style='B') # Basic Binary
weight.r.wc <- nb2listw(cnxn.r, style='C') # Global. Sums over all links to n

#moran(counties$POP2000, listw=weight.r.ww, n=length(weight.r.ww$neighbours), S0=Szero(weight.r.ww))
# identical to moran.test, moran.test gives more info

mi.r.ww <- moran.test(counties$POP2000, weight.r.ww, randomisation=F); mi.r.ww
mi.r.wb <- moran.test(counties$POP2000, weight.r.wb, randomisation=F); mi.r.wb
mi.r.wc <- moran.test(counties$POP2000, weight.r.wc, randomisation=F); mi.r.wc

plot(counties['POP2000'], breaks='jenks', key.pos=1, main='Population by County for Year 2000',
     axes=T)


png('./figures/moransI_ww.png')
moran.plot(counties$POP2000, listw=weight.r.ww, main="Moran's I: Row Standardized",
           xlab='Populations', ylab='Spatially Lagged Populations')
dev.off()

png('./figures/moransI_wb.png')
moran.plot(counties$POP2000, listw=weight.r.wb, main="Moran's I: Binary",
           xlab='Populations', ylab='Spatially Lagged Populations')
dev.off()

png('./figures/moransI_wc.png')
moran.plot(counties$POP2000, listw=weight.r.wc, main="Moran's I: Global",
           xlab='Populations', ylab='Spatially Lagged Populations')
dev.off()

# 4. Moran’s I evaluation (3 pts)
#   1) Select two relevant attributes of your interest (other than POP2000)
#   2) Choose W as your weight matrix style
#   3) Use moran.test to visualize and discuss the spatial autocorrelation of these two
#     attributes, respectively
#   4) Use moran.mc to check the significance of the spatial autocorrelation of these two
#     attributes, respectively

# Looking at male vs female distribution
plot(counties['MALES'], breaks='jenks', key.pos=1, main='Spatial Distribution of Males', axes=T)
plot(counties['FEMALES'], breaks='jenks', key.pos=1, main='Spatial Distribution of Females', axes=T)
# Female population is smaller, especially in rural areas.

# using weight.r.ww
moran.test(counties$MALES, listw=weight.r.ww, randomisation=T)
moran.plot(counties$MALES, listw=weight.r.ww, main="Moran's I - Male Population - Row Standardized",
           xlab='Populations', ylab='Spatially Lagged Populations')
moran.test(counties$FEMALES, listw=weight.r.ww, randomisation=T)
moran.plot(counties$FEMALES, listw=weight.r.ww, main="Moran's I - Female Population - Row Standardized",
           xlab='Populations', ylab='Spatially Lagged Populations')

mc.male <- moran.mc(counties$MALES, listw=weight.r.ww, nsim=5000)
plot(mc.male, type='b', pch=16, col='deepskyblue3', xlab='Male Population', 
     ylim=c(0,10), xlim=c(-0.2, 0.4))
mc.female <- moran.mc(counties$FEMALES, listw=weight.r.ww, nsim=5000)
plot(mc.female, type='b', pch=16, col='deepskyblue3', xlab='Female Population', 
     ylim=c(0,10), xlim=c(-0.2, 0.4))

# 5. Local Moran’s I evaluation (3 pts)
#   1) Use the same two attributes as above
#   2) Calculate local Moran (localmoran)
#   3) Use histogram and map of local Moran I’s to identify and discuss possible spatial clusters
#   4) When needed, thematically map these two attributes for discussion; find the names of
#     the interest counties

localm.male <- localmoran(counties$MALES, listw=weight.r.ww)
localm.female <- localmoran(counties$FEMALES, listw=weight.r.ww)

pop.male <- cbind(counties, localm.male)
hist(pop.male$Ii)
plot(pop.male['Ii'], breaks='equal', key.pos=1, main="Local Moran's I - Male Population", axes=T)

pop.female <- cbind(counties, localm.female)
hist(pop.female$Ii)
plot(pop.female['Ii'], breaks='equal', key.pos=1, main="Local Moran's I - Female Population", axes=T)

# 6. Bonus (2 pts; 1 day after due)
#   1) Make a function to map the HH,LL, HL,LH counties