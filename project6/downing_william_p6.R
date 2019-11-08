library(sf)
library(dplyr)
library(spdep)

# 1. Data
#   1) Indiana County Census data

counties <- st_read('./data/raw/census_counties', layer='Census_County_TIGER00_IN')

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

png('./figures/pop2000_countyJenks.png')
plot(counties['POP2000'], breaks='jenks', key.pos=4, main='Population by County for Year 2000',
     axes=T, xlab='Longitude (m)', ylab='Latitude (m)')
dev.off()
# maybe add a comparison of neighborhood weights

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
png('./figures/males_map.png')
plot(counties['MALES'], breaks='jenks', key.pos=1, main='Spatial Distribution of Males', axes=T)
dev.off()
png('./figures/females_map.png')
plot(counties['FEMALES'], breaks='jenks', key.pos=1, main='Spatial Distribution of Females', axes=T)

# Female population is smaller, especially in rural areas.

# using weight.r.ww
moran.test(counties$MALES, listw=weight.r.ww, randomisation=F)
png('./figures/moran_test_males.png')
moran.plot(counties$MALES, listw=weight.r.ww, main="Moran's I - Male Population - Row Standardized",
           xlab='Populations', ylab='Spatially Lagged Populations')
dev.off()

moran.test(counties$FEMALES, listw=weight.r.ww, randomisation=F)
png('./figures/moran_test_females.png')
moran.plot(counties$FEMALES, listw=weight.r.ww, main="Moran's I - Female Population - Row Standardized",
           xlab='Populations', ylab='Spatially Lagged Populations')
dev.off()


mc.male <- moran.mc(counties$MALES, listw=weight.r.ww, nsim=5000); mc.male
png('./figures/moran_mc_males.png')
plot(mc.male, type='b', pch=16, col='deepskyblue3', xlab='Male Population', 
     ylim=c(0,10), xlim=c(-0.2, 0.4))
dev.off()

mc.female <- moran.mc(counties$FEMALES, listw=weight.r.ww, nsim=5000); mc.female
png('./figures/moran_mc_females.png')
plot(mc.female, type='b', pch=16, col='deepskyblue3', xlab='Female Population', 
     ylim=c(0,10), xlim=c(-0.2, 0.4))
dev.off()

# 5. Local Moran’s I evaluation (3 pts)
#   1) Use the same two attributes as above
#   2) Calculate local Moran (localmoran)
#   3) Use histogram and map of local Moran I’s to identify and discuss possible spatial clusters
#   4) When needed, thematically map these two attributes for discussion; find the names of
#     the interest counties

localm.male <- localmoran(counties$MALES, listw=weight.r.ww)
localm.female <- localmoran(counties$FEMALES, listw=weight.r.ww)

pop.male <- cbind(counties, localm.male)
png('./figures/male_hist.png')
hist(pop.male$Ii, main="Local Moran's I for the Male Population", xlab="Local i")
dev.off()
png('./figures/male_local_i.png')
plot(pop.male['Ii'], breaks='jenks', key.pos=1, main="Local Moran's I - Male Population", axes=T)
dev.off()

pop.female <- cbind(counties, localm.female)
png('./figures/female_hist.png')
hist(pop.female$Ii, main="Local Moran's I for the Female Population", xlab="Local i")
dev.off()
png('./figures/female_local_i.png')
plot(pop.female['Ii'], breaks='jenks', key.pos=1, main="Local Moran's I - Female Population", axes=T)
dev.off()

# 6. Bonus (2 pts; 1 day after due)
#   1) Make a function to map the HH,LL, HL,LH counties