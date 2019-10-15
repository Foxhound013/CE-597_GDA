library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(rgeos)


# 1. Given a building shapefile of Purdue campus, make an R program to calculate 
#   (you need to implement the formulas, 4 pts)
#
#   1) the length (perimeter), area and centroid for each building
#   2) add the above results as 4 columns associate with each building
#   3) do the same with the built-in functions in R by adding another 4 columns
#   4) evaluate/summarize the differences (histogram, mean, standard deviation etc)

raw_data <- st_read('./data/raw/buildings_shpfile', layer='buildings')

# drop any invalid polygons
data <- raw_data[which(st_is_valid(raw_data)),]
# drop empty polygons
data <- data[which(!st_is_empty(data)),]

# quickly get a view of the data
tmap_mode('view')
tm_shape(raw_data) + tm_polygons()

tm_shape(data) + tm_polygons()

# get a quick look at a polygon to understand structure
tmp <- data$geometry[1][[1]][1] 
# above, we're requesting the first polygon, the first list of that polygon, and
# the first batch of data in that list (which happens to be 5 row x 2 column matrix.)

# all of your polygons should be two columns with N number of row.

# Perimeter Calculation
# Formula: Sum( sqrt( (yi+1 - yi)^2 + (xi+1 - xi)^2) )

# accepts a dataframe but expects a geometry column
ld_perimeter <- function(data) {
  perimeters <- list()

  for (i in data$geometry) {
    # check polygon type
    if (st_is(i,'POLYGON')) {
      y <- diff(i[[1]][,2])
      x <- diff(i[[1]][,1])
      
      perimeters <- append(perimeters, sum(sqrt(y^2+x^2)))
    } else if (st_is(i,'MULTIPOLYGON')){
      multi_perim <- 0
      for (j in 1:length(i)) {
        y <- diff(i[[j]][[1]][,2])
        x <- diff(i[[j]][[1]][,1])
        
        multi_perim <- multi_perim + sum(sqrt(y^2+x^2))
      }
      #sum the multipolygon
      perimeters <- append(perimeters, multi_perim)
    }
  }
  return(unlist(perimeters))
}

data$ld_perim <- ld_perimeter(data)
summary(data$ld_perim-data$PERIMETER)
# my method doesn't seem to be perfect. There are some strange edge cases but the 1st and 3rd
# quartiles are fine. (compared to the inbuilt perimeters)


# Area Calculation
# Formula: 1/2 * sum( (xi+1-xi)*(yi+yi+1) )

tmp <- data$geometry
y <- head(tmp[[1]][[1]][,2], -1) + tail(tmp[[1]][[1]][,2], -1)
x <- diff(tmp[[1]][[1]][,1])

.5 * sum(y*x)
st_area(data[1,])

ld_area <- function(data) {
  area <- list()
  
  for (i in data$geometry) {
    # check polygon type
    if (st_is(i,'POLYGON')) {
      y <- head(i[[1]][,2], -1) + tail(i[[1]][,2], -1)
      x <- diff(i[[1]][,1])
      
      area <- append(area, .5 * sum(y*x))
    } else if (st_is(i,'MULTIPOLYGON')) {
      multi_area <- 0
      for (j in 1:length(i)) {
        y <- head(i[[j]][[1]][,2], -1) + tail(i[[j]][[1]][,2], -1)
        x <- diff(i[[j]][[1]][,1])
        multi_area <- multi_area + .5 * sum(y*x)
      }
      
      area <- append(area, multi_area)
    }
  }
  return(unlist(area))
}

data$ld_area <- ld_area(data)
summary(data$ld_area - data$Shape_Area)
# everything appears to be good, there are some discrepancies but overall it's dead on.

# Centroid Calculation
# Formulas:
#   x_bar = 1/(6A) * Sum( (yi+1 - yi) * (xi^2 + xi*xi+1 + xi+1^2) )
#   y_bar = 1/(6A) * Sum( (xi - xi+1) * (yi^2 + yi*yi+1 + yi+1^2) )

#x_bar
y <- diff(data$geometry[[1]][[1]][,2])
x <- head(data$geometry[[1]][[1]][,1], -1)^2 + 
  head(data$geometry[[1]][[1]][,1], -1)*tail(data$geometry[[1]][[1]][,1], -1) +
  tail(data$geometry[[1]][[1]][,1], -1)^2
  
x_bar <- (1/(6*data$ld_area[1]) * sum(y*x))*-1


x <- head(data$geometry[[1]][[1]][,1], -1) - tail(data$geometry[[1]][[1]][,1], -1)
y <- head(data$geometry[[1]][[1]][,2], -1)^2 +
  head(data$geometry[[1]][[1]][,2], -1)*tail(data$geometry[[1]][[1]][,2], -1) +
  tail(data$geometry[[1]][[1]][,2], -1)^2

y_bar <- (1/(6*data$ld_area[1]) * sum(y*x))*-1

st_geometry(tmp) <- st_centroid(data[1,])
plot(tmp$geometry)
plot(st_centroid(tmp$geometry), add=T, pch=3)
tmp$geometry
pts <- do.call(rbind, st_geometry(data)) 
# the above line is just to pull the centroid back out of the attributes

rbind(st_geometry(tmp))

# this function will return two columns of x and y centroid values
# area should be calculated prior to running this function
# requires a geometry column and area column. Area must be defined when called
ld_centroid <- function(data, area) {
  data <- data$geometry
  
  # Initialize a container to store centroid data
  centroids <- data.frame(x_bar = rep(NA, length(data)),
                          y_bar = rep(NA, length(data))) 
  
  for (i in 1:length(data)) {
    # verify polygon type
    if (st_is(data[[i]], 'POLYGON')) {
      #x_bar
      y <- diff(data[[i]][[1]][,2])
      x <- head(data[[i]][[1]][,1], -1)^2 + 
        head(data[[i]][[1]][,1], -1)*tail(data[[i]][[1]][,1], -1) +
        tail(data[[i]][[1]][,1], -1)^2
      
      x_bar <- (1/(6*area[i]) * sum(y*x))*-1
      
      #y_bar
      x <- head(data[[i]][[1]][,1], -1) - tail(data[[i]][[1]][,1], -1)
      y <- head(data[[i]][[1]][,2], -1)^2 +
        head(data[[i]][[1]][,2], -1)*tail(data[[i]][[1]][,2], -1) +
        tail(data[[i]][[1]][,2], -1)^2
      
      y_bar <- (1/(6*area[i]) * sum(y*x))*-1
      
      centroids[i, ] <- rbind(x_bar, y_bar)
    } else if (st_is(data[[i]], 'MULTIPOLYGON')) {
      x_list <- rep(list(NULL),length(data[[i]]))
      y_list <- rep(list(NULL),length(data[[i]]))
      next()
    #   for (j in 1:length(data[[i]])) {
    #     #x_bar
    #     y <- diff(data[[i]][[j]][[1]][,2])
    #     x <- head(data[[i]][[j]][[1]][,1], -1)^2 + 
    #       head(data[[i]][[j]][[1]][,1], -1)*tail(data[[i]][[j]][[1]][,1], -1) +
    #       tail(data[[i]][[j]][[1]][,1], -1)^2
    #     
    #     x_bar <- (1/(6*area[i]) * sum(y*x))*-1# this bit isn't technically right.
    #     # to fix it, it would require tweaking the area function to return the individual
    #     # areas for each polygon
    # 
    #     #y_bar
    #     x <- head(data[[i]][[j]][[1]][,1], -1) - tail(data[[i]][[j]][[1]][,1], -1)
    #     y <- head(data[[i]][[j]][[1]][,2], -1)^2 +
    #       head(data[[i]][[j]][[1]][,2], -1)*tail(data[[i]][[j]][[1]][,2], -1) +
    #       tail(data[[i]][[j]][[1]][,2], -1)^2
    #     
    #     y_bar <- (1/(6*area[i]) * sum(y*x))*-1 # this bit isn't technically right.
    #     # to fix it, it would require tweaking the area function to return the individual
    #     # areas for each polygon
    #     
    #     # now, you need to store each multipoly's center to a list, for future storage in 
    #     # the data frame.
    #     x_list[[j]] <- append(x_list[[j]], x_bar)
    #     y_list[[j]] <- append(y_list[[j]], y_bar)
    #   }
    #   x_list <- sapply(x_list, cbind)
    #   y_list <- sapply(y_list, cbind)
    #   
    #   print(centroids[890:898,])
    #   centroids[[1]][i][[1]] <- x_list
    #   centroids[[2]][i][[1]] <- y_list
    }
  }
  return(centroids)
}

centroid_data <- ld_centroid(data, data$ld_area)
data$ld_centroid_x <- centroid_data$x_bar
data$ld_centroid_y <- centroid_data$y_bar


# calculate perimeter with inbuilt function
data$builtIn_perimeter <- st_length(st_cast(data, 'MULTILINESTRING'))
data$builtIn_area <- st_area(data)
pts <- data.frame(st_geometry(data) %>% st_centroid %>% st_coordinates)
data$builtIn_centroidX <- pts$X
data$builtIn_centroidY <- pts$Y



# 2. Given the WL tweets data of year 2014, make an R program to (use built-in functions, 6pts)
#
#   1) Find the closest tweets for each building, add the sum (the number of tweets) to that
#     building
#   2) For the tweets data frame( layer), associate the closest building ID to each tweet
#   3) Find the “busiest” buildings in WL (campus)
#   4) Evaluate/discuss the ‘busiest’ buildings in terms of time period (e.g. day or night etc.)
#   5) Show your results with appropriate maps
#     - Note – if the distance of a tweet to the closest building is too large, you may want
#       to discard that tweet from this building
#     - For spatial operation command, check
#       https://r-spatial.github.io/sf/reference/index.html; there is an “Articles” pull down
#       button. Also look at https://github.com/rstudio/cheatsheets/blob/master/sf.pdf
#     - Option – you may download OSM buildings of WL (see my early notes) and use these
#       verse the one I provided (which has only Purdue buildings)






