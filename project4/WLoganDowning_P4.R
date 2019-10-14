library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)


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
    y <- diff(i[[1]][,2])
    x <- diff(i[[1]][,1])
    
    perimeters <- append(perimeters, sum(sqrt(y^2+x^2)))
  }
  
  return(perimeters)
}

# sapply(data$geometry, function(x) x[[1]])

ld_perimeter(data)

# this is failing on multipolygons ...

# you can add an st_is check to determine if it is a multipolygon

for (i in data$geometry) {
  print(i[[1]][,1])

}
i[[1]]

i[[1]][,2]
i[[1]][[1]]

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






