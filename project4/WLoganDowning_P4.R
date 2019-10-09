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






