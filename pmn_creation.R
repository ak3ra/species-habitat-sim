setwd("C:/Users/akera/Documents/code/comp599_simulation")

# Load libraries
library(landscapeR)
library(raster)
library(sp)
library(rgdal)
library(reshape2)
library(foreach)
library(doParallel)
library(gdistance)
library(dplyr)
library(tidyverse)
library(igraph)
library(gtools)
library(Matrix)


# Define landscape input file
landscape.file.name <- "starting_landscape.tif"


#################################################_
##### 1. Habitat Metrics #####
###
###
### About this section:
###   This section imports a raster landscape (tiff file) and derives from it a series
###   of habitat metrics based on land cover. These metrics will later be used as inputs for
###   species distribution models.
#################################################_


##########################################################_
##### Calculating habitat metrics (inputs for SDMs) #####_
##########################################################_

# Import landscape file
rs <- raster(landscape.file.name)

# Create blank raster with same dimensions and CRS as landscape
r <- setValues(rs, 0)

##### Land cover calculations #####_

# Calculate land cover within radius
x <- coordinates(rs) # get the coordinates for each pixel in landscape

lc5 <- raster::extract(rs, x, buffer = 5, small = TRUE)


lc50 <- raster::extract(rs, x, buffer = 50, small = TRUE)
lc100 <- raster::extract(rs, x, buffer = 100, small = TRUE) 
lc250 <- raster::extract(rs, x, buffer = 250, small = TRUE) 

lc500 <- raster::extract(rs, x, buffer = 500, small = TRUE) 

### Forest cover in 100m radius 
# For loop to get percent cover of given class (or multiple classes) for each pixel
lc.prop <- NULL
for(i in 1:length(lc100)){
  lc.prop[i] <- sum(unlist(lc100[i]) %in% c(3,4))/length(unlist(lc100[i])) # change number to change cover class. change lc file name to change radius
}


# Plot percent cover 
r2 <- raster(nrows=100, ncols=100, xmn=656000, xmx=659000, ymn=5588000, 
             ymx=5591000, resolution=30, crs = CRS("+init=epsg:5362"))
lc.matrix <- as.matrix(lc.prop)
r2 <- setValues(r, lc.matrix)
plot(r2)

# Export raster
writeRaster(r2, "forest_cover_100m.tif", format = "GTiff")