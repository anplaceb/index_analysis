# Script with functions

library(terra)
print("Access to functions")

# Define bands
im2list <- function (image){
  # Loads the bands of an image into a list of rasters
  # Input can be either the raster object or the path to it
  
  if(is.character(image) == 1 ){image <- rast(image)} # if path is given, load
  # as image
  return(c('blue' = image[[1]], 
           'green' = image[[2]],
           'red' = image[[3]], 
           're1' = image[[4]],
           're2' = image[[5]], 
           're3' = image[[6]],
           'nir' = image[[7]], 
           'nir2' = image[[8]],
           'swir1' = image[[9]], 
           'swir2' = image[[10]]
           )
           )
}

norm_diff <- function(x,y){round((x-y)/(x+y)*100 + 100)}
mcari_index <- function(x, y, z) {round(((x - y) - (x - z)) * (x/y))}
satvi_index <- function(x, y, z)  {round(((x - y) / (x + y + 5000)) * (15000) - (z/ 2))}
savi_index <- function(x, y) {round((15000*(x-y))/((x+y+5000)))}

# Functions for indices
calculate_indices <- function(image){
  bands <- im2list(image)
  return(c('ndvi'= lapp(x = c(bands$nir, bands$red), fun=norm_diff), 
         'mcari' = lapp(x=c(bands$re1, bands$red, bands$green), fun=mcari_index),
         'satvi'= lapp(x = c(bands$swir1, bands$red, bands$swir2), fun=satvi_index),
         'nbr' = lapp(x = c(bands$nir, bands$swir2), fun=norm_diff),
         'nbr2' = lapp(x = c(bands$nir2, bands$swir2), fun=norm_diff),
         'gndvi'= lapp(x = c(bands$nir, bands$green), fun=norm_diff),
         'savi'= lapp(x = c(bands$nir, bands$red), fun=savi_index),
         'ndwi' = lapp(x = c(bands$green, bands$nir), fun=norm_diff),
         'i1' = lapp(x = c(bands$re1, bands$red), fun=norm_diff),
         'i2' = lapp(x = c(bands$re1, bands$swir1), fun=norm_diff),
         i3 = lapp(x = c(bands$re1, bands$swir2), fun=norm_diff),
         i4 = lapp(x = c(bands$red, bands$swir1), fun=norm_diff),
         i5 = lapp(x = c(bands$red, bands$swir2), fun=norm_diff),
         i6 = lapp(x = c(bands$swir1, bands$swir2), fun=norm_diff)
         ))
}
 