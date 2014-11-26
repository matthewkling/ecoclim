

#############################################

# this function loads study area boundary shapefile

getBoundary <- function(directory, filename){
      w <- getwd()
      setwd(directory)
      b <- readOGR(dsn=directory, layer=filename)
      setwd(w)
      return(b)
}
loadShapefile <- getBoundary

############################################

# this function clips climate raster by boundary

crp <- function(raster, polygon){
      raster <- crop(raster, polygon)  # crop raster to boundary extent (rectangle)
      if(class(polygon)!="RasterLayer"){polygon <- rasterize(polygon, raster)}  # convert boundary to raster mask
      projection(raster) <- projection(polygon)  # force sync coordinate systems -- they must be known to match!
      mask(raster, polygon)
}


# wrapper function
boundaryCrop <- function(raster, boundary.directory, boundary.filename) {
      boundary <- getBoundary(boundary.directory, boundary.filename)
      crp(raster, boundary)
}


