

#############################################

#'  Load a shapefile.
#'
#'  This function loads a shapefile into memory for use in spatial or
#'  cartographic operations.
#'
#'  @param directory The full path to the file folder where the shapefile lives, as a character.
#'  @param filename The bare name of the shapefile, without extenstion, as a character.
#'  @return A spatial object.
#'  @aliases getBoundary

loadShapefile <- function(directory, filename){
      w <- getwd()
      setwd(directory)
      b <- rgdal::readOGR(dsn=directory, layer=filename)
      setwd(w)
      return(b)
}

############################################

#' Clip a raster.
#'
#' Convenience wrapper function that clips a raster object by a shapefile.
#' ASSUMES THE TWO OBJECTS HAVE MATCHING COORDINATE SYSTEMS.
#'
#' @param raster A *Raster object to be clipped.
#' @param polygon A spatial polygon object used for clipping.
#' @return A clipped *Raster object.
#' @aliases crp
clp <- function(raster, polygon){
      raster <- crop(raster, polygon)  # crop raster to boundary extent (rectangle)
      if(class(polygon)!="RasterLayer"){polygon <- rasterize(polygon, raster)}  # convert boundary to raster mask
      projection(raster) <- projection(polygon)  # force sync coordinate systems -- they must be known to match!
      mask(raster, polygon)
}

#######################

#' Wrapper around loading and clipping by a shapefile.
#'
#' This function loads a shapefile and then uses it to clip a raster object.
#'
#' @param raster A *Raster object to be clipped.
#' @param boundary.directory The file folder path of the boundary shapefile.
#' @param boundary.filename The bare filename (no extension) of the shapefile.
#' @return A clipped *Raster object.
boundaryCrop <- function(raster, boundary.directory, boundary.filename) {
      boundary <- getBoundary(boundary.directory, boundary.filename)
      crp(raster, boundary)
}


#######################

#' Split a Raster* object into strips.
#'
#' Partition a raster into vertical strips. Useful for parallel processing.
#'
#' @param raster A *Raster object to be partitioned.
#' @param nchunks Integer representing number of partitions.
#' @param chunk Integer representing which of the paritions to create.
#' @param subextent Optional extent object indicating a subdomain of the raster
#'   that should be partitioned.
#' @return A cropped strip of the input raster object.
partitionRaster <- function(raster, nchunks, chunk, subextent=NA){
      e <- extent(raster)
      if(!is.na(subextent)) e <- subextent
      x <- seq(e@xmin, e@xmax, (e@xmax-e@xmin)/(nchunks))
      e@xmin <- x[chunk] - 2 * res(raster)[1]
      e@xmax <- x[chunk+1]  + 2 * res(raster)[1]
      return(trim(crop(raster, e)))
}


####################

#' Stack up a single band from each of several multiband files.
#'
#' This is a simple convenience function that pulls a named or numbered band
#' from each of several multiband files and stacks them up.
#'
#' @param paths A character vecor of raster filenames.
#' @param band A single integer or srting denoting which layer to grab.
#' @return A raster stack with one layer from each file in paths.
stackBands <- function(paths, band){
      for(i in paths){
            r <- brick(i)
            r <- subset(r, band)
            if(i==paths[1]) s <- r else{s <- stack(s, r)}
      }
      s
}


