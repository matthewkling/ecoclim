

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
            r <- raster::subset(r, band)
            if(i==paths[1]) s <- r else{s <- stack(s, r)}
      }
      s
}


####################

#' Faster version of raster::trim.
#'
#' Raster::trim can be insanely slow for large files. This alternative, modified
#' slightly from
#' http://blog.snap.uaf.edu/2012/09/10/speeding-up-r-raster-package-trim-function/,
#' runs way faster. But unlike the raster package version there's no guarantee
#' it won't run into memory limitations.
#' @param x Raster or matrix.
#' @param out Output file format, either "raster", or "matrix".
trimFast <- function(x,out="raster"){
      if(!any(out==c("matrix","raster"))) stop("output must be a matrix or raster")
      if(class(x)=="matrix" & out=="raster") stop("if you supply a matrix, you must use out='matrix'")
      if(class(x)=="RasterLayer") {
            if(out=="raster") { cres <- 0.5*res(x); crs <- projection(x); y <- x }
            x <- matrix(as.array(x),nrow=nrow(x),ncol=ncol(x))
      }
      if(class(x)!="matrix") { stop("x must be a matrix or raster")
      } else {
            r.na <- c.na <- c()
            for(i in 1:nrow(x)) r.na <- c(r.na, all(is.na(x[i,])))
            for(i in 1:ncol(x)) c.na <- c(c.na, all(is.na(x[,i])))
            r1 <- 1 + which(diff(which(r.na))>1)[1]
            r2 <- nrow(x) -  which(diff(which(rev(r.na)))>1)[1]
            c1 <- 1 + which(diff(which(c.na))>1)[1]
            c2 <- ncol(x) - which(diff(which(rev(c.na)))>1)[1]
            x <- x[r1:r2,c1:c2]
            if(out=="raster") {
                  xs <- xFromCol(y,col=c(c1,c2)) + c(-1,1)*cres[1]
                  ys <- yFromRow(y,row=c(r2,r1)) + c(-1,1)*cres[2]
                  x <- raster(x,xmn=xs[1],xmx=xs[2],ymn=ys[1],ymx=ys[2],crs=crs)
            }
      }
      return(x)
}


