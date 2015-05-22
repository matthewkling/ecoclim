

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
            x <- matrix(raster::as.array(x),nrow=nrow(x),ncol=ncol(x))
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



####################

#' Inverted pwdSample for presence-absence data.
#'
#' Select a subsample of evaluation presence and absence points, each of which
#' is equidistant to the nearest training presence and absence. This *may* help
#' remove bias generated by spatial autocorrelation in the same way that
#' dimso::pwdSample does for presence-only data.
#' @param TP, TA, EP, EA Spatial points objects: Training and Evaluation,
#'   Presence and Absence.
#' @param tr Numeric: maximum threshold difference between an eval point's
#'   distance to nearest TA and TP to be considered similar.
#' @param max_points Integer: largest number of solutions to return for
#'   evaluation presence and absence points.
#' @return A list of two spatial points objects, one for presence evals and one
#'   for absence evals.
pwdSamplePA <- function (TP, TA, EP, EA, tr = 0.2, max_points=1000){
      distHaversine <- function(p1, p2) {
            r <- 6378137
            toRad <- pi/180
            p1 <- p1 * toRad
            p2 <- p2 * toRad
            p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2])
            dLat <- (p[, 4] - p[, 2])
            dLon <- (p[, 3] - p[, 1])
            a <- sin(dLat/2) * sin(dLat/2) + cos(p[, 2]) * cos(p[,
                                                                 4]) * sin(dLon/2) * sin(dLon/2)
            dist <- 2 * atan2(sqrt(a), sqrt(1 - a)) * r
            as.vector(dist)
      }
      distfun <- function(x, y) {
            n <- nrow(x)
            m <- nrow(y)
            dm <- matrix(ncol = m, nrow = n)
            for (i in 1:n) {
                  dm[i, ] <- distHaversine(x[i, , drop = FALSE], y)
            }
            return(dm)
      }

      if (inherits(TP, "SpatialPoints")) TP <- coordinates(TP)
      if (inherits(TA, "SpatialPoints")) TA <- coordinates(TA)
      if (inherits(EP, "SpatialPoints")) EP <- coordinates(EP)
      if (inherits(EA, "SpatialPoints")) EA <- coordinates(EA)

      TP <- as.matrix(TP)[, 1:2]
      TA <- as.matrix(TA)[, 1:2]
      EP <- as.matrix(EP)[, 1:2]
      EA <- as.matrix(EA)[, 1:2]

      # EVAL PRESENCE
      pdists <- distfun(EP, TP) # dists from each eval presence to each training presence
      adists <- distfun(EP, TA)
      mpdist <- apply(pdists, 1, min) # dist from each eval presence to nearest training presence
      madist <- apply(adists, 1, min)
      mdists <- as.data.frame(cbind(mpdist, madist))
      mdists$valid <- apply(mdists, 1, function(x) max(x) / min(x) < (1 + tr))
      EP <- EP[mdists$valid,]

      # EVAL ABSENCE
      pdists <- distfun(EA, TP) # dists from each eval presence to each training abs
      adists <- distfun(EA, TA)
      mpdist <- apply(pdists, 1, min) # dist from each eval presence to nearest training abs
      madist <- apply(adists, 1, min)
      mdists <- as.data.frame(cbind(mpdist, madist))
      mdists$valid <- apply(mdists, 1, function(x) max(x) / min(x) < (1 + tr))
      EA <- EA[mdists$valid,]

      rows <- min(nrow(EA), nrow(EP), max_points, na.rm=T)
      EP <- EP[sample(1:nrow(EP), rows),]
      EA <- EA[sample(1:nrow(EA), rows),]
      EP <- as.data.frame(EP)
      coordinates(EP) <- c("x", "y")
      EA <- as.data.frame(EA)
      coordinates(EA) <- c("x", "y")

      return(list(eval_pres=EP, eval_abs=EA))
}





####################

#' Turn a data matrix into a raster stack.
#'
#' Create a raster stack from a matrix, where each matrix row is a pixel and
#' each column is a layer.
#'
#' @param data A matrix.
#' @param template A raster layer to be used as spatial template; dims must
#'   match data.
#' @return A raster stack.
stackMatrix <- function(data, template){
      n <- colnames(data)
      s <- lapply(1:ncol(data), function(x) raster(as.matrix(data[,x]), template=template))
      s <- do.call("stack", s)
      names(s) <- n
      s
}


####################

#' Stack rasters with differing extents.
#'
#' The native raster::stack function fails if the input layers don't have
#' identical extents. This is a workaround that extends each input layer by a
#' buffer, and then intersects them. It will only work if all input layers share
#' other key spatial characteristics like resolution, projection, etc.
#'
#' @param x Layers to stack, as a vector of raster file paths or a list of
#'   raster layers.
#' @param intersect Logical: Should only the minimum intersection of the input
#'   layers be retained? (If FALSE, the default, layers will be extended to
#'   ratain all data.)
#' @return A raster stack.
motleyStack <- function(x, intersect=F){
      if(class(x) == "list"){
            m <- x
      } else{
            m <- lapply(x, raster)
      }

      # determine minimal buffer size
      b <- lapply(m, function(z) as.vector(raster::extent(z)))
      b <- do.call("rbind", b)
      b <- apply(b, 2, function(z) max(z) - min(z))
      b <- ceiling(max(b) / min(res(m[[1]]))) + 1
      if(intersect) buffer <- 0
      if(!intersect) buffer <- b

      # extend and intersect layers
      m <- lapply(m, function(z) extend(z, buffer))
      e <- lapply(m, extent)
      for(i in 1:length(e)) e <- lapply(e, function(z) raster::intersect(z, e[[i]]))
      m <- lapply(m, function(z) crop(z, e[[1]]))
      do.call("stack", m)
}
