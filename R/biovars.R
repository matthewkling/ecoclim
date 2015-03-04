

#' Calculate the 19 bioclimatic variables.
#'
#' This function calculates the 19 bioclimatic variables based on monthly precip
#' and monthly min and max temperatures. It is very similar to dismo::biovars
#' but differs in a couple key respects. First, it's much much faster. Second,
#' it only works on vectors, so to use it on rasters it should be paired with
#' raster::calc. Third, it works on time series in addition to normals, if fed
#' the right data. Fourth, Bio4 is not multiplied by 100 (dismo just does that
#' to save some resources). And finally, for Bios 8, 9, 18, and 19, if multiple
#' quarters have the same min or max value, this version computes the mean
#' rather than simply using the first one.
#'
#' @param x Numeric vector of length 36 or 42. If calculating biovars for a
#'   climate normal, should be 1 value per month for precip, tmax, and tmin,
#'   arranged p1:p12 tx1:tx12 tn1:tn12. If calculating for a time series, uses
#'   14 months per variable, with 13 and 14 being Jan-Feb of the next year. This
#'   is automtically determined based on the length of the input vector.
#' @return Numeric vector of length 19, representing the 19 bioclimatic variables
#'   in order.
biovariables <- function(x){

      if(is.na(x[1])) return(rep(NA, 19))

      # arrange
      x <- matrix(c(x, rep(NA, length(x))), ncol=6)

      # if only 12 months are provided, december loops to january
      if(nrow(x)==12) x <- rbind(x, x[1:2,])

      # derive intermediate monthly variables
      x[,4] <- (x[,2] + x[,3])/2 # monthly mean temperatures
      x[1:12,5] <- caTools::runmean(x[,1], 3, alg="fast", endrule="trim", align="left") * 3 # rolling quarterly ppt
      x[1:12,6] <- caTools::runmean(x[,4], 3, alg="fast", endrule="trim", align="left") # rolling quarterly tmean

      # current-year months
      x <- x[1:12,]

      # annual averages
      means <- colMeans(x)

      # bio1: annual mean temperature
      b1 <- means[4]

      # bio2: annual mean diurnal range
      b2 <- means[2] - means[3]

      # bio4: temperature seasonality (SD)
      # this differs from hijmans in that he multiplies the result by 100 to preserve sig figs and store as an integer. we just store the raw float.
      b4 <- sd(x[,4])

      # bio4 alternate: temperature seasonality (CV)
      # decided not to include this alternate bio4 that had been proposed by USGS -- cv isn't as inherently meaningful for temp as for precip.
      #b4a <- b4 / (b1 + 273.15) * 100

      # bio5: max temperature of warmest month
      b5 <- max(x[,2])

      # bio6: min temperature of coldest month
      b6 <- min(x[,3])

      # bio7: annual temperature range
      b7 <- b5 - b6

      # bio3: isothermality
      b3 <- b2 / b7 * 100

      # bio10: mean temperature of warmest quarter
      b10 <- max(x[,6])

      # bio11: mean temperature of coldest quarter
      b11 <- min(x[,6])

      # bio12: annual precipitation
      b12 <- means[1] * 12

      # bio13: precipitation of wettest month
      b13 <- max(x[,1])

      # bio14: precipitation of driest month
      b14 <- min(x[,1])

      # bio16: precipitation of wettest quarter
      b16 <- max(x[,5])

      # bio17: precipitation of driest quarter
      b17 <- min(x[,5])

      # bio15: precipitation seasonality (CV)
      b15 <- sd(x[,1]) / (1 + means[1]) * 100

      # bio8: mean temperature of wettest quarter
      # biovars 8, 9, 18, 19 differ from hijmans method in that take the average, rather than the first, if n>1
      b8 <- mean(x[,6][x[,5]==b16])

      # bio9: mean temperature of driest quarter
      b9 <- mean(x[,6][x[,5]==b17])

      # bio18: precipitation of warmest quarter
      b18 <- mean(x[,5][x[,6]==b10])

      # bio19: precipitation of coldest quarter
      b19 <- mean(x[,5][x[,6]==b11])

      return(c(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19))
}
