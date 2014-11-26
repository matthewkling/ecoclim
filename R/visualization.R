



######################################

#' Adjust coordinates for small multiples plotting.
#'
#' This function is used to progressively offset the coordinates of map layers
#' in long-format tabulated geographic data so that they can be plotted tightly
#' with overlapping extents. It is designed for use on multi-layer raster data
#' that has been converted into a data frame.
#'
#' It returns a vector of adjusted coordinates equal in
#' length to the input coordinates.
#'
#' @param target Numeric vector of coordinates to be adjusted.
#' @param group Numeric vector of group factors, equal in length to target.
#' @param offset Numeric value representing offset distance per group.
#' @param summary Logical

flake <- function(target, group, offset, summary=F){

      # set numbering for offset facets
      groups <- as.integer(as.factor(group)) - 1

      # rounding resolution
      steps <- target[2:length(target)] - target[1:length(target)-1]
      resolution <- median(steps[steps != 0], na.rm=T)

      if(summary==T){
            target <- min(target, na.rm=T)
            l <- plyr::round_any(target + offset * groups, resolution, floor)
            l <- unique(l)
            names(l) <- levels(as.factor(group))
            return(l)
      }

      plyr::round_any(target + offset * groups, resolution, floor)
}


########################################

#' ggplot2's element_blank().
#'
#' Abbreviated version of the element_blank() function from ggplot2
eb <- function() ggplot2::element_blank()
