



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


#######################################

#' Add reference lines to a ggplot
#'
#' This is a convenience function that creates x and/or y axis reference lines
#' that can be added to a ggplot object.
#'
#' @param axis Character vector specifying the axis, either "x", "y", or "xy".
#' @param xintercept, yintercept Numeric value.
#' @param color Color.
#' @param size Size.
#' @return A ggplot layer or layers.
#'
axes <- function(axis, xintercept=0, yintercept=0, color="gray", size=1){
      if(axis=="y"){return(geom_vline(xintercept=xintercept, color=color, size=size))}
      if(axis=="x"){return(geom_hline(yintercept=yintercept, color=color, size=size))}
      if(axis=="xy"){return(list(geom_vline(xintercept=xintercept, color=color, size=size),
                                 geom_hline(yintercept=yintercept, color=color, size=size)))}
}

########################################

#' White panel
#'
#' Set ggplot panel background and grid to white.
whiteness <- function(){theme(panel.background=element_blank(), panel.grid=element_blank())}
