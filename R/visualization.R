



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
axes <- function(axis="xy", xintercept=0, yintercept=0, color="gray", size=1){
      if(axis=="y"){return(geom_vline(xintercept=xintercept, color=color, size=size))}
      if(axis=="x"){return(geom_hline(yintercept=yintercept, color=color, size=size))}
      if(axis=="xy"){return(list(geom_vline(xintercept=xintercept, color=color, size=size),
                                 geom_hline(yintercept=yintercept, color=color, size=size)))}
}

########################################

#' White panel
#'
#' Set ggplot panel background and grid to blank.
whiteness <- function(){ggplot2::theme(panel.background=element_blank(), panel.grid=element_blank())}

#######################################

#' Density level for a percentage contour
#'
#' Get the 2D density breakpoint needed to generate a contour encompassing a
#' specific percentage of probability density.
#'
#' @param x,y Numeric vectors corresponding to scatterplot coordinates.
#' @param prob Vector of numerics between 0 and 1, representing the proportion of probability density to include in the contour.
#' @return A vector of length equal to probs, specifying the breakpoints corresponding to each prob.
contourLevel <- function(x,y,prob=0.95) {
      kk <- MASS::kde2d(x,y)
      dx <- diff(kk$x[1:2])
      dy <- diff(kk$y[1:2])
      sz <- sort(kk$z)
      c1 <- cumsum(sz) * dx * dy
      approx(c1, sz, xout = 1 - prob)$y
}
