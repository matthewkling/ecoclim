


#' Circular data transformation.
#'
#' Add angle and distance variables to a data frame.
polarize <- function(data, xvar, yvar, xyratio, xorigin=0, yorigin=0){
      data$distance <- sqrt((data[,xvar]-xorigin)^2 + ((data[,yvar]-yorigin) * xyratio)^2)
      data$angle <- acos((data[,xvar]-xorigin) / data$distance) * 180 / pi
      data$angle[data[,yvar]<yorigin] <- 360 - data$angle[data[,yvar]<yorigin]
      return(data)
}



#' Generate circular legend data.
#'
#' Construct a data frame to be used in color wheel legend plotting.
cwdata <- function(data, xvar, yvar, resolution=10, origin=c(0,0)){
      xrange <- range(data[,xvar], na.rm=T)
      yrange <- range(data[,yvar], na.rm=T)
      xmag <- plyr::round_any(max(abs(xrange)), (xrange[2]-xrange[1])/20, ceiling)
      ymag <- plyr::round_any(max(abs(yrange)), (yrange[2]-yrange[1])/20, ceiling)
      xyratio <- xmag / ymag
      xbinwidth <- xmag/resolution*2
      ybinwidth <- ymag/resolution*2
      ldata <- expand.grid(x=seq(-xmag, xmag, xbinwidth), y=seq(-ymag, ymag, ybinwidth))
      ldata <- polarize(ldata, "x", "y", xyratio, xorigin=origin[1], yorigin=origin[2])
      data <- polarize(data, xvar, yvar, xyratio, xorigin=origin[1], yorigin=origin[2])
      return(list(data=data, legend_data=ldata, xmag=xmag, ymag=ymag, xbinwidth=xbinwidth, ybinwidth=ybinwidth))
}

#' Scales for colorwheel legend
#'
#' Get fill and alpha scales for use in legend construction.
cwscales <- function(flip=NULL,
                     left=left <- hsv(.5,1,1), # blue
                     bottom=hsv(.75,1,1), # purple
                     right=hsv(1,1,1), # red
                     top=hsv(.25,1,1)){ # green

      if("vertical" %in% flip){
            temp <- top
            top <- bottom
            bottom <- temp
      }
      if("horizontal" %in% flip){
            temp <- left
            left <- right
            right <- temp
      }

      fill <- scale_fill_gradientn(colours=c(right, top, left, bottom, right), limits=c(0, 360), guide="none")
      color <- scale_color_gradientn(colours=c(right, top, left, bottom, right), limits=c(0, 360), guide="none")
      alpha <- scale_alpha(range=c(0,1), guide="none")
      return(list(fill, color, alpha))
}



#' Build a colorwheel object
#'
#' Generate colorwheel components
#'
#'  @param data A data frame.
#'  @param xvar, yvar Names of data frame variables, as strings.
#'  @param resolution Integer: one-dimensional granularity of raster legend.
#'  @param fade Color at center of legend.
#'  @return A colorwheel object consisting of data, plots, and scales.
cw <- function(data, xvar, yvar, resolution=10, fade="white", origin=c(0,0), flip=NULL){
      require(ggplot2)
      d <- cwdata(data, xvar, yvar, resolution, origin=origin)

      p <- ggplot(d$legend_data, aes(x, y, fill=angle, alpha=distance)) +
            geom_raster(fill=fade, alpha=1) +
            geom_raster() +
            cwscales(flip)

      dcon <- d$data
      dcon[,xvar] <- plyr::round_any(dcon[,xvar], d$xbinwidth)
      dcon[,yvar] <- plyr::round_any(dcon[,yvar], d$ybinwidth)
      pcon <- ggplot(dcon, aes_string(xvar, yvar, fill="angle", alpha="distance")) +
            geom_raster(fill=fade, alpha=1) +
            geom_raster() +
            #xlim(-d$xmag, d$xmag) + ylim(-d$ymag, d$ymag) +
            cwscales(flip)

      return(list(data=d$data,
                  legend_data=d$legend_data,
                  legend_data_constrained=dcon,
                  legend=p,
                  legend_constrained=pcon,
                  scales=cwscales(flip)))
}

