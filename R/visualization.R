



######################################

# this function is used to progressively offset the coordinates of map layers in long-format tabulated geographic data so that they can be plotted tightly with overlapping extents
# it is generally used on multi-layer raster data that has been converted into a data frame
# it takes a vector of coordinates (target), a vector of factors indicating the layer (criterion), and an offset distance (offset)
# it returns a vector of adjusted coordinates equal in length to the input corrdinates

flake <- function(target, criterion, offset, summary=F){

      # set numbering for offset facets
      groups <- as.integer(as.factor(criterion)) - 1

      # rounding resolution
      steps <- target[2:length(target)] - target[1:length(target)-1]
      resolution <- median(steps[steps != 0], na.rm=T)

      if(summary==T){
            target <- min(target, na.rm=T)
            l <- plyr::round_any(target + offset * groups, resolution, floor)
            l <- unique(l)
            names(l) <- levels(as.factor(criterion))
            return(l)
      }

      plyr::round_any(target + offset * groups, resolution, floor)
}


###################################


shade <- function(obj, margin=.25){
      hill <- raster("Z:/cdata/Matt_stuff/terrain/hillshade_UCM_90m.asc")
      frame <- extent(obj)
      frame@xmin <- frame@xmin - margin
      frame@xmax <- frame@xmax + margin
      frame@ymin <- frame@ymin - margin
      frame@ymax <- frame@ymax + margin
      hill <- crop(hill, frame)
      hill <- as.data.frame(rasterToPoints(hill))
      names(hill) <- c("x", "y", "hillshade")
      l <- geom_raster(data=hill, aes(x, y, alpha=hillshade), color="black")
      return(l)
}


###################################

ggAddCartography <- function(plot, mask, features){
      # this funciton adds cartography layers to a ggplot object
      # plot should be a ggplot map object
      # mask can be a spatial polygon layer
      # features should be a vector of requested layers chosen from c("hillshade", "rivers", "states")

      if("hillshade" %in% features){
            hill <- raster("Z:/cdata/Matt_stuff/terrain/hillshade_UCM_90m.asc")
            hill <- crp(hill, mask)
            hill <- as.data.frame(rasterToPoints(hill))
            names(hill) <- c("x", "y", "hillshade")
            plot <- plot +
                  geom_raster(data=hill, aes(x, y, alpha=hillshade), color="black") +
                  scale_alpha_continuous(range=c(1, 0), guide="none")
      }


      plot
}


########################################

eb <- ggplot2::element_blank()
