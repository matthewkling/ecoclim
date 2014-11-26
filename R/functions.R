

####################################

# function to capitalize the first letter of a string
capFirst <- function(x){
      first <- substr(x, 1, 1)
      rest <- substr(x, 2, nchar(x))
      paste0(toupper(first), rest)
}

######################################

# this function parses a vector of strings, returning a data frame with factors found in those strings
# it takes a folder path (or alternatively a character vector) as input,
# and it returns a data frame with a row for each file in the folder (or for each item in the vector) and columns containing matched factors

parseMetadata <- function(paths, is.dir=T, variables=NULL, keys=NULL, recursive=T, drops=NULL, skips=NULL, simplify=T){

      # paths: a character vector of complete file directory paths
      # variables: optional character vector selecting the subset of default keys titles to employ
      # keys: a list of named character vectors, representing variable title and factors
      # is.dir: (T/F) is paths a file directory, or just a vector of character strings to parse
      # recursive: (T/F) should subdirectories be searched
      # drops: a character vector of path patterns that will cause that path to be dropped from results
      # skips: a character vector of path patterns to ignore during matching, typically used if folder names contain factor patterns
      # simplify: (T/F) should columns with all NA values be removed from result

      # default keys
      key <- list(
            variable=c("tmin", "tmax", "tmean", "ppt", "tdmean", "vpr", "dpd", "pfog",
                       "Tmin", "Tmax", "Tave", "PPT", "cmd", "CMD", "NFFD", "nffd", "FFP", "ffp"),
            month=c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "14"),
            season=c("sp", "sm", "fl", "at", "wt"),
            stat=c("delta", "anomaly", "percentile", "zscore",
                   "mk_sl", "mk_D", "mk_S", "mk_tau", "mk_varS", "ts_slope", "ts_intercept", "ts_mResidual", "ts_change"),
            scenario=c("B1", "A2", "A1B"),
            model=c("bccr_bcm20", "cccma_cgcm3", "csiro_mk30", "gfdl_cm2", "gfdl_cm21", "giss_eh", "ipsl_cm4", "miroc32_Hires", "miroc32_medres", "mpi_echam5", "mri_cgcm232a", "ncar_ccsm30", "ukmo_hadcm3", "UKMO_HadGEM1"),
            run=c("run1", "run2", "run3", "run4", "run5"),
            year=1800:2200
      )

      # restrict keys to the requested subset, and add extras supplied manually
      if(!is.null(variables)) key <- key[variables]
      if(!is.null(keys)) key <- c(key, keys)

      # get names if needed, and format as data frame
      if(is.dir==T) paths <- list.files(paths, full.names=T, recursive=recursive)
      d <- data.frame(path=paths, stringsAsFactors=F)
      d$info <- d$path

      # standardize punctuation
      for(separator in c("/", "\\.", " ")) d$info <- gsub(separator, "_", d$info)
      d$info <- paste0("_", d$info, "_")

      # mask patterns that need to be ignored
      if(!is.null(skips)) for(skip in skips) d$info <- gsub(skip, "___", d$info)

      # serach and match key set
      for(k in names(key)){
            for(v in key[[k]]){
                  d[grepl(paste0("_", v, "_"), d$info), k] <- v
            }
      }

      # drops results as needed
      if(!is.null(drops)) for(drop in drops) d <- d[grepl(drop, d$path)==F,]
      if(simplify==T) d <- d[,colSums(is.na(d))<nrow(d)] # drop columns with all NAs
      d <- d[,!names(d)=="info"] # drop info column

      d

}

prs <- parseMetadata # add a second name for backward-compatibility




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

# this is a simple translation dictionary that can be used to look up names, units, abbreviations, etc

translate <- list(tmin=list(words="minimum temperature", units="(deg C)", delta="(deg C)"),
                  tmax=list(words="maximum temperature", units="(deg C)", delta="(deg C)"),
                  tmean=list(words="mean temperature", units="(deg C)", delta="(deg C)"),
                  tdmean=list(words="dew point", units="(deg C)", delta="(deg C)"),
                  ppt=list(words="precipitation", units="(mm)", delta="(ratio)"),
                  vpr=list(words="vapor pressure", units="(Pa)", delta="(Pa)"),
                  cmd=list(words="climatic moisture deficit", units="(mm)", delta="(ratio)"),
                  nffd=list(words="number of frost-free days", units="(days)", delta="(days)"),
                  ffp=list(words="frost-free period", units="(days)", delta="(days)"),

                  mk_sl=list(words="Mann-Kendall p-value", units="MK p-value"),
                  ts_change=list(words="Theil-Sen change magnitude", units="_variable_units_"),
                  delta=list(words="Delta", units="_variable_delta_"),
                  zscore=list(words="Standard (z) score", units="standard\ndeviations"),
                  percentile=list(words="Anomaly percentile", units="percentile"),

                  "01"=list(words="January", abbv="Jan", letter="J"),
                  "02"=list(words="February", abbv="Feb", letter="F"),
                  "03"=list(words="March", abbv="Mar", letter="M"),
                  "04"=list(words="April", abbv="Apr", letter="A"),
                  "05"=list(words="May", abbv="May", letter="M"),
                  "06"=list(words="June", abbv="Jun", letter="J"),
                  "07"=list(words="July", abbv="Jul", letter="J"),
                  "08"=list(words="August", abbv="Aug", letter="A"),
                  "09"=list(words="September", abbv="Sep", letter="S"),
                  "10"=list(words="October", abbv="Oct", letter="O"),
                  "11"=list(words="November", abbv="Nov", letter="N"),
                  "12"=list(words="December", abbv="Dec", letter="D"),
                  "14"=list(words="Annual", abbv="Annual", letter="Avg"))


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


###########################################

# helper function lets other functions translate between important values -- largely deprecated but retain for backward-compatibility
lookup <- function(query, response) {
      colors <- rainbow(n=3, s=1, v=.5, start=.25, end=.6)
      d <- data.frame(row.names=c("an", "sp", "sm", "fl", "at", "wt", "cmd", "tmin", "tave", "tmea", "tmean", "tmax", "pcpt", "ppt", "PPT", "1900_1980", "1981_2011", "1991_2011", "2001_2011", "1961_1990", "2011_2040", "2041_2070", "2071_2100"),
                      words=c("annual", "spring", "summer", "fall", "fall", "winter", "CMD", "minimum temperature", "mean temperature", "mean temperature", "mean temperature", "maximum temperature", "precipitation", "precipitation", "precipitation", NA, NA, NA, NA, NA, NA, NA, NA),
                      units=c(NA, NA, NA, NA, NA, NA, "(mm)", "(deg C)", "(deg C)", "(deg C)", "(deg C)", "(deg C)", "(mm)", "(mm)", "(mm)", NA, NA, NA, NA, NA, NA, NA, NA),
                      delta.units=c(NA, NA, NA, NA, NA, NA, "ratio", "(degrees C)", "(degrees C)", "(degrees C)", "(degrees C)", "(degrees C)", "ratio", "ratio", "ratio", NA, NA, NA, NA, NA, NA, NA, NA),
                      delta.agreement.units=c(NA, NA, NA, NA, NA, NA, "(ratio)", "(degrees C)", "(degrees C)", "(degrees C)", "(degrees C)", "(degrees C)", "(ratio)", "(ratio)", "(ratio)", NA, NA, NA, NA, NA, NA, NA, NA))
      return(as.character(d[query, response]))
}



#############################################

# this convenience function is a wrapper around data frame subsetting, designed for use inside loops that iterate through raster data directories

sift <- function(data, ...){
      # data: data frame from which record(s) will be selected
      # ...: filtration factors, saved as variables with names matching data columns.
      variables <- data.frame(..., stringsAsFactors=F)
      for(var in names(variables)) data <- data[data[,var]==variables[1,var],]
      return(data)
}

#########################################

# this function takes a metadata frame (likely generated via parseMetadata) and compiles all the files into a single data frame. supports multiband files.
frameRasters <- function(metadata) {
      # metadata: a data frame with a "path" variable and other metadata variables

      for(i in 1:nrow(metadata)){
            s <- stack(metadata$path[i])
            if(nlayers(s)==1) names(s) <- "value"
            s <- as.data.frame(rasterToPoints(s))

            # add columns for identifying factors
            for(j in names(metadata)[2:ncol(metadata)]){
                  s[,j] <- metadata[,j][i]
            }

            if(i==1) ss <- s else{ss <- rbind(ss, s)}
      }

      return(ss)
}


########################################

eb <- ggplot2::element_blank()
