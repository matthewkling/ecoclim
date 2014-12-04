

####################################

#' Capitalize first letter of a string.
#'
#' Capitalizes the first letter of a string.
#' @param x A character vector.
capFirst <- function(x){
      first <- substr(x, 1, 1)
      rest <- substr(x, 2, nchar(x))
      paste0(toupper(first), rest)
}


######################################

#' Parse factors found in filenames or strings.
#'
#' This function parses a vector of strings, returning a data frame of factors
#' found in those strings. It takes a folder path (or alternatively a character
#' vector) as input, and it returns a data frame with a row for each file in the
#' folder (or for each item in the vector) and columns containing matched
#' factors.
#'
#' @param paths A character vector of complete file directory paths.
#' @param variables Optional character vector selecting the subset of default
#'   keys titles to employ.
#' @param keys A list of named character vectors, representing variable title
#'   and factors.
#' @param is.dir Logical indicating whether path is a file directory, or just a
#'   vector of character strings to parse.
#' @param recursive Logical indicating whether subdirectories should be
#'   searched.
#' @param drops A character vector of path patterns that will cause that path to
#'   be dropped from results.
#' @param skips A character vector of path patterns to ignore during matching,
#'   typically used if folder names contain factor patterns.
#' @param simplify Logical indicating whether columns with all NA values should
#'   be removed from result.
#'
#' @return Data frame with a column for the parsed strings and for each of the
#'   factors.
#' @aliases prs

parseMetadata <- function(paths, is.dir=T, variables=NULL, keys=NULL, recursive=T, drops=NULL, skips=NULL, simplify=T){

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




########################################

#' Translate common climate data abbreviations.
#'
#' This is a simple translation dictionary that can be used to look up things
#' like names and units, for climate variables, statistics, and months.
#'
#' @param key A character vector.
#' @param to A character vector, must be either "words", "units", "delta",
#'   "abbv", or "letter". These are not all available for every type of key.
#' @return A character vector of translated results.

translate <- function(key, to="words"){
      dict <- list(tmin=list(words="minimum temperature", units="(deg C)", delta="(deg C)"),
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

      sapply(key, FUN=function(x)dict[[x]][[to]])
}



#############################################

#' Subsetting by loop iterators.
#'
#' This convenience function is a wrapper around data frame subsetting, designed
#' for use inside loops that iterate through raster data directories.
#'
#' @param data Data frame from which records will be selected.
#' @param ... Filtration factors, saved as varialbes with names matching data
#'   columns.
#' @return Data frame subsetted to match filtration factors.

sift <- function(data, ...){
      variables <- data.frame(..., stringsAsFactors=F)
      for(var in names(variables)) data <- data[data[,var]==variables[1,var],]
      return(data)
}

#########################################

#' Create a data frame from raster files
#'
#' This function takes a metadata frame (likely generated via parseMetadata) and
#' compiles all the files into a single data frame. Supports multiband files.
#'
#' @param metadata A data frame with a "path" variable and other metadata
#'   variables.
#' @return A data frame, with a row for each pixel of each raster in metadata.
frameRasters <- function(metadata) {

      for(i in 1:nrow(metadata)){
            s <- raster::stack(metadata$path[i])
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


