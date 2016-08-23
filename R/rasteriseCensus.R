#' Rasterise Census Subplace
#'
#' Rasterise a census polygon SPDF possibly containing summary data.
#' The function randomly assign the correct number of sample units
#' in each of a number of categories e.g. energy carrier for heating:
#' as summarised in each row of the SPDF as points to a polygon   
#' It can be used to: 
#' make a total 
#' make number of people 
#' make sex distribution make age distribution
#' 
#' @param x A SpatialPolygonsDataFrame
#' @param ref An extent object
#' @param verbose Print messages or not
#' @param refres Numeric vector of length 2 (x,y): A reference resolution
#' @param drpnames A character vector of column names that should not be included
#' @param ... Arguments to be passed to/from other methods
#' @export

# todo: change to be able to use with a weight vector (example struture density from remote snesing image)

rasteriseCensus <- function(x, ref = ext, verbose = FALSE, refres, 
                            drpnames=c("ID", "Geometry_s", "GAVPrimar0", "Geometry_1", "OBJECTID", 
                                       "SP_CODE", "SP_Code", "MP_CODE", "MP_Code", "MN_CODE", "MN_MDB_C", 
                                       "DC_MN_C", "Shape_Leng", "Shape_Area", "fakeData", "GAVPrimary", 
                                       "Total"), ...){
  res = pointifyCensus(spdf = x, dropnames = drpnames, verbose = TRUE)
  rm(x)
  srl = split(res, res$category)
  cts = as.character(sapply(srl, function(x) unique(x@data$category)))
  rm(res)
  b = brick(ref, nl = length(srl), nrow = refres[1], ncol = refres[2])
  for (i in 1:length(srl)){
    ct = unique(srl[[i]]@data$category)
    if (verbose == TRUE) message(i," " , ct, "\nrefres is ", paste(refres, " "))
    vls = rasterize(x = SpatialPoints(srl[[i]]),
                    y = b,  
                    fun = "count")
    b = setValues(b, getValues(vls), layer = i)
  }
  names(b) <- cts
  return(b)
  # grid : spp = SpatialGrid(points2grid(SpatialPoints(coordinates(b))))
}

#' Sexify
#' Change  SPDF to match a se proportion
#' 
#' @param x A SpatialPolygonsDataFrame
#' @param prop Numeric that contains the sex proportion
#' @export

sexify <- function(x, prop = 0.5){
  res = x * prop
  res
}


#' gridzle
#' Maak 'n rooster (SpatialGrid) van 'n raster
#' @param r raster

gridzle <- function(r){
  res = SpatialGrid(points2grid(SpatialPoints(coordinates(r))))
  res
}
