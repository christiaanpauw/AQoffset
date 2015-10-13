#' Pointify census polygon
#' @description
#' Takes a SpatialPolygonsDataFrame with summary information and generates the 
#' appropriate number of random points and return a SpatialPointsDataFrame
#' @param spdf A SpatialPolygonsDataFrame
#' @param dropnames A character vector of column names that should not be included
#' @param verbode Do you want messages
#' @details The numeric columns are counts of responses to categorical questions in the census
#' e.g. Which energy carrier do you use for heating.
#' 
#' 

pointifyCensus <- function(spdf, 
                           dropnames=c("ID", "OBJECTID", "Geometry_s", "GAVPrimar0", "Geometry_1", 
                                       "SP_CODE","SP_Code", "MP_CODE", "MP_Code" ,"MN_CODE", "MN_MDB_C", "DC_MN_C",
                                       "Shape_Leng", "Shape_Area", "fakeData", "GAVPrimary", "Total")){
  dropidx <- na.omit(match(dropnames, names(spdf)))
  if (verbose == TRUE) message(dim(spdf@data))
  if (verbose == TRUE) message("names : ", paste(names(spdf), " "))

  if (length(dropidx) > 0) spdf <- spdf[,-dropidx]
  numidx <- grep("numeric|integer", sapply(spdf@data, class ))
  if (verbose == TRUE) message(paste(numidx, " "))
  if (length(numidx) < 1) stop("There must be some numeric columns")
  # VERVANG INF MET NA
  spdf <- spdf[,numidx]
  if (length(spdf@data[spdf@data == Inf]) > 0) spdf@data[spdf@data == Inf] <- 0
  if (length(spdf@data[is.na(spdf@data)]) > 0) spdf@data[is.na(spdf@data)] <- 0
  if (verbose == TRUE) message(dim(spdf@data))
  if (verbose == TRUE) message(paste(sapply(spdf@data, max, na.rm = TRUE), " "))
  res.list <- lapply(1:length(spdf), function(i){
    SpatialPointsDataFrame(coords = spsample(x = spdf[i, ], n = sum(spdf@data[i,], na.rm = TRUE), type = "random"),
                           data = data.frame(category = sample(x = rep(names(spdf@data), times = spdf@data[i, ]), size = sum(spdf@data[i,]), replace = FALSE),
                                             n = 1),
                           proj4string = CRS(proj4string(spdf)),
                           bbox = NULL
    )
  })
  res <- do.call("rbind.SpatialPointsDataFrame", res.list)
}