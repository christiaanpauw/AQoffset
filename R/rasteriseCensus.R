# rasterise census subplace

# randomly assign the right number of households as points to a polygon
# randomly assign the right proportion to each of a number of categories
# e.g. energy carrier for heating: coal, wood, elec
# make a total
# make number of people
# make sex distribution
# make age distribution

#' Rasterise a census polygon SPDF possibly containing summary data.
#' 
#' @details
#' The function randomly assign the correct number of sample units
#' in each of a number of categories e.g. energy carrier for heating:
#' as summarised in each row of the SPDF as points to a polygon   
#' It can be used to: 
#' make a total 
#' make number of people 
#' make sex distribution make age distribution
#' @param x A SpatialPolygonsDataFrame
#' @param drpnames A character vector of column names that should not be included

rasteriseCensus <- function(x, 
                            drpnames=c("ID", "Geometry_s", "GAVPrimar0", "Geometry_1", 
                                       "SP_Code", "MP_Code", "fakeData", "GAVPrimary", "Total")){
  res = pointifyCensus(spdf = x, dropnames = drpnames)
  rl = lapply(split(res, res$category), 
              function(x) rasterize(coordinates(x), ref))
  brick(rl)
}
