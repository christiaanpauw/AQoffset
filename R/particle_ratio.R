#' Particle Ratio
#' 
#' Calculate the equivalence of sources in terms of the resultant
#' concentrations at each receptor.
#'
#' @author Rebecca Garland, Mogesh Naidoo, Christiaan Pauw
#' @param source1 numeric vector of the same length as nlayers(conc1). Total emissions
#' @param conc1 A Raster object giving the concentrations resulting from source1
#' over the area identified by the raster's origin and extent. Source1 is the 
#' source to be offset.
#' @param source2 numeric vector of the same length as nlayers(conc2).
#' @param conc2 A Raster object giving the concentrations resulting from source2 
#' over the area identified by the raster's origin and extent. Source2 is the 
#' offset source (i.e. the source that does the offsetting).
#' @return A raster or raster stack with the source equivalence ratios (source 
#' impact ratio). A large number means there is a large offset opportunity or 
#' that the offset source (source 2) has a much larger impact than the source to
#' be offset (source1).
#' @export

particle_ratio <- function(source1 = NULL, 
                           conc1 = NULL, 
                           source2 = NULL, 
                           conc2 = NULL) {
  
  if (is.null(source1) | 
      is.null(conc1) | 
      is.null(source2) | 
      is.null(conc2)) {
    stop("No NULL arguments allowed.")
  }
  
  rat1 <- source1 / conc1 # impact ratio of the source to be offset
  rat2 <- source2 / conc2 # impact ratio of the offset source
  ratrat <- rat1 / rat2
  return(ratrat)
}
 
# Offset requirement for source 1 though source 2. 
# Boundary. Assume intervention effect %
# efficiency needed at every receptor 
# conveniance function click on netcdf for conc1 and conc2

# test data
# src1 <- sample(x = 175:500, size = 365, replace = TRUE)
# src2 <- sample(x = 50:89, size = 365, replace = TRUE)
# 
# concentrations1 <- stack()
# concentrations2 <- stack()
# for (d in 1:365) {
#   concentrations1 <- addLayer(concentrations1, 
#                               raster(nrow = 3, 
#                                  ncol = 3, 
#                                  vals = sample(x = 0:200, 
#                                                  size = 9, 
#                                                  replace = TRUE),
#                                  xmn = 0, 
#                                  ymn = 0,
#                                  xmx = 12, 
#                                  ymx = 30))
#   
#   concentrations2 <- addLayer(concentrations2,
#                               raster(nrow = 3, 
#                                  ncol = 3, 
#                                  vals = sample(x = 0:70, 
#                                                size = 9, 
#                                                replace = TRUE),
#                                  xmn = 0, 
#                                  ymn = 0,
#                                  xmx = 12, 
#                                  ymx = 30))
# }
# 
# ratios <- particle_ratio(source1 = src1, 
#                          conc1 = concentrations1, 
#                          source2 = src2, 
#                          conc2 = concentrations2)






