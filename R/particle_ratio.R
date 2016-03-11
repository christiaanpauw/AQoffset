#' particle_ratio Calculate the equavalence of sources in terms of the 
#' resultant concentrations at each receptor
#' @return A raster or raster stack with the source equavalence ratios (source impact ratio)
#' . A large number means there is a large offset opportunity or that the offset source (source 2) has a
#' much larger impact than the source to be offset (source1)
#' @author Rebecca Garland, Mogesh Naidoo, Christiaan Pauw
#' @param source1 numeric vector of the same length as nlayers(conc1). Total emissions
#' @param conc1 A Raster object with concentrations resulting from source1.
#' @param source2 numeric vector of the same length as nlayers(conc2)
#' @param conc2 A Raster object with concentrations resulting from source2

particle_ratio <- function(source1 = rep(400, 365), 
                            conc1 = baseline365[[grep("Indus[[:print:]]*_pm10[[:print:]]", names(baseline365))]], 
                            source2 = rep(x = 15, 365), 
                            conc2 = baseline365[[grep("H[[:print:]]*_pm10[[:print:]]*", names(baseline365))]]){
  
  rat1 <- source1 / conc1
  rat2 <- source2 / conc2
  ratrat <- rat1 / rat2
  ratrat
}
 
# Offset requirement for source 1 though source 2. 
# Boundary. Assume intervention effect %
# efficeincy needed at every receptor 
# conveniance function click on netcdf for conc1 and conc2


