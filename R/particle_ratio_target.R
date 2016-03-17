#' Particle Ratio Target
#' 
#' Calculates the target of an offset intervention according to the 
#' particle equavalence approach (using the particle_ratio function)
#' 
#' @author Christiaan Pauw
#' @param result What to return as result. Options: c("target", "effect boundary").
#' result == "target" returns the proportion of the offset source to be reduced in
#' order to offset the managed activity at every receptor. 
#' result == "effect boundary" returns a one-layer raster with 1 in all the cells
#' where an offset can take place and NA in all others. This can be used to 
#' select (mask) from other rasters.
#' @param int.eff Proportion of source 2 that can be addressed by intervention.
#' @param effect Options: c("shortterm", "longterm"). 
#' effect == "shortterm": do the calculation in terms of an immediate/short-term 
#' effect. This approach gives the result as exceedance counts (standard must 
#' not be NULL). 
#' effect = "longterm" aggregates the concentrations.
#' @param units Numeric. How many units does source 2 consists of?. If NULL, 
#' report proportion default NULL.
#' @param minrat The minimum source ratio for which an offset can be done.
#' @param source1 Numeric. Vector of the same length as nlayers(conc1). Total emissions.
#' @param conc1 A Raster object with concentrations resulting from source1. Average emissions.
#' @param source2 Numeric. Vector of the same length as nlayers(conc2).
#' @param conc2 A Raster. Object with concentrations resulting from source2.
#' @return A raster with the target expressed as proportion of the offset source
#' to be reduced to offset the managed activity at every receptor (when result = "target") 
#' or the boudary of the area that can be offset using the particle equivalence approach when the 
#' efficiency of the intervention is int.eff.
#' @export

particle_ratio_offset  <- function(result = c("target", "effect boundary")[1],
                                   int.eff = 0.5,
                                   effect = c("shortterm","longterm")[1],
                                   units = NULL, 
                                   minrat = 100, 
                                   source1 = rep(40, 365), 
                                   conc1 = NULL, 
                                   source2 = rep(x = 15, 365), 
                                   conc2 = NULL){
  
  if (!is.numeric(int.eff)) {
    stop("int.eff must be numeric (a proportion between 0 and 1)")}
  if (int.eff > 1 | int.eff < 0) {
    stop("int.eff must be a proportion between 0 and 1")}
  if (all(is.na(match(c("target", "effect boundary"), result)))) {
    stop("result  must be 'target' or 'effect boundary'")
  }
  if (all(is.na(match(c("shortterm","longterm"), effect)))) {
    stop("effect must be 'shortterm' or 'longterm'")
  }
  
  if (effect == "shortterm"){
    # The immediate/short-term effect case
    ratrat <- particle_ratio(source1 = get("source1"), 
                             conc1 = get("conc1"), 
                             source2 = get("source2"), 
                             conc2 = get("conc2"))
    
    # raster counts: But what exctly do you count
    
  }
  
  if (effect == "longterm"){
    # The long-term case
    conc1s <- calc(conc1, fun = sum)
    conc2s <- calc(conc2, fun = sum)
    source1s <- sum(source1)
    source2s <- sum(source2)
    ratrat.cum <- particle_ratio(source1 = get("source1s"), 
                                 conc1 = get("conc1s"), 
                                 source2 = get("source2s"), 
                                 conc2 = get("conc2s"))
  }
  
  if (result == "target"){
    # watter proporsie of hoeveel units
    if (!is.null(units)){
      if (!is.numeric(units)) {
        stop("Units must be either NULL or numeric")
      }
      # Die eenheid benadering: hoeveel eenhede om die impak af te set
      res <- ratrat / units
    } else {
      # Die proporsionele benadering
      res 
    }
  }
  
  if (result == "effect boundary"){
    
   
  }
  
  
  
}