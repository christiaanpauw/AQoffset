#' consolodateAdditiveOutcomes Function to add burden of disease outcomes in 
#' rasterised format based on the layer name. The expectation is that a raster 
#' with names like "mortality.all.cause.PM10" and "mortality.all.cause.SO2" are given 
#' where mortality.all.cause.PM10 is all-cause mortality due to PM10 exposure and 
#' mortality.all.cause.SO2 is all-cause mortality due to SO2 exposure. If the relative 
#' risks per pollutant used to construct the input raster represented the unique contribution
#' of that pollutant, the outcomes can be added. If the exposure reponse functions where 
#' based on co-exposure to more than one pollutant - one cannot do this or you will
#' have to allocate weights. Allowance must also possibly be made for synergic effects
#' 
#' @return A raster with one layer for each outcome
#' @param r A Raster with burden of disease outcomes
#' @param polnames A character vector with names for pollutants as they are found 
#' in the layernames of r

consolodateAdditiveOutcomes <- function(r, polnames = c("PM10", "SO2")){
  nn <- gsub(paste(polnames, collapse = "|"), "", names(r))
  outs <- unique(nn)
  chronics <- grep("Chronic|chronic", nn)
  idx <- match(nn , outs)
  if (length(chronics) > 0) idx <- idx[-match(chronics, idx)]
  res <- stackApply(r[[idx]], idx, sum,  na.rm = TRUE)
  if (length(chronics) > 0) res2 <- stackApply(r[[chronics]], chronics, mean,  na.rm = TRUE)
  names(res) <- outs[idx]
  names(res2) <- outs[chronics]
  if (length(chronics) > 0) res <- stack(res, res2)
  res
}
