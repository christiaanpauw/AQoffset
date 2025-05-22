#' Consolidate Additive Outcomes from Pollutant-Specific Rasters
#'
#' This function takes a `RasterStack` where individual layers represent specific
#' health outcomes attributed to different pollutants (e.g., "mortality.all.cause.PM10",
#' "mortality.all.cause.SO2"). It sums these outcomes across pollutants to create
#' consolidated outcome layers (e.g., "mortality.all.cause").
#'
#' The function works by removing specified pollutant names (`polnames`) from the
#' layer names of the input `RasterStack`. Layers that share the same base name
#' after this removal are then grouped and summed using `raster::stackApply`.
#'
#' **Important Consideration:** This function assumes that the health outcomes
#' attributed to different pollutants are additive. This is appropriate if the
#' concentration-response functions used to generate the input rasters represent
#' the unique contribution of each pollutant. If there's overlap or synergistic
#' effects captured in the input data, simple addition might not be suitable.
#'
#' @param r A `RasterStack` object. Layer names should consistently include
#'   pollutant identifiers that can be specified in `polnames`. For example,
#'   `names(r) <- c("outcomeA.PM10", "outcomeA.SO2", "outcomeB.PM10")`.
#' @param polnames A character vector listing the pollutant names as they appear
#'   in the layer names of `r`. These substrings will be removed from layer names
#'   to identify common underlying outcomes. Defaults to `c("PM10", "SO2")`.
#' @return A `RasterStack` where each layer represents a health outcome summed
#'   across all specified pollutants. Layer names will be the base outcome names
#'   (e.g., "outcomeA", "outcomeB").
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("raster", quietly = TRUE)) {
#'   library(raster)
#'
#'   # Create sample RasterLayers
#'   mort_pm10 <- raster(nrows=5, ncols=5, val=runif(25, 0, 10))
#'   mort_so2 <- raster(nrows=5, ncols=5, val=runif(25, 0, 5))
#'   resp_pm10 <- raster(nrows=5, ncols=5, val=runif(25, 0, 15))
#'
#'   # Create a RasterStack with appropriate names
#'   # Format: outcome_identifier.pollutant_identifier
#'   health_outcomes_stack <- stack(mort_pm10, mort_so2, resp_pm10)
#'   names(health_outcomes_stack) <- c("mortality.all.cause.PM10",
#'                                     "mortality.all.cause.SO2",
#'                                     "respiratory.disease.PM10")
#'
#'   print("Original stack:")
#'   print(health_outcomes_stack)
#'
#'   # Consolidate outcomes by summing across pollutants "PM10" and "SO2"
#'   consolidated_stack <- consolidateAdditiveOutcomes(
#'     r = health_outcomes_stack,
#'     polnames = c(".PM10", ".SO2") # Note: Using ".POLLUTANT" to avoid partial matches
#'                                   # if outcome names themselves contain "PM10" or "SO2"
#'   )
#'
#'   print("Consolidated stack:")
#'   print(consolidated_stack)
#'   # Expected output:
#'   # A RasterStack with two layers:
#'   # 1. "mortality.all.cause" (sum of mort_pm10 and mort_so2)
#'   # 2. "respiratory.disease" (from resp_pm10, as no other "respiratory.disease" layers)
#'
#'   plot(consolidated_stack)
#' }
#' }
consolidateAdditiveOutcomes <- function(r, polnames = c("PM10", "SO2")){
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("Package 'raster' is required. Please install it.", call. = FALSE)
  }

  # Create a single regex pattern from polnames: "POLLUTANT1|POLLUTANT2|..."
  # This ensures that if one pollutant name is a substring of another, the longer one is prioritized
  # (though gsub processes left-to-right, so specific ordering in polnames might matter if complex overlaps)
  # For simple cases like ".PM10", ".SO2", direct paste is fine.
  pattern_to_remove <- paste(polnames, collapse = "|")
  
  # Remove pollutant names from layer names to get base outcome names
  base_outcome_names <- gsub(pattern_to_remove, "", names(r))
  
  if (all(base_outcome_names == names(r))) {
    warning("None of the specified 'polnames' were found in the layer names of 'r'. ",
            "The original RasterStack will be returned without consolidation.")
    return(r)
  }
  
  unique_base_outcomes <- unique(base_outcome_names)
  
  # Create an index list for stackApply:
  # This links each layer in the original stack 'r' to a unique base outcome.
  indices <- match(base_outcome_names, unique_base_outcomes)
  
  # Sum layers that have the same base outcome name
  # stackApply will apply 'sum' to subsets of 'r' defined by 'indices'
  result_stack <- raster::stackApply(r, indices = indices, fun = sum, na.rm = TRUE)
  
  # Assign the unique base outcome names to the layers of the resulting stack
  # The order of unique_base_outcomes[unique(indices)] should match stackApply's output order.
  # A safer way is to name them based on the unique values of 'base_outcome_names'
  # corresponding to the 'indices' used by stackApply.
  # stackApply names layers like "index_1", "index_2", etc.
  # We need to map these back to unique_base_outcomes.
  # The unique values of 'indices' correspond to the positions in 'unique_base_outcomes'.
  # So, layer "index_1" corresponds to unique_base_outcomes[1], "index_2" to unique_base_outcomes[2], etc.
  
  # Correctly name the layers of the result stack
  # `unique(indices)` gives the sorted unique index values that `stackApply` used.
  # `unique_base_outcomes[sort(unique(indices))]` might also work if unique_base_outcomes was sorted.
  # The simplest is to use the unique_base_outcomes directly, assuming stackApply processes them in order of appearance or sorted unique indices.
  # Let's use the unique_base_outcomes based on the order of unique indices.
  ordered_unique_indices <- sort(unique(indices))
  names(result_stack) <- unique_base_outcomes[ordered_unique_indices]

  return(result_stack)
}

[end of R/consolidateAdditiveOutcomes.R]
