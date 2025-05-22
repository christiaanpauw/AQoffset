#' Aggregate Health Outcome Rasters from CREP
#'
#' This function takes a list of `RasterStack` objects, typically representing
#' different health outcomes or scenarios from a Concentration-Response Exposure
#' Pathway (CREP) analysis. It aggregates each stack into a single layer (summing
#' pixel values) and then combines these aggregated layers into a single `RasterStack`.
#' Additionally, it standardizes layer names using a series of `gsub` replacements.
#'
#' The internal helper function `som` is used for summing raster values with `na.rm = TRUE`.
#' Any `NA` values in the final raster stack are replaced with a very small number (0.000001).
#'
#' @param r A list of `RasterStack` objects. Each `RasterStack` in the list
#'   is expected to represent a specific health outcome or scenario, potentially
#'   containing multiple layers (e.g., time periods like days) that will be summed.
#' @return A single `RasterStack` where each layer is the sum of layers from one
#'   of the input `RasterStack` objects. Layer names are standardized according to
#'   predefined `gsub` rules (e.g., "mortality" becomes "MORT").
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("raster", quietly = TRUE)) {
#'   library(raster)
#'
#'   # Create sample RasterStacks to simulate CREP output for two outcomes
#'   # Outcome 1: Mortality
#'   r1_day1 <- raster(nrows=5, ncols=5, val=runif(25, 0, 5))
#'   r1_day2 <- raster(nrows=5, ncols=5, val=runif(25, 0, 3))
#'   stack_mortality <- stack(r1_day1, r1_day2)
#'   names(stack_mortality) <- c("Cases_mortality_due.to.PM2.5_all.ages_day1",
#'                              "Cases_mortality_due.to.PM2.5_all.ages_day2")
#'
#'   # Outcome 2: Respiratory hospital admissions
#'   r2_day1 <- raster(nrows=5, ncols=5, val=runif(25, 0, 10))
#'   r2_day2 <- raster(nrows=5, ncols=5, val=runif(25, 0, 8))
#'   stack_resp_hosp <- stack(r2_day1, r2_day2)
#'   names(stack_resp_hosp) <- c("Cases_respiratory_hospital.admissions_among.children_day1",
#'                                "Cases_respiratory_hospital.admissions_among.children_day2")
#'
#'   # Put them in a list
#'   list_of_stacks <- list(mortality_outcome = stack_mortality,
#'                          respiratory_outcome = stack_resp_hosp)
#'
#'   # Aggregate the stacks
#'   aggregated_stack <- aggregateCREPstack(r = list_of_stacks)
#'
#'   # Print the result
#'   print(aggregated_stack)
#'   print(names(aggregated_stack))
#'   # Expected names might be like: "MORT_PM2.5_all", "esp.hosp.ad_children"
#'   # (depending on the full set of gsub rules)
#'
#'   # Plot one of the aggregated layers
#'   plot(aggregated_stack[[1]])
#' }
#' }
aggregateCREPstack <- function(r){ # r is a list of RasterStacks
  # summarise the number of cases across all days for each outcome stack
  
  # Helper function to sum raster layers, removing NAs
  som <- function(x) {sum(x, na.rm = TRUE)}

  # Apply the sum function to each RasterStack in the list 'r'
  # Each element of the list 'bOutcome' will become a single RasterLayer (sum of its original layers)
  r_aggregated_list <- lapply(X = r, FUN = function(bOutcome_stack) {
    return(raster::calc(x = bOutcome_stack, fun = som))
  })
  
  # Combine the single-layer rasters (each representing an aggregated outcome) into one RasterStack
  final_stack <- raster::stack(r_aggregated_list)
  
  # Standardize layer names using a series of gsub rules
  # These rules are specific to the naming conventions of the CREP outputs.
  current_names <- names(final_stack)
  new_names <- gsub("mortality", "MORT", current_names)
  new_names <- gsub("among.", "", new_names) # "among." often followed by age group
  new_names <- gsub("respiratory", "esp.", new_names) # "respiratory"
  new_names <- gsub("hospital.admissions", "hosp.ad", new_names)
  new_names <- gsub("Cases_", "", new_names) # Prefix "Cases_"
  new_names <- gsub("due.to.", "", new_names) # "due.to." often followed by pollutant
  new_names <- gsub("Cardiovascular|cardiovascular", "CV", new_names)
  new_names <- gsub("all.ages", "all", new_names)
  # Add more specific rules if needed, e.g., for pollutant names or other details
  # Example: gsub("PM2.5", "PM25", new_names)

  names(final_stack) <- new_names
  
  # Replace any remaining NA values with a very small number
  # This might be for preventing issues in further calculations, but can also mask true NAs.
  final_stack[is.na(final_stack)] <- 0.000001
  
  return(final_stack)
}
[end of R/aggregateCREPstack.R]
