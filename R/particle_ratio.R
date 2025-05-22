#' Calculate Source Impact Equivalence Ratio
#'
#' This function calculates the ratio of "impact ratios" for two different emission
#' sources. The impact ratio for a source at a given location is defined as its
#' total emissions divided by the concentration it produces at that location.
#' The function then computes the ratio of these impact ratios:
#' `(emissions1 / concentration1) / (emissions2 / concentration2)`.
#'
#' This resulting ratio raster can be used in air quality offsetting evaluations.
#' It indicates the relative effectiveness of an offsetting source (source 2)
#' compared to the source being offset (source 1) at different locations.
#'
#' @param source1 Numeric vector. Total emissions from source 1 (the source to be
#'   offset). The length of this vector should ideally correspond to the number
#'   of layers in `conc1` if they represent different time periods or scenarios,
#'   allowing for time-varying emission rates. If a single value, it's recycled.
#' @param conc1 A `RasterStack` or `RasterBrick` object representing the concentrations
#'   resulting from source 1. Each layer can represent a different time period
#'   or scenario.
#' @param source2 Numeric vector. Total emissions from source 2 (the offsetting source).
#'   Similar to `source1`, its length should ideally match `nlayers(conc2)`.
#'   If a single value, it's recycled.
#' @param conc2 A `RasterStack` or `RasterBrick` object representing the concentrations
#'   resulting from source 2.
#' @return A `Raster*` object (same type as inputs if they match, otherwise likely a
#'   `RasterStack`). Cell values represent the calculated source impact equivalence
#'   ratio. A large value at a location means that source 2 is relatively more
#'   impactful per unit of emission than source 1 at that location (i.e., a smaller
#'   amount of source 2 emissions produces an equivalent concentration change
#'   compared to source 1). Conversely, it can also mean source 1 is relatively
#'   less impactful per unit of its emission.
#' @author Rebecca Garland, Mogesh Naidoo, Christiaan Pauw
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("raster", quietly = TRUE)) {
#'   library(raster)
#'
#'   # Example: Emissions for 5 time periods (e.g., days)
#'   src1_emissions <- c(100, 110, 105, 100, 115) # Emissions for source 1 per period
#'   src2_emissions <- c(50, 55, 52, 48, 53)    # Emissions for source 2 per period
#'
#'   # Create sample concentration rasters for 5 time periods
#'   concentrations1 <- stack()
#'   concentrations2 <- stack()
#'
#'   set.seed(123) # for reproducibility
#'   for (d in 1:5) {
#'     # Concentrations from source 1
#'     c1_layer <- raster(nrow = 3, ncol = 3,
#'                        vals = sample(x = 10:50, size = 9, replace = TRUE),
#'                        xmn = 0, ymn = 0, xmx = 3, ymx = 3)
#'     concentrations1 <- addLayer(concentrations1, c1_layer)
#'
#'     # Concentrations from source 2
#'     c2_layer <- raster(nrow = 3, ncol = 3,
#'                        vals = sample(x = 5:30, size = 9, replace = TRUE),
#'                        xmn = 0, ymn = 0, xmx = 3, ymx = 3)
#'     concentrations2 <- addLayer(concentrations2, c2_layer)
#'   }
#'   names(concentrations1) <- paste0("s1_period_", 1:5)
#'   names(concentrations2) <- paste0("s2_period_", 1:5)
#'
#'   print("Source 1 Emissions:")
#'   print(src1_emissions)
#'   print("Source 1 Concentrations (summary of first layer):")
#'   print(summary(values(concentrations1[[1]])))
#'
#'   print("Source 2 Emissions:")
#'   print(src2_emissions)
#'   print("Source 2 Concentrations (summary of first layer):")
#'   print(summary(values(concentrations2[[1]])))
#'
#'   # Calculate the particle ratio (source impact equivalence ratio)
#'   # This will perform calculations layer by layer if inputs are time-matched.
#'   equivalence_ratios <- particle_ratio(source1 = src1_emissions,
#'                                        conc1 = concentrations1,
#'                                        source2 = src2_emissions,
#'                                        conc2 = concentrations2)
#'
#'   print("Equivalence Ratios (summary of first layer):")
#'   print(summary(values(equivalence_ratios[[1]])))
#'   # plot(equivalence_ratios, main = "Source Impact Equivalence Ratios")
#'
#'   # Example with single emission values (recycled across layers)
#'   single_val_ratio <- particle_ratio(source1 = 100, conc1 = concentrations1,
#'                                      source2 = 50, conc2 = concentrations2)
#'   print("Ratios with single emission values (summary of first layer):")
#'   print(summary(values(single_val_ratio[[1]])))
#'
#' }
#' }
particle_ratio <- function(source1 = NULL,
                           conc1 = NULL,
                           source2 = NULL,
                           conc2 = NULL) {

  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("Package 'raster' is required. Please install it.", call. = FALSE)
  }

  if (is.null(source1) || is.null(conc1) || is.null(source2) || is.null(conc2)) {
    stop("No NULL arguments allowed. All parameters (source1, conc1, source2, conc2) must be provided.")
  }

  # Validate inputs
  if (!inherits(conc1, "Raster") || !inherits(conc2, "Raster")) {
    stop("`conc1` and `conc2` must be Raster* objects (RasterLayer, RasterStack, or RasterBrick).")
  }
  if (!is.numeric(source1) || !is.numeric(source2)) {
    stop("`source1` and `source2` must be numeric (single value or vector).")
  }

  # Handle emission vectors: if single value, recycle; if vector, match nlayers
  n_layers_conc1 <- raster::nlayers(conc1)
  n_layers_conc2 <- raster::nlayers(conc2)

  if (length(source1) == 1 && n_layers_conc1 > 1) {
    source1_adj <- rep(source1, n_layers_conc1)
  } else if (length(source1) != n_layers_conc1) {
    stop("Length of `source1` emissions vector does not match number of layers in `conc1`.")
  } else {
    source1_adj <- source1
  }

  if (length(source2) == 1 && n_layers_conc2 > 1) {
    source2_adj <- rep(source2, n_layers_conc2)
  } else if (length(source2) != n_layers_conc2) {
    stop("Length of `source2` emissions vector does not match number of layers in `conc2`.")
  } else {
    source2_adj <- source2
  }
  
  # If conc1 and conc2 have different numbers of layers, raster math might still work if one is single layer
  # but the problem context implies they should usually match if sources are vectors.
  if (n_layers_conc1 != n_layers_conc2 && length(source1_adj) != length(source2_adj)) {
      warning("Number of layers in conc1 and conc2 (or lengths of adjusted source vectors) differ. ",
              "Raster calculations will proceed using raster's recycling rules, which might be unintended.")
  }


  # Calculate impact ratios: emissions / concentration
  # Raster arithmetic handles element-wise operations.
  # If sourceX_adj is a vector of same length as nlayers(concX),
  # then sourceX_adj[i] / concX[[i]] is computed for each layer.
  impact_ratio1 <- source1_adj / conc1 # Unit: emissions per unit concentration (e.g., (kg/s) / (ug/m^3))
  impact_ratio2 <- source2_adj / conc2 # Same unit

  # Calculate the ratio of impact ratios
  # (emissions1/concentration1) / (emissions2/concentration2)
  # This is equivalent to: (emissions1 * concentration2) / (emissions2 * concentration1)
  equivalence_ratio_raster <- impact_ratio1 / impact_ratio2
  # Alternative calculation:
  # equivalence_ratio_raster <- (source1_adj * conc2) / (source2_adj * conc1)
  # This might be more stable if some concentrations are very small.
  # However, stick to the definition for now.

  # Add names to the output raster layers if input rasters had names
  if (!is.null(names(conc1))) {
      names(equivalence_ratio_raster) <- paste0("equiv_ratio_", names(conc1))
  } else if (!is.null(names(conc2))) {
      names(equivalence_ratio_raster) <- paste0("equiv_ratio_", names(conc2))
  } else {
      names(equivalence_ratio_raster) <- paste0("equiv_ratio_layer", 1:raster::nlayers(equivalence_ratio_raster))
  }

  return(equivalence_ratio_raster)
}

# Test data and example usage (commented out in original, useful for understanding)
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
[end of R/particle_ratio.R]
