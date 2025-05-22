#' Create a Summary Table of Mean Raster Values for Impact Assessment
#'
#' This function generates a table summarizing the mean values of specific
#' pollutant layers from three input raster datasets: baseline, project, and
#' deficit (or difference). It's designed to provide a quick comparison of
#' these scenarios.
#'
#' For each input raster (`base`, `proj`, `def`), the function identifies layers
#' containing the pollutant specified by `pmvar` (using `grep` on layer names).
#' It then calculates the mean of cell values for the selected layer(s).
#' The `summary()` function is then applied to this mean value(s).
#'
#' @note If `grep(pmvar, names(raster_input))` matches multiple layers,
#'   `raster::cellStats(raster_input_matched_layers, mean)` will return a vector
#'   of mean values (one for each matched layer). The `summary()` function will then
#'   summarize this vector. If `grep` matches only a single layer, `cellStats`
#'   returns a single mean value, and `summary()` on this single value will produce
#'   a six-number summary where all values (Min., 1st Qu., Median, Mean, 3rd Qu., Max.)
#'   are identical to that single mean. This behavior should be considered when
#'   interpreting the output.
#'
#' @param base A `Raster*` object (e.g., `RasterLayer`, `RasterStack`) representing
#'   the baseline scenario. Layer names should allow selection via `pmvar`.
#' @param proj A `Raster*` object representing the project scenario.
#' @param def A `Raster*` object representing the deficit or difference scenario
#'   (e.g., project - baseline).
#' @param pmvar Character string used as a pattern for `grep` to select the
#'   relevant pollutant layer(s) from `base`, `proj`, and `def`. Defaults to "pm".
#' @param impname Character string. The row name used for the `def` scenario in the
#'   output table. Defaults to "Deficit".
#' @return A `data.frame` with three rows (named "Baseline", "Project", and the
#'   value of `impname`). The columns are the six standard summary statistics
#'   (Min., 1st Qu., Median, Mean, 3rd Qu., Max.) applied to the mean cell value(s)
#'   of the selected pollutant layer(s) for each scenario.
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("raster", quietly = TRUE)) {
#'   library(raster)
#'
#'   # Create sample RasterLayers
#'   base_pm_layer <- raster(nrows=10, ncols=10, val=runif(100, 5, 15))
#'   names(base_pm_layer) <- "concentration_pm2.5_baseline"
#'
#'   proj_pm_layer <- raster(nrows=10, ncols=10, val=runif(100, 10, 20))
#'   names(proj_pm_layer) <- "concentration_pm2.5_project"
#'
#'   def_pm_layer <- proj_pm_layer - base_pm_layer
#'   names(def_pm_layer) <- "concentration_pm2.5_difference"
#'
#'   # Generate the impact table focusing on "pm2.5"
#'   impact_summary_table <- make_impact_table(base = base_pm_layer,
#'                                             proj = proj_pm_layer,
#'                                             def = def_pm_layer,
#'                                             pmvar = "pm2.5",
#'                                             impname = "Difference (Proj-Base)")
#'   print(impact_summary_table)
#'   # Expected output: A data frame where each row provides a 6-number summary.
#'   # If only one layer matches "pm2.5" in each input, the summary for that row
#'   # will have all 6 values equal to the mean of that layer.
#'
#'   # Example with multi-layer stack (if pmvar matches multiple)
#'   base_multi <- stack(base_pm_layer, base_pm_layer * 1.1)
#'   names(base_multi) <- c("scenario1_pm_total", "scenario2_pm_total")
#'   proj_multi <- stack(proj_pm_layer, proj_pm_layer * 0.9)
#'   names(proj_multi) <- c("scenario1_pm_proj", "scenario2_pm_proj")
#'   def_multi <- proj_multi - base_multi # This might not be meaningful if layers differ
#'   names(def_multi) <- c("def1_pm", "def2_pm")
#'
#'   multi_layer_table <- make_impact_table(base_multi, proj_multi, def_multi,
#'                                          pmvar = "pm", impname = "Avg Deficit")
#'   print(multi_layer_table)
#'   # Here, cellStats will average the two layers in base_multi, then summary is on that.
#'   # If grep finds multiple layers, cellStats(stack, mean) returns a vector of means.
#'   # Summary of this vector is then calculated. This is a more typical use of summary().
#' }
#' }
make_impact_table <- function(base, proj, def, pmvar = "pm", impname = "Deficit"){
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("Package 'raster' is required. Please install it.", call. = FALSE)
  }

  # Helper function to get layer indices and calculate summary of means
  get_summary_of_means <- function(raster_obj, pollutant_var) {
    layer_indices <- grep(pollutant_var, names(raster_obj))
    if (length(layer_indices) == 0) {
      warning(paste("No layers matching '", pollutant_var, "' found in raster: ",
                    deparse(substitute(raster_obj)), ". Returning NA summary.", sep=""))
      # Return a data frame of NAs that matches summary() output structure
      return(summary(as.numeric(NA))) # Summary of NA gives 6 NAs
    }
    # Calculate mean of cell values for selected layer(s)
    # If multiple layers, cellStats returns a vector of means.
    # If one layer, cellStats returns a single mean value.
    mean_values <- raster::cellStats(raster_obj[[layer_indices]], mean, na.rm = TRUE)
    return(summary(mean_values)) # summary() on vector or single number
  }

  summary_base <- get_summary_of_means(base, pmvar)
  summary_proj <- get_summary_of_means(proj, pmvar)
  summary_def <- get_summary_of_means(def, pmvar)

  # Combine into a data frame
  # rbind will handle if summaries are single numbers (converted to data frame row)
  # or vectors (summary output is already a named vector)
  res <- as.data.frame(rbind(
    Baseline = summary_base,
    Project = summary_proj,
    Deficit_temp_name = summary_def # Use temp name for row
  ))
  
  # Change the rowname for the deficit part
  rownames(res)[rownames(res) == "Deficit_temp_name"] <- impname
  
  return(res)
}
[end of R/make_impact_table.R]
