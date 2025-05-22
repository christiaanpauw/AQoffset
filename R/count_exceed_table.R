#' Create a Formatted Table of Raster Layer Summaries (Exceedances)
#'
#' This function takes a `RasterBrick` or `RasterStack` (`x`), typically the
#' output of the `count_exceed()` function, and generates a summary table.
#' The table shows each concentration level (derived from layer names of `x`)
#' and the percentage of total observations that fall into that exceedance category.
#'
#' The input `x` is expected to have layers where each layer represents a
#' threshold, and the cell values (often constant per layer in `count_exceed`
#' output) represent counts or sums related to that threshold. The function
#' normalizes these values by the total sum across all layers and cells to get
#' percentages.
#'
#' @param x A `RasterBrick` or `RasterStack`. Typically, this is the output from
#'   the `count_exceed()` function, where layer names indicate thresholds
#'   (e.g., "more.than.10", "more.than.20").
#' @param cp Character string. A caption for the generated table. Defaults to "".
#' @param ... Currently not used by the function.
#' @return A `knitr::kable` object, which is a formatted table suitable for
#'   markdown or other document types. The table has two columns: "Concentration"
#'   (derived from layer names) and "% observations in exceedance".
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("raster", quietly = TRUE) &&
#'     requireNamespace("knitr", quietly = TRUE)) {
#'   library(raster)
#'
#'   # 1. Create sample data (simulating output from count_exceed)
#'   # Layer 1: "more.than.0", sum of values = 1000
#'   # Layer 2: "more.than.10", sum of values = 500
#'   # Layer 3: "more.than.20", sum of values = 200
#'   # (Assuming for count_exceed output, these are constant layers
#'   # where the value * ncell = total_sum_for_layer)
#'   # For simplicity, we'll use cellStats(x, sum) which sums all cells in a layer.
#'
#'   r1 <- raster(nrows=1, ncols=1, val=1000) # Total for threshold 1
#'   r2 <- raster(nrows=1, ncols=1, val=500)  # Total for threshold 2
#'   r3 <- raster(nrows=1, ncols=1, val=200)  # Total for threshold 3
#'   exceed_output_sim <- stack(r1, r2, r3)
#'   names(exceed_output_sim) <- c("more.than.0", "more.than.10", "more.than.20")
#'
#'   # 2. Generate the table
#'   exceed_summary_table <- count_exceed_table(
#'     x = exceed_output_sim,
#'     cp = "Summary of Exceedances by Concentration Threshold"
#'   )
#'
#'   # 3. Print the kable object
#'   print(exceed_summary_table)
#'
#'   # Example with more realistic constant layers from count_exceed
#'   # (where each layer has a constant value representing total cells exceeding)
#'   const_r1 <- setValues(raster(nrows=5,ncols=5), 100) # 100 cells exceeded threshold 1
#'   const_r2 <- setValues(raster(nrows=5,ncols=5), 50)  # 50 cells exceeded threshold 2
#'   const_r3 <- setValues(raster(nrows=5,ncols=5), 20)  # 20 cells exceeded threshold 3
#'   exceed_brick_actual <- brick(const_r1, const_r2, const_r3)
#'   names(exceed_brick_actual) <- c("more.than.0", "more.than.10", "more.than.20")
#'
#'   actual_table <- count_exceed_table(
#'      x = exceed_brick_actual,
#'      cp = "Exceedance Percentages from Constant Layer Brick"
#'   )
#'   print(actual_table)
#' }
#' }
count_exceed_table <- function(x, cp = "", ...){
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("Package 'raster' is required. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' is required for this function. Please install it.", call. = FALSE)
  }

  # The original comment "bar_exceed(x, ttl = ...)" suggests a link, but this function is table-focused.
  x[is.na(x)] <- 0 # Replace NA values with 0 before summing.

  concLevels <- names(x) # Get layer names, e.g., "more.than.10"

  # Calculate sum of cell values for each layer
  # For count_exceed output, each layer is constant, so cellStats(layer, sum) = layer_value * ncell(layer)
  layer_sums <- raster::cellStats(x, sum)

  # Calculate total sum of values across all cells and all layers
  # This is the denominator for percentage calculation.
  # sum(getValues(x)) sums all cell values from all layers.
  # If x comes from count_exceed, its values are counts of cells,
  # so sum(getValues(x)) is total_cell_counts_from_original_raster * number_of_thresholds if not careful.
  # The original `sum(getValues(x))` might be problematic if x has many layers from count_exceed.
  # It should likely be based on the sum of the *original counts represented by each layer* if that's the intent.
  # However, if each layer in `x` represents the *number of cells exceeding a threshold*,
  # then `df <- data.frame(cellStats(x, sum) / sum(getValues(x)), ...)`
  # would mean: (total cells exceeding threshold_i * N_cells_in_layer_i) / (sum over all j (total cells exceeding threshold_j * N_cells_in_layer_j))
  # This seems to be calculating the proportion of the "sum of layer sums".
  # A more intuitive interpretation might be: for each threshold, what percentage of *total area* or *total possible exceedances* does it represent.
  # Given the original code, I will document its current behavior.
  # `sum(getValues(x))` sums all values in the brick. If each layer is a constant value V_i,
  # then sum(getValues(x)) = ncell(x) * sum(V_i for each layer).
  # And cellStats(x,sum) gives a vector [V1*ncell, V2*ncell, ...].
  # So the calculation becomes (Vi*ncell) / (ncell * sum(Vj)) = Vi / sum(Vj).
  # This means it's the proportion of the layer's constant value relative to the sum of all layers' constant values.
  
  total_sum_of_layer_values <- sum(layer_sums, na.rm = TRUE)
  if (total_sum_of_layer_values == 0) {
      warning("Total sum of values in raster 'x' is 0. Percentages will be NaN or Inf.")
      # Create a data frame with 0 percentages or NA if preferred
      percentages <- rep(0, length(layer_sums))
  } else {
      percentages <- (layer_sums / total_sum_of_layer_values) * 100
  }


  df <- data.frame(percentages, stringsAsFactors = FALSE)
  df$concentration <- gsub("\\.", " ", concLevels) # Clean up layer names for display
  # Reorder columns: concentration first, then percentage
  df <- df[, c("concentration", "percentages")]
  names(df) <- c("Concentration", "% observations in exceedance") # Set final column names
  rownames(df) <- NULL # Remove row names

  return(knitr::kable(df, caption = cp, digits = 2))
}
[end of R/count_exceed_table.R]
