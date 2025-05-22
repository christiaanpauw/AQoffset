#' Analyze Raster Layer Exceedances Across Multiple Thresholds
#'
#' This function processes a `RasterStack` or `RasterBrick` (`s`) to analyze how
#' many cells exceed a sequence of thresholds. For each threshold in a defined
#' sequence (from `min` to `max` by `by`), it calculates the total number of cells
#' in the input raster `s` (optionally filtered by pollutant `pol`) that are
#' greater than that threshold.
#'
#' The output is a `RasterBrick` where each layer corresponds to one of the
#' thresholds in the sequence. All cells *within* a specific layer of the output
#' brick have the same constant value: the total count of cells from the input `s`
#' that exceeded that layer's corresponding threshold. Layers for which this
#' count is zero (i.e., no cells exceeded the threshold) are converted to `NA` values.
#'
#' @note
#' The function `knipNA` is called if `knip=TRUE` but is not defined within this
#' package or standard R. This will cause an error if `knip=TRUE` and `knipNA`
#' is not available in the environment.
#' The interpretation of "count exceed" here is specific: it's not a per-cell
#' count of exceedance frequency, but rather spatial total of cells exceeding
#' for each threshold.
#'
#' @param s A `RasterStack` or `RasterBrick` object.
#' @param pol An optional character string. If provided, `s` is filtered to
#'   include only layers whose names contain `pol`. If `NULL` (default), all
#'   layers in `s` are processed.
#' @param min Numeric. The starting value for the sequence of exceedance
#'   thresholds. Defaults to 0.
#' @param max Numeric. The maximum value for the sequence of exceedance
#'   thresholds. Defaults to 100.
#' @param by Numeric. The increment for the sequence of exceedance thresholds.
#'   Defaults to 10.
#' @param knip Logical. If `TRUE`, the function attempts to crop the output
#'   raster using an external function `knipNA` to remove NA-only rows/columns.
#'   Defaults to `FALSE`. See note about `knipNA`.
#' @return A `RasterBrick`. Each layer corresponds to a threshold value defined
#'   by `seq(min, max, by)`. The values in each layer are constant and represent
#'   the total number of cells in the input `s` (after optional `pol` filtering)
#'   that exceeded that specific threshold. Layers for thresholds with zero
#'   exceeding cells are set to `NA`. Layer names indicate the threshold
#'   (e.g., "more.than.10").
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("raster", quietly = TRUE)) {
#'   library(raster)
#'   # Create a sample RasterStack
#'   r1 <- raster(nrows=10, ncols=10, val=runif(100, 0, 50))
#'   r2 <- raster(nrows=10, ncols=10, val=runif(100, 0, 50))
#'   s_sample <- stack(r1, r2)
#'   names(s_sample) <- c("LayerA_pollutantX", "LayerB_pollutantX")
#'
#'   # Calculate exceedances for thresholds 0, 10, 20, ..., 50
#'   exceed_brick <- count_exceed(s = s_sample, pol = "pollutantX",
#'                                min = 0, max = 50, by = 10)
#'   print(exceed_brick)
#'   # Each layer (e.g., "more.than.10") will be a constant raster.
#'   # The value will be the total number of cells in s_sample's layers
#'   # that were > 10.
#'
#'   # Example: Get the constant value from the first layer
#'   if (nlayers(exceed_brick) > 0) {
#'     print(values(exceed_brick[[1]])[1])
#'   }
#'
#'   # If knipNA function were defined and knip = TRUE:
#'   # cropext_example <- knipNA(exceed_brick[[1]], out = "extent") # If knipNA existed
#'   # exceed_brick_cropped <- crop(exceed_brick, cropext_example) # If knipNA existed
#' }
#' }
count_exceed <- function(s, pol = NULL, min = 0, max = 100, by = 10, knip = FALSE){
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("Package 'raster' is required. Please install it.", call. = FALSE)
  }

  if (!is.null(pol)){
    idx = grep(pattern = pol, x = names(s))
    if (length(idx) == 0) {
      stop("`pol` does not match any layer names in `s`. Use a valid character string or NULL.")
    }
    s_filtered <- s[[idx]] # Use a new variable to avoid altering original s if not intended
  } else {
    s_filtered <- s
  }

  ct <- seq(from = min, to = max, by = by)
  # Create an empty brick with the same extent and resolution as s_filtered
  # but with number of layers equal to length of ct
  b <- raster::brick(s_filtered[[1]], nl = length(ct), values = FALSE) # More robust brick creation

  for (i in 1:length(ct)){
    threshold_value <- ct[i] # Current threshold
    # Define function: returns 1 if x > threshold_value, else 0
    fun <- function(x) { x > threshold_value }
    # Apply this function to s_filtered. result_calc is a boolean RasterStack/Brick.
    result_calc <- raster::calc(s_filtered, fun = fun)
    # Sum all cell values in result_calc. This gives total count of cells exceeding threshold.
    # If s_filtered has multiple layers, this sums across all cells and all layers.
    # If the intent is to sum exceedances *per layer* of s_filtered, then calc should be applied layer-wise
    # or sum should be applied to each layer of result_calc.
    # The current sum(calc(...)) means v is a single number if s_filtered is one layer,
    # or sum over all cells of all layers if s_filtered is multi-layer.
    # Assuming s_filtered should be treated as a single entity for exceedance calculation:
    total_exceeding_cells <- sum(raster::getValues(result_calc), na.rm = TRUE)

    # Set all cells of layer i in brick b to this single sum
    # If total_exceeding_cells is NA (e.g. all s_filtered is NA), it stays NA.
    if (is.na(total_exceeding_cells)) {
        b <- raster::setValues(b, NA, layer = i)
    } else {
        b <- raster::setValues(b, total_exceeding_cells, layer = i)
    }
  }
  names(b) <- paste("more.than.", as.character(ct), sep="")

  # For layers where the sum was 0 (no exceedances), set all values to NA
  # cellStats(b, sum) will give a vector of sums for each layer.
  # Since each layer is constant, this sum is just value * ncell.
  # A simpler check: if the constant value of a layer is 0, set it to NA.
  for (i in 1:raster::nlayers(b)) {
    layer_val <- raster::getValues(b[[i]])[1] # Get the constant value of the layer
    if (!is.na(layer_val) && layer_val == 0) {
      b[[i]] <- NA
    }
  }
  
  # Drop layers that are entirely NA (originally were all zeros)
  # This can be done by checking if the first value (constant value) is NA.
  layers_to_keep_idx <- which(!is.na(raster::getValues(b)[1,])) # Check first row; if constant, this works
  if (length(layers_to_keep_idx) == 0 && raster::nlayers(b) > 0) {
      message("All layers resulted in zero exceedances and were set to NA. Returning empty brick or brick of NAs.")
      # Depending on desired behavior, could return NULL or the NA brick.
  }
  if (length(layers_to_keep_idx) > 0 && length(layers_to_keep_idx) < raster::nlayers(b)) {
      b <- b[[layers_to_keep_idx]]
  }


  if (knip){
    if (exists("knipNA", mode = "function")){ # Check if knipNA exists as a function
      message("Attempting to knip NA regions using external function 'knipNA'.")
      # knipNA should ideally take a RasterBrick and return an extent or cropped brick
      # Assuming knipNA is designed to work on a single layer for extent calculation:
      if (raster::nlayers(b) > 0) {
        cropext <- knipNA(b[[1]], out = "extent") # Assuming knipNA exists and has this signature
        b <- raster::crop(b, cropext)
      } else {
        message("'knip' is TRUE, but the raster to be cropped has no layers.")
      }
    } else {
      warning("'knip' is TRUE, but the function 'knipNA' was not found in the environment.")
    }
  }
  return(b)
}

#' Bar Plot of Raster Layer Summaries (Exceedances)
#'
#' Creates a bar plot summarizing values from a `RasterBrick` or `RasterStack`,
#' typically one generated by `count_exceed`. Each bar represents a layer
#' (e.g., an exceedance threshold), and its height is the sum of cell values
#' in that layer.
#'
#' @param z A `RasterBrick` or `RasterStack`. Often the output of `count_exceed()`.
#'   For `count_exceed` output, each layer is constant, so `cellStats(z, sum)`
#'   would be `value * ncell`.
#' @param cap Character string. Caption for the plot (currently not used in function body).
#'   Defaults to "".
#' @param ttl Character string. Main title for the bar plot. Defaults to "".
#' @param axn Logical. If `TRUE` (default), custom axis labels derived from layer
#'   names are drawn. If `FALSE`, default barplot axis naming occurs.
#' @param ces.ax Numeric. `cex.axis` for the custom x-axis labels. Defaults to 0.9.
#' @param yl Character string. Label for the y-axis. Defaults to "Events: days x cell".
#' @param xl Character string or expression. Label for the x-axis. Defaults to
#'   `expression(paste(mu,plain(g/m)^3))`.
#' @param plot Logical. If `TRUE` (default), a bar plot is generated. If `FALSE`,
#'   the matrix of sums (heights of bars) is returned instead.
#' @param ... Additional arguments passed to `barplot()`.
#' @return If `plot = TRUE`, invisibly returns the result of `barplot()` (typically
#'   a numeric vector of bar midpoints). If `plot = FALSE`, returns a matrix
#'   where each entry is the sum of cells for the corresponding layer in `z`.
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("raster", quietly = TRUE)) {
#'   library(raster)
#'   # Create a sample RasterBrick (e.g., from count_exceed)
#'   # Layer 1: all cells = 500 (e.g., 500 cells exceeded threshold A)
#'   # Layer 2: all cells = 200 (e.g., 200 cells exceeded threshold B)
#'   r_layer1 <- raster(nrows=10, ncols=10, val=500)
#'   r_layer2 <- raster(nrows=10, ncols=10, val=200)
#'   exceed_data <- brick(r_layer1, r_layer2)
#'   names(exceed_data) <- c("more.than.10", "more.than.50")
#'
#'   # Generate bar plot
#'   bar_exceed(z = exceed_data,
#'              ttl = "Exceedance Summary",
#'              yl = "Total Cells Exceeding Threshold",
#'              xl = "Threshold Value")
#'
#'   # Get the data instead of plotting
#'   exceed_sums <- bar_exceed(z = exceed_data, plot = FALSE)
#'   print(exceed_sums)
#' }
#' }
bar_exceed <- function(z,
                       cap = "", # cap is not used
                       ttl = "",
                       axn = TRUE,
                       ces.ax = 0.9,
                       yl = "Events: days x cell",
                       xl = expression(paste(mu,plain(g/m)^3)),
                       plot = TRUE,
                       ...) {
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("Package 'raster' is required. Please install it.", call. = FALSE)
  }
  # For count_exceed output, each layer is constant. Sum is value * ncell.
  # If z can have varying values per cell, cellStats(z, sum) is appropriate.
  mxStats <- raster::cellStats(z, stat = sum, na.rm = TRUE) # Vector of sums, one for each layer

  if (plot) {
    mp <- graphics::barplot(height = mxStats,
            beside = TRUE, # beside=TRUE is for grouped bars; here mxStats is 1D vector.
            las = 1, # Style of axis labels (0=parallel, 1=horizontal, 2=perpendicular, 3=vertical)
            main = ttl, ylab = yl, xlab = xl,
            names.arg = if(!axn) names(z) else NULL, # Use layer names if axn is FALSE for default labeling
            ...)
    if (axn) { # Custom axis labels
      graphics::axis(1, at = mp, labels = gsub("greater_than_|more.than.", ">", names(z)),
           cex.axis = ces.ax, las = 2) # las=2 for perpendicular labels
    }
    invisible(mp) # Return bar midpoints invisibly
  } else {
    return(as.matrix(mxStats)) # Return as a column matrix
  }
}

#' Print Summary Statistics of Raster Layers (Exceedances)
#'
#' Generates a formatted table summarizing cell statistics (sum of cell values)
#' for each layer in a `RasterBrick` or `RasterStack`. This is typically used
#' with outputs from functions like `count_exceed`.
#'
#' @param z A `RasterBrick` or `RasterStack`.
#' @param type Character string. Specifies the output format.
#'   "k" for `knitr::kable` (default), "l" for `Hmisc::latex`.
#' @param ttl Character string. Title for the summary column in the table.
#'   Defaults to "Count of exceedences".
#' @return If `type = "k"`, returns a `knitr::kable` object.
#'   If `type = "l"`, calls `Hmisc::latex()` which prints LaTeX code and
#'   returns a `latex` object.
#'   Requires `knitr` or `Hmisc` package depending on `type`.
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("raster", quietly = TRUE) && requireNamespace("knitr", quietly = TRUE)) {
#'   library(raster)
#'   # Create a sample RasterBrick (e.g., from count_exceed)
#'   r_layer1 <- raster(nrows=10, ncols=10, val=500)
#'   r_layer2 <- raster(nrows=10, ncols=10, val=200)
#'   exceed_data <- brick(r_layer1, r_layer2)
#'   names(exceed_data) <- c("more.than.10", "more.than.50")
#'
#'   # Print kable output
#'   kable_output <- print_exceed(z = exceed_data, type = "k",
#'                                ttl = "Total Cells Exceeding")
#'   print(kable_output)
#'
#'   # For LaTeX output (requires Hmisc and a LaTeX environment)
#'   # if (requireNamespace("Hmisc", quietly = TRUE)) {
#'   #   latex_output <- print_exceed(z = exceed_data, type = "l")
#'   #   # To view, you'd typically compile this in a LaTeX document.
#'   # }
#' }
#' }
print_exceed <- function(z, type = c("k", "l")[1], ttl = "Count of exceedences"){
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("Package 'raster' is required. Please install it.", call. = FALSE)
  }
  # As with bar_exceed, cellStats(z, sum) is used.
  dt_values <- raster::cellStats(z, stat = sum, na.rm = TRUE)
  dt <- data.frame(Value = dt_values) # Create a data frame
  rownames(dt) <- gsub("greater_than_|more.than.", ">", names(z)) # Use > for brevity
  colnames(dt) <- ttl # Set the column name to ttl

  if (type == "k") {
    if (!requireNamespace("knitr", quietly = TRUE)) {
      stop("Package 'knitr' is required for type='k'. Please install it.", call. = FALSE)
    }
    return(knitr::kable(dt, caption = ttl)) # Add caption to kable
  }
  if (type == "l") {
    if (!requireNamespace("Hmisc", quietly = TRUE)) {
      stop("Package 'Hmisc' is required for type='l'. Please install it.", call. = FALSE)
    }
    # Hmisc::latex has many options. This is a basic call.
    # The title argument in Hmisc::latex refers to the LaTeX table caption.
    return(Hmisc::latex(dt, title = ttl, rowlabel = "Threshold", file = "")) # file="" to print to console
  }
  # If type is neither 'k' nor 'l'
  message("Invalid type specified. Returning raw data frame.")
  return(dt)
}

# Example of internal data structure (not run):
# ff <- freq(kwaza_API, merge = TRUE) # kwaza_API not defined here
# rownames(ff) <- ff[,1]
# ff <- ff[,-1]
# rowSums(ff, na.rm = TRUE)
[end of R/count_exceed.R]
