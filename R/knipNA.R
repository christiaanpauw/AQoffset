#' Crop Raster to Minimal Extent of Non-NA Values
#'
#' This function identifies the smallest possible bounding box that contains all
#' non-NA cells in an input `RasterLayer`. It can then return either this
#' calculated `Extent` object or a new `RasterLayer` cropped to this extent.
#'
#' The function works by converting the raster to a matrix indicating NA positions,
#' then determining the first and last rows and columns that contain at least one
#' non-NA value.
#'
#' @note The check `attr(class(r), "package") != "raster"` is used to validate
#'   the input raster type, which is less common than `inherits(r, "RasterLayer")`.
#'
#' @param r A `RasterLayer` object.
#' @param out Character string specifying the type of output. Must be either
#'   `"extent"` (to return an `Extent` object) or `"raster"` (to return a
#'   cropped `RasterLayer`). Defaults to `"extent"`.
#' @return Depending on the `out` parameter:
#'   \itemize{
#'     \item If `out = "extent"`, an `Extent` object.
#'     \item If `out = "raster"`, a new `RasterLayer` object cropped to the
#'       minimal extent of non-NA cells.
#'   }
#'   If the input raster `r` contains no non-NA cells, the original extent of `r`
#'   is returned (if `out = "extent"`) or `r` is returned uncropped (if `out = "raster"`
#'   after being processed by `crop` with its original extent).
#' @references Based on an answer by Marie Auger-Methe on Stack Overflow:
#'   \url{http://stackoverflow.com/questions/19680079/r-crop-no-data-of-a-raster}
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("raster", quietly = TRUE)) {
#'   library(raster)
#'
#'   # Create a sample raster with NA borders
#'   r <- raster(nrows = 10, ncols = 10, xmn = 0, xmx = 10, ymn = 0, ymx = 10)
#'   values(r) <- NA
#'   # Add some non-NA data in the middle
#'   r[3:7, 4:8] <- runif(25) # A 5x5 block of data
#'
#'   # Get the minimal extent object
#'   min_extent <- knipNA(r, out = "extent")
#'   print("Minimal Extent:")
#'   print(min_extent)
#'   # Expected: xmin=3, xmax=8, ymin=2, ymax=7 (adjusting for cell indexing vs. extent)
#'   # Raster extent is 1-based for rows/cols from origin, check actual output carefully.
#'   # The example on SO uses cell numbers. The function translates to extent.
#'
#'   # Get the cropped raster
#'   r_cropped <- knipNA(r, out = "raster")
#'   print("Cropped Raster:")
#'   print(r_cropped)
#'   # plot(r, main = "Original Raster with NA Borders")
#'   # plot(r_cropped, main = "Cropped Raster (Non-NA Area)")
#'
#'   # Example with an all-NA raster
#'   r_all_na <- raster(nrows = 5, ncols = 5)
#'   values(r_all_na) <- NA
#'   extent_all_na <- knipNA(r_all_na, out = "extent")
#'   print("Extent for all-NA raster:")
#'   print(extent_all_na) # Should be the original extent
#'   raster_all_na_cropped <- knipNA(r_all_na, out = "raster")
#'   print(hasValues(raster_all_na_cropped)) # Should be FALSE or raster is empty
#' }
#' }
knipNA <- function(r, out = c("extent", "raster")[1]){
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("Package 'raster' is required. Please install it.", call. = FALSE)
  }

  # Input validation
  if (!inherits(r, "RasterLayer")) {
    # Changed check to be more standard with S4 classes from raster package
    stop("Input `r` should be a RasterLayer object.")
  }
  if (length(out) != 1 || !out %in% c("extent", "raster")) {
    stop("`out` parameter must be either 'extent' or 'raster'.")
  }

  # Get a matrix indicating NA positions (TRUE for NA, FALSE for non-NA)
  r_na_matrix <- matrix(is.na(raster::getValues(r)), nrow = raster::nrow(r), byrow = TRUE)

  # Find columns and rows that are not entirely NA
  # colSums(r_na_matrix) gives count of NAs in each column. If count == nrow(r), col is all NA.
  # rowSums(r_na_matrix) gives count of NAs in each row. If count == ncol(r), row is all NA.
  cols_with_data <- which(colSums(r_na_matrix) != raster::nrow(r))
  rows_with_data <- which(rowSums(r_na_matrix) != raster::ncol(r))

  # If there's any data, calculate the new extent
  if (length(rows_with_data) > 0 && length(cols_with_data) > 0) {
    # Determine the extent from the first and last rows/columns containing data
    # rowNotNA[1] is the first row index with data
    # rowNotNA[length(rowNotNA)] is the last row index with data
    # colNotNA[1] is the first col index with data
    # colNotNA[length(colNotNA)] is the last col index with data
    # raster::extent() can take cell numbers or row/col numbers to define extent
    # The original function uses r, first_row, last_row, first_col, last_col
    new_extent <- raster::extent(r,
                                 r1 = rows_with_data[1],
                                 r2 = rows_with_data[length(rows_with_data)],
                                 c1 = cols_with_data[1],
                                 c2 = cols_with_data[length(cols_with_data)])
  } else {
    # No data found (all cells are NA) or only lines of NAs, return original extent
    # Or, if one dimension has data but other doesn't (e.g. a single row/col of data)
    # this case might need specific handling if extent from r,1,1,1,1 is not desired.
    # For an all-NA raster, rows_with_data/cols_with_data will be empty.
    new_extent <- raster::extent(r) # Default to original extent if no non-NA cells
  }

  if (out == "extent"){
    return(new_extent)
  } else { # out == "raster"
    # Crop the original raster to the new extent
    r_cropped <- raster::crop(r, new_extent)
    return(r_cropped)
  }
  # The original code had a comment "#Plot the rasters for comparison."
  # This is typically not done inside a non-plotting utility function.
}
[end of R/knipNA.R]
