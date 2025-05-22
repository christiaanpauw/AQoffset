#' Generate 'at' Sequence for Levelplot or Similar Visualizations
#'
#' This convenience function creates a sequence of break points (often referred to
#' as `at` values in functions like `rasterVis::levelplot`) for color scales in
#' raster visualizations. It offers several options to control the range and
#' characteristics of the sequence, such as starting at zero, enforcing symmetry
#' around zero, or using user-defined minimum/maximum values.
#'
#' @param r A `Raster*` object (e.g., `RasterLayer`, `RasterStack`, `RasterBrick`)
#'   from which minimum and maximum values can be determined if not overridden.
#' @param n Numeric. The desired number of intervals in the sequence. The output
#'   vector will have `n + 1` break points. Defaults to 20.
#' @param beginnul Logical. If `TRUE`, the minimum value of the sequence (`mn`)
#'   is set to 0. This overrides other minimum settings unless `simetries` is also
#'   `TRUE` and leads to a negative minimum. Defaults to `FALSE`.
#' @param maksimum Numeric. An optional user-defined maximum value for the sequence.
#'   If `NULL` (default), the maximum is derived from `maxValue(r)` or calculated
#'   symmetrically if `simetries = TRUE`.
#' @param minimum Numeric. An optional user-defined minimum value for the sequence.
#'   If `NULL` (default), the minimum is derived from `minValue(r)` or calculated
#'   based on `beginnul` or `simetries` settings. This parameter is overridden
#'   by `beginnul = TRUE` or `simetries = TRUE`.
#' @param simetries Logical. If `TRUE`, the sequence is made symmetric around zero,
#'   i.e., `mn = -mx` (or `mx = -mn` if `mn` is defined and positive).
#'   The maximum absolute value determines the range. Defaults to `FALSE`.
#' @return A numeric vector of `n + 1` break points suitable for use as `at`
#'   values in plotting functions.
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("raster", quietly = TRUE)) {
#'   library(raster)
#'
#'   # Create a sample raster
#'   r_sample <- raster(nrows = 10, ncols = 10)
#'   set.seed(123)
#'   values(r_sample) <- runif(ncell(r_sample), min = -50, max = 150)
#'
#'   # Default 'at' sequence with 20 intervals
#'   at_default <- maak_at(r_sample)
#'   print(at_default)
#'
#'   # Sequence with 10 intervals
#'   at_10_intervals <- maak_at(r_sample, n = 10)
#'   print(at_10_intervals)
#'
#'   # Sequence starting at zero
#'   at_start_zero <- maak_at(r_sample, beginnul = TRUE)
#'   print(at_start_zero)
#'
#'   # Symmetric sequence around zero
#'   at_symmetric <- maak_at(r_sample, simetries = TRUE, n = 10)
#'   print(at_symmetric) # Should be like c(-150, ..., 0, ..., 150)
#'
#'   # User-defined maximum
#'   at_custom_max <- maak_at(r_sample, maksimum = 200)
#'   print(at_custom_max)
#'
#'   # User-defined minimum (will be overridden by simetries or beginnul if true)
#'   at_custom_min <- maak_at(r_sample, minimum = -20, n = 5)
#'   print(at_custom_min)
#'
#'   # Symmetric sequence with a defined maximum (minimum becomes -maximum)
#'   at_sym_with_max <- maak_at(r_sample, maksimum = 100, simetries = TRUE, n = 4)
#'   print(at_sym_with_max) # Should be c(-100, -50, 0, 50, 100)
#'
#'   # Sequence starting at 0, but also symmetric (results in only positive or zero)
#'   # In this case, simetries would try to make mn = -mx. If beginnul=T, mn=0.
#'   # The logic will prioritize: simetries then beginnul then minimum.
#'   at_zero_sym <- maak_at(r_sample, beginnul = TRUE, simetries = TRUE)
#'   print(at_zero_sym) # If mx determined, mn becomes -mx. If then beginnul=T, mn becomes 0.
#'                      # The current logic: simetries sets mn=-mx. beginnul then sets mn=0.
#'                      # So if mx > 0, range becomes [0, mx].
#' }
#' }
maak_at <- function(r, n = 20, beginnul = FALSE, maksimum = NULL, minimum = NULL, simetries = FALSE){
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("Package 'raster' is required. Please install it.", call. = FALSE)
  }
  if (!inherits(r, "Raster")) {
      stop("Input 'r' must be a Raster* object.")
  }
  if (n <= 0) {
      stop("'n' must be a positive integer for the number of intervals.")
  }

  # Determine initial maximum (mx)
  if (is.null(maksimum)) {
    # Use ceiling to get a round number if not user-defined
    mx <- ceiling(raster::maxValue(r, na.rm = TRUE))
  } else {
    mx <- maksimum
  }

  # Determine initial minimum (mn)
  if (is.null(minimum)) {
    mn <- floor(raster::minValue(r, na.rm = TRUE))
  } else {
    mn <- minimum
  }

  # Adjust mn and mx based on logical flags
  if (simetries) {
    max_abs <- max(abs(c(mn, mx)), na.rm = TRUE) # Determine largest absolute value
    mx <- max_abs
    mn <- -max_abs
  }

  if (beginnul) {
    # If simetries was true, mn might be negative. beginnul forces it to 0.
    # If simetries was false, mn is either from raster or user 'minimum'.
    mn <- 0
    if (mx < 0 && simetries == FALSE) { # If original max was negative and we start at 0
        mx <- 0 # Avoid mn > mx, though seq would handle it by creating a decreasing sequence.
                # Or, user might intend [0, original_negative_max_as_positive]
                # Forcing mx = 0 if mx < mn might be too opinionated.
                # Let seq handle mn > mx if that's the case.
    }
  }
  
  # Final check for user-defined minimum if other flags didn't set it
  # This logic is a bit convoluted. The order of operations for flags matters.
  # Current precedence: simetries -> beginnul -> explicit minimum.
  # If 'minimum' was set by user, but 'beginnul' or 'simetries' is TRUE, 'minimum' is overridden.
  # The original code had: `if (!is.null(minimum)) mn <- minimum` at the end,
  # which means user-supplied 'minimum' would override 'beginnul' and 'simetries' effects on 'mn'.
  # Let's stick to a clearer precedence: specific flags override general min/max from raster,
  # and explicit min/max parameters override raster values but can be modified by flags.

  # Re-apply explicit minimum if no other flag has set mn, or to override raster min.
  if (!is.null(minimum) && !beginnul && !simetries) {
      mn <- minimum
  }
  # Re-apply explicit maksimum (already handled, but ensure consistency)
  if (!is.null(maksimum)) {
      mx <- maksimum
  }
  
  # Ensure mn <= mx for a typical increasing sequence.
  # If after all adjustments mn > mx, seq() will produce a decreasing sequence or error if length.out=1.
  # Or we can enforce mn <= mx.
  if (mn > mx) {
      warning(paste("Calculated minimum (", mn, ") is greater than maximum (", mx,
                    "). Sequence will be decreasing or may error if n=0.", sep=""))
      # Optionally, could swap: temp = mn; mn = mx; mx = temp;
      # Or set mn = mx for a single point if n=0, though n must be >0.
  }
  if (mn == mx && n > 0) { # If range is zero, create a sequence of identical values
      at <- rep(mn, n + 1)
  } else {
      # Use length.out to ensure exactly n+1 points (n intervals)
      at <- seq(from = mn, to = mx, length.out = n + 1)
  }

  return(at)
}
[end of R/maak_at.R]
