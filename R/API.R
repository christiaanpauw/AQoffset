# define RRs
RR.accut.mort <- as.data.frame(t(data.frame(
                            PM10  = c(1.0062, 1.0074,  1.0086,  0.048, 24, 10),
                            PM2.5 = c(1.0110, 1.0150,  1.0190,  0.1 ,  24, 10),
                            SO2   = c(1.0030, 1.0040,  1.0048,  0.026, 24, 10),
                            O3    = c(1.00023, 1.0051, 1.0078,  0.033,  8, 10), # Note: O3 appears twice, R will use the last one
                            O3    = c(1.0028, 1.0046,  1.0066,  0.030,  1, 10), # This will be the O3 definition used
                            NO2   = c(1.0018, 1.003, 1.0034,  0.020,  1, 10),
                            row.names = c("low", "central" ,"high","coef","period.h", "incr")
                            )))
RR.accut.mort[, "stat"] <- c("mean", "mean", "mean", "max", "max", "max") # O3 row name will be O3.1 due to duplication

total.RR <- function(rr){sum((rr-1))}

#' Pollutant Sub Index (PSI)
#'
#' Calculates the Pollutant Sub-Index (PSI) for a given concentration and
#' exposure metric coefficient. The PSI represents the contribution of a single
#' pollutant to the overall Air Pollution Index (API).
#'
#' @param x A numeric value or vector representing the pollutant concentration(s).
#' @param Exp.met A numeric value or vector representing the exposure metric
#'   coefficient(s) corresponding to the pollutant(s) in `x`. If `x` is a
#'   vector, `Exp.met` should typically be a corresponding vector or a single
#'   value to be recycled.
#' @param verbose Logical. If `TRUE`, messages detailing the input values
#'   will be displayed. Defaults to `FALSE`.
#' @return A numeric value or vector representing the calculated Pollutant
#'   Sub-Index(es). The output will have row names set to "pol.sub.idx".
#' @export
#' @examples
#' # Single value calculation
#' conc_pm10 <- 50 # µg/m³
#' coef_pm10 <- 0.048
#' psi_value <- PSI(x = conc_pm10, Exp.met = coef_pm10)
#' print(psi_value)
#'
#' # Vector calculation
#' concentrations <- c(pm10 = 50, pm2.5 = 25, so2 = 10)
#' coefficients <- c(pm10 = 0.048, pm2.5 = 0.1, so2 = 0.026)
#' psi_vector <- PSI(x = concentrations, Exp.met = coefficients, verbose = TRUE)
#' print(psi_vector)

PSI <- function(x, Exp.met, verbose=FALSE){
  psi = x * Exp.met
  if(verbose==TRUE) message("x= ", paste(x, collapse=", "))
  if(verbose==TRUE) message("Exp.met = ", paste(Exp.met, collapse=", "))
  # If x is a named vector, psi will inherit names. If not, it's a simple vector.
  # Forcing row.names is more typical for data.frames.
  # If psi is a vector, this will make it a 1-row matrix.
  # Consider if this is the desired behavior or if returning a named vector is sufficient.
  # For now, retaining original behavior.
  original_names <- names(psi)
  psi <- as.matrix(psi)
  if (!is.null(original_names)) colnames(psi) <- original_names
  row.names(psi) = "pol.sub.idx"
  psi
}

#' Air Pollution Index (API)
#'
#' Calculates the Air Pollution Index (API) using the [PSI()] function for
#' pollutant sub-indices. This function processes a time-series data frame of
#' pollutant concentrations, calculates daily averages, computes PSIs, and
#' then sums them to get the API.
#'
#' It can also reshape the output and assign results to the global environment.
#'
#' @section Non-standard practices:
#' This function exhibits some non-standard behaviors:
#' \itemize{
#'   \item **Global Assignment:** If `cast.df` is `TRUE`, it assigns the
#'     reshaped data frame to `.GlobalEnv` using the name provided in `out`.
#'     It also unconditionally assigns a vector of summed PSIs (named "psi")
#'     to `.GlobalEnv`. This can clutter the global workspace and make
#'     code harder to follow.
#'   \item **Dynamic Package Installation:** The function attempts to install
#'     `openair` and `reshape2` if they are not found. Package dependencies
#'     should ideally be declared in the `DESCRIPTION` file and installed by
#'     the user or package manager, rather than by a function at runtime.
#' }
#'
#' @param conc A data frame containing pollutant concentrations. It **must**
#'   include a column named "date" (of class `POSIXct` or `Date`) and
#'   columns for each pollutant (e.g., `pm10`, `pm2.5`, `so2`).
#'   Pollutant column names should match those in the internal `coefs` data frame
#'   (e.g., pm10, pm2.5, so2, o3mx8, o3mx1, no2h1).
#' @param verbose Logical. If `TRUE`, messages detailing intermediate steps and
#'   data dimensions will be displayed. Defaults to `FALSE`.
#' @param cast.df Logical. If `TRUE`, the resulting PSI values will be melted
#'   and cast into a wide format data frame, where rows are dates and columns
#'   are pollutants. This casted data frame is also assigned to the global
#'   environment. Defaults to `TRUE`.
#' @param out Character string. The name for the casted data frame to be
#'   assigned to the global environment if `cast.df` is `TRUE`.
#'   Defaults to "out".
#' @param TZ Character string. The timezone to be used for date manipulations,
#'   passed to [lubridate::ymd()]. Defaults to "Africa/Johannesburg".
#' @return If `cast.df` is `TRUE`, returns the casted data frame (and assigns
#'   it to `.GlobalEnv`). If `cast.df` is `FALSE`, returns a numeric vector
#'   of summed PSI values for each time point (this vector is also always
#'   assigned to `.GlobalEnv` as "psi").
#' @references Air Quality Index from Cairncross, John and Zunckel Atm.Envir 41 (2007)
#' @export
#' @examples
#' \dontrun{
#' # Example requires 'openair' and 'reshape2' packages.
#' # It also modifies the global environment.
#'
#' # 1. Create sample concentration data
#' sample_dates <- seq(as.POSIXct("2023-01-01 00:00:00", tz = "UTC"),
#'                     by = "hour", length.out = 72)
#' sample_conc_data <- data.frame(
#'   date = sample_dates,
#'   pm10 = runif(72, 10, 100),
#'   so2 = runif(72, 5, 50)
#' )
#'
#' # 2. Call API function
#' # This will create 'psi' and 'my_api_output' in the global environment.
#' api_result <- API(conc = sample_conc_data,
#'                   verbose = TRUE,
#'                   cast.df = TRUE,
#'                   out = "my_api_output",
#'                   TZ = "UTC")
#'
#' # 3. Inspect results
#' print(head(api_result))
#' if (exists("psi")) {
#'   print(head(psi))
#' }
#' if (exists("my_api_output")) {
#'   print(head(my_api_output))
#'   # Clean up global environment
#'   rm(psi, my_api_output)
#' }
#' }

API <- function(conc = conc, # Parameter 'conc' is used before it's assigned a default from itself. This is unusual.
                               # It relies on 'conc' existing in the calling environment or being passed.
                               # Better to set 'conc = NULL' or a more explicit default if intended.
                verbose=FALSE,
                cast.df =TRUE,
                out = "out",
                TZ = "Africa/Johannesburg"){

  # Dynamic package installation (non-standard practice)
  if (requireNamespace("openair", quietly = TRUE) == FALSE){
    message("Attempting to install 'openair' package.")
    install.packages("openair", dependencies = TRUE)
    if(!requireNamespace("openair", quietly = TRUE)) stop("Package 'openair' is required but could not be installed/loaded.")
  }
  if (requireNamespace("reshape2", quietly = TRUE) == FALSE){
    message("Attempting to install 'reshape2' package.")
    install.packages("reshape2", dependencies = TRUE)
    if(!requireNamespace("reshape2", quietly = TRUE)) stop("Package 'reshape2' is required but could not be installed/loaded.")
  }

  # Ensure lubridate is available for ymd
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    message("Attempting to install 'lubridate' package.")
    install.packages("lubridate", dependencies = TRUE)
    if(!requireNamespace("lubridate", quietly = TRUE)) stop("Package 'lubridate' is required but could not be installed/loaded.")
  }


  conc_avg <- openair::timeAverage(conc, avg.time="day", data.thresh=0.1, statistic="mean")
  # Store original dates from averaged data for later use with psi names
  # timeAverage renames the date column to 'date'.
  avg_dates <- conc_avg$date

  coefs <- data.frame(pm10  = c(0.048),
                      pm2.5 = c(0.1), # Not used in default RR.accut.mort for API?
                      so2   = c(0.026),
                      o3mx8  = c(0.033), # from Cairncross et al.
                      o3mx1  = c(0.030), # from Cairncross et al.
                      no2h1   = c(0.020), # from Cairncross et al.
                      # corol8h = c(025), # Typo? CO or O3? 025 is octal for 21. Assuming typo and removing.
                      row.names = c("coefficients")
                      )

  if(verbose==TRUE) message("Available coefficients: ", paste(names(coefs), collapse=", "))
  if(verbose==TRUE) message("Pollutants in input (after timeAverage): ", paste(names(conc_avg), collapse=", "))

  # Filter conc_avg columns to match available coefs (and keep 'date')
  # Ensure 'date' column is not removed if it's not in coefs.
  common_pollutants <- intersect(names(conc_avg), names(coefs))
  if (length(common_pollutants) == 0) {
    stop("No common pollutants found between input data and API coefficients after daily averaging.")
  }
  conc_filtered <- conc_avg[, c("date", common_pollutants), drop = FALSE]
  coefs_filtered <- coefs[, common_pollutants, drop = FALSE]


  if(verbose==TRUE) message("Filtered coefficients: ", paste(names(coefs_filtered), collapse=", "))
  if(verbose==TRUE) message("dim coefs_filtered: ", paste(dim(coefs_filtered), collapse = " by "))
  if(verbose==TRUE) message("dim conc_filtered (pollutant part): ", paste(dim(conc_filtered[,common_pollutants, drop=FALSE]), collapse = " by "))
  if(verbose==TRUE) message("Pollutants being processed: ", paste(names(conc_filtered)[-1], collapse=", ")) # Exclude 'date'

  # Convert date column with specified timezone
  # lubridate::ymd is for year-month-day strings, but conc_avg$date is already POSIXct
  # If re-parsing is needed, ensure format matches. Typically, it's already in correct POSIXct.
  # Forcing timezone:
  conc_filtered$date <- lubridate::with_tz(conc_filtered$date, tzone = TZ)


  # Apply PSI function row-wise to the pollutant columns
  # The PSI function expects x and Exp.met. coefs_filtered is a 1-row data.frame.
  # We need to pass the numeric vector of coefficients.
  psi_values <- apply(conc_filtered[, common_pollutants, drop = FALSE], 1,
                FUN=function(x_row) PSI(x = x_row, Exp.met = as.numeric(coefs_filtered[1, names(x_row)]))
               )
  # apply returns a matrix where columns are original rows, rows are results of PSI.
  # We need to transpose it to have results per date as rows.
  # And then ensure it's a data frame or suitable structure.
  # The current PSI returns a 1-row matrix. So psi_values will be a matrix.
  # If PSI returned a vector, psi_values would be matrix with pollutants as rows, time as cols.

  # If psi_values is a matrix where each column is a time point and each row is a pollutant's PSI:
  if (is.matrix(psi_values)) {
     # If PSI returns a 1-row matrix, psi_values might be [1, n_dates], containing summed PSIs already if Exp.met was a vector.
     # Let's re-check PSI: it returns a 1-row matrix, colnames are pollutant names.
     # So, apply will return a list of these 1-row matrices if Exp.met is specific to x_row names.
     # Or if Exp.met in PSI is always coefs_filtered, it would be a matrix from apply.

     # Current PSI takes x (a row of concs) and Exp.met (all coefs).
     # PSI(x, Exp.met) will internally select matching coefs if Exp.met is named.
     # Or, if Exp.met is just the filtered coefs_filtered[1, names(x_row)], it's fine.
     # The result of PSI is a 1-row matrix: "pol.sub.idx" | pol1 | pol2 | ...

     # This means psi_values will be a matrix of dimensions [1, number_of_dates]
     # where each element is itself a 1-row matrix. This is not ideal.
     # Let's simplify PSI's return for this use case or process its output better.

     # Assuming PSI returns a named vector:
     # psi_output_list <- apply(conc_filtered[, common_pollutants, drop = FALSE], 1,
     #           FUN=function(x_row) {
     #             # Ensure Exp.met passed to PSI is only for pollutants in x_row
     #             current_coefs <- as.numeric(coefs_filtered[1, names(x_row)])
     #             names(current_coefs) <- names(x_row)
     #             PSI(x = x_row, Exp.met = current_coefs) # Assuming PSI returns a named vector [pol1=val1, pol2=val2]
     #           })
     # psi_df <- do.call(rbind, psi_output_list) # This would make a data frame
     # For now, sticking to the original structure as much as possible and documenting.
     # The original code `psi = apply(conc,1,FUN=function(x) PSI(x,coefs))`
     # passed the full `coefs` to `PSI` and `conc` before filtering.
     # This means `PSI` itself must have handled the matching of `x` elements to `coefs`.
     # Let's assume `conc_filtered` has only relevant pollutants and `coefs_filtered` matches them.
     # `PSI(x_row, as.numeric(coefs_filtered[1,]))` if order is guaranteed.
     # Or `PSI(x_row, coefs_filtered[1, names(x_row)])` is safer.

     # The output of the original `apply(conc,1,FUN=function(x) PSI(x,coefs))`
     # where `conc` was the daily averaged data (with a date column)
     # and `coefs` was the full coefficient set, would lead to `psi` being a list if `PSI`
     # returned a matrix. Each element of the list would be the 1-row matrix from `PSI`.
     # Let's assume the goal is a matrix/df of PSI values: rows=dates, cols=pollutants
     
     psi_list <- lapply(1:nrow(conc_filtered), function(i) {
         row_data <- conc_filtered[i, common_pollutants, drop = FALSE]
         # PSI returns a 1-row matrix, extract the numeric vector
         as.numeric(PSI(x=as.numeric(row_data), Exp.met=as.numeric(coefs_filtered[1,common_pollutants])))
     })
     psi_matrix <- do.call(rbind, psi_list)
     colnames(psi_matrix) <- common_pollutants
     psi_df <- as.data.frame(psi_matrix)
     psi_df$date <- avg_dates # Add back the date column

     if(verbose==TRUE) message("Dimensions of PSI matrix (dates x pollutants): ", paste(dim(psi_df), collapse="x"))
  } else {
    stop("PSI calculation did not result in a recognized matrix structure.")
  }

  # Sum PSIs for each date to get the overall API
  # Original code: names(psi) <- dates; psi.s = sapply(psi,sum)
  # This implies 'psi' was a list where each element was the output of PSI for a date,
  # and that output could be summed (e.g., if it was a vector of sub-indices for that date).
  # If PSI's output `c(pol1=val, pol2=val)` (a named vector), then `sapply(psi, sum)` works.
  # Given PSI returns a 1-row matrix, `sum` over that matrix sums all its elements.

  # Summing rows of our psi_df (excluding the date column if added)
  psi_s <- rowSums(psi_df[, common_pollutants, drop = FALSE], na.rm = TRUE)
  names(psi_s) <- avg_dates # Name summed PSIs by date

  # Global assignment (non-standard practice)
  assign("psi", psi_s, envir=.GlobalEnv)
  if(verbose==TRUE) message("'psi' (summed daily PSIs) assigned to global environment.")


  if(cast.df==TRUE){
    # Melting and casting the psi_df (which has dates and pollutant PSIs)
    # We need 'date' for casting.
    psi_df_for_melt <- psi_df
    # If psi_df doesn't have 'date', add it from avg_dates
    if (!"date" %in% names(psi_df_for_melt)) psi_df_for_melt$date <- avg_dates
    
    # Ensure date column is first for typical melt format, or specify id.vars
    psi_df_for_melt <- psi_df_for_melt[, c("date", common_pollutants)]

    suppressMessages( m <- reshape2::melt(psi_df_for_melt, id.vars = "date", variable.name = "pollutant", value.name = "psi_value") )
    if(verbose==TRUE) message("Melted data column names: ", paste(names(m), collapse=", "))

    # Original code cast L1 (date) ~ variable (pollutant). This seems correct.
    # `dcast(data=m, L1~variable, fun.aggregate=sum)` implies m has L1 and variable.
    # Our `m` has `date` and `pollutant`.
    suppressMessages( m_c <- reshape2::dcast(data=m, date ~ pollutant, value.var="psi_value", fun.aggregate=sum) )
    # The sum aggregate here is on psi_value, but there should be only one psi_value per date and pollutant. Sum is okay.

    # Global assignment (non-standard practice)
    assign(paste(out) , m_c , envir=.GlobalEnv)
    if(verbose==TRUE) message("Casted data frame '", out, "' assigned to global environment.")
    return(m_c)
  } else {
    return(psi_s)
  }
}


API.color <- data.frame(API.value=0:10, stringsAsFactors = FALSE,
                       Band = c(rep("low",4),rep("moderate",3),rep("high",3),"very high"),
                       Color = rgb(red  = c(0,   154, 255, 255, 255,  255, 255, 139, 205, 139, 139),
                                   green= c(255, 205, 255, 215, 165,  99,  0,   35,  96,  28,  0),
                                   blue = c(0,   50,  0,   0,   0,    71,  0,   35,  144, 98,  139),
                                   maxColorValue=255)
                        )

#' Raster Air Pollution Index (rasterAPI)
#'
#' Creates a raster representation of the Air Pollution Index (API) from a
#' `RasterStack` of pollutant concentrations. It processes layer names to
#' identify sources, pollutants, and averaging periods, then calculates
#' Pollutant Sub-Indices (PSI) and aggregates them into an API.
#'
#' @note Layer Naming Convention: The names of the layers in the input `RasterStack`
#'   are crucial. They must follow a specific pattern, typically:
#'   `"ID_pollutant_averagingPeriod_otherInfo_cycle"`.
#'   For example, `"SourceA_pm10_24h_scenario1_D01"`. The function parses these
#'   names using `sep` to extract metadata. The positions of ID, pollutant,
#'   averaging period, and cycle are defined by `idpos`, `polpos`,
#'   `aveperiodpos`, and `cyclepos`.
#'
#' @param s A `RasterStack` object where each layer represents a pollutant
#'   concentration surface. Layer names must follow the convention described above.
#' @param reftab A data frame containing reference values for pollutants,
#'   specifically relative risk (RR) coefficients. It must have pollutants as
#'   lowercase rownames (e.g., "pm10", "so2") and a column named "coef"
#'   containing the coefficients for PSI calculation. Defaults to an internal
#'   `RR.accut.mort` table.
#' @param idpos Numeric. The position of the source identifier (ID) in the
#'   layer name string (when split by `sep`). Defaults to 1.
#' @param polpos Numeric. The position of the pollutant name in the layer name
#'   string. Defaults to 2.
#' @param aveperiodpos Numeric. The position of the averaging period information
#'   in the layer name string. Defaults to 3.
#' @param cyclepos Numeric. The position of the cycle information (e.g., day, hour)
#'   in the layer name string, used for temporal aggregation. Defaults to 5.
#' @param sep Character. The separator used in layer names. Defaults to "_".
#' @param aggregate Logical. If `TRUE`, the function can perform aggregation.
#'   If `aggregate = TRUE` and `per_source = TRUE`, it returns mean values per source ID.
#'   If `aggregate = TRUE` and `per_source = FALSE` (default), it calculates the mean of
#'   daily cycle APIs. Defaults to `FALSE`.
#' @param per_source Logical. If `TRUE` and `aggregate = TRUE`, the function
#'   aggregates results per unique source ID and returns a `RasterStack` with
#'   mean values for each source. Defaults to `FALSE`.
#' @param verbose Logical. If `TRUE`, messages detailing intermediate steps,
#'   parsed names, and data dimensions will be displayed. Defaults to `FALSE`.
#' @param return.full Logical. If `TRUE`, the output `RasterStack` will include
#'   the original input layers (`s`), the calculated PSI layers, and the final
#'   API layers. If `FALSE` (default), only the final API layers are returned.
#' @return A `RasterStack` or `RasterLayer`.
#'   - If `aggregate = TRUE` and `per_source = TRUE`, returns a `RasterStack`
#'     of mean values aggregated by source ID.
#'   - Otherwise, returns a `RasterStack` of API values, typically one layer
#'     per cycle (e.g., daily API). If `aggregate = TRUE` (and `per_source = FALSE`),
#'     this is further reduced to a single `RasterLayer` representing the mean API.
#'   - If `return.full = TRUE`, includes input, PSI, and API layers.
#'   - Returns `NULL` implicitly or stops with a message if no matching pollutants
#'     are found in `reftab`.
#' @export
#' @examples
#' \dontrun{
#' # Example requires the 'raster' package.
#' if (requireNamespace("raster", quietly = TRUE)) {
#'   library(raster)
#'
#'   # 1. Create sample RasterStack
#'   r1 <- raster(nrows=10, ncols=10, val=runif(100, 0, 50))
#'   r2 <- raster(nrows=10, ncols=10, val=runif(100, 0, 20))
#'   r3 <- raster(nrows=10, ncols=10, val=runif(100, 0, 60))
#'   r4 <- raster(nrows=10, ncols=10, val=runif(100, 0, 30))
#'
#'   s_sample <- stack(r1, r2, r3, r4)
#'   # Names: ID_pollutant_periodXX_extra_cycle
#'   names(s_sample) <- c("IndA_pm10_24h_sc1_D01", "IndA_so2_24h_sc1_D01",
#'                        "IndA_pm10_24h_sc1_D02", "IndA_so2_24h_sc1_D02")
#'
#'   # 2. Define a reftab (or use default RR.accut.mort if pollutants match)
#'   # Ensure rownames are lowercase pollutants and 'coef' column exists.
#'   custom_reftab <- data.frame(
#'     coef = c(0.05, 0.1, 0.03), # Example coefficients
#'     row.names = c("pm10", "pm2.5", "so2")
#'   )
#'
#'   # 3. Call rasterAPI
#'   api_raster_output <- rasterAPI(s = s_sample,
#'                                  reftab = custom_reftab,
#'                                  verbose = TRUE)
#'
#'   # 4. Inspect output
#'   print(api_raster_output)
#'   plot(api_raster_output)
#'
#'   # Example with aggregation per source
#'   api_per_source <- rasterAPI(s = s_sample, reftab = custom_reftab,
#'                               aggregate = TRUE, per_source = TRUE)
#'   print(api_per_source)
#'
#'   # Example with full return
#'   api_full <- rasterAPI(s = s_sample, reftab = custom_reftab,
#'                         return.full = TRUE)
#'   print(names(api_full))
#' }
#' }
rasterAPI <- function(s,
                      reftab = RR.accut.mort,
                      idpos = 1,
                      polpos = 2,
                      aveperiodpos = 3,
                      cyclepos = 5, # Position for day/cycle identifier
                      sep = "_",
                      aggregate = FALSE, per_source = FALSE,
                      verbose = FALSE,
                      return.full = FALSE){

  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("Package 'raster' is required for rasterAPI function. Please install it.", call. = FALSE)
  }
  # Constructing the pattern for gsub:
  # It expects parts separated by 'sep'.
  # '([[:print:]]+)' matches any printable character, good for general parts.
  # For aveperiodpos, the original pattern '([[:alnum:]]+)([[:punct:]]*)([[:digit:]]*)'
  # was specific. If aveperiodpos is just one part, it simplifies.
  # Let's assume each pos refers to one segment from splitting by sep.
  # The pattern below is trying to capture all parts up to the max used position.
  max_pos <- max(idpos, polpos, aveperiodpos, cyclepos)
  patt_parts <- rep('([[:print:]]+)', max_pos) # Use [[:print:]] for general text, [[:alnum:]] for alphanumeric
  
  # Adjust if specific parts have more constrained patterns, e.g. aveperiodpos
  # The original pattern `patt = paste('([[:print:]]+)', sep,'{1}', '([[:print:]]+)', sep,'{1}','([[:alnum:]]+)([[:punct:]]*)([[:digit:]]*)', sep="")`
  # was trying to capture structure within the aveperiodpos itself.
  # This makes parsing complex. A simpler approach is to split names by 'sep'.

  layer_names <- names(s)
  split_names <- strsplit(layer_names, split = sep, fixed = TRUE)

  # Check if all names split into enough parts
  min_parts_observed <- min(sapply(split_names, length))
  if (min_parts_observed < max_pos) {
    stop(paste("Some layer names do not have enough parts when split by '", sep,
               "'. Expected at least ", max_pos, " parts.", sep=""))
  }

  # Extract parts based on positions
  # Need to handle cases where a name might not have a part at a high cyclepos if not all names are uniform
  safe_extract <- function(name_parts, pos) {
      if (pos <= length(name_parts)) return(name_parts[pos])
      return(NA_character_) # Or handle as an error
  }

  id <- sapply(split_names, safe_extract, pos = idpos)
  pols <- sapply(split_names, safe_extract, pos = polpos)
  per_full <- sapply(split_names, safe_extract, pos = aveperiodpos) # Full period string, e.g., "24h"
  cyc <- sapply(split_names, safe_extract, pos = cyclepos) # Cycle, e.g., "D01"

  # Original parsing for period:
  # patt2 = '([[:digit:]]*)([[:alpha:]]*)' # e.g., "24h" -> "24", "h"
  # per.d = gsub(patt2, "\\1", per_full)
  # per.l = gsub(patt2, "\\2", per_full)
  # This can be done with str_extract from stringr or base R regex
  per.d <- sub("([0-9]+).*", "\\1", per_full) # Extracts leading digits
  per.l <- sub("[0-9]+(.*)", "\\1", per_full) # Extracts trailing non-digits

  if (verbose == TRUE) {
    message("Pattern for parsing layer names implies splitting by '", sep, "'.")
    message("Source IDs (id): ", paste(unique(id), collapse=", "))
    message("Pollutants (pols): ", paste(unique(pols), collapse=", "))
    message("Averaging Periods (per_full): ", paste(unique(per_full), collapse=", "))
    message("  Numeric part of period (per.d): ", paste(unique(per.d), collapse=", "))
    message("  Text part of period (per.l): ", paste(unique(per.l), collapse=", "))
    message("Cycles (cyc): ", paste(unique(cyc), collapse=", "))
  }

  tab <- data.frame(orig = layer_names, id = id, pols = tolower(pols), # Standardize pols to lower for matching
                    per = per_full, per.d = per.d, per.l = per.l, cycle = cyc,
                    stringsAsFactors = FALSE)
  if (verbose == TRUE) message("Parsed layer metadata table (head):\n", paste(utils::capture.output(head(tab)), collapse="\n"))

  # Get the coefs and subset the stack
  rownames(reftab) <- tolower(rownames(reftab)) # Ensure reftab rownames are lowercase
  if (verbose == TRUE){message("Available coefficients in reftab for: ", paste(rownames(reftab), collapse=", "),
                               "\nPollutants found in layers: ", paste(unique(tab$pols), collapse=", "))}

  # Match pollutants from layers with those in reftab
  tab$coefs <- reftab[tab$pols, "coef"] # This will be NA if pol not in reftab

  # Filter out layers for which no coefficient is available
  valid_layers_idx <- !is.na(tab$coefs)
  if (sum(valid_layers_idx) == 0) {
    message("No matching pollutants found between layer names and reftab. Check pollutant names and reftab contents.")
    return(NULL) # Or stop("No processable layers.")
  }
  
  s_filtered <- s[[which(valid_layers_idx)]]
  tab_filtered <- tab[valid_layers_idx, ]
  
  if (verbose == TRUE) message("Processing ", nlayers(s_filtered), " layers with valid coefficients.")

  # Make pollution sub-index (PSI)
  # psi_layers <- s_filtered * tab_filtered$coefs # Raster arithmetic: stack * vector
  # Multiplying a RasterStack by a vector works layer-wise: s_filtered[[i]] * tab_filtered$coefs[i]
  psi_layers <- raster::overlay(s_filtered, fun = function(x) { x * tab_filtered$coefs }) # Should be equivalent and perhaps clearer
  names(psi_layers) <- paste("PSI", names(s_filtered), sep="-")
  if (verbose == TRUE) message("PSI calculation complete. PSI layer names: ", paste(head(names(psi_layers)), collapse=", "))


  # Aggregate to one score per id (source) if requested
  if (aggregate == TRUE && per_source == TRUE) {
    if (verbose == TRUE) message("Aggregating per source ID.")
    # stackApply needs indices that correspond to unique values of tab_filtered$id
    # These indices link each layer in psi_layers to a unique source ID.
    source_id_factor <- as.factor(tab_filtered$id)
    id_s <- raster::stackApply(psi_layers, indices = source_id_factor, fun = mean, na.rm = TRUE)
    names(id_s) <- paste("meanPSI_source", levels(source_id_factor), sep="_")
    return(id_s)
  }

  # Aggregate PSIs by cycle (e.g., sum all PSIs for D01, then for D02)
  # This results in an API per cycle.
  if (verbose == TRUE) message("Aggregating PSIs by cycle: ", paste(unique(tab_filtered$cycle), collapse=", "))
  cycle_factor <- as.factor(tab_filtered$cycle)
  api_by_cycle <- raster::stackApply(psi_layers, indices = cycle_factor, fun = sum, na.rm = TRUE)
  names(api_by_cycle) <- paste("API_cycle", levels(cycle_factor), sep="_")

  if (aggregate == TRUE){ # Further aggregate the per-cycle APIs into one overall API map
    if (verbose == TRUE) message("Aggregating cycle APIs into a single mean API raster.")
    final_api <- raster::calc(api_by_cycle, fun = mean, na.rm = TRUE)
    names(final_api) <- "API_mean_all_cycles"
  } else {
    final_api <- api_by_cycle # Return the API per cycle
  }

  if (verbose == TRUE) message("Final API names: ", paste(names(final_api), collapse=", "))

  if (return.full == TRUE) {
    if (verbose == TRUE) message("Returning full stack: original, PSIs, and APIs.")
    # Ensure no name clashes if s_filtered is not the same as original s
    return(raster::stack(s, psi_layers, final_api)) # Stack original s, calculated PSIs, and final API(s)
  } else {
    return(final_api)
  }
}

[end of R/API.R]
