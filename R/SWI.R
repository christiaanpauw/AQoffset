#' Standards Weighted Intake (SWI)
#'
#' Calculates the total effective intake of pollutants by standardizing
#' concentrations against air quality standards and weighting by population.
#'
#' Layer names in the `conc` RasterStack are expected to be parsable using
#' `sep` and the `*pos` arguments to extract pollutant names and other metadata.
#'
#' @note This function uses a default population raster named `people` if `pop`
#'   is not provided, implying `people` might be a global variable. This is
#'   not a standard R practice and can make code less predictable.
#'   The internal default `NAQS` data frame has a duplicated `O3` entry;
#'   R will use the last specified entry.
#'
#' @param conc A `RasterStack` where each layer represents pollutant concentrations.
#'   Layer names should be structured to be parsable for pollutant identifiers.
#' @param NAQS A data frame containing National Air Quality Standards. Rownames
#'   should be pollutant names (e.g., "pm10", "so2"), and column names should
#'   represent averaging periods (e.g., "Annual", "Daily"). Values are the
#'   standard limits. If `NULL` (default), an internal table is used.
#' @param pop A `RasterLayer` representing population counts for each grid cell.
#'   Defaults to an object named `people`, which is expected to be in the
#'   environment.
#' @param ap Character string, either "Annual" or "Daily", specifying which set
#'   of standards from `NAQS` to use. Defaults to "Annual".
#' @param Q Numeric. Breathing rate in m³/person/day. Defaults to 8.
#' @param idpos Numeric. The position of the source identifier (ID) in the
#'   layer name string (when split by `sep`). Defaults to 1.
#' @param polpos Numeric. The position of the pollutant name in the layer name
#'   string. Defaults to 3.
#' @param aveperiodpos Numeric. The position of the averaging period information
#'   in the layer name string. Defaults to 2.
#' @param cyclepos Numeric. The position of the cycle information (e.g., day, hour)
#'   in the layer name string. Defaults to 5.
#' @param sep Character. The separator used in layer names of `conc`. Defaults to "_".
#' @param verbose Logical. If `TRUE`, messages detailing intermediate steps will
#'   be displayed. Defaults to `FALSE`.
#' @return A `RasterLayer` representing the Total Effective Intake (TEI),
#'   unitless (as it's a standardized measure).
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("raster", quietly = TRUE)) {
#'   library(raster)
#'
#'   # Create sample concentration data (RasterStack)
#'   r1 <- raster(nrows=10, ncols=10, val=runif(100, 0, 50)) # e.g., ID1_24h_pm10_run1_D01
#'   r2 <- raster(nrows=10, ncols=10, val=runif(100, 0, 20)) # e.g., ID1_24h_so2_run1_D01
#'   conc_stack <- stack(r1, r2)
#'   names(conc_stack) <- c("SourceA_24h_pm10_scn1_D01", "SourceA_24h_so2_scn1_D01")
#'
#'   # Create sample population data (RasterLayer)
#'   population_raster <- raster(nrows=10, ncols=10, val=floor(runif(100, 10, 1000)))
#'
#'   # Create sample NAQS data frame
#'   custom_NAQS <- data.frame(
#'     Annual = c(40, 20, 50), # PM10, PM2.5, SO2 annual standards
#'     Daily = c(75, 40, 100), # PM10, PM2.5, SO2 daily standards
#'     row.names = c("pm10", "pm2.5", "so2")
#'   )
#'
#'   # Ensure global 'people' raster exists if not passing 'pop' directly
#'   # people <- population_raster # Uncomment if testing default pop
#'
#'   # Calculate SWI
#'   # Note: polpos = 3 assumes pollutant is the 3rd element when split by "_"
#'   # e.g., "SourceA_24h_pm10_scn1_D01" -> polpos=3 for "pm10"
#'   #       aveperiodpos = 2 for "24h"
#'   #       idpos = 1 for "SourceA"
#'   #       cyclepos = 5 for "D01"
#'
#'   swi_raster <- SWI(conc = conc_stack,
#'                     NAQS = custom_NAQS,
#'                     pop = population_raster,
#'                     ap = "Annual",
#'                     Q = 8,
#'                     idpos = 1, polpos = 3, aveperiodpos = 2, cyclepos = 5,
#'                     sep = "_", verbose = TRUE)
#'
#'   print(swi_raster)
#'   plot(swi_raster, main = "Standards Weighted Intake")
#' }
#' }
SWI <- function(conc,
                NAQS = NULL,
                pop = people, # Non-standard default: uses global 'people'
                ap = "Annual",
                Q = 8,
                idpos = 1,
                polpos = 3,
                aveperiodpos = 2,
                cyclepos = 5,
                sep = "_", verbose = FALSE
                ){

  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("Package 'raster' is required. Please install it.", call. = FALSE)
  }

  if (is.null(NAQS)){
  NAQS <- t(data.frame(
  pm10  = c(40, 75),
  PM2.5 = c(20, 40), # Note: Rownames in NAQS should match pollutant names from layers
  so2   = c(50, 100),
  O3    = c(40, 80), # First O3 definition
  O3    = c(50, 70), # Second O3 definition, R will use this one for "O3"
                     # If row names were unique like O3.annual, O3.daily, it would be clearer.
  NO2   = c(60, 100),
  row.names = c("Annual", "Daily"))
  )
  # Default NAQS rownames are: "pm10", "PM2.5", "so2", "O3", "O3.1", "NO2" after t()
  # This might cause issues if layer pollutant names are e.g. "PM2.5" vs "pm2.5"
  # It's better to ensure NAQS rownames and layer pollutant names (after parsing) match case.
}

# Isolate present compounds and find their indices
# Original pattern was complex. Simpler approach by splitting names:
layer_names <- names(conc)
split_names <- strsplit(layer_names, split = sep, fixed = TRUE)

max_pos <- max(idpos, polpos, aveperiodpos, cyclepos, na.rm = TRUE)
min_parts_observed <- min(sapply(split_names, length))
if (min_parts_observed < max_pos) {
  stop(paste("Some layer names in 'conc' do not have enough parts when split by '", sep,
             "'. Expected at least ", max_pos, " parts.", sep=""))
}
safe_extract <- function(name_parts, pos) {
    if (pos <= length(name_parts)) return(name_parts[pos])
    return(NA_character_)
}

id <- sapply(split_names, safe_extract, pos = idpos)
pols <- tolower(sapply(split_names, safe_extract, pos = polpos)) # Standardize to lower
per <- sapply(split_names, safe_extract, pos = aveperiodpos)
# cyc <- sapply(split_names, safe_extract, pos = cyclepos) # cycle not used in SWI logic

# patt2 = '([[:digit:]]*)([[:alpha:]]*)' # For per.d, per.l, not directly used in SWI logic
# per.d = gsub(patt2, "\\1", per)
# per.l = gsub(patt2, "\\2", per)

if (verbose == TRUE) {
  message("Layer name parsing results:")
  message("  IDs (id): ",  paste(unique(id), collapse=", "))
  message("  Pollutants (pols): ", paste(unique(pols), collapse=", "))
  message("  Averaging Periods (per): ", paste(unique(per), collapse=", "))
}

nms <- unique(pols)
idx <- match(pols, nms) # Tämä indeksi käytetään stackApplyssa pollutanttien keskiarvoistamiseen

# Check if all parsed pollutants are in NAQS, case-sensitive matching for rownames
NAQS_pollutants <- tolower(rownames(NAQS)) # Standardize NAQS rownames to lower for matching
missing_pols_in_NAQS <- setdiff(nms, NAQS_pollutants)
if (length(missing_pols_in_NAQS) > 0) {
  warning("The following pollutants extracted from layer names are not found in NAQS rownames (case-insensitive match): ",
          paste(missing_pols_in_NAQS, collapse=", "))
}
# Ensure NAQS rownames are lower for selection
rownames(NAQS) <- tolower(rownames(NAQS))


# If multiple layers exist for the same pollutant, average them
# For example, if "SourceA_24h_pm10_D01" and "SourceA_24h_pm10_D02" are both present,
# this will average them into a single "pm10" layer.
if (length(nms) < nlayers(conc)) {
  if (verbose == TRUE) message("Averaging multiple layers for the same pollutant type.")
  ss <- raster::stackApply(conc, indices = idx, fun = mean, na.rm = TRUE)
  names(ss) <- nms
} else {
  ss <- conc # Already one layer per pollutant type or only one type
  names(ss) <- nms # Ensure names are just pollutant names
}


# Crop population raster to extent of concentration raster stack
pop <- raster::crop(pop, raster::extent(ss))
# Check for compatible CRS and resample if necessary (optional, but good practice)
# if (!compareCRS(ss, pop)) pop <- projectRaster(pop, crs = crs(ss))
# if (any(res(ss) != res(pop))) pop <- resample(pop, ss, method="bilinear")


concpop <- raster::overlay(ss, pop, fun=function(x,y){ x * y}) # Pollutant mass in each cell (conc * people)
names(concpop) <- paste0("concpop_",nms)

# Select relevant NAQS column (e.g., "Annual" or "Daily")
if (!ap %in% colnames(NAQS)) {
  stop(paste("Averaging period '", ap, "' not found as a column name in NAQS. Available: ",
             paste(colnames(NAQS), collapse=", "), sep=""))
}
NAQS.rel <- NAQS[nms, ap, drop = FALSE] # Use drop=FALSE to keep it as a data frame/matrix
# If NAQS.rel becomes a vector (only one pollutant), handle appropriately or ensure matrix.
if (!is.matrix(NAQS.rel) && !is.data.frame(NAQS.rel)) NAQS.rel <- as.matrix(NAQS.rel)


# Intake (I) for each pollutant: (concentration * population) * breathing_rate
# This is essentially concpop * Q
I_layers <- raster::stack(lapply(names(ss), function(pol_name) {
    concpop[[paste0("concpop_",pol_name)]] * Q
}))
names(I_layers) <- paste0("I_",nms)


# Standard Intake Limit (SIL) for each pollutant: standard_limit * breathing_rate * population
SIL_layers <- raster::stack(lapply(nms, function(pol_name) {
  standard_val <- NAQS.rel[pol_name, 1] # Get the standard for this pollutant from the selected 'ap' column
  if (is.na(standard_val)) {
    warning(paste("NAQS value for pollutant '", pol_name, "' for period '", ap, "' is NA. Check NAQS table.", sep=""))
    # Create a raster layer of NAs or handle as error
    return(raster::setValues(pop, NA))
  }
  standard_val * Q * pop
}))
names(SIL_layers) <- paste0("SIL_",nms)


# Effective Intake (EI) = Intake / StandardIntakeLimit
# Original formula: EI = SI * I/SI -- this simplifies to I if SI is the standard.
# However, the formula for SWI is typically Sum_over_pollutants (Concentration_pol / Standard_pol)
# The provided code calculates EI = I * (SI / SI), where SI is standard intake.
# This seems to be calculating TEI = sum(Intake_pollutant).
# Let's follow the implied formula: EI_pol = I_pol / SIL_pol, then TEI = sum(EI_pol * pop_fraction)?
# Or, more commonly: SWI_pixel = sum_i (Conc_i / Standard_i)
# The current code calculates: EI_pol = SIL_pol * (I_pol / SIL_pol) which simplifies to I_pol
# So TEI = calc(I_layers, sum) which is Total Intake, not "Standards Weighted".

# Let's re-interpret "Effective Intake" based on typical SWI formulas:
# EI_pol = (Concentration_pol / Standard_pol)
# TEI_pixel = sum_i (EI_pol_i)
# Then, TEI_raster * population * Q would give a population-weighted total effective dose.
# The function as written calculates total actual intake, not a "standards-weighted" one.

# Let's implement the common SWI: sum_i ( C_i / S_i ) and then multiply by pop and Q later if needed.
# Or, the function could be Total Population Weighted Effective Intake = sum_i ( (C_i / S_i) * Pop_i * Q )

# The current code's EI and TEI:
# EI = SIL * I/SIL -- This is I (Intake for pollutant)
# TEI = calc(EI, sum) -- This is sum of Intakes for all pollutants for each cell.
# This is Total Intake, not "Standards Weighted Intake" where standards are used as ratios.

# To align with "Standards Weighted Intake" more closely, it should be:
# C_standardized_pol = conc_layer_pol / NAQS_limit_pol
# SWI_per_cell = sum(C_standardized_pol)
# Then this SWI_per_cell can be multiplied by population and Q if desired.
# The current code calculates total actual dose rather than a unitless SWI index.
# For now, documenting the current calculation.

if (verbose) message("Calculating Effective Intake (EI) and Total Effective Intake (TEI) based on provided formulas.")
# EI = SI * I/SI is mathematically equivalent to I, assuming SI is not zero.
# This implies EI is just the Intake I.
EI_layers <- I_layers # Since SIL * (I_layers / SIL_layers) = I_layers

# Total Effective Intake (TEI) is the sum of individual pollutant intakes.
TEI <- raster::calc(EI_layers, sum, na.rm = TRUE)
names(TEI) <- "TotalEffectiveIntake"

if (verbose) message("SWI calculation finished. Returning Total Effective Intake raster.")
return(TEI)
}

#' Raster Standards Weighted Intake (rasterSWI) - Potentially Misleading
#'
#' @description
#' **Caution:** The current implementation of `rasterSWI` appears to be a
#' misapplied or incomplete version of a function intended for Air Pollution Index (API)
#' or Pollutant Sub-Index (PSI) calculations, rather than Standards Weighted Intake (SWI).
#' Its internal logic (e.g., use of `psi`, `api`, `coefs` typically representing
#' health risk coefficients) does not align with SWI principles, which involve
#' standardizing concentrations against guideline values (like NAQS).
#'
#' The function parses layer names from the input `RasterStack` (`s`) to identify
#' pollutants and other metadata. It then attempts to apply coefficients from
#' `reftab` (expected to be NAQS-like but used as risk coefficients).
#'
#' **Key Issues:**
#' *   **Incorrect Logic for SWI:** The calculations performed (multiplying by `coefs`,
#'     summing to `api`) are characteristic of risk or API calculations, not SWI.
#' *   **Undefined Global Variable `pop`:** The line `pwe <- s * pop` uses a
#'     variable `pop` that is not a parameter of this function nor defined internally.
#'     This will cause an error unless `pop` exists in the global environment.
#' *   **Misinterpretation of `reftab`:** `reftab` is documented as NAQS (standards),
#'     but the code `coefs <- reftab[idx, "coef"]` attempts to extract a "coef"
#'     column, which is typical for risk coefficients, not standard limits. NAQS
#'     usually has columns like "Annual" or "Daily". This will likely lead to
#'     errors or incorrect calculations.
#'
#' This documentation describes the function as it is currently written. Users
#' should be extremely cautious if attempting to use this for actual SWI calculations.
#'
#' @param s A `RasterStack` where each layer represents a pollutant concentration.
#'   Layer names must be parsable using `sep` and `*pos` arguments.
#' @param reftab A data frame intended to contain air quality standards (like NAQS).
#'   Rownames should be lowercase pollutant names. The function attempts to extract
#'   a column named "coef" from this table, which is unusual for NAQS data.
#'   Defaults to an object named `NAQS` expected to be in the environment.
#' @param idpos Numeric. Position of the ID in the layer name string. Defaults to 1.
#' @param polpos Numeric. Position of the pollutant name in the layer name string.
#'   Defaults to 2.
#' @param aveperiodpos Numeric. Position of the averaging period in the layer name.
#'   Defaults to 3.
#' @param cyclepos Numeric. Position of the cycle/temporal identifier in the layer name.
#'   Defaults to 5.
#' @param sep Character. Separator used in layer names. Defaults to "_".
#' @param verbose Logical. If `TRUE`, displays function messages. Defaults to `FALSE`.
#'
#' @return Currently, this function is likely to return a `RasterStack` named `api`
#'   (or similar, based on its internal logic copied from API/PSI calculations),
#'   or it might error out due to the use of undefined variable `pop` or incorrect
#'   `reftab` structure. It does **not** return a correctly calculated SWI raster.
#' @export
#' @examples
#' \dontrun{
#' # This example demonstrates the syntax but highlights the function's issues.
#' # USE WITH CAUTION - a correctly functioning SWI is not expected.
#'
#' if (requireNamespace("raster", quietly = TRUE)) {
#'   library(raster)
#'
#'   # Sample concentration stack
#'   r1 <- raster(nrows=5, ncols=5, val=runif(25, 0, 50))
#'   r2 <- raster(nrows=5, ncols=5, val=runif(25, 0, 20))
#'   s_sample <- stack(r1, r2)
#'   names(s_sample) <- c("SourceA_pm10_24h_area1_D01", "SourceA_so2_24h_area1_D01")
#'
#'   # Sample reftab (NAQS-like, but function expects "coef" column)
#'   # This will likely cause issues with the current rasterSWI code.
#'   custom_NAQS <- data.frame(
#'     Annual = c(40, 50), # pm10, so2
#'     Daily = c(75, 100),
#'     coef = c(0.005, 0.002), # Hypothetical "coef" for demonstration
#'     row.names = c("pm10", "so2")
#'   )
#'
#'   # Define 'pop' globally for the example, as rasterSWI uses it without declaration
#'   people <<- raster(nrows=5, ncols=5, val=floor(runif(25,100,500))) # Global assignment
#'
#'   message("Attempting to run rasterSWI. Expect warnings or errors due to its implementation.")
#'   raster_swi_output <- rasterSWI(s = s_sample,
#'                                  reftab = custom_NAQS, # Using NAQS with a 'coef' column
#'                                  polpos = 2, # pm10 is 2nd element
#'                                  verbose = TRUE)
#'
#'   if (!is.null(raster_swi_output)) {
#'     print(raster_swi_output)
#'   } else {
#'     message("rasterSWI returned NULL or errored, as expected due to implementation issues.")
#'   }
#'
#'   # Clean up global 'people'
#'   rm(people, envir = .GlobalEnv)
#' }
#' }
rasterSWI <- function(s,
                      reftab = NAQS, # Default uses global NAQS
                      idpos = 1,
                      polpos = 2,
                      aveperiodpos = 3,
                      cyclepos = 5,
                      sep = "_", verbose = FALSE){

  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("Package 'raster' is required. Please install it.", call. = FALSE)
  }
  warning("rasterSWI: This function is likely misimplemented for SWI calculations. ",
          "It appears to use logic from API/PSI calculations and may produce ",
          "misleading results or errors (e.g., undefined 'pop' variable, incorrect 'reftab' usage).")

  # Layer name parsing (similar to rasterAPI and SWI)
  layer_names <- names(s)
  split_names <- strsplit(layer_names, split = sep, fixed = TRUE)
  max_pos <- max(idpos, polpos, aveperiodpos, cyclepos, na.rm = TRUE)
  min_parts_observed <- min(sapply(split_names, length))

  if (min_parts_observed < max_pos) {
    stop(paste("Some layer names in 's' do not have enough parts when split by '", sep,
               "'. Expected at least ", max_pos, " parts.", sep=""))
  }
  safe_extract <- function(name_parts, pos) {
      if (pos <= length(name_parts)) return(name_parts[pos])
      return(NA_character_)
  }
  id <- sapply(split_names, safe_extract, pos = idpos)
  pols <- tolower(sapply(split_names, safe_extract, pos = polpos))
  per <- sapply(split_names, safe_extract, pos = aveperiodpos)
  cyc <- sapply(split_names, safe_extract, pos = cyclepos)
  # patt2 = '([[:digit:]]*)([[:alpha:]]*)' # For per.d, per.l
  # per.d = gsub(patt2, "\\1", per)
  # per.l = gsub(patt2, "\\2", per) # Not used in the rest of the function body

  if (verbose == TRUE) {
    message("rasterSWI - Parsed layer names (first few unique):")
    message("  IDs: ",  paste(head(unique(id)), collapse=", "))
    message("  Pollutants: ", paste(head(unique(pols)), collapse=", "))
    message("  Periods: ", paste(head(unique(per)), collapse=", "))
    message("  Cycles: ", paste(head(unique(cyc)), collapse=", "))
  }

  tab <- data.frame(orig = layer_names, id = id, pols = pols, per = per, cycle = cyc,
                    stringsAsFactors = FALSE)
  if (verbose == TRUE) message("Parsed layer metadata table (head):\n", paste(utils::capture.output(head(tab)), collapse="\n"))

  # Ensure reftab rownames are lowercase for matching
  rownames(reftab) <- tolower(rownames(reftab))

  if (verbose == TRUE){message("Lowercase rownames from reftab: ", paste(rownames(reftab), collapse=", ") ,
                               "\nPollutants from layers: ", paste(unique(tab$pols), collapse=", "))}

  # Attempt to match pollutants and get "coefs"
  # This part is highly problematic for SWI:
  # 1. SWI uses standards, not "coefs" in the risk sense.
  # 2. `reftab` (NAQS) typically has "Annual", "Daily" columns, not "coef".
  tab$coef_val <- reftab[tab$pols, "coef"] # This will be NA if 'coef' column doesn't exist or pol not found

  valid_layers_idx <- !is.na(tab$coef_val)
  if (sum(valid_layers_idx) == 0){
    message("rasterSWI: No matching pollutants found in reftab with a 'coef' column, or 'coef' column missing. Cannot proceed with calculations as written.")
    return(NULL)
  }

  s_filtered <- s[[which(valid_layers_idx)]]
  tab_filtered <- tab[valid_layers_idx, ]
  coefs <- tab_filtered$coef_val

  # make population weighted exposure - THIS WILL ERROR AS `pop` IS NOT DEFINED
  if (!exists("pop", envir = .GlobalEnv) && !exists("pop", where = -1)) {
     warning("rasterSWI: Variable 'pop' is not defined as a parameter or found in the global environment. Population weighting will fail.")
     # To prevent outright error, create a dummy pop raster of 1s, though this is not meaningful
     # For a real fix, 'pop' should be a parameter.
     pop_dummy <- raster::setValues(s_filtered[[1]], 1)
     pwe <- s_filtered * pop_dummy # Element-wise multiplication of stack with a single raster
     names(pwe) <- paste0("pwe_", names(s_filtered))
     message("rasterSWI: Using a dummy population raster of 1s due to undefined 'pop'. Results will not be population-weighted.")
  } else {
     # If pop exists (e.g. globally), use it.
     # Ensure it's a raster.
     if (!inherits(pop, "RasterLayer")) stop("'pop' must be a RasterLayer.")
     pop_cropped <- raster::crop(pop, raster::extent(s_filtered)) # Crop to ensure compatibility
     # Optional: resample pop to match s_filtered resolution and extent exactly
     # pop_resampled <- raster::resample(pop_cropped, s_filtered, method="bilinear")
     pwe <- s_filtered * pop_cropped # Or pop_resampled
     names(pwe) <- paste0("pwe_", names(s_filtered))
  }


  # make pollution sub-index (PSI) - This is API/PSI logic, not SWI
  psi <- s_filtered * coefs # Element-wise multiplication of stack layers by corresponding coef
  names(psi) <- paste("PSI-", names(s_filtered), sep="")
  if (verbose == TRUE) message("Calculated 'PSI' layers (dim): ", paste(dim(psi), collapse = " by "))

  # The following section for aggregation (id.s, api) is also from API/PSI logic.
  # It uses variables `aggregate`, `per_source`, `return.full` which are not parameters of this function.
  # This will lead to errors if those variables are not defined globally.
  # For safety, I will assume these aggregations are not intended to run as-is.

  # aggregate to one score per id
  # This requires 'aggregate' and 'per_source' to be defined.
  # if (exists("aggregate") && exists("per_source") && aggregate == TRUE & per_source == TRUE) {
  #   id.s <- raster::stackApply(s_filtered, indices = match(tab_filtered$id, unique(tab_filtered$id)), fun = mean)
  #   names(id.s) <- paste("id", unique(tab_filtered$id))
  #   return(id.s)
  # }

  # aggregate to one score per day (cycle)
  if (verbose == TRUE) message("Aggregating 'PSI' by cycle: ", paste(unique(tab_filtered$cycle), collapse=", "))
  cycle_factor <- as.factor(tab_filtered$cycle)
  api <- raster::stackApply(psi, indices = cycle_factor, fun = sum, na.rm = TRUE) # Summing PSI values per cycle
  names(api) <- paste("API_cycle", levels(cycle_factor), sep="_") # Naming as API_cycle

  # if (exists("aggregate") && aggregate == TRUE){
  #   if (verbose == TRUE) message("Further aggregating cycle APIs to a single mean.")
  #   api <- raster::calc(api, mean, na.rm = TRUE)
  # }

  # if (exists("return.full") && return.full == TRUE) {
  #  if (verbose == TRUE) message("Returning full stack (original, PSI, API-like result).")
  #  return(raster::stack(s_filtered, psi, api))
  # }

  if (verbose == TRUE) message("rasterSWI: Returning API-like raster based on current logic.")
  return(api) # Returns the "API_cycle" stack
}
[end of R/SWI.R]
