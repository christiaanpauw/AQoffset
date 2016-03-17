#' project boundary
#' @description The Air Pollution Impacts Protocol requires that a project bounbary be 
#' defined as that the spatial and temporal extent of the atmospheric states resulting 
#' from an activity above a certain threshold. This function takes a raster of concentrations
#' resulting from an activity and a threshold value and selects the valid project extent
#' @return A single layer raster shruck to the minimum
#' neccesary extent and with NAs in all remaining cells within that 
#' extent that falls outside of the project boundary and 1 in the cells 
#' that fall within. This object is then used as a mask to select the valid 
#' region for further steps in the air pollution impacts or offsets 
#' accouting
#' @param r Raster* object with concentrations resulting from an activity
#' @param period Numeric The averaging period represented by each layer of r expressed as
#' a fraction of 365 days, i.e. 1/365 for daily values, 1/24/365 for hourly values
#' @param target_period Character Apply function for a chronic or an acute scenario or both. 
#' options are "chronic", "acute", "both". Defaults to "both"
#' @param thresh_acute Numeric The threshold for the 24 hour averaging period
#' @param period_acute The period for which the acute threshold is valid. Default 1/365
#' @param thresh_chronic Numeric The threshold for the 1 year averaging period
#' @param period_chrinic The period for which the chronic threshold is valid. Default 365/365
#' @param chronic_cutoff Numeric Proportion data needed to construct a valid chronic estimate. 
#' Default 0.9
#' @param only.mask Logical If TRUE the function only returns a mask and not the raster
#' @param verbose Logical Messages or not

project_boundary <- function(r, 
                             period = 1/365, 
                             target_period = "both",
                             thresh_acute = 19,
                             period_acute = 1/365,
                             thresh_chronic = 2.5,
                             period_chronic = 365/365,
                             chronic_cutoff = 0.9,
                             vebose = FALSE, 
                             return.mask = FALSE, maskname = "masker", only.mask = FALSE){
  if (target_period == "acute" | target_period == "both"){
    # maak seker die vergemiddeldingsperiod is reg
    # jy kan opsom maar nie afsom nie. i.e. jy kan ure dae maak maar nie andersom nie
    
    # toets teen die drempelwaarde en som op tot een laag
    ACC <- r > thresh_acute
    ACC <- calc(ACC, toets_enige)
    #assign("ACC", ACC, envir = .GlobalEnv)
    res = ACC
  }
  
  if (target_period == "chronic" | target_period == "both"){
    # som op tot die regte vlak as dit minder as 'n jaar is moet jy mooi dink. 364 dae is seker OK 
    # maar is 200 dae nog steeds genoeg om 'n jaarlikse gemiddeld te gee. Ek werk hier op 90%
    
    if (chronic_cutoff <  (nlayers(r) * period_acute) / period_chronic){
      CHR = calc(r, mean, na.rm = TRUE)
      CHR = CHR > thresh_chronic
      CHR = calc(CHR, toets_enige)
    } else { 
      stop("There isn't enbough data to a chronic estimate. \nRun agian with target_period = 'acute'")}
    
  }
  
  if (target_period == "both"){
    # sit die twee lae op mekaar
    res = stack(ACC, CHR)
    res = calc(res, toets_enige)
  }
  
  # knip die kante af
  #assign("res", res, envir = .GlobalEnv)
  if (any(getValues(res) == 0)) res[which(getValues(res) == 0)] = NA
  res = knipNA(res, out = "raster")
  
  # oorweeg dit op die bestek as 'n blok op google earth te druk
  
  # gebruik nou res om r te crop en te masker
  r <- crop(r, extent(res))
  r <- mask(r, mask = res)
  if (return.mask) {assign(maskname, res, envir = .GlobalEnv)}
  if (!only.mask) {return(r)} else {res}
}

# hulpfunksie 
toets_enige <- function(x) ifelse(sum(x, na.rm = TRUE) > 0, 1, 0)
