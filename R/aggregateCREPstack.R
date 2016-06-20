#' aggregateCREPstack
#' @param r Raster stack from output from rasterCREP 
#' @param fun function Default to sum

aggregateCREPstack <- function(r){
  # summarise the number of cases across all days
  r <- lapply(X = r, FUN = function(bOutcome) {
    return(calc(x = bOutcome, fun = sum))
  })
  r <- stack(r)
  
  names(r) <- gsub("among.", "", names(r))
  names(r) <- gsub("espiratory", "esp.", names(r))
  names(r) <- gsub("hospital.admissions", "hosp.admin", names(r))
  names(r) <- gsub("Cases_", "", names(r))
  names(r) <- gsub("due.to.", "", names(r))
  names(r) <- gsub("Cardiovascular|Cardiovascular", "CV", names(r))
  names(r) <- gsub("all.ages", "all", names(r))
  r
}

