#' aggregateCREPstack
#' @param r Raster stack from output from rasterCREP 
#' @param fun function Default to sum

aggregateCREPstack <- function(r){
  # summarise the number of cases across all days
  
  som <- function(x) {sum(x, na.rm = TRUE)}

  r <- lapply(X = r, FUN = function(bOutcome) {
    return(calc(x = bOutcome, fun = som))
  })
  r <- stack(r)
  
  names(r) <- gsub("mortality", "MORT", names(r))
  names(r) <- gsub("among.", "", names(r))
  names(r) <- gsub("espiratory", "esp.", names(r))
  names(r) <- gsub("hospital.admissions", "hosp.ad", names(r))
  names(r) <- gsub("Cases_", "", names(r))
  names(r) <- gsub("due.to.", "", names(r))
  names(r) <- gsub("Cardiovascular|cardiovascular", "CV", names(r))
  names(r) <- gsub("all.ages", "all", names(r))
  r[is.na(r)] <- 0.000001
  r
}

