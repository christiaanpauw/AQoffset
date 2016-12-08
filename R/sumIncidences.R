#' sumIncidences
#' @description Sommeer die uitkoms van aggregateCREPstack
#' @param outcomesStack Raster stack. assumes that outcomesStack is a raster stack with 1 layer per outcome


sumIncidences <- function(outcomesStack) {
  dfIncidences <- data.frame(matrix(ncol = 2), 
                             stringsAsFactors = FALSE)
  names(dfIncidences) <- c("outcome", "total_cases")
  dfIncidences <- dfIncidences[-1,]
  
  for (i in 1:nlayers(outcomesStack)) {
    outcome <- tolower(names(outcomesStack)[i])
    totalIncidences <- sum(getValues(outcomesStack[[i]]), na.rm = TRUE)
    dfIncidences[nrow(dfIncidences) +1,] <- c(outcome, totalIncidences)
  }
  dfIncidences$outcome <- gsub(pattern = "cases_", 
                               replacement = "", 
                               x = dfIncidences$outcome, 
                               fixed = TRUE)
  dfIncidences$outcome <- gsub(pattern = "..", 
                               replacement = ".", 
                               x = dfIncidences$outcome, 
                               fixed = TRUE)
  dfIncidences$outcome <- gsub(pattern = ".", 
                               replacement = " ", 
                               x = dfIncidences$outcome, 
                               fixed = TRUE)
  
  dfIncidences$total_cases <- as.numeric(dfIncidences$total_cases)
  return(dfIncidences)
}