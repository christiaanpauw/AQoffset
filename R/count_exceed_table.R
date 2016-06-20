#' Make table of exceedance counts
#' @param x a raster stack resultsing from the count_exceed function
#' @author Christiaan J Pauw
#' @param cp Character The caption

count_exceed_table <- function(x, cp = "", ...){
  #bar_exceed(x, ttl = "Aggregated count of days when SO2 from \nhouseholds exceeded specified concentration")
  x[is.na(x)] <- 0
  concLevels <- names(x)
  df <- data.frame(cellStats(x, sum) / sum(getValues(x)), stringsAsFactors = FALSE) *100
  df$concentration <- gsub("\\.", " ", concLevels)
  df[[3]] <- df[[1]]
  df <- df[,2:3]
  names(df) <- c("Concentration", "% observations in exceedance")
  rownames(df) <- NULL
  kable(df, caption = cp, digits = 2)
}

