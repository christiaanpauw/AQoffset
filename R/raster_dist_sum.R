#' Raster Distribution Summary
#' 
#' Summarise a many layered raster stack into a 7 point summary 
#' stack. It gives the the followng percentiles: 1%, 25%, median, 75% and 99%
#' and mean, standard deviation, and inter quartile range and arbitrary user specified 
#' range. Optionally provides minimum and maximum
#' 
#' @param s A raster stack
#' @param minmax Logical. Do you want minimum and maximum
#' @param trim Proportion of data excluded from range 
#' @return A raster
#' @export

raster_dist_sum <- function(s, minmax = FALSE, trim = 1/100){
  if (nlayers(s) < 10) stop("Not much use making a 7 point summary with so few layers, is it?\n Yours has ", nlayers(s) )
  if (trim > 1 | trim < 0) stop("trim must be between 0 and 1: default is 1% thus 0.01")
  require(openair)
  sumfun <- function(x){
    prt = stats::quantile(x, probs = c(0.01, 0.25, 0.50, 0.75, 0.99), na.rm = TRUE)
    names(prt) = c("one%", "twentyfive", "median", "seventyfive", "ninetynine%")
    iqr = prt[4] - prt[2]
    pps = c(trim/2, 1-trim/2)
    
    bestek = diff(range(stats::quantile(x, probs = pps, na.rm = TRUE)))
    names(bestek) = paste("range.", 100 * (1-trim), ".perc", sep="")
    names(iqr) = "IQR"
    res = c(min = min(x), mean = mean(x), max = max(x), std.dev = sd(x))
    res2 = c(res, prt, iqr, bestek)[c(1, 5, 6, 7, 2, 8, 9, 3, 4, 10, 11)]
    if (minmax == TRUE){
      return(res2)
    }
    return(res2[-c(1,8)])
  } 
  s2 <- calc(s, sumfun)
  s2[s2==0] <- NA
  s2
}

#' Raster Average Type
#' 
#' Calculates the averages of unique names within a raster stack
#' 
#' @param s A raster stack
#' @param patt Character vector used to isolate unique values within s
#' @param func The function to be applied on subsets of the raster stack
#' @return Raster with function applied to subsets
#' @export

raster_ave_type <- function(s, patt = "_24h\\.[[:digit:]]+", func = "mean"){
  if (class(s) != "RasterStack") stop("s must be of class RasterStack")
  cats <- unique(gsub(patt, "", names(s)))
  idx <- lapply(cats, function(x) grep(x, names(s)))
  idx <- do.call("rbind", lapply(1:length(idx), function(i) data.frame(i = idx[[i]], j = i)))[,2]
  res <- stackApply(s, indices = idx, fun = func)
  names(res) <- cats
  res
}
