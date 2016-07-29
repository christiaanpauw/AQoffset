#' Raster Distribution plot
#' 
#' Convenience function to summarise a raster stack and plot the result as a 
#' trellis plot
#' 
#' @param ss A raster stack
#' @param multi A list containing a character vector of layer name components or NULL
#' @param mn The main title of the resulting plot as a character string. Defaults to empty string
#' @param sb The plot subtitle as a character string
#' @param th The theme to be applied temporarily to the plotting procedure
#' @param scales A list with components x (columns) and     y (rows). Each of 
#' these components must be a numeric vector of length 2 defining the range for each marginal plot.
#' Default contains a logical that prevents drawing marginal graphics
#' @param ... Arguments to be passed to raster_dist_sum 
#' @param meanonly Logical Return only the mean
#' @param tab_out Logical Write out a table
#' @export

raster_dist_plot <- function(ss, 
                             multi = list(NULL, 
                                          c("IndustryY_pm10", "IndustryX_so2"))[[1]], 
                             mn = "", 
                             sb ="",
                             th = BuRdTheme,
                             scales=list(draw=FALSE), 
                             meanonly = FALSE, 
                             tab_out = FALSE,
                             debug = FALSE, beginnul = FALSE,
                             ...){
  if (!require(rasterVis)){
    message("Ek probeer raserVis installeer")
    install.packages("rasterVis")
    require(rasterVis)
  }
  if (is.null(multi)){
    x <- raster_dist_sum(s = ss, ... )
    if (tab_out){
      d <- data.frame(matrix(cellStats(x, mean), ncol = 1))
      rownames(d) <- names(x)
      colnames(d) <- "mean"
      return(d)
    }
    if (meanonly){
      if (length(grep("mean", names(x))) > 0){
        levelplot(x[[grep("mean", names(x))]], par.settings = BuRdTheme, main = mn, sub =sb, scales = scales, margin = list(draw = FALSE), ...)
      } else {
        levelplot(x, par.settings = BuRdTheme, main = mn, sub =sb, scales = scales, ...)
      }
    } else {
      levelplot(x, par.settings = BuRdTheme, main = mn, sub =sb, scales = scales, ...)
    }
  } else {
    if (!is.character(multi)) stop("multi must be NULL or a character vector")
    ev <- new.env()
    for (i in 1:length(multi)){
      x <- ss[[grep(multi[i], names(ss))]]
      assign(multi[i], x, envir = ev)
    }
    rm(ss)
    l <- do.call(function(x) mget(x, envir = ev), list(ls(ev)))
    res2 <- lapply(l, raster_dist_sum)
    res2 <- lapply(res2, function(x) x[[2:7]])
    for (i in 1:length(multi)){
      names(res2[[i]]) <- paste(names(res2[i]), c("25th",  "median", "mean", "75th%", "99th", "std.dev"))
    }
    res = stack(res2)
    lo = c(6,length(multi))
    mmv <- min(minValue(res))
    if (beginnul) mmv <- 0
    message(mmv)
    if (mmv > 0){
      att <- seq(0, max(maxValue(res)), length = 20)
    } else {
      att <- seq(mmv, max(maxValue(res)), length = 20)
    }
    
    if (tab_out){
      rn <- unique(gsub("([[:alnum:]]+)\\.([[:print:]]+)", "\\2", names(res)))
      cln <- unique(gsub("([[:alnum:]]+)\\.([[:print:]]+)", "\\1", names(res)))
      m <- matrix(cellStats(res, mean), ncol = 2, dimnames = list(rn, cln))
      d <- data.frame(m)
      rownames(d) <- rn
      colnames(d) <- cln
      return(d)
    }
    
    if (meanonly){
      res <- res[[grep("mean", names(res))]]
      levelplot(res, par.settings = th, main = mn, sub =sb, scales = scales)
    } else {
      levelplot(res, par.settings = th, layout = lo, main = mn, sub =sb, scales =scales, at = att)
    }
    }
}
