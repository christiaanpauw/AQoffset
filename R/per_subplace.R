#' per_subplace Get from a raster mean per polygon
#' @param r Raster
#' @param spdf SPDF
#' @param nm Character Column name in sdpf with name
#' @export
#' 

per_subplace <- function(r, spdf, nm = "SP_NAME"){
  res <- sapply(1:length(spdf), function(i) {
    r2 <- mask(PM_impact, spdf[i, ])
    r2 <- calc(r2, mean)
    cellStats(r2, mean)
  })
  names(res) <- as.character(spdf@data[,nm])
  res
}