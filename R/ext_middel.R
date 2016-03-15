# maak 'n 4km raster van 'n middelpunt van 'n extent objek
#' Extent Middel
#' 
#' Makes a 4km raster of a midpoint of an extent object
#' 
#' @param r Raster or extent objectt
#' @param grdsize Numeric. The grid size.
#' @param diam Numeric. The diameter.
#' @param crs Character or object of class CRS.PROJ.4 type description of a Coordinate 
#' Reference System (map projection).
#' @export

ext_middel <- function(r, grdsize = 4, diam = 12, crs = "+proj=utm + south + zone=35 ellps=WGS84"){
  xmid <- mean(extent(r)[1:2])
  ymid <- mean(extent(r)[3:4])
  xmin <- xmid - diam/2
  xmax <- xmid + diam/2
  ymin <- ymid - diam/2
  ymax <- ymid + diam/2
  out <- extent(c(xmin, xmax, ymin, ymax))
  r <- raster(out, nrows = diam %/% grdsize, ncols = diam %/% grdsize, crs = crs)
  r
}