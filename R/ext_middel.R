# maak 'n 4km raster van 'n middelpunt van 'n extent objek

ext_middel <- function(ex, grdsize = 4, diam = 12, crs = "+proj=utm + south + zone=35 ellps=WGS84"){
  xmid <- mean(extent(hh_annual)[1:2])
  ymid <- mean(extent(hh_annual)[3:4])
  xmin <- xmid - diam/2
  xmax <- xmid + diam/2
  ymin <- ymid - diam/2
  ymax <- ymid + diam/2
  out <- extent(c(xmin, xmax, ymin, ymax))
  r <- raster(out, nrows = diam %/% grdsize, ncols = diam %/% grdsize, crs = crs)
  r
}