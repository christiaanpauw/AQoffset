#' SWI Calculate Standards weighted intake of pollutant
#' @param conc raster The concentration of every pollutant
#' @param pop raster The # of people in each grid cell
#' @param ap Character "Annual" of "Daily"
#' 

NAQS <- data.frame(
  PM10  = c(40, 80),
  PM2.5 = c(25, 75),
  SO2   = c(50, 100),
  O3 =    c(40, 80),
  O3 =    c(50, 70),
  NO2 =   c(60, 100), 
  row.names = c("Annual", "Daily")
  )

x = data.frame(PM10 = 100, SO2 = 200)
ap = "Annual"
NAQS.rel = NAQS[ap,names(x)]

standard.relative.conc <- x / NAQS.rel
standard.relative.conc
PM10.eq <- standard.relative.conc * NAQS.rel[,"PM10"] # PM10 moet wees wat dit was
PM10.eq
