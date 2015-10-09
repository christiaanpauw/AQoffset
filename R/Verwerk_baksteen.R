# Prosesseer GROOT raster brick met hh en besoedeling inligting en loop ERF
rm(list = ls())

library("sp")
library("raster")
library("rgdal")
library("rgeos")
library("RColorBrewer")
colours   <- colorRampPalette(brewer.pal(7, "OrRd")[])(50) # sit -1 in die [] as jy nie die ligste wil hê nie
groene <- colorRampPalette(brewer.pal(7,"Greens")[])(50)
bloue <- colorRampPalette(brewer.pal(7,"Blues")[])(50)
sp.theme(set = TRUE, regions = list(col = colours))

Datadir = "Data"
Datafile = function(f) {
  file.path(Datadir, f)
}

source('~/Documents/Rpakette/AQoffset/R/plume_funs.R')
load("bfnames.Rda")
# hou die brick op skyf
bf <- brick(x = "popbrick.nc")
#bf <- readAll(r)
inMemory(bf)

# dis siek maak ek moes die name appart stoor
bf@data@names <- bfn

# populasie
people <- bf$Totaal.hh * 3.7

# population extraction functions. Select the appropriate number of people to apply ERF to
# you need a propability fnction of age and sex
# e.g. 
sexify <- function(x, prop = 0.5){
  res = x *prop
}
bf$men <- sexify(people) # mens wil eintlik 'n funskie maak wat 'n hele stack maak met die regte geslag en ouderdomsgroepe
bf$women <- people - bf$men

# blootstelling
bf$poltot <- bf$small.point + bf$large.point + bf$hh
bf$prop.large.point <- (bf$large.point / bf$poltot) 
bf$prop.hh <- (bf$hh / bf$poltot) 
bf$ex <- people * bf$poltot

# laat irrelevant uit
bf <- dropLayer(x = bf, i = c(6:9))

plot(bf)

par(mfcol = c(2,1))
col = terrain.colors(n = 1000)
image(bf$prop.large.point, col = col,
      main="Large point source: absolute and relative contribution")
contour(bf$prop.large.point, add=T)
contour(bf$large.point, col="red", lwd=2, vfont = c("sans serif", "bold"), add=T)
image(bf$prop.hh, col = col,useRaster = TRUE,
      main="Household: absolute and relative contribution")
contour(bf$prop.hh, add=T, lwd = 0.7)
contour(bf$hh, col="blue", lwd=1, vfont = c("sans serif", "bold"),add=T)
par(mfcol=c(1,1))

# maak besoedelings totale




# steekpreof van punte waar monitors gaan staan
spoints <- sample(1:ncell(bf), size = 10)
ssp <- raster(extent(bf))
ncol(ssp) <- ncol(bf)
nrow(ssp) <- nrow(bf)
ssp[] <- NA
ssp[spoints] <- (bf$small.point[spoints] + bf$large.point[spoints] + bf$hh[spoints])
plot(ssp)

ssp.spdf <- rasterToPoints(ssp, spatial = TRUE)
ssp.spdf[spoints] <- (bf$small.point[spoints] + bf$large.point[spoints] + bf$hh[spoints])
plot(ssp.spdf)
# concentrations at sample points



