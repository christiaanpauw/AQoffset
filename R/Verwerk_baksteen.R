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
bf$ex <- people * (bf$small.point + bf$large.point + bf$hh)
plot(bf)
# maak besoedelings totale

# blootstrellings funksie totaal * konsentrasie


