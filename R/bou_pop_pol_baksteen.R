# Bou baksteen

# voorbeeld vanaf http://www.maths.lancs.ac.uk/~rowlings/Teaching/UseR2012/plume.html
# Barry Rowlingson 2012
# Lancaster University

source('~/Documents/Rpakette/AQoffset/R/plume_funs.R')

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

load("/Users/christiaanpauw/Dropbox/Verifikasie2013/HAQ-EMMNorth/GIS/daveytonSP.Rda")
EMM <- daveytonSP
new.proj <- "+proj=utm + south + zone=35 ellps=WGS84" 
require(rgdal)
EMM <- spTransform(EMM, CRS(new.proj))
ext = extent(EMM)

middelpunt <- SpatialPointsDataFrame(coords = matrix(apply(EMM@bbox, 1, mean),nrow = 1),
                                     data = data.frame(id=1, name="a"),
                                     proj4string = CRS(proj4string(EMM)),
                                     bbox = NULL)

minpunt <- SpatialPointsDataFrame(coords = matrix(apply(EMM@bbox, 1, min),nrow = 1),
                                  data = data.frame(id=1, name="a"),
                                  proj4string = CRS(proj4string(EMM)),
                                  bbox = NULL)

buitepunt <- SpatialPointsDataFrame(coords = matrix(apply(EMM@bbox, 1, min)-c(2200,2200),nrow = 1),
                                    data = data.frame(id=1, name="a"),
                                    proj4string = CRS(proj4string(EMM)),
                                    bbox = NULL)

ssize = 100
households <- SpatialPointsDataFrame(coords = spsample(x = EMM, n = ssize, type = "random"),
                                     data = data.frame(id = 1:ssize, name = paste("sample point",1:ssize)),
                                     proj4string = CRS(proj4string(EMM)),
                                     bbox = NULL
)

# verwysings raster
rye <- kolomme <- 100
ref = raster(ext = extent(EMM)*3, nrows = rye, ncols = kolomme, )
projection(ref) = proj4string(EMM)
writeRaster(ref, filename = "base.nc", overwrite=TRUE)
ref[] <- 0

# raster die poligoon
require(raster)
r <- raster(nrow = rye, ncol=kolomme)
extent(r) <- extent(ref)
EMMr <- rasterize(EMM, ref, field = "Electricity", fun=function(x, ...) sum(x, na.rm = TRUE)/(rye*kolomme))

# of maak 'n raster stapel met al die inligting
rl <- lapply(17:25, function(i) rasterize(EMM, ref, field = names(EMM@data[i]), fun=function(x, ...) sum(x, na.rm = TRUE)/(rye*kolomme)))
s <- do.call("stack", rl)
names(s@layers) <- names(EMM@data[17:25])
writeRaster(s, filename = "Subplace2.nc")

# sommasie raster 
totaal.hh <- sum(s, na.rm = TRUE)

# !!! jy kan die hele stack of brick as 'n enkele inset in in formule gebruik bv. sum(s), s + x , s*x
# met ander woorde jy moet die exp funksie en die impak funskie definisser om te werk met stacks
# die logika wat op die vlak van een sel werk behoort (as jy die reg vektoriseer) op die vlak van die hele brick te werk
# Dit gaan monte-carlo ook soveel makliker maak want jy vaireer die hele layer of  die hele stack met iets soos s2 = s*rnorm(n = ncell(s))

# vir berekeinge op elke sel gerbuik calc(r, fun = sum)
# vir opsomming gebruik cellStats
# vir berekening oor lae gebruik overlay(): r.mean <- overlay(r, r2, fun=mean)
# fokus met focal()
# stackApply computes summary type layers for subsets of a RasterStack or RasterBrick.


# maak 'n stack elk vir populasie, energie, PM, SO2 ens
# PM en SO2 is self elkeen 'n stack met daaglikse data
# b <- brick(s1, s2, s3)

########################## 1 jaar #######################

# maak en skryf 'n leë steen met 365 dae
source1.b <- brick(extent(EMM)*3, nl=365, nrows = rye, ncols = kolomme)
source2.b <- source1.b
source3.b <- source2.b

writeRaster(source1.b, filename = "PointSource365.nc", overwrite=TRUE)
writeRaster(source2.b, filename = "SmallPointSource365.nc", overwrite=TRUE)
writeRaster(source3.b, filename = "HouseholdSource365.nc", overwrite=TRUE)
writeRaster(source3.b, filename = "AllSources365.nc", overwrite=TRUE)

source1.b <- brick("PointSource365.nc")
source2.b <- brick("SmallPointSource365.nc")
source3.b <- brick("HouseholdSource365.nc")
source.all.b <- brick("AllSources365.nc")

# toets
sapply(ls(pattern = "source[[:digit:]]+\\.b"), function(x) do.call("inMemory", list(get(x))))

# simuleer data vir 365 dae op die verkeerde manier (in die geheue)
# daaglikse windrigting en sterkte
kk = rnorm(365, 5)
pp = pi/rnorm(365, mean = 4)
# simuleer pluime
x <- sapply(1:365, function(x) plume(src = minpunt, dst = SpatialPoints(ref),  a = 50, b = 15, k = get("kk")[x], phi = get("pp")[x]))
y <- sapply(1:365, function(x) plume(src = buitepunt, dst = SpatialPoints(ref), a = 40, b = 50, k = get("kk")[x], phi = get("pp")[x]))
z <- sapply(1:365, function(x){
  res <- colSums(do.call("rbind",lapply(1:nrow(households),
                                        function(j){
                                          #message(get("kk"))
                                          #message(get("pp"))
                                          plume(src = households[j,], dst = SpatialPoints(ref), a = 10, b = 15, k = get("kk")[x], phi = get("pp")[x])  
                                        })))
  message(x)
  return(res)
})

# Skryf die waardes in
source1.b <- setValues(x = source1.b, x) # hierdie moet met 'n meer komplekse (block) funksie gedoen word vir groter rasters
source2.b <- setValues(x = source2.b, y)
source3.b <- setValues(x = source3.b, z)
source.all.b <- setValues(x = source.all.b, getValues(source1.b) + getValues(source2.b) + getValues(source3.b))

# inspekteer
animate(source1.b, n=1, pause = 0.1)
animate(source2.b, n=1, pause = 0.1)
animate(source3.b, n=1, pause = 0.1)
animate(source.all.b, n=1, pause = 0.1)

# die regte manier is iets soos hierdie: 
# source1.b <- writeStart(source1.b, filename = "PointSource365.nc", overwrite=TRUE)
# ##tr <- blockSize(source1.b)
# for (i in 1:365) {
#   v <- matrix(plume(src = minpunt, dst = SpatialPoints(ref),  
#                             a = 10, b = 15, k = rnorm(1,5), 
#                             phi = pi/rnorm(1, mean = 4)), ncol=kolomme)
#   source1.b <- writeValues(source1.b, x, start = ((i-1)*rye*kolomme)+1)
# }
# source1.b <- writeStop(source1.b)

################### aggregasie ###################
source1.year <- stackApply(source1.b, indices=rep(1, nlayers(source.all.b)), mean, na.rm = TRUE ) 
source2.year <- stackApply(source2.b, indices=rep(1, nlayers(source.all.b)), mean, na.rm = TRUE ) 
source3.year <- stackApply(source3.b, indices=rep(1, nlayers(source.all.b)), mean, na.rm = TRUE ) 
all.year <- stackApply(source.all.b, indices=rep(1, nlayers(source.all.b)), mean, na.rm = TRUE ) 
year.brick <- brick(source1.year, source2.year, source3.year, all.year)
plot(year.brick)
writeRaster(year.brick, filename = "yearAll.nc")

################### blootgestelde populasie ###################
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

exposed.year <- brick(ext, nl=365, nrows = rye, ncols = kolomme)
writeRaster(exposed.year, filename = "ExposureSource1.nc")
exposed.year <- brick("ExposureSource1.nc")
exposed.year <- setValues(exposed.soure1, people * year.brick, na.rm=TRUE)


