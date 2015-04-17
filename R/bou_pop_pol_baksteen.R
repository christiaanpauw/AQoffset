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

ssize = 30
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

# bronne
source1 <- plume(src = minpunt, dst = SpatialPoints(ref), a = 10, b = 15, k = 5, phi = pi/4) 
source3 <- plume(src = buitepunt, dst = SpatialPoints(ref), a = 40, b = 50, k = 5, phi = pi/4) 
# hierdie is nogal stadig want dit rbind: werk aan iets vinniger
# source2 <- source3/4
source2 <- colSums(do.call("rbind", lapply(1:nrow(households),function(x){
  plume(src = households[x,], dst = SpatialPoints(ref), a = 1, b = 15, k = 5, phi = pi/4)
})))

source1.r <- raster(extent(EMM)*3, nrows = rye, ncols = kolomme)
source1.r[] <- source1
source2.r <- raster(extent(EMM)*3, nrows = rye, ncols = kolomme)
source2.r[] <- source2
source3.r <- raster(extent(EMM)*3, nrows = rye, ncols = kolomme)
source3.r[] <- source3

# stack pollution
pop.stack <- stack(source1.r, source2.r, source3.r)
names(pop.stack@layers) <- c("small point", "hh", "large point")

# maak nog 'n klomp stacks en sit hulle in in Brick en skryf hulle na CDF op skyf
br <- crop(brick(x = list(s, totaal.hh, pop.stack)), ext)
bfn <- c(names(s@layers), "Totaal.hh", names(pop.stack@layers))
writeRaster(x = br,  filename='popbrick.nc', overwrite=TRUE)
save(bfn, ext, file ="bfnames.Rda")

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
x <- sapply(1:365, function(x) plume(src = minpunt, dst = SpatialPoints(ref),  a = 10, b = 15, k = rnorm(1,5), phi = pi/rnorm(1, mean = 4)))
y <- sapply(1:365, function(x) plume(src = buitepunt, dst = SpatialPoints(ref), a = 40, b = 50, k = rnorm(1,5), phi = pi/rnorm(1, mean = 4)))
z <- sapply(1:365, function(x){
  kk = rnorm(1, 5)
  pp = pi/rnorm(1, mean = 4)
  res <- colSums(do.call("rbind",lapply(1:nrow(households),
                           function(x){
                             #message(get("kk"))
                             #message(get("pp"))
                             plume(src = households[x,], dst = SpatialPoints(ref), a = 1, b = 15, k = get("kk"), phi = get("pp"))  
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

