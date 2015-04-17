# voorbeeld vanaf http://www.maths.lancs.ac.uk/~rowlings/Teaching/UseR2012/plume.html
# Barry Rowlingson 2012
# Lancaster University

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
# plume function
# f=α exp(−(d/β)^2 e^−κcos(θ−ϕ)2)
# Where α is the pollution level at distance zero, 
# β is a distance scale factor, 
# ϕ is the major angle of the pollution plume, and 
# κ is the eccentricity. If κ is 0 then the plume becomes a circularly symmetric exponential decay, and as κ increases the plume becomes more stretched in the direction of the ϕ parameter. 
# You can consider κ and ϕ as the wind strength and direction respectively.

pfunc = function(d2, ang, a, b, k, phi) {
  ## plume value for distance-squared d2
  a * exp(-(d2 * exp(-k * cos(ang - phi))^2/b^2))
}

plume <- function(src, dst, a, b, k, phi) {
  ## plume value for src at dst
  src = coordinates(src)
  dst = coordinates(dst)
  d2 = (dst[, 1] - src[, 1])^2 + (dst[, 2] - src[, 2])^2
  ang = atan2(dst[, 2] - src[, 2], dst[, 1] - src[, 1])
  pfunc(d2, ang, a, b, k, phi)
}

toOSGB = function(s) {
  spTransform(s, CRS("+init=epsg:27700"))
}
cumbria = toOSGB(readOGR(Datadir, "cumbria", verbose = FALSE))
nuclear = toOSGB(readOGR(Datadir, "nuclear", verbose = FALSE))
ukgaz = readOGR(Datadir, "ukgaz", verbose = FALSE)
plot(cumbria)
plot(nuclear, add = TRUE, pch = 19, col = "red")
plot(ukgaz, add = TRUE)
title("Base map")

plevel = raster(extent(cumbria), nrows = 500, ncols = 500)
plevel[] = plume(nuclear, SpatialPoints(plevel), 12000, 400, 5, pi/4)
projection(plevel) = proj4string(cumbria)
plot(plevel, col = colours)
plot(cumbria, add = TRUE)
plot(nuclear, add = TRUE, pch = 19,  col = "darkgreen")
plot(ukgaz, pch =19, col ="blue", cex = 0.1, add = TRUE)
title("Plume Spread")

ukgaz$exposure = extract(plevel, ukgaz)

invpfunc <- function(ang, a, b, k, phi, f) {
  ### inverse plume function - compute distance given f
  d2 = -log(f/a) * b^2/exp(-k * cos(ang - phi))^2
  sqrt(d2)
}

contourPlume <- function(src, a, b, k, phi, f) {
  ### return a SpatialPolygon of a plume function at value f
  ### by sweeping round 360 degrees and computing the distance
  ang = seq(0, 2 * pi, len = 360)
  d = invpfunc(ang, a, b, k, phi, f = f)
  xy = coordinates(src)
  polys = SpatialPolygons(list(Polygons(list(Polygon(cbind(xy[, 1] + d * sin(ang), xy[, 2] + d * cos(ang)))),
                                        ID = 1)))
  proj4string(polys) = proj4string(src)
  polys
}

rivers = toOSGB(readOGR(Datadir, "rivers", verbose = FALSE))
lakes = toOSGB(readOGR(Datadir, "naturalwater", verbose = FALSE))
nineK = contourPlume(nuclear, 12000, 400, 5, pi/4, 9000)
rivers9k = gIntersection(nineK, rivers, byid = TRUE)

plot(rivers9k, col = "blue", lwd = 3)
plot(rivers, col = "blue", lty = 2, add = TRUE)
plot(nineK, add = TRUE)
plot(lakes, add = TRUE, col = "blue")
title("River sections with >9000 exposure")


lakes = toOSGB(readOGR(Datadir, "naturalwater", verbose = FALSE))
plot(plevel, col = colours)
plot(cumbria, add = TRUE)
plot(lakes, add = TRUE, col = "#0000FF")
title("Lakes")

# The extract function can now be used to sum the plume within polygons. 
# We multiply by the grid cell area in metres and divide by 109 to get the total pollution in grams 
# that has gone into the lake.

lakes$pollution = extract(plevel, lakes, sum, small = TRUE) * prod(res(plevel))/1e+09

# We can then plot the ten most polluted lakes by total pollution amount. 
# Notice the use of the RColorBrewer and lattice packages to modify the default behaviour 
# of the spplot function. We add sp.text to the panel function to label the 
# lakes - the default label position sits right over the feature so we adjust its position slightly.

library("lattice")
library("RColorBrewer")
top10 = order(lakes$pollution, decreasing = TRUE)[1:10]
spplot(lakes[top10, ], "pollution", panel = function(...) {
  panel.polygonsplot(...)
  sp.text(coordinates(lakes[top10, ]), lakes$name[top10], adj = 0)
}, main = "Lake exposure")

################################################################################
# My poging
################################################################################


load("/Users/christiaanpauw/Dropbox/Verifikasie2013/HAQ-EMMNorth/GIS/qalabotjaSP.Rda")
EMM <- qalabotjaSP
new.proj <- "+proj=utm + south + zone=35 ellps=WGS84" 
require(rgdal)
EMM <- spTransform(EMM, CRS(new.proj))

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
pol.level = raster(extent(EMM)*3, nrows = rye, ncols = kolomme)
projection(pol.level) = proj4string(EMM)
pol.level[] <- 0

# raster die poligoon
require(raster)
r <- raster(nrow = rye, ncol=kolomme)
extent(r) <- extent(pol.level)
EMMr <- rasterize(EMM, r, field = "Electricity", fun=function(x, ...) sum(x)/(rye*kolomme))

# of maak 'n raster stapel met al die inligting
rl <- lapply(17:25, function(i) rasterize(EMM, r, field = names(EMM@data[i]), fun=function(x, ...) sum(x)/(rye*kolomme)))
s <- do.call("stack", rl)
names(s@layers) <- names(EMM@data[17:25])

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
source1 <- plume(src = minpunt, dst = SpatialPoints(pol.level), a = 10, b = 15, k = 5, phi = pi/4) 
source3 <- plume(src = buitepunt, dst = SpatialPoints(pol.level), a = 40, b = 50, k = 5, phi = pi/4) 
# hierdie is nogal stadig want dit rbind: werk aan iets vinniger
source2 <- colSums(do.call("rbind", lapply(1:nrow(households),function(x){
  plume(src = households[x,], dst = SpatialPoints(pol.level), a = 1, b = 15, k = 5, phi = pi/4)
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
br <- brick(x = list(s, totaal.hh, pop.stack))
br@data@names <- c(names(s@layers), "Totaal.hh", names(pop.stack@layers))
bf <- (x = br, filename='popbrick.nc', format="CDF", overwrite=TRUE)


##################### Spatial Pixel benadering ###################
# voordeel is jy kan meer data aan een punt koppel maar miskien kan jy dit met bands ook doen

# maak SpatialPixelsDataFrame
pix <- SpatialPixelsDataFrame(EMMr, data = data.frame(hh=EMMr@data@values))

length(source2)
# sommeer die lae vir totale blootstelling en sit dit by die raster
tot <- source1+source2+source3
pol.level[] = tot

# Kry die blootstelling by 'n spefifieke punt
steekproefpunte <- spsample(x = EMM, n = 10, type = "regular") 
steekproefblootstelling <- extract(pol.level, steekproefpunte)
steekproefSPDF <- SpatialPointsDataFrame(coords = coordinates(steekproefpunte), 
                                         data = data.frame(steekproefblootstelling), 
                                         proj4string = CRS(proj4string(EMM)), bbox = NULL)
plot(steekproefpunte, pch = 19, add=TRUE)

# knip hom kleiner na jy geplot het, inspekteer extent(pol.level)
my_ext <- extent(3727193, 3734767,-3428000,3417000) #(xmin, xmax, ymin, ymax)
pol.level <- crop(pol.level, my_ext) #

# plot
plot(pol.level, col = colours)
plot(EMMr, col = groene[EMMr@data$value], add= TRUE, legend = FALSE)
# plot(raster(matrix(source1, nrow=rye, byrow=TRUE)), col = colours)
# plot(raster(matrix(source2, nrow=rye, byrow = TRUE)), col = colours)
# plot(raster(matrix(source3, nrow=rye, byrow = TRUE)), col = colours)
plot(minpunt, col = "darkblue", pch = 10, cex = 1, add=TRUE)
plot(buitepunt, col = "darkblue", pch = 10, cex = 1, add=TRUE)
plot(households, cex = 0.3, col = "darkblue", add=TRUE)
plot(EMM, add = TRUE)

# oorweeg kontoere vir die verskillende bronne en die totaal

# Eintlik moet jy hierdie as 'n funksie met 'n brick doen
# Blootstellingsfunksie: Iets soos: populasie * konsentrasie (doen binne in in SPixelsDF)
# blootstelling aan bron 1: 
pix@data[is.na(pix@data)] <- 0
pix@data$exp1.pm <- source1 * pix@data$hh
pix@data$exp2.pm <- source2*pix@data$hh
pix@data$exp3.pm <- source3*pix@data$hh
pix@data$exp.tot.pm <- pix@data$exp1 + pix@data$exp2 + pix@data$exp3

# som blootstelling op 
sum.exp.pm <- data.frame(sum.exp1.pm = sum(pix@data$exp1.pm), sum.exp2.pm =sum(pix@data$exp2.pm),
           sum.exp3.pm =sum(pix@data$exp3.pm), sum.exp.tot.pm =sum(pix@data$exp.tot.pm))

# Impak funksie: blootstelling op konsentrasie  per populasie eienskappe (ouderdom, geslag, voorkoms) en CRF


