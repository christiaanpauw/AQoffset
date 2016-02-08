library(rasterVis)

setwd("~/Documents/Rpakette/AQoffset/")
source('/Users/christiaanpauw/Dropbox/NovaSuid/EHEO BRONNE/QOL and HEALTH/BOD_CALC/R-scripts/End.Point.Info.European.R')
source("~/Dropbox/Rfunctions/summarise.sicklist.R")
source('~/Documents/Rpakette/AQoffset/R/rasterConcentrationResponse.R')
source('~/Documents/Rpakette/AQoffset/R/knipNA.R')
source('~/Documents/Rpakette/AQoffset/R/API.R')

THRESHOLD <- 0

people <- raster("MDC_People.nc")
exposed.year <- raster("ExposureSource1.nc")
point <- brick("PointSource365.nc")

# BASELINE SCENARIO
baselineS <- brick("baseline365.nc")
yearBase <- brick("yearBaselineAll.nc")
baselineAPI = rasterAPI(s = baseline365)

# PROJECT SCENARIO
projectS <- brick("project365.nc")
projectAPI = rasterAPI(s = project365)
# Implementeer drempelwaarde
projectAPI[projectAPI < THRESHOLD] <- NA

# IMPACT
impactAPI <- baselineAPI - projectAPI

year.brick <- raster("yearAll.nc")
year.brick[year.brick==0] <- NA
year.brick <- knipNA(r = year.brick, out = "raster")

load("/Users/christiaanpauw/Documents/Rpakette/AQoffset/MDB.Rda")




# skep gewigte en ouderdomme in endlist
weights <- c(1, 0.01, 0.001, 0.005, 1, 0.1, 0.1, 0.1, 0.02, 0.01, 0.02, 0.01)
minage <- c( 30, 15, 16, 15, 0,  18,   0,   0,  5,  20,  5, 15)
maxage <- c(120, 65, 65, 65, 5, 120, 120, 120, 15, 120, 15, 120)
#names(weights) <- names(endlist)
for (i in 1:length(endlist)){
  endlist[[i]]$weight <- weights[i]
  endlist[[i]]$min_age <- minage[i]
  endlist[[i]]$max_age <- maxage[i]
}

x <- rasterConcentrationResponse(conc = year.brick, pollutantname = "PM10",
                               popr = people, incidence.rate = 0.01, 
                               verbose = TRUE, base.conc=0, 
                               RR = c(point = 1.1, upper=1.12, lower = 1.05), 
                               fun.form = "linear")

x.crep <- rasterCREP(sl = endlist, pollutant = "pm10", cconc = year.brick, ppopr = people, bbase.conc = 0)

# maak 'n dataframe met die verskillede elemente van die rastersteen
cc <- data.frame(do.call("rbind", strsplit(names(x.crep), split = "_")))
names(cc) <- c("Pollutant", "Parameter_Type", "Parameter_level", "Outcome")
cc$orig <- names(x.crep)

# Kies vir nou eers die lae risiko
aidx <- which(cc$Parameter_level=="a")
x.crep.point <- subset(x = x.crep, subset = aidx)
xt <- knipNA(x.crep.point[[1]], out = "extent")
x.crep.point <- crop(x.crep.point, xt)
plot(x.crep.point)

# gee elkeen 'n gewig 
gewigte <- rnorm(n = nlayers(x.crep.point))
maal.w <- function(x){x * gewigte}
s <- calc(calc(x = x.crep.point, fun = maal.w), sum)
plot(s)

# hoe maak mens 'n geweegde RR vir DALY?
# risiko = uitkoms.gewig * waarskynlikhied
# waarskynlikhied is die waarskynlikhied van die uitkoms teen daardie konsentrasie

nies = data.frame(gewig = 0.05, RR = 1.5)
hoes = data.frame(gewig = 0.10, RR = 1.15)
dooi = data.frame(gewig = 1, RR = 1.0005)

df = rbind(nies, hoes, dooi)
rownames(df) <- c("nies", "hoes", "dooi")

conc = 100
base = 50
delta = conc-base
beta = log(df$RR)/delta
beta * df$gewig # geweegde risiko 
sum(beta * df$gewig) # total inkrementele risiko vir die verskil


### RR benadering API twee scenarios en 'n risk diff scenario


### Akute situasie The annual standard is aimed at addressing long term exposure
### but acute exposure is also an issue. I think we can address this by counting
### excedence events per year
