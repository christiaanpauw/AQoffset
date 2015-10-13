# lees en proseseer kwaZa calpuff data
#rm(list=ls())


# Biblioteke en bronne ----------------------------------------------------

library(reshape2)
library(dplyr)
library(maptools)
library(rgdal)
library(raster)
library(plyr)
library(gstat)
library(fields)

source('~/Documents/Rpakette/AQoffset/R/pointifyCensus.R')

# Stel gidse en name ------------------------------------------------------
datadir <- "/Users/christiaanpauw/Documents/EHOP_calpuff_hh_tseries_jan_jun_2015/"
fn <- list.files(datadir, pattern = ".txt")
kmfn <- paste(datadir, "EHOP_calpuff_points.kml", sep="")
SPfn <- "kwazaSP2011heat.Rda"
erwefn <- "kwazamokuhle.erwe.Rda"

# Calpuff data ------------------------------------------------------------

lst <- lapply(fn, function(x){
  res = read.table(paste(datadir, x , sep=""), sep = ",", header = TRUE, row.names = NULL, stringsAsFactors = FALSE)  
  res$date = as.POSIXct(res$date, tz = "Africa/Johannesburg")
  res$place = gsub("TSERIES_so2_1hr_conc_kwaz_hh_2015_jan_jun_|TSERIES_pm10_1hr_conc_kwaz_hh_2015_jan_jun_|_tseries_clean_date.txt", "", x)
  res
  } )
names(lst) <- gsub("TSERIES_so2_1hr_conc_kwaz_hh_2015_jan_jun_|TSERIES_pm10_1hr_conc_kwaz_hh_2015_jan_jun_|_tseries_clean_date.txt", "", fn)

dta <- rbind.fill(lst)
m <-  melt(dta, id.vars = c("date", "place"))
m$place <- tolower(substr(m$place, 1, 6))
m$day <- as.Date(m$date)
dt <- dcast(m, formula = day + place ~ variable, fun.aggregate = mean, na.rm = TRUE)
dt <- dt[dt$place != "kwazam_ambient", ]
dtl <-split(dt, f = dt$place, drop = TRUE)
daylist <- split(dt, f = dt$day, drop = FALSE)

dtII <- dcast(m, formula = date + place ~ variable, fun.aggregate = mean, na.rm = TRUE)
dtlII <-      split(dtII, f = dtII$place, drop = TRUE)
datelist <- split(dtII, f = dtII$date, drop = FALSE)

nll <- max(sapply(lst, nrow))
nd <- max(sapply(dtl, nrow))

# Georafiese verwysingsdata -----------------------------------------------
# lees die poligoon met die buurt grense en die erwe
load(paste(datadir, SPfn, sep="")) # sy naam is kwazaSP2011heat
kwazaSP2011heat$place <- tolower(substr(kwazaSP2011heat$SP_NAME, 1, 6))
load(paste(datadir, erwefn, sep="")) # sy naam is kwaza
b = brick(extent(bbox(kwazaSP2011heat)), ncol = 20, nrow=20, nl = nd)
names(b) <- unique(as.character(dt$day))
r = rasterize(krds_df, y = b) # die punte waar die data
plot(r)
plot(kwazaSP2011heat, add=TRUE)

# lees kml leêr met die punte
krds <- getKMLcoordinates(kmlfile = paste(datadir, "EHOP_calpuff_points.kml", sep=""), ignoreAltitude = TRUE)
krds_df <- do.call("rbind", krds)
krdsSP <- SpatialPointsDataFrame(coords = krds_df,
                                 data = data.frame(LETTERS[1:length(krds)]), 
                                 proj4string = CRS(proj4string(kwazaSP2011heat)))
krdsSP$SP_Name <- over(krdsSP, kwazaSP2011heat)$SP_Name
krdsSP$SP_Name <- as.factor(krdsSP$SP_Name)


# Poligoon benadering: Maak ’n lys van SPDFs met konsentrasie -------------
days <- lapply(1:length(daylist), function(i){
  idx <- na.omit(match(kwazaSP2011heat$place, daylist[[i]]$place))
  res <- kwazaSP2011heat[idx, ]
  res$pm10 <- daylist[[i]]$pm10
  res$so2 <- daylist[[i]]$so2
  res
})

# plot as jy wil
# lapply(1:length(days), function(i) plot(days[[1]], col = heat.colors(100)[days[[i]]$so2]))



# Kriging -----------------------------------------------------------------

# net vir een dag
x = krds_df  #[,c(2,1)]
pm10 = dt[1:nrow(krds_df),"pm10"]
fit <- Krig(x, pm10)
vls <- predictSurface(fit)["z"]
contour(vls$z)

# nou per dag gebasseer op daylist
fitl.pm10 <- lapply(daylist, function(x){
  fit <- Krig(krds_df[,c(2,1)], x$pm10, m = 2)
  predictSurface(fit)[["z"]]
})

# Populasie ---------------------------------------------------------------
pc <- pointifyCensus(kwazaSP2011heat)
