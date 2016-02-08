#' @description readMike Read a text files output from Calpuff consisting of a time 
#' series of points with pollutant concentration and transfrom into a raster 
#' @param f A filename (character)
#' @param pol A pollutant name (character). Default is "pm10".
#' @param src The pollutant source (character). Defauls to "HH". 
#' @param aggr.level The level of aggregation. Choose between "hour", "day" or "year". Defaults to "day"
#' @param datadir The directory where the data is written if write = TRUE
#' @param write Logical. Should the data be writen our as NetCDF file? Defaults to TRUE
#' @param ovw Logical. Do you want to overwrite the NEtCDF file if one by the name on nn already exits
#' @param save Logical. Do you want to save an .Rda file with the resulting raster
#' @param nn Character. The name of the file you want to save. Defaults to "out"
#' @param verbose Logical. Give progress messafes or not?


readMike <- function(f = fn[1], 
                     pol = "pm10", 
                     src = "HH", 
                     aggr.level = c("hour", "day", "year")[2], 
                     datadir = "~/EOP_Data/", 
                     write = TRUE, 
                     ovw = TRUE,
                     save = FALSE,
                     nn = "out", 
                     verbose = FALSE){
  
  require(sp)
  require(raster)
  if (verbose == TRUE) message("Ons begin lees")
  x = readLines(fn[1])
  x = strsplit(x, "\n")
  if (verbose == TRUE) message("Klaar gelees. Ek het ", length(x), " waarnemings ingelees")
  
  lt = sapply(x, function(y) {
    i <-  strsplit(y, "[[:space:]]")
    i})
  rm(x)
  dt = sapply(lt, function(i){
    i[i!=""]
  } )
  rm(lt)
  ll <- fivenum(sapply(dt, length))[2] - 3
  naam <- dtf <- lapply(1:length(dt), function(i) dt[[i]][1:(length(dt[[i]])- ll)])
  nml <- sapply(naam, paste, collapse="_")
  vls <- lapply(1:length(dt), function(i) as.numeric(dt[[i]][(length(dt[[i]])- ll +1):length(dt[[i]])]))
  rm(dt)
  d <- data.frame(do.call("cbind", vls))
  colnames(d) <- nml
  points <- SpatialPoints(data.frame(x = vls[[3]], y = vls[[4]]), 	proj4string = CRS("+proj=utm +zone=35"))
  pointsdf <- SpatialPointsDataFrame(points, 	data = d)
  if (verbose == TRUE) message("Punte gemaak. Die volgende deel kan 'n rukkie vat")
  ref <- extent(points)
  xm <- max(table(points@coords[ ,1]))
  ym <- max(table(points@coords[ ,2]))
  r <- raster(ref, nrow = xm, ncol = ym)
  rr <- rasterize(pointsdf, r)
  rr <- rr[[-c(1:5)]]
  names(rr)  <- gsub("X", paste(src, "_", pol,"_h",sep=""), names(rr) )
  if (verbose) message("names rr ", paste(head(names(rr))))
  if (verbose == TRUE) message("Raster gemaak")
  if (aggr.level == "hour"){
    if (write == TRUE){
      nn = paste(datadir, gsub("\\.nc", "", nn), ".nc", sep="")
      if (verbose  == TRUE) message("Ek skryf hom uit na ", nn)
      writeRaster(rr, file = nn, format="CDF", overwrite = ovw)
    }
    if (save  == TRUE){
      save(rr, file = paste(datadir, gsub("\\.nc", "", nn), "_hour",".Rda", sep=""))
    }
    return(rr)
  }
  if (aggr.level == "day"){
    patt <- paste("(", paste(src, "_", pol,"_h",sep=""),"[[:digit:]]{4}_[[:digit:]]+)_[[:digit:]]+$", sep ="")
    if (verbose == TRUE) message("patt is ", patt)
    days <- gsub(patt, "\\1", gsub("_h", "_24h", names(rr)))
    dateidx <- match(days, unique(days))
    if (verbose == TRUE) message(length(unique(days)), " unieke dae")
    rr_d <- stackApply(rr, indices = dateidx, mean, na.rm = TRUE )
    if (verbose == TRUE) message("Ek vergemiddeld per dag")
    names(rr_d)  <- gsub("layer\\.", paste(src, "_", pol, "_24h", sep=""), names(rr_d) )
    if (verbose) message("names rr ", paste(head(names(rr))))
    if (write == TRUE){
      rn = paste(datadir, gsub("\\.nc", "", nn), "_day",".nc", sep="")
      if (verbose == TRUE) message("Ek skryf die raster na ", rn)
      writeRaster(rr_d, file = rn, format="CDF", overwrite = ovw)
    }
    if (save  == TRUE){
      save(rr_d, file = paste(datadir, gsub("\\.nc", "", nn), "_day",".Rda", sep=""))
    }
    
    return(rr_d)
  }
  
}
