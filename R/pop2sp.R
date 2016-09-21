#' Pop to Sp
#' 
#' Function that edits and plots a SpacialPolygonsDataFrame
#' 
#' @param p Data frame that contains point estimates
#' @param place Character vector referring to the column containing locations
#' @param SPDF A SpacialPolygonsDataFrame
#' @param var Character Name of the variable under consideration
#' @param raster Logical to Rasterise a census polygon SPDF possibly containing 
#' summary data if TRUE.
#' @param refres Numeric vector of length 2 (x,y): A reference resolution
#' @param plot Logical that creates a level and contour plot if TRUE
#' @param debug Logical to display values for debugging process
#' @param ... Arguments to be passed to/from other methods
#' @export

pop2sp <- function(p = fuel_house, place = "place", SPDF = KWA, 
                   var = "SP_NAME" , raster = FALSE, refres = c(40, 40), 
                   plot = FALSE, debug = FALSE, resamp = FALSE, ... ){
	
  require(reshape2)
  require(dplyr)
  require(lattice)
  
  names(p)[2] = "var"
	tms = length(p[,place]) / (length(p[,place]) - length(which(p[,place] == ""))) - 1
	for (i in 1:tms) {
		p[,place] = ifelse(p[,place]  == "", lag(p[,place]), p[,place])
		}
		names(p)[grep(place, names(p))] <- "place"
		
		p$place <- tolower(p$place)
		p$place <- gsub(" ", "\\.", p$place)
		
		m = melt(p)
		m = m[!is.na(m$var), ]
		d =  dcast(m , place ~ var + variable, sum)
		dropidx = grep("NA_|^$", names(d))
		if (length(dropidx) > 0) d = d[, -dropidx]
		
		# Maak die name kleinletters 
		SPDF@data[,var] = tolower(SPDF@data[,var])
		SPDF@data[,var] = gsub(" ", "\\.", SPDF@data[,var])
		
		# 
		SPDF@data = data.frame(SPDF@data[,var])
		colnames(SPDF@data) = "place"	
		SPDF@data = d[match(SPDF@data$place, d$place), ]
		
		if (raster == FALSE) return(SPDF)
		
		r = rasteriseCensus(SPDF, ref = extent(SPDF), refres = refres)
		
		if (debug == TRUE) assign("r", r, envir = .GlobalEnv)
		
		if (plot == TRUE){
		  if (resamp) r <- resample(r, raster(extent(r), ncol = 100, nrow = 100))
		  levelplot(r, ...) 
		}
}

# hulpfunkie om spelfoute in popProjek uitvoer reg te maak
# use: coal.cook$place = fix.popProjek(coal.cook, place, "maphela", "mapehla")

#' Fix popProjek
#' 
#' Fixes the wrongful spelling of a place, the default case being mapehla
#' 
#' @param x Data frame containing the places to be fixed
#' @param var Character vector. The column name referring to places
#' @param asis Character vector. Identifier of the wronful spelling
#' @param mustbe Character vector to replace wrongful spelling
#' @export

fix.popProjek <- function(x, var = "place", asis = "maphela", mustbe = "mapehla"){
	x[grep(asis, x[ ,var]), var] <- mustbe
	x[,var]
}


## Hulpfunksie wat die SPDF se lyne plot
#addpol <- function(){layer(sp::sp.polygons(SPDF, lwd = 0.25))}


