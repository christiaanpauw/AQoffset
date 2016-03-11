


pop2sp <- function(p = fuel_house, place = "place", SPDF = KWA, var = "SP_NAME" , raster = FALSE, refres = c(40, 40), plot = FALSE, ... ){
	
  require(reshape2)
  require(dplyr)
  
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
		dropidx = grep("NA_", names(d))
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
		
		if (plot == TRUE){
		  levelplot(r, ...) 
		}
}

# hulpfunkie om spelfoute in popProjek uitvoer reg te maak
# use: coal.cook$place = fix.popProjek(coal.cook, place, "maphela", "mapehla")

fix.popProjek <- function(x, var = "place", asis = "maphela", mustbe = "mapehla"){
	x[grep(asis, x[ ,var]), var] <- mustbe
	x[,var]
}


## Hulpfunksie wat die SPDF se lyne plot
#addpol <- function(){layer(sp::sp.polygons(SPDF, lwd = 0.25))}


