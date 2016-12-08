#' spatial sampling, with selection probabilities defined in a raster
#' @author Michael Scroggie
#' @source https://scrogster.wordpress.com/2012/04/22/using-r-for-spatial-sampling-with-selection-probabilities-defined-in-a-raster/
#' @param probrast Raster with probabilities sum(getValues(probrast), na.rm=T) should be 1
#' @param N integer Number of points to select
#' @return SpatialPoints
#' @usage samppoints<-probsel(probrast, 300)

probsel<-function(probrast, N, ...){
  x<-getValues(probrast)
  #set NA cells in raster to zero
  x[is.na(x)]<-0
  samp<-sample(nrow(probrast)*ncol(probrast), size=N, prob=x, ...)
  samprast<-raster(probrast)
  samprast[samp]<-1 #set value of sampled squares to 1
  #convert to SpatialPoints
  points<-rasterToPoints(samprast, fun=function(x){x>0})
  points<-SpatialPoints(points)
  return(points)
}

#' make_probrast
#' @param r Raster
#'
make_probrast <- function(r){
  #stopifnot(attr(class(r),"package") == "raster")
  r <- r/sum(getValues(r), na.rm=T)
  r
}


# maak raster elke sel in N
# maak gewig raster met p = 1
# gewig * r som in N

