
#' knipNA create a new extent with NA-only columns cut out
#' @reference From an answer by Marie Auger-Methe on http://stackoverflow.com/questions/19680079/r-crop-no-data-of-a-raster



knipNA <- function(r, out = c("extent", "raster")[1]){
if (attr(class(s), "package") != "raster") stop("r should be a raster")
if (length(out) != 1) stop("out must be either 'extent' or 'raster' \n it's current length is ", length(out))
if (is.na(match(out,  c("extent", "raster")))) stop(paste("\nout must be either 'extent' or 'raster'\n it is currently " , out, sep=""))

#Transform the raster into a matrix that identifies whether the values are NA or not.
r1NaM <- is.na(as.matrix(r))

#Find the columns and rows that are not completely filled by NAs
colNotNA <- which(colSums(r1NaM) != nrow(r))
rowNotNA <- which(rowSums(r1NaM) != ncol(r))

#Find the extent of the new raster by using the first and last columns and rows that are not completely filled by NAs. 

r3Extent <- extent(r, rowNotNA[1], rowNotNA[length(rowNotNA)],
   colNotNA[1], colNotNA[length(colNotNA)])
if (out == "extent"){
	return(r3Extent )
	} else {
		r3 <- crop(r, r3Extent)
		return(r3)
	}


#Plot the rasters for comparison.
}

