#' read.select.cdf Convenience function to read cdf file and select layers
#' @return A Raster* object
#' @param fn. Character. The name of the raster file. Defaults to file.choose()
#' @param select. Regular expression. A part of the name that you want to use to select layers. 
#' NULL to select all layers. Defaults to NULL
#' @param verbose Do you want additional messages. Defaults to FALSE

read.select.cdf <- function(fn = file.choose(), select = NULL, verbose = FALSE){
  if (require(ncdf4) == FALSE){
    message("Jy benodig ncdf4. Ek gaan hom probeer laai")
    install.packages("ncdf4", dependencies = TRUE)
    require(ncdf4)
  }
  r  <- stack(fn)
  message("r has ", nlayers(r), " layers")
  if (verbose == TRUE) message("some sample layers names ", paste(sample(names(r), 15), " "))
  if (!is.null(select)){
    idx <- grep(select, names(r))
    if (length(idx) == 0) stop("No layer names match select. Here is a sample: \n", 
                               paste(sample(names(r), 15), " "),  
                               "\nEither choose select = NULL or use a valid selection")
    r <- r[[idx]]
  }
  r
}