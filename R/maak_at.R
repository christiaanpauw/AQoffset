#' maak_at
#' @description geriefsfunksie om at vir levelplot te maak
#' @param r a Raster* objek
#' @param n  
#' @return Numeric 
#' @export

maak_at <- function(r, n = 20, beginnul = FALSE, maksimum = NULL, minimum = NULL, simetries = FALSE){
  if (is.null(maksimum)) {
    mx <- ceiling(max(maxValue(r), na.rm = TRUE)) 
  } else {
    mx <- maksimum 
  }
  
  mn <- floor(min(minValue(r), na.rm = TRUE)) 

  if (beginnul){
    mn <- 0
    } else {
      if (simetries){
        mn <- -1 * mx
      } else{
        if (!is.null(minimum)) mn <- floor(min(minValue(r), na.rm = TRUE))
      } 
    }
  if (!is.null(minimum)) mn <- minimum
  at <-  seq(mn, mx, mx/n)
  at
}
