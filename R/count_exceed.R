#' count_exceed 
#' @description count excedences of user- provided values in raster
#' @param s A raster or raster stack
#' @param pol A pollutant name as a character string that matches a layer in the raster stack s
#' @param min Numeric. Value of the first cut point
#' @param max Numeric. Value of the maximum cut point
#' @return A raster

count_exceed <- function(s, pol = NULL, min = 0, max = 100, by = 10, knip = FALSE){
  
  if (!is.null(pol)){
    idx = grep(pattern = pol, x = names(s))
    if (length(idx) == 0) {
      stop("pol does not match the names of s. Use a valid character string or NULL for everything")}
    s = s[[grep(pol, names(s))]]
  } 
  
  ct <- seq(from = min, to = max, by = by)
  b <- brick(extent(s), nl = length(ct))
  res(b) <- res(s)
  
  for (i in 1:length(ct)){
    if (i == 0) {ctv = 0} else {ctv = ct[i]}
    fun <- function(x) {z <- x > ctv ; return(z)}
    v <- sum(calc(s, fun = fun))
    v[is.na(v)] <- 0
    b = setValues(b, getValues(v), layer = i)
  }
  names(b) <- paste("more.than.",as.character(ct), sep="")
  #drop layer that has only zeros 
  keeps <- which(cellStats(b, sum) != 0)
  b <- b[[keeps]]
  b[b == 0] <- NA
  if (knip){
    if (exists("knipNA")){
      cropext <- knipNA(b[[1]], out = "extent")
      b = crop(b, cropext)
    }
  } 
  return(b)
}

# make a barplot of an exceedance object
bar_exceed <- function(z, 
                       cap = "", 
                       ttl = "", 
                       axn = TRUE, 
                       ces.ax = 0.9, 
                       yl = "Events: days x cell", 
                       xl = expression(paste(mu,plain(g/m)^3)), 
                       plot = TRUE,
                       ...) {
  dm = dim(z)
  mxStats <- matrix(cellStats(z, sum))
  
  if (plot) {
    mp <- barplot(height = mxStats, 
            beside = TRUE, 
            las = 1, 
            main = ttl, ylab = yl, xlab = xl,
            axisnames = axn, ...)
    axis(1, at = mp, labels = gsub("greater_than_|more.than.", ">", names(z)), cex.axis = ces.ax, las = 2)
  } else {
    return(mxStats)
  }
}

print_exceed <- function(z, type = c("k", "l")[1], ttl = "Count of exceedences"){
  dt <- data.frame(cellStats(z, sum))
  rownames(dt) <- gsub("greater_than_", ">", names(z))
  names(dt) <- ttl
  if (type == "k") {
    require(knitr)
    return(kable(dt))
  }
  if (type == "l") {
    require(Hmisc)
    latex(dt, title = "")
  }
}

# ff <- freq(kwaza_API, merge = TRUE)
# rownames(ff) <- ff[,1]
# ff <- ff[,-1]
# rowSums(ff, na.rm = TRUE)
