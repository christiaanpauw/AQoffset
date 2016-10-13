#' Rasterise Census Subplace
#'
#' Rasterise a census polygon SPDF possibly containing summary data.
#' The function randomly assign the correct number of sample units
#' in each of a number of categories e.g. energy carrier for heating:
#' as summarised in each row of the SPDF as points to a polygon
#' It can be used to:
#' make a total
#' make number of people
#' make sex distribution make age distribution
#'
#' @param x A SpatialPolygonsDataFrame
#' @param ref An extent object
#' @param weightraster Raster with weights - typically a population raster like the utput from worldpop
#' @param verbose Print messages or not
#' @param refres Numeric vector of length 2 (x,y): A reference resolution
#' @param drpnames A character vector of column names that should not be included
#' @param cleanSP Logical Do you want to remove rows where *spcode* is NA
#' @param spcode Character name of variable for which rows are removed when NA
#' @param ... Arguments to be passed to/from other methods
#' @export

# todo: change to be able to use with a weight vector (example struture density from remote snesing image)

rasteriseCensus <- function(x, ref = NULL, cleanSP = TRUE, spcode = "SP_Name",
                            weightraster = NULL,
                            verbose = FALSE,
                            refres = NULL,
                            drpnames=c("ID", "Geometry_s", "GAVPrimar0", "Geometry_1", "OBJECTID",
                                       "SP_CODE", "SP_Code", "MP_CODE", "MP_Code", "MN_CODE", "MN_MDB_C",
                                       "DC_MN_C", "Shape_Leng", "Shape_Area", "fakeData", "GAVPrimary",
                                       "Total"),
                            ...){
  if(!is.null(weightraster)){
    #weight <- make_probrast(weightraster)

    proj4string(x) <- proj4string(weightraster)
    x <- spTransform(x, CRSobj = weightraster@crs)

    # Verwyder alle kolomme genoem in drpnames
    dropidx <- na.omit(match(drpnames, names(x)))
    if (verbose == TRUE) message(paste(dim(x@data), collapse = " by "))
    if (verbose == TRUE) message("names : ", paste(names(x), " "))
    if (length(dropidx) > 0) x <- x[,-dropidx]
    if (verbose == TRUE) message("names : ", paste(names(x), " "))

    # Verwyder lee kolomme
    leegidx <- which(sapply(x@data, function(i) all(is.na(i))) == TRUE)
    if (length(leegidx) > 0) x@data <- x@data[-leegidx]

    # Isoleer net die numeriese kolomme met wpositiewe aardes in
    numidx <- grep("numeric|integer", sapply(x@data, class ))
    nulindeks <- which(colSums(x@data[numidx], na.rm = TRUE) == 0)
    if (length(nulindeks) > 0){
      x@data <- x@data[,-numidx[nulindeks]]
    }
    numidx <- grep("numeric|integer", sapply(x@data, class ))
    # maak alle oorblywende NA 0
    x@data[is.na(x@data)] <- 0

    # worldpop_hh_sp <- SpatialPointsDataFrame(weightraster,
    #                                          data = data.frame(v = getValues(weightraster)),
    #                                          proj4string = CRS(proj4string(weightraster)))
    # names(x)
    # lapply(names(x), function(i) {
    #   worldpop_hh_sp@data[ ,i] <<- over(worldpop_hh_sp, x)[ ,i]
    # })

    #R <- rasterize(worldpop_hh_sp, worldpop_hh, field = "Coal", fun = sum)

    # if an area does not have a SP code : remove it
    if (cleanSP){
      droprow <- which(is.na(x@data[,spcode]))
      if (length(droprow) > 0){
        x <- x[-droprow, ]
      }
    }

    refras <- raster(extent(weightraster), ncol = ncol(weightraster), nrow = nrow(weightraster))
    if (verbose) message(paste(names(x)[numidx]," "))
    rl <- lapply(1:length(numidx), function(i) maak_weeg_raster(x, ref = refras, fn = names(x)[numidx][i], FUN = sum, verbose = verbose))
    CR <- stack(rl)
    return(CR)
  }

  if (is.null(ref)) {
   if (is.null(weightraster)){
     ref <- extent(x)
   } else {ref <- extent(weightraster)}
  }

  if (is.null(refres)) {
    if (is.null(weightraster)){
      refres <- c(10, 10)
    } else {refres <- res(weightraster)}
  }

  if (is.null(weightraster)){
    if (verbose) message("Daar is geen gewigte, ek stat eweredig !")
  ress = pointifyCensus(spdf = x, dropnames = drpnames, verbose = TRUE)
  rm(x)
  cts = as.character(unique(ress@data$category))
  if (verbose) message("cts gemaak. Dis: ", paste(cts, " "))
  b = brick(ref, nl = length(cts), nrow = refres[1], ncol = refres[2])
  for (i in 1:length(cts)){
    if (verbose == TRUE) message(i," " , cts[i], "\nrefres is ", paste(refres, " "))
    vls = rasterize(x = SpatialPoints(ress[ress@data$category == cts[i],]),
                    y = b,
                    fun = "count")
    b = setValues(b, getValues(vls), layer = i)
  }
  if (verbose) message("b gemaak. Dimensies: ", paste(dim(b), " "))
  if (nlayers(b) == length(cts)) names(b) <- cts
  return(b)
  }
  # grid : spp = SpatialGrid(points2grid(SpatialPoints(coordinates(b))))
}

#' Sexify
#' Change  SPDF to match a se proportion
#'
#' @param x A SpatialPolygonsDataFrame
#' @param prop Numeric that contains the sex proportion
#' @export

sexify <- function(x, prop = 0.5){
  res = x * prop
  res
}


#' gridzle
#' Maak 'n rooster (SpatialGrid) van 'n raster
#' @param r raster

gridzle <- function(r){
  res = SpatialGrid(points2grid(SpatialPoints(coordinates(r))))
  res
}


#' make_probrast
#' @description make a probability raster r/sum(r)
#' @param r Raster
#' @export

make_probrast <- function(r){
  #stopifnot(attr(class(r),"package") == "raster")
  r <- r/sum(getValues(r), na.rm=T)
  r
}

