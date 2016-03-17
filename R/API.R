# define RRs
RR.accut.mort <- as.data.frame(t(data.frame(
                            PM10  = c(1.0062, 1.0074,  1.0086,  0.048, 24, 10),
                            PM2.5 = c(1.0110, 1.0150,  1.0190,  0.1 ,  24, 10),
                            SO2   = c(1.0030, 1.0040,  1.0048,  0.026, 24, 10),
                            O3 =    c(1.00023, 1.0051, 1.0078,  0.033,  8, 10),
                            O3 =    c(1.0028, 1.0046,  1.0066,  0.030,  1, 10),
                            NO2 =   c(1.0018, 1.003, 1.0034,  0.020,  1, 10),
                            row.names = c("low", "central" ,"high","coef","period.h", "incr")
                            )))
RR.accut.mort[, "stat"] <- c("mean", "mean", "mean", "max", "max", "max")

total.RR <- function(rr){sum((rr-1))}

#' Pollutant Sub Index
#' 
#' Function that calculates and returns the PSI
#' 
#' @param x A raster
#' @param Exp.met Vector of coefficients
#' @param verbose Logical to display function messages
#' @export

PSI <- function(x, Exp.met, verbose=FALSE){
  psi = x * Exp.met
  if(verbose==TRUE) message("x= ", x)
  if(verbose==TRUE) message("Exp.met = ", Exp.met)
  row.names(psi) = "pol.sub.idx"
  psi
}

#' Air Pollutin Index
#' 
#' Function that calculates the API by making use of the PSI function
#' 
#' @param conc Raster that contains the concentrations.
#' @param verbose Logical that displays function messages if TRUE.
#' @param cast.df Logical that will cast the data frame if true.
#' @param out Character vector that contains the name of the casted
#' data frame to be assigned to the global environment.
#' @param TZ Character vector that contains the city
#' @references Air Quality Index from Cairncross, John and Zunckel Atm.Envir 41 (2007)
#' @export

API <- function(conc = conc, verbose=FALSE, cast.df =TRUE, out = "out", TZ = "Africa/Johannesburg"){
  if (require(openair) == FALSE){
    install.packages("openair", dependencies = TRUE)
    require(openair)
  }
  if (require(reshape2) == FALSE){
    install.packages("reshape2", dependencies = TRUE)
    require(reshape2)
  }
  conc = timeAverage(conc, avg.timex="day", data.thresh=0.1, statistic="mean")
  coefs <- data.frame(pm10  = c(0.048),
                      pm2.5 = c(0.1),
                      so2   = c(0.026),
                      o3mx8  = c(0.033),
                      o3mx1  = c(0.030),
                      no2h1   = c(0.020),
                      corol8h = c(025),
                      row.names = c("coefficients")
                      )
  # Shorten coefs
  if(verbose==TRUE) message(paste(names(coefs), " "))
  if(verbose==TRUE) message(paste(names(conc), " "))
  ymd(conc[,"date"], tz = TZ)
  coefs = coefs[,na.omit(match(names(conc),names(coefs)))]
  conc = conc[,na.omit(match(names(coefs),names(conc)))]
  if(verbose==TRUE) message(paste(names(coefs), " "))
  if(verbose==TRUE) message("dim coefs ",paste(dim(coefs), collapse = " by "))
  if(verbose==TRUE) message("dim conc ",paste(dim(conc), collapse = " by "))
  if(verbose==TRUE) message(paste(names(conc), " "))
  psi = apply(conc,1,FUN=function(x) PSI(x,coefs))
  names(psi) <- dates
  if(verbose==TRUE) message("dim psi= ", dim(psi))
  psi.s = sapply(psi,sum)
  assign("psi",psi.s,envir=.GlobalEnv)
  
  if(cast.df==TRUE){
    suppressMessages( m <- melt(psi) )
    if(verbose==TRUE) message(names(m))
    suppressMessages( m.c<- dcast(data=m,L1~variable,fun.aggregate=sum) )
    assign(paste(out) , m.c , envir=.GlobalEnv)
    m.c
  } else {psi.s}
  
  }

API.color <- data.frame(API.value=0:10, stringsAsFactors = FALSE, 
                       Band = c(rep("low",4),rep("moderate",3),rep("high",3),"very high"),
                       Color = rgb(red  = c(0,   154, 255, 255, 255,  255, 255, 139, 205, 139, 139),
                                   green= c(255, 205, 255, 215, 165,  99,  0,   35,  96,  28,  0),
                                   blue = c(0,   50,  0,   0,   0,    71,  0,   35,  144, 98,  139),
                                   maxColorValue=255)
                        )
## Maak voorbeeld data
# conc1 = source1.b[[1]]
# conc2 = source2.b[[1]]
# s = stack(conc1, conc2)
# requires very specific layer name ID_pollutant_period
# names(s) <- c("IndusX_so2_24h", "HH_pm2.5_24h")
# plot(s)
# s2 <- stack(s,s, s,s)

#' Raster Air Pollution Index
#' 
#' Creates a raster for the API
#' 
#' @param s Raster or a raster stack
#' @param reftab Data frame containing information relating to accute mortality
#' @param idpos Numeric referring to the ID position
#' @param polpos Numeric referring to the position of the pollutant.
#' @param aveperiodpos Numeric referring to the average period position.
#' @param cyclepos Numeric referring to the cycle position.
#' @param sep Character vector containing the seperator to be used.
#' @param aggregate Logical that initializes aggregation when TRUE.
#' @param per_source Logical that returns the source ID.
#' @param verbose Logical that displays function messages if TRUE.
#' @param return.full Logical that stacks the raster objects and returns it when TRUE.
#' @export

rasterAPI <- function(s, 
                      reftab = RR.accut.mort, 
                      idpos = 1, 
                      polpos = 2, 
                      aveperiodpos = 3, 
                      cyclepos = 5, 
                      sep = "_",
                      aggregate = FALSE, per_source = FALSE, 
                      verbose = FALSE, 
                      return.full = FALSE){
  patt = paste('([[:print:]]+)', sep,'{1}', '([[:print:]]+)', sep,'{1}','([[:alnum:]]+)([[:punct:]]*)([[:digit:]]*)', sep="")
  id = gsub(patt, paste('\\', idpos, sep=""), names(s))
  pols = gsub(patt, paste('\\', polpos, sep=""), names(s))
  per = gsub(patt, paste('\\', aveperiodpos, sep=""), names(s))
  cyc = gsub(patt, paste('\\', cyclepos, sep=""), names(s))
  patt2 = '([[:digit:]]*)([[:alpha:]]*)'
  per.d = gsub(patt2, "\\1", per)
  per.l = gsub(patt2, "\\2", per)
  if (verbose == TRUE) message("patt: ", unique(patt))
  if (verbose == TRUE) message("id: ",  unique(id))
  if (verbose == TRUE) message("pols: ", unique(pols))
  if (verbose == TRUE) message("per: ", unique(per))
  if (verbose == TRUE) message("per.d: ", unique(per.d))
  if (verbose == TRUE) message("per.l: ", unique(per.l))
  
  tab = data.frame(orig = names(s), id = id, pols = pols, per = per, per.d = per.d, per.l = per.l, cycle = cyc)
  if (verbose == TRUE) message(str(tab))
  # get the coefs and subset the stack
  rownames(reftab) <- tolower(rownames(reftab))
  if (verbose == TRUE){message("rownames reftab: ", paste(rownames(reftab), " ") , "\npols: ", paste(unique(tab$pols), " "))}
  idx <- na.omit(match(tab$pols, rownames(reftab)))
  if (length(idx) > 0){
  coefs <- reftab[idx, "coef"]
  
  # make pollution sub-index
  psi <- s * coefs
  names(psi) <- paste("PSI-", names(psi), sep="")
  if (verbose == TRUE) message("dim psi: ", paste(dim(psi), collapse = " by "))
  
  # aggregate to one score per id
  id.s <- stackApply(s, indices = match(tab$id, unique(tab$id)), fun = mean)
  names(id.s) <- paste("id", unique(tab$id))
  if (aggregate == TRUE & per_source == TRUE) return(id.s)
  
  # aggregate to one score per day
  if (verbose == TRUE) message("tab$cycle ", tab$cycle)
  api <- stackApply(psi, indices = match(tab$cycle, unique(tab$cycle)), fun = sum)
  
  if (aggregate == TRUE){
    if (verbose == TRUE) message("Ons aggregeer")
    api <- calc(api, mean)
  } 
  
  names(api) <- paste("API_cycle", unique(tab$cycle))
  if (verbose == TRUE) message("names api: ", head(names(api), 1))
  if (return.full == TRUE) api <- stack(s,psi, api)
  api
  } else {message("nothing to do!")}
}
