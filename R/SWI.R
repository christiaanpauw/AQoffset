#' Standards Weighted Intake
#' 
#' Calculates the total effective intake of pollutants by using
#' standardisation
#' 
#' @param conc Raster. The concentration of every pollutant
#' @param NAQS Data frame of the National Air Quality Standards
#' @param pop Raster. The number of people in each grid cell
#' @param ap Character verctor. "Annual" or "Daily"
#' @param Q Numeric. Breathing rate in m3 per person per day
#' @param idpos Numeric referring to the ID position
#' @param polpos Numeric referring to the position of the pollutant.
#' @param aveperiodpos Numeric referring to the average period position.
#' @param cyclepos Numeric referring to the cycle position.
#' @param sep Character vector containing the seperator to be used.
#' @export

SWI <- function(conc, 
                NAQS = NULL, 
                pop = people, 
                ap = "Annual", 
                Q = 8,
                idpos = 1, 
                polpos = 3, 
                aveperiodpos = 2, 
                cyclepos = 5,
                sep = "_", verbose = FALSE
                ){

  if (is.null(NAQS)){
  NAQS <- t(data.frame(
  pm10  = c(40, 75),
  PM2.5 = c(20, 40),
  so2   = c(50, 100),
  O3 =    c(40, 80),
  O3 =    c(50, 70),
  NO2 =   c(60, 100), 
  row.names = c("Annual", "Daily"))
  )
}

#isolate present compounds and find their indices
#patt = "([[:alpha:]]+_)([[:alnum:]]+_)([[:print:]]+$)"
patt = paste('([[:print:]]+)', sep,'{1}', '([[:print:]]+)', sep,'{1}','([[:alnum:]]+)([[:punct:]]*)([[:digit:]]*)', sep="")
id = gsub(patt, paste('\\', idpos, sep=""), names(conc))
pols = gsub(patt, paste('\\', polpos, sep=""), names(conc))
per = gsub(patt, paste('\\', aveperiodpos, sep=""), names(conc))
cyc = gsub(patt, paste('\\', cyclepos, sep=""), names(conc))
patt2 = '([[:digit:]]*)([[:alpha:]]*)'
per.d = gsub(patt2, "\\1", per)
per.l = gsub(patt2, "\\2", per)
if (verbose == TRUE) message("patt: ", unique(patt))
if (verbose == TRUE) message("id: ",  paste(unique(id), " "))
if (verbose == TRUE) message("pols: ", paste(unique(pols), ""))
if (verbose == TRUE) message("per: ", unique(per))
if (verbose == TRUE) message("per.d: ", unique(per.d))
if (verbose == TRUE) message("per.l: ", unique(per.l))
nms = unique(pols)
idx = match(pols, unique(pols))
ss = stackApply(conc, indices = idx, fun = mean)
names(ss) <- nms
#assign("ss", ss, envir = .GlobalEnv)

pop <- crop(pop, extent(ss))
concpop = overlay(ss, pop, fun=function(x,y){ x * y})

#select relevant NAQS
#print(NAQS)
NAQS.rel = NAQS[nms, ap]
# Intake 
I = concpop * Q 
# Standard intake for each pollutant
SIL = list()
for (i in 1:length(nms)) {
  SIL[[i]] = NAQS.rel[i] * Q * pop #was people
}

SI = stack(SIL)
names(SI) = nms

#Effective intake
EI = SI * I/SI

# Total effective intake
TEI <- calc(EI, sum)
return(TEI)
}

#' Raster SWI
#' 
#' Creates a raster for the standars weighted intake
#' 
#' @param s A raster or raster stack
#' @param reftab A data frame with the standards. 
#' Colnames are the averaging periods. Rownames are the pollutants.
#' @param idpos Numeric. Position of the ID 
#' @param polpos Numeric. Position of the pollutant 
#' @param aveperiodpos Numeric. Position of the average period
#' @param cyclepos Numeric. Position of the cycle
#' @param sep Character vector containing the seperator to be used.
#' @param verbose Logical that displays function messages if TRUE.
#' @export

rasterSWI <- function(s, 
                      reftab = NAQS, 
                      idpos = 1, 
                      polpos = 2, 
                      aveperiodpos = 3, 
                      cyclepos = 5, 
                      sep = "_", verbose = FALSE){
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
  if (verbose == TRUE) message("pols: ", paste(unique(pols), " "))
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
    # make population weighted exposur
    pwe <- s * pop
   
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



