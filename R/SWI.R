#' SWI Calculate Standards weighted intake of pollutant
#' @param conc raster The concentration of every pollutant
#' @param pop raster The # of people in each grid cell
#' @param ap Character "Annual" of "Daily"
#' @param Q numeric Breathing rate. Default 8 m3 per day
#' @param refpol one of c("PM10", "PM2.5", "SO2", "O3", "O3.1", "NO2"). Default
#' @papam reftab A dataframe with the standards. Colnames are the averaging periods. Rownames are the pollutants

NAQS <- t(data.frame(
  PM10  = c(60, 120),
  PM2.5 = c(25, 75),
  SO2   = c(50, 100),
  O3 =    c(40, 80),
  O3 =    c(50, 70),
  NO2 =   c(60, 100), 
  row.names = c("Annual", "Daily"))
  )

conc = data.frame(PM10 = 100, SO2 = 200)
ap = "Annual"
NAQS.rel = NAQS[ap,names(conc)]
Q = 8
refpol = "PM10"

# Intake 
if (ap == "Annual") {
  Q = Q  * 365}

I = conc * Q * pop

# Standard intake
SI = NAQS.rel * Q * pop 

# weeg my manier
#standard.relative.conc <- conc / NAQS.rel
#standard.relative.conc
#PM10.eq <- standard.relative.conc * NAQS.rel[,"PM10"] # PM10 moet wees wat dit was
#PM10.eq

# PM10 equivalent intake 
PM10.eq * Q * pop

EI = unlist(SI[,refpol]) * I/SI

# Total effective intake
TEI <- sum(EI)


rasterSWI <- function(s, 
                      reftab = NAQS, 
                      idpos = 1, 
                      polpos = 2, 
                      aveperiodpos = 3, 
                      cyclepos = 5, 
                      sep = "_", verbose = TRUE){
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



