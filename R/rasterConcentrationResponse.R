#' rasterConcentrationResponse
#' @description Excecute a concentration response function on rasterised input data 
#' @param conc A raster of concentration values
#' @param popr A raster of population numbers
#' @param base.conc A scalar giving the base concentration of the pollutant
#' @param fun.form The from of the concentration response function. 
#' Currently supports "linear"(default) and "log-linear" 
#' @param beta The Beta values from the original regression model from which the CRF is derived.
#' Defaults to NULL. If NULL it is calculated from RR  and delta ( beta = log(RR)/delta )
#' @param RR The relative risk associated with an concentration increase of delta 
#' @param delta The concentration difference for which RR is defined. Defaults to 10
#' @param cases A raster containing the number of cases of the outcome of concern in the population.
#' @param incidence.rate The incidence rate of the outcome of concern.
#' @param est The type of estimate that the values of RR represent. 
#' e.g. c("low estimate","cental estimate","high estimate"). Must be the same length as RR
#' @param outcomename User specified name of the outcome. Default NULL
#' @param pollutantname User specified name of the pollutant. Default NULL,
#' @param unit.cost The cost of the outcome per case
#' @param costing.var The name of the varible containing the unit cost
#' @param verbose Do you want to be anoyed with a lot of messaged 
#' @param debug Set to TRUE if the function misbehaves so you can see whats going on

#' @details polluntant This is used to match the name of the pollutant in the concentration raster 
#' with that in the concentration response information (typically given as a sicklist)

#' @examples 
#' cases = people$total * rnorm(n = ncell(people), mean = 0.01, sd = 0.003)
library(roxygen2)

rasterConcentrationResponse <- function(conc, popr, base.conc, 
                                        fun.form = c("linear","log-linear")[1],
                                        beta = NULL, RR = NULL, delta = 10,
                                        cases = NULL, incidence.rate = NULL,
                                        est = c("low estimate","cental estimate","high estimate"),
                                        outcomename = NULL, pollutantname = NULL,
                                        unit.cost = NULL, costing.var = NULL, risk.only = FALSE,
                                        verbose = FALSE, debug = TRUE){
  #if (verbose == TRUE) message("beta =", beta)
  
  if (attributes(class(popr)) != "raster") stop("popr must be a raster")
  if (attributes(class(conc)) != "raster") stop("conc must be a raster")
  
  # Maak seker al die raster pas op mekaar
  if (all.equal(extent(popr), extent(conc)) == FALSE) stop("The population and concentration rasters must have the same extent")
  
  if (is.null(cases) & is.null(incidence.rate)) stop("There must be at least cases or an incidence rate")
  if (is.null(cases)==TRUE) {d <- incidence.rate * popr} else {d <- cases}
  # stoor naam vir later
  names.RR.orig <- names(RR)
  if (length(names.RR.orig) == 0) names.RR.orig <- paste("Risk",letters[1:length(RR)], sep="_")
  if (verbose == TRUE) message("names.RR.orig: ", names.RR.orig, " ", length(names.RR.orig))
  
  if(is.null(beta)==TRUE) {
    if (is.null(RR)) stop("There must be at least a beta or a relative risk (RR) ")
    if(verbose==TRUE) if (!is.null(names.RR.orig)) message("names RR = ", paste(names.RR.orig, " "))
    RR = unlist(RR)
    if(verbose==TRUE) message("delta=", delta)#"log(RR)= ",log(RR)) 
    if(verbose==TRUE) message("structure of RR: ", str(RR))
    beta = log(RR)/delta
    if(verbose==TRUE) message("Defined beta from RR and delta: RR= ", paste(RR," "), "delta=", delta )
  }
  # Verwyder 0 konsentrasies en populasies
  conc[conc == 0] <- NA
  popr[popr == 0] <- NA
  conc <- mask(conc, popr)
  
  if (verbose==TRUE) message("RR = ", paste(RR, " "))
  if (verbose==TRUE) message("base.conc = ", base.conc)
  if (verbose==TRUE) message("fun.form = ", fun.form)
  if (verbose==TRUE) message("beta = ", paste(beta, " "))
  # stel besoedelstof naam
  if (is.null(pollutantname)){
    names(conc) <- "pollutant"
  } else {
    names(conc) <- pollutantname
  }
  if (verbose==TRUE) message("names conc = ", paste(names(conc), " "))
  # stel geval name
  if (is.null(outcomename)){
    names(d) <- "outcome"
  } else {
    names(d) <- outcomename
  }
  
  # maak die betas tot 'n raster (maak later 'n funksie hiervoor)
  bs = brick(extent(conc), nl = length(beta), nrows = nrow(conc), ncols = ncol(conc))
  names(bs) <- names.RR.orig
  if (verbose == TRUE) message("names bs: ", paste(names(bs), " "))
  values(bs) <- matrix(rep(beta, ncell(bs)), ncol = length(beta), byrow=TRUE)
  
  # hulpfunksies om op rasters te gebruik binne overlay()
  exp.maal = function(x,y) return(exp(x * y))
  mag = function(x,y) return(exp(x ^ y))
  
  if(fun.form == "linear"){
    if(verbose==TRUE) message("Jy is binne in  fun.form == 'linear'")
    excess.conc = conc - base.conc
    excess.conc[excess.conc < 0]  <- NA
    RR = overlay(bs, excess.conc, fun = exp.maal) #RR=exp[beta(X-Xo)] # check teen die oorspronklike beta
    if(verbose==TRUE) message("Names conc: ", names(conc))
    if(verbose==TRUE) message("names.RR.orig: ", names.RR.orig)
    if(verbose==TRUE) message("nlayers RR: ", nlayers(RR))
    names(RR) <- paste(names(conc), names.RR.orig, names(d), sep="_")
  }
  
  if(fun.form == "log-linear"){
    if(verbose==TRUE) message("Jy is binne in  fun.form == 'log.linear'")
    cbreuk <- ((conc+1)/(base.conc+1))
    cbreak[cbreuk == 0] <- NA
    RR = overlay(cbreuk, bs, fun = mag)
    names(RR) <- paste(names(conc), names.RR.orig, sep="_")
  }
  
  if (risk.only){return(RR)}
  
  AF =  (RR-1)/RR  #AF=(RR-1)/RR     # dalk monte carlo hier!
  if(verbose==TRUE) message("dim AF ", paste(dim(AF), collapse = " by "))
  if(verbose==TRUE) message("dim d ", paste(dim(d), collapse = " by "))
  AM =  AF * d #AM = AF * cases
  if(verbose==TRUE) message("dim AM ", paste(dim(AM), collapse = " by "))
  
  # Sorg dat die name leesbaar is
  names(AF) <- paste("Attr.Fraction", names(AF), sep="_")
  names(AM) <- paste("Attr.Insidence", names(RR), sep="_")
  names(popr) <- paste("Pop", names(popr), sep="_")
  names(d) <- paste("Cases", names(d), sep="_")
  names(conc) <- paste("Concentration", names(conc), sep="_")
  names(RR) <- paste("RR", names(RR), sep="_")
  
  #Stapel hulle op mekaar
  out <- stack(popr, d, conc, RR, AF, AM)
  out
}

# werk baie presiese name en kategorië by ¡¡¡¡¡

rasterCREP <- function(sl = endlist, pollutant = "PM10", cconc = year.brick, ppopr = people, 
                       bbase.conc = 10, iincidence.rate = 0.01, rrisk.only = TRUE, ...){
  
  polidx = which(tolower(summarise.sicklist(sl)$pollutant) == tolower(pollutant))
  if (length(polidx) < 1) stop("There is no pollutant ", pollutant, " in sl")
  sl = sl[polidx] #nou vir pollutant length =1 maar dalk 'n lopp oor meer 
  
  ParkEnv = new.env() # ons gaan ons rasters daar bêre
  
  if (verbose == TRUE){
    message("pollutant = ", pollutant, "\nrrisk.only = ", rrisk.only)
  } 
  
  for (i in 1:length(sl)){ 
    if (verbose == TRUE){
      message("i: ", i, "\n", class(sl),  " ", length(sl))
      message("sl pollutant: ", sl[[i]]$pollutant)
      message("sl outcome: ", sl[[i]]$end.point)
      message("sl effect: ", sl[[i]]$Effect, "\n_________________________")
    }
    x = rasterConcentrationResponse(conc = cconc, popr = ppopr, base.conc = bbase.conc, 
                                fun.form = c("linear","log-linear")[1],
                                beta = sl[[i]]$beta, RR = sl[[i]]$RR, delta = 10,
                                cases = NULL, incidence.rate = iincidence.rate,
                                est = c("low estimate","cental estimate","high estimate"),
                                pollutantname = pollutant, outcomename = sl[[i]]$end.point,
                                unit.cost = NULL, costing.var = NULL, risk.only = rrisk.only,
                                verbose = get("verbose"), debug = TRUE)
    message("names x: ", names(x))
    assign(paste("x",as.character(i), sep="_"), x, envir = ParkEnv)
    if (verbose == TRUE) message("Jy verlaat sl lus nommer ", i)
  }
  items = length(ls(envir = ParkEnv))
  s <- brick(extent(cconc), nrows = dim(cconc)[1], ncols = dim(cconc)[2])
  message("names s: ", names(s))
  for (i in 1:items){
    s = addLayer(s, get(ls(envir = ParkEnv)[i], envir = ParkEnv))
    message(i, "\n _________________________________")
  }
  s  
}

