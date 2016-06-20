
#' Raster Concentration Response
#' 
#' Excecute a concentration response function on rasterised input data 
#' 
#' @param conc A raster of concentration values
#' @param popr A raster of population numbers
#' rasterConcentrationResponse
#' @description Excecute a concentration response function on rasterised input data 
#' @param conc A raster object of concentration values for a particular pollutant.
#' @param popr A raster object of population numbers.
#' @param base.conc A scalar giving the base concentration of the pollutant
#' @param fun.form The form of the concentration response function. 
#' Currently supports "linear" (default) and "log-linear" 
#' @param beta The Beta values from the original regression model from which the
#' concentration response function (CRF) is derived.
#' Defaults to NULL. If NULL, it is calculated from RR and delta (beta = log(RR)/delta)
#' @param RR A single numeric value. The relative risk associated with a 
#' concentration increase of delta.
#' @param delta A single numeric value. The concentration difference for which 
#' RR is defined. Defaults to 10.
#' @param cases A raster containing the number of cases of the outcome of 
#' concern in the population.
#' @param incidence.rate The incidence rate of the outcome of concern.
#' @param est The type of estimate that the values of RR represent. 
#' e.g. c("low estimate","cental estimate","high estimate"). Must be the same 
#' length as RR
#' @param outcomename User specified name of the outcome. Defaults to NULL.
#' @param pollutantname User specified name of the pollutant. Defaults to NULL.
#' @param unit.cost The cost of the outcome per case
#' @param costing.var The name of the varible containing the unit cost
#' @param risk.only Logical. Returns the RR raster if TRUE
#' @param out Character vector. The desired output. Defaults to "all"
#' @param verbose Do you want to be annoyed with a lot of messages, or not?
#' @param debug Set to TRUE if the function misbehaves so you can see what's 
#' going on
#' @details pollutant This is used to match the name of the pollutant in the 
#' concentration raster 
#' with that in the concentration response information (typically given as a sicklist)
#' @examples 
#' cases = people$total * rnorm(n = ncell(people), mean = 0.01, sd = 0.003)
#' @return IF risk.only, the function returns a raster stack with the same number 
#' of layers as conc; if not...?
#' # CAN ONLY WORK WITH *ONE* OUTCOME ASSOCIATED WITH *ONE* POLLUTANT AT A TIME
rasterConcentrationResponse <- function(conc, 
                                        popr, 
                                        base.conc, 
                                        fun.form = c("linear","log-linear")[1],
                                        beta = NULL, 
                                        RR = NULL, 
                                        delta = 10, 
                                        cases = NULL,
                                        incidence.rate = NULL,
                                        est = c("low estimate","cental estimate","high estimate"),
                                        outcomename = NULL, 
                                        pollutantname = NULL,
                                        unit.cost = NULL,
                                        costing.var = NULL, 
                                        risk.only = FALSE,
                                        out = c("all", "popr", "cases", "conc", "RR", "AF", "AM")[1],
                                        verbose = FALSE, 
                                        debug = FALSE, ...) {
  
  if (attributes(class(popr)) != "raster") {stop("popr must be a raster")}
  if (attributes(class(conc)) != "raster") {stop("conc must be a raster")}
  
  # Maak seker al die rasters pas op mekaar
  if (!all.equal(extent(popr), extent(conc))) {
    stop("The population and concentration rasters must have the same extent.")
  }
  
  # If population-wide number of cases of particular health outcome is unknown,
  # calculate it
  if (is.null(cases) & is.null(incidence.rate)) {
    stop("There must be at least cases or an incidence rate.")
  }
  if (is.null(cases)) {
    cases <- popr * incidence.rate
  }
  
  # if 'beta' is NULL, determine it
  if(is.null(beta)) {
    if (is.null(RR)) {
      stop("There must at least be a beta or a relative risk (RR).")
    }

    beta <- log(RR)/delta
    if (verbose) {
      message("Defined beta from RR and delta: ",
              " RR = ", RR, 
              " delta = ", delta,
              " beta = ", beta)
    }
  }
  
  # Verwyder 0 konsentrasies en populasies
  conc[conc == 0] <- NA
  popr[popr == 0] <- NA
  
  if (verbose) {
    message("RR = ", paste(RR, " "))
    message("delta = ", delta)
    message("beta = ", paste(beta, " "))
    message("base.conc = ", base.conc)
    message("fun.form = ", fun.form)
  }

  # hulpfunksies om op rasters te gebruik binne overlay()
  # exp.maal <- function(x,y) {return(exp(x * y))}
  # mag <- function(x,y) {return(exp(x ^ y))}
  
  if (fun.form == "linear") {
    if (verbose) {
      message("Jy is binne in  fun.form == 'linear'")
    }
    excess.conc <- conc - base.conc
    excess.conc[excess.conc < 0]  <- NA
    RR <- exp(beta * excess.conc)
    #RR <- overlay(bs, excess.conc, fun = exp.maal) 
    #RR=exp[beta(X-Xo)] # check teen die oorspronklike beta
    
    if (debug) {
      message("nlayers RR: ", nlayers(RR))
    }
    names(RR) <- paste(names(conc), names(cases), sep = "_")
  }
  
  if (fun.form == "log-linear") {
    if(verbose) {
      message("Jy is binne-in fun.form == 'log.linear'")
    }
    cbreuk <- ((conc+1)/(base.conc+1))
    cbreuk[cbreuk == 0] <- NA
    #RR <- overlay(cbreuk, bs, fun = mag)
    RR <- exp(cbreuk ^ beta)
    names(RR) <- paste(names(conc), sep="_")
  }
  
  names(RR) <- paste("RR", names(RR), sep="_")
  if (risk.only) {return(RR)}
  
  # determine the attributable fraction
  AF <- (RR-1)/RR  #AF=(RR-1)/RR     # dalk monte carlo hier!

  # determine the number of cases that can be attributed to exposure to air 
  # pollution
  AM <-  AF * incidence.rate * popr
  if (debug) {
    message("dim AM ", paste(dim(AM), collapse = " by "))
  }
  
  # Sorg dat die name leesbaar is
  names(AF) <- paste("Attr.Fraction", names(AF), sep="_")
  names(AM) <- paste("Attr.Insidence", names(AM), sep="_")
  names(popr) <- paste("Pop", names(popr), sep="_")
  names(cases) <- paste("Cases", names(cases), sep="_")
  names(conc) <- paste("Concentration", names(conc), sep="_")
  
  #Gee 'n enkele laag uit as die gebruiker so verkies
  if (out =="popr") return(popr)
  if (out =="conc") return(conc)
  if (out =="cases") return(cases)
  if (out =="AF") return(AF)
  if (out =="AM") return(AM)
  
  # Andersins, stapel almal opmekaar en return
  return(stack(popr, cases, conc, RR, AF, AM))
}

#' Raster Concentration Response Enhanced Program
#' 
#' Function uses rasterConcentrationResponse, but adds specific names and categories
#' 
#' @param sl Sicklist
#' @param pollutant Character vector. The reference pollutant
#' @param cconc A ranster of concentration values
#' @param ppopr A raster of population numbers
#' @param bbase.conc A scalar giving the base concentration of the pollutant
#' @param iincidence.rate The incidence rate of the outcome of concern.
#' @param rrisk.only Logical. Returns the RR raster if TRUE
#' @param verbose Logical. Display messages or not.
#' @param ... Arguments to be passed to/from other methods
#' @export

# wrapper function for rasterConcentrationResponse that loops through a sicklist
# and ensures that the concentration data is sent to above-mentioned function
# in the correct format for the sickness' effect type (i.e. acute, chronic)
rasterCREP <- function(sl = NULL, 
                       pollutant = "PM10", 
                       cconc = NULL, 
                       ppopr = NULL, 
                       bbase.conc = 10, 
                       ccases = NULL,
                       iincidence.rate = NULL, 
                       rrisk.only = TRUE, 
                       verbose = FALSE,
                       output = "all",
                       ddelta = 10) {
  
  ppopr = crop(ppopr, extent(cconc))
  polidx <- which(tolower(pollutant) == tolower(summarise.sicklist(sl)$pollutant))
  if (length(polidx) < 1) {
    stop("There is no pollutant ", pollutant, " in sl")
  }
  
  # get the outcomes associated with the pollutant
  sl <- sl[polidx] #nou vir pollutant length =1 maar dalk 'n loop oor meer 
  
  # maak 'n lys om die rasters in te stoor
  lsRasters <- vector(mode = "list", length = length(sl))
  names(lsRasters) <- names(sl)
    
  if (verbose) {
    message("pollutant = ", pollutant, "\nrrisk.only = ", rrisk.only)
  } 
  
  # loop through all the outcomes assoc with the pollutant
  for (i in 1:length(sl)) { 
    if (verbose) {
      message("i: ", i, "\n", class(sl),  " ", length(sl))
      message("sl pollutant: ", sl[[i]]$pollutant)
      message("sl outcome: ", sl[[i]]$end.point)
      message("sl effect: ", sl[[i]]$Effect)
    }
    
    if (tolower(sl[[i]]$Effect) %in% c("chronic")) {
      gemiddeld = function(x) {mean(x, na.rm = TRUE)}
      concObj <- calc(x = cconc, fun = gemiddeld)
    } else {
      concObj <- cconc
    }
    
    res <- rasterConcentrationResponse(conc = concObj, 
                                       popr = ppopr, 
                                       base.conc = bbase.conc, 
                                       fun.form = c("linear","log-linear")[1],
                                       beta = sl[[i]]$beta, 
                                       RR = sl[[i]]$RR, 
                                       delta = ddelta,
                                       cases = ccases,
                                       incidence.rate = sl[[i]][["Incidence.rate"]],
                                       est = c("low estimate",
                                               "cental estimate","high estimate"),
                                       pollutantname = pollutant, 
                                       outcomename = sl[[i]]$end.point,
                                       unit.cost = NULL, 
                                       costing.var = NULL, 
                                       risk.only = rrisk.only,
                                       debug = FALSE,
                                       out = output)
    lsRasters[[i]] <- res; rm(res)
  }
  
  # so at this point lsRasters is a list of raster stacks - one stack per outcome
  # listed in the sicklist
  return(lsRasters)
  
}

