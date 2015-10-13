# maak verdeling uit sy beskrywing
# Wat jy benodig het is 'n lys met 2 elemente in elke lys-item
# 1. dist : Die naam van die verdeling
# 2. param: 'n benoemde lys van parameters
# die funksie sit dan die teks bymekaar en om binne in do.call(  ) te sit en uit te voer. Daar het dit nog x nodig 


####################################################
# Maak 'n S4 klass vir 'n verdelinglys

setClass("distlist", 
         representation = list(nm = "character", 
                            dist = "character",
                            params="numeric"),
         prototype = list(nm="a", dist = "unif", c(min=1, max=2)))

#######################################################
# Funksie om die parameter lys te maak

maak.distlist <- function(nm,  dist , params){
  out = new(Class = "distlist", nm = nm, dist=dist, params = params)
  out
}

####################################################
roep.param <- function(l, x = NULL, n = NULL, verbose = FALSE){
  if (class(l) != "distlist") stop("l must be of the class distlist, use maak.distlist() to create a list of parameters")
  if (require(randtoolbox) == FALSE) {
    message("You need randtoolbox, my brother. Don't worry, I will install it")
    install.packages("randtoolbox", dependencies = TRUE)
    if (require(randtoolbox) == TRUE) message("Sien jy, ek het gesê alles gaan regkom") else ("Dit wou nie werk nie, jys op jou eie, my ou")
  }
  # If x is NULL use a sobol object but then n cannot be NULL 
  if (is.null(x)){
    if(is.null(n)){
      stop("n and x cannot both be NULL")
    }
    x = sobol(n)
  }
  if (!is.numeric(x)) stop("x must be  numeric")
  if (any(x < 0)) stop("x must be between 0 and 1. If you don't have x you can set n to a suitably high number, e.g. n=1000")
  if (any(x > 1)) stop("x must be between 0 and 1. If you don't have x you can set n to a suitably high number, e.g. n=1000")
  
  agn = lapply(c("p", names(l@params)), as.name)
  d = paste("q", l@dist, sep="")
  vls = list()
  vls[[1]] = x
  for (i in 1:length(l@params)){
    vls[[i+1]] <- l@params[i]
  }
  names(vls) <- agn
  do.call(d, vls)
}

####################################################

bou.verdeling <- function(x){
  if (require(MASS) == FALSE) {
    message("You need MASS, my brother. Don't worry, I will install it")
    install.packages("MASS", dependencies = TRUE)
    if (require(MASS) == TRUE) message("Sien jy, ek het gesê alles gaan regkom") else ("Dit wou nie werk nie, jys op jou eie, my ou")
  }
}

####################################################
# prmlist <- list(a=1, b=2, c=3, d=4, e=5, f=6, g=7, h=8)
# with(prmlist, a * b %% c - d / e ^ f + g %/% h)

prmlst <- list(maak.distlist(nm = "a", dist = "unif", params = c(min=100, max=200)), 
               maak.distlist(nm = "b", dist = "norm", params = c(mean = 2 , sd = 1)),
               maak.distlist(nm = "c", dist = "weibull", params = c(shape = 2 , scale = 1))
                                  )

monticarleer <- function(prmlst, my.formula = "a * b ^ c", n=100){
  names(prmlst) = sapply(prmlst, function(x) x@nm)
  res = lapply(prmlst, function(x) roep.param(l = x, n=n))
  names(res) = names(prmlst)
  out = with(res, eval(parse(text = my.formula)))
  out
}


