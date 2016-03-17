<<<<<<< HEAD
#' Summarise Sicklist
#' 
#' Script to summarise a sicklist into dataframe for presentation in report
#' 
#' @param sicklist A sicklist
#' @export

summarise.sicklist <- function(sicklist){
  require(plyr)
  df=NULL
  if(all(sapply(sicklist, class)=="data.frame") == FALSE) {sicklist = lapply(sicklist, function(x) as.data.frame(t(unlist(x))))}
  df=do.call("rbind.fill",sicklist)
  df
  
}
=======
# Script to summarise a sicklist into dataframe for presentation in report

>>>>>>> d5ff41ee851c818691bd72758f77a0f8e60d1bdc

#summarise.sicklist <- function(sicklist){
# require(plyr)
# df=NULL
#f or(i in 1:length(sicklist)){
# df=rbind.fill(df,as.data.frame(t(unlist(sicklist[[i]]))))
# message("nommer ", i, "gedoen:  ", names(sicklist[i]))
# df
# }
# df
<<<<<<< HEAD
# }
=======
# }


summarise.sicklist <- function(sicklist){
  require(plyr)
  df=NULL
  if(all(sapply(sicklist, class)=="data.frame") == FALSE) {sicklist = lapply(sicklist, function(x) as.data.frame(t(unlist(x))))}
  df=do.call("rbind.fill",sicklist)
  df
  
}
>>>>>>> d5ff41ee851c818691bd72758f77a0f8e60d1bdc
