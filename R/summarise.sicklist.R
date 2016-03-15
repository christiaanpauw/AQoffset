# Script to summarise a sicklist into dataframe for presentation in report


#summarise.sicklist <- function(sicklist){
# require(plyr)
# df=NULL
#f or(i in 1:length(sicklist)){
# df=rbind.fill(df,as.data.frame(t(unlist(sicklist[[i]]))))
# message("nommer ", i, "gedoen:  ", names(sicklist[i]))
# df
# }
# df
# }


summarise.sicklist <- function(sicklist){
  require(plyr)
  df=NULL
  if(all(sapply(sicklist, class)=="data.frame") == FALSE) {sicklist = lapply(sicklist, function(x) as.data.frame(t(unlist(x))))}
  df=do.call("rbind.fill",sicklist)
  df
  
}