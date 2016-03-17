kwa_populasie <- function(){
households <- rasteriseCensus(KWA, ref = hh_24, refres = c(nrow(hh_24), ncol(hh_24)))
total_households <- calc(households, sum, na.rm = TRUE)
names(total_households) <- "Total_households"
total_households[total_households == 0] <- NA
people <- total_households*3.7
people[people == 0] <- NA
names(people) <- "total"
}