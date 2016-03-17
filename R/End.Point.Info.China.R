### Health end-point info for use in dose-repsponse functions - China 
## source for mortality rates: STATS SA, 2014

rebase.rr <- function(RR, base.orig = 10) {
  res <- 1 + (RR-1) / base.orig
  return(res)
}

mortality.all.cause.PM10 = list(end.point = "All cause mortality",
                          pollutant = "PM10",
                          PM2.5fraction = 0.5,
                          relationship = "linear",
                          RR = c(rebase.rr(1.03)), # vir 'n inkrement van 1
                          beta = NULL,
                          Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[1:6],
                          Unit.Cost = 17113501,
                          Effect = "Acute",
                          Incidence.rate =  8.6/1000/365 ,
                          Incidence.rate.source = "CIA World fact book and STATS SA") 

mortality.all.cause.SO2 = list(end.point = "All cause mortality",
                           pollutant = "SO2",
                           relationship = "linear",
                           RR = c(rebase.rr(1.04)),
                           beta = NULL,
                           Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[1:6],
                           Unit.Cost = 17113501,
                           Effect = "Acute",
                           Incidence.rate =  8.6/1000/365 ,
                           Incidence.rate.source = "CIA World fact book and STATS SA")

mortality.cardiovascular.diseases.PM10 = list(end.point = "Mortality due to Cardiovascular diseases",
                           pollutant = "PM10",
                           PM2.5fraction = 0.5,
                           relationship = "linear",
                           RR = c(rebase.rr(1.04)),
                           beta = NULL,
                           Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[1:6],
                           Unit.Cost = 17113501,
                           Effect = "Acute",
                           Incidence.rate =  (8.6/1000/365)*0.144 ,
                           Incidence.rate.source = "CIA World fact book and STATS SA")

mortality.cardiovascular.diseases.SO2 = list(end.point = "Mortality due to Cardiovascular diseases",
                                         pollutant = "SO2",
                                         relationship = "linear",
                                         RR = c(rebase.rr(1.04)),
                                         beta = NULL,
                                         Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[1:6],
                                         Unit.Cost = 17113501,
                                         Effect = "Acute",
                                         Incidence.rate =  (8.6/1000/365)*0.144,
                                         Incidence.rate.source = "CIA World fact book and STATS SA")

mortality.respiratory.diseases.PM10 = list(end.point = "Mortality due to respiratory diseases",
                                         pollutant = "PM10",
                                         PM2.5fraction = 0.5,
                                         relationship = "linear",
                                         RR = c(rebase.rr(1.06)),
                                         beta = NULL,
                                         Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[2:6],
                                         Unit.Cost = 17113501,
                                         Effect = "Acute",
                                         Incidence.rate = (8.6/1000/365)*0.127,
                                         Incidence.rate.source = "CIA World fact book and STATS SA")

mortality.respiratory.diseases.SO2 = list(end.point = "Mortality due to respiratory diseases",
                                      pollutant = "SO2",
                                      relationship = "linear",
                                      RR = c(1.10),
                                      beta = NULL,
                                      Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[2:6],
                                      Unit.Cost = 17113501,
                                      Effect = "Acute",
                                      Incidence.rate =  (8.6/1000/365)*0.127,
                                      Incidence.rate.source = "CIA World fact book and STATS SA")

cardiovascular.hospital.admissions.PM10 = list(end.point = "Cardiovascular hospital admissions: all ages",
                         pollutant = "PM10",
                         PM2.5fraction = 0.5,                  
                         relationship = "linear",
                         RR = c(rebase.rr(1.07)),
                         beta = NULL,
                         Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[2:6],
                         Unit.Cost = 4451,
                         Effect = "Acute",
                         Incidence.rate = 723/100000/365,
                         Incidence.rate.source = "CAFE")

cardiovascular.hospital.admissions.SO2 = list(end.point = "Cardiovascular hospital admissions: all ages",
                                          pollutant = "SO2",
                                          relationship = "linear",
                                          RR = c(rebase.rr(1.19)),
                                          beta = NULL,
                                          Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[2:6],
                                          Unit.Cost = 4451,
                                          Effect = "Acute",
                                          Incidence.rate = 723/100000/365,
                                          Incidence.rate.source = "CAFE")

respiratory.hospital.admissions.PM10 = list(end.point = "Respiratory hospital admissions: all ages",
                      pollutant = "PM10",
                      PM2.5fraction = 0.5,                 
                      relationship = "linear",
                      RR = c(rebase.rr(1.12)),
                      beta = NULL,
                      Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[2:6],
                      Unit.Cost = 41577,
                      Effect = "Acute",
                      Incidence.rate = 617/100000/365,
                      Incidence.rate.source = "INTARESE")

respiratory.hospital.admissions.SO2 = list(end.point = "Respiratory hospital admissions: all ages",
                                       pollutant = "SO2",
                                       relationship = "linear",
                                       RR = c(rebase.rr(1.15)),
                                       beta = NULL,
                                       Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[2:6],
                                       Unit.Cost = 41577,
                                       Effect = "Acute",
                                       Incidence.rate = 617/100000/365,
                                       Incidence.rate.source = "INTARESE")

chronic.respiratory.illness.among.adults.PM10 = list(end.point = "Chronic respiratory illness among adults",
                                          pollutant = "PM10",
                                          PM2.5fraction = 0.5,
                                          relationship = "linear",
                                          RR = c(rebase.rr(1.31)),
                                          beta = NULL,
                                          Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[4:5],
                                          Unit.Cost = 35,
                                          Effect = "Chronic",
                                          Incidence.rate = 30/100,
                                          Incidence.rate.source = "CAFE")

chronic.respiratory.illness.among.children.PM10 = list(end.point = "Chronic respiratory illness among children",
                                                 pollutant = "PM10",
                                                 PM2.5fraction = 0.5, 
                                                 relationship = "linear",
                                                 RR = c(rebase.rr(1.44)),
                                                 beta = NULL,
                                                 Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[2:3],
                                                 Unit.Cost = 35,
                                                 Effect = "Chronic",
                                                 Incidence.rate = 15/100,
                                                 Incidence.rate.source = "CAFE")

endlist=list(mortality.all.cause.PM10 = mortality.all.cause.PM10,
             mortality.all.cause.SO2 = mortality.all.cause.SO2,
             mortality.cardiovascular.diseases.PM10 = mortality.cardiovascular.diseases.PM10,
             mortality.cardiovascular.diseases.SO2 = mortality.cardiovascular.diseases.SO2,
             mortality.respiratory.diseases.PM10 = mortality.respiratory.diseases.PM10,
             mortality.respiratory.diseases.SO2 = mortality.respiratory.diseases.SO2,
             cardiovascular.hospital.admissions.PM10 = cardiovascular.hospital.admissions.PM10,
             cardiovascular.hospital.admissions.SO2 = cardiovascular.hospital.admissions.SO2,
             respiratory.hospital.admissions.PM10 = respiratory.hospital.admissions.PM10,
             respiratory.hospital.admissions.SO2 = respiratory.hospital.admissions.SO2,
             chronic.respiratory.illness.among.adults.PM10 = chronic.respiratory.illness.among.adults.PM10,
             chronic.respiratory.illness.among.children.PM10 = chronic.respiratory.illness.among.children.PM10)
# SO2 
# SO2 exposure above 4 pphm (104 mcg/m3), (p = .03), relative risk 1.18 for 500 hr/yr
