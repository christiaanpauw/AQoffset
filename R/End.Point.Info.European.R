### Health end-point info for use in dose-repsponse functions - European 

mortality.all.cause = list(end.point = "All cause mortality in adults 30 years and older",
                          pollutant = "PM2.5",
                          PM2.5fraction = 1,
                          relationship = "linear",
                          RR = c(1.002,1.006,1.011),
                          beta = NULL,
                          Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[5:6],
                          Unit.Cost = 17113501,
                          Effect = "Chronic",
                          Incidence.rate =  17.23/1000 ,
                          Incidence.rate.source = "STATS SA"
                          ) 

WLDs= list(end.point = "Work loss days: 15-64 years",
                          pollutant = "PM2.5",
                          PM2.5fraction = 1,
                          relationship = "linear",
                          RR = c(1.0039,1.0046,1.0053),
                          beta = NULL,
                          Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[4:5],
                          Unit.Cost = 1000,
                          Effect = "Chronic",
                          Incidence.rate =  4.5/1,
                          Incidence.rate.source = "CAFE"
                          )

MRADs = list(end.point = "Minor restricted activity days: 18-64 years",
                                    pollutant = "PM2.5",
                                    PM2.5fraction = 1,
                                    relationship = "linear",
                                    RR = c(1.006,1.0074,1.0088),
                                    beta = NULL,
                                    Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[4:5],
                                    Unit.Cost = 184,
                                    Effect = "Chronic",
                                    Incidence.rate = 7.8/1,
                                    Incidence.rate.source = "CAFE"
                                    )

RADs = list(end.point = "Restricted activity days: 18-64 years",
                                      pollutant = "PM2.5",
                                      PM2.5fraction = 1,
                                      relationship = "linear",
                                      RR = c(1.00417,1.00475,1.00533),
                                      beta = NULL,
                                      Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[4:5],
                                      Unit.Cost = 184,
                                      Effect = "Chronic",
                                      Incidence.rate = 19/1,
                                      Incidence.rate.source = "CAFE"
                                      )

infant.mortality= list(end.point = "Infant mortality: children < 5 years",
                                      pollutant = "PM10",
                                      PM2.5fraction = 0.5,
                                      relationship = "linear",
                                      RR = c(1.002,1.004,1.007),
                                      beta = NULL,
                                      Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[1:2],
                                      Unit.Cost = 17113501,
                                      Effect = "Chronic",
                                      Incidence.rate = 37.9/1000,
                                      Incidence.rate.source = "STATSSA"
                                      )

chronic.bronchitis= list(end.point = "Chronic bronchitis: adults aged 18 years and older",
                       pollutant = "PM10",
                       PM2.5fraction = 0.5,
                       relationship = "linear",
                       RR = c(1.002,1.022,1.038),
                       beta = NULL,
                       Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[4:6],
                       Unit.Cost = 752245,
                       Effect = "Chronic",
                       Incidence.rate = (2.3+2.8)/2/100,
                       Incidence.rate.source = "Ehrlich et al"
                       )

cardiovascular.hospital.admissions = list(end.point = "Cardiovascular hospital admissions: all ages",
                       pollutant = "PM10",
                       PM2.5fraction = 0.5,                  
                       relationship = "linear",
                       RR = c(1.0003,1.0006,1.0009),
                       beta = NULL,
                       Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[2:6],
                       Unit.Cost = 4451,
                       Effect = "Chronic",
                       Incidence.rate = 723/100000,
                       Incidence.rate.source = "CAFE"
                       )

respiratory.hospital.admissions = list(end.point = "Respiratory hospital admissions: all ages",
                      pollutant = "PM10",
                      PM2.5fraction = 0.5,                 
                      relationship = "linear",
                      RR = c(1.0007,1.0009,1.001),
                      beta = NULL,
                      Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[2:6],
                      Unit.Cost = 41577,
                      Effect = "Chronic",
                      Incidence.rate = 617/100000,
                      Incidence.rate.source = "INTARESE"
                       )

asthma.medication.use.children = list(end.point = "Asthma medication use: children aged 5-14",
                                       pollutant = "PM10",
                                       PM2.5fraction = 0.5,
                                       relationship = "linear",
                                       RR = c(0.9983,1.0004,1.0026),
                                       beta = NULL,
                                       Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[3],
                                       Unit.Cost = 109,
                                       Effect = "Chronic",
                                       Incidence.rate = 8.1/100/10,
                                       Incidence.rate.source = "CAFE + MRC "
                                       )


bronchodilator.usage.asmathic.adults = list(end.point = "Bronchodilator usage: adults aged 20 and older with asthma",
                                      pollutant = "PM10",
                                      PM2.5fraction = 0.5,
                                      relationship = "linear",
                                      RR = c(0.9995,1.0005,1.0015),
                                      beta = NULL,
                                      Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[4:6],
                                      Unit.Cost = 116,
                                      Effect = "Chronic",
                                      Incidence.rate = 8.1/100/2  ,
                                      Incidence.rate.source = "CAFE"
                                      )

lower.respiratory.symptoms.among.children = list(end.point = "Lower respiratory symptoms (including cough) among children",
                                            pollutant = "PM10",
                                            PM2.5fraction = 0.5, 
                                            relationship = "linear",
                                            RR = c(1.0017,1.0034,1.0051),
                                            beta = NULL,
                                            Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[3],
                                            Unit.Cost = 35,
                                            Effect = "Chronic",
                                            Incidence.rate = 15/100 ,
                                            Incidence.rate.source = "CAFE"
                                                                 )

lower.respiratory.symptoms.among.adults = list(end.point = "Lower respiratory symptoms (including cough) in symptomatic adults",
                                          pollutant = "PM10",
                                          PM2.5fraction = 0.5,
                                          relationship = "linear",
                                          RR = c(1.0001,1.0012,1.0022),
                                          beta = NULL,
                                          Xpop = c("Xinfants","Xbabies","Xkids","Xteens","Xadults","Xaged")[5],
                                          Unit.Cost = 35,
                                          Effect = "Chronic",
                                          Incidence.rate = 30/100 ,
                                          Incidence.rate.source = "CAFE"
                                                                 )

endlist=list(mortality.all.cause=mortality.all.cause,
             WLDs=WLDs,
             MRADs=MRADs,
             RADs=RADs,
             infant.mortality=infant.mortality,
             chronic.bronchitis=chronic.bronchitis,
             cardiovascular.hospital.admissions=cardiovascular.hospital.admissions,
             respiratory.hospital.admissions=respiratory.hospital.admissions,
             asthma.medication.use.children=asthma.medication.use.children,
             bronchodilator.usage.asmathic.adults=bronchodilator.usage.asmathic.adults,
             lower.respiratory.symptoms.among.children=lower.respiratory.symptoms.among.children,
             lower.respiratory.symptoms.among.adults=lower.respiratory.symptoms.among.adults
             )
# SO2 
# SO2 exposure above 4 pphm (104 mcg/m3), (p = .03), relative risk 1.18 for 500 hr/yr
