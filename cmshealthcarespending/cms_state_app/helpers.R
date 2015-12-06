library(googleVis)
require(datasets)
#library(dplyr)

#setwd("/Users/joshkelsey/desktop/NYCDSA/Project2/cms_state_app/data")
#library(dplyr)
#ip <-read.csv("Medicare_Charge_Inpatient_DRG100_DRG_Summary_by_DRGState_FY2013.csv", sep=",", fill=TRUE, header=TRUE)

#average_covered_charges_ip <- select(ip, DRG.Definition, Average.Covered.Charges) %>% 
#  rename(., value = Average.Covered.Charges) 

#average_total_payments_ip <- select(ip, DRG.Definition, Average.Total.Payments) %>% 
#  rename(., value = Average.Total.Payments)

#average_medicare_payments_ip <- select(ip, DRG.Definition, Average.Medicare.Payments) %>% 
#  rename(., value = Average.Medicare.Payments)


#cost_util_map <- function(var) { 
#mapdata <- ip[ which(ip$DRG.Definition=='176 - PULMONARY EMBOLISM W/O MCC'), ]
#GeoStates <- gvisGeoChart(mapdata, "Provider.State", var,
#                          options=list(region="US", 
#                                       displayMode="regions", 
#                                       resolution="provinces",
#                                       width=600, height=400))

#plot(GeoStates)
#}

#mapdata <- ip[ which(average_medicare_payments_ip$DRG.Definition=='176 - PULMONARY EMBOLISM W/O MCC'), ]
#GeoStates <- gvisGeoChart(mapdata, "Provider.State", "Average.Medicare.Payments",
#                          options=list(region="US", 
#                                       displayMode="regions", 
#                                       resolution="provinces",
#                                       width=600, height=400))

#plot(GeoStates)