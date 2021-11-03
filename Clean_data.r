# The goal of this script is to gather data selected by the user, unzip it, convert it into csv format and aggregate only columns of interest.



library(haven)
setwd('C:\\Users\\murad\\Documents\\DHS_Egypt_Surveys')

zips <- list.files(path='put_zips_here', pattern='*.zip', full.names=T, recursive=F, ignore.case = T)
unzips <- list.files(path ='unzips_here', pattern = '*dta', full.names=T, recursive=F, ignore.case=T)

outpath1='unzips_here'

l_zips <- length(zips)
l_unzips <- length(unzips)

if (l_zips != l_unzips) {
  
  mapply(unzip, zipfile = zips, exdir=outpath1)
  
} else {
  
  print('nothing to unzip')
}



  # this is for dataset type matching
  AN <- str('AN') #Antenatal Care observation and exit data related to a specific provider
  AT <- str('AT') #ART - Antiretroviral data related to Outpatient, Inpatient or Pharmacy units
  CL <- str('CL') #Unit Check List
  CN <- str('CN') #Consultations
  CO <- str('CO') #Community
  CS <- str('CS') #Country Specific Module
  CT <- str('CT') #VCT HIV Counseling & Testing data related to Outpatient, Inpatient, Laboratory or Pharmacy units
  FC <- str('FC') #Facility inventory and MCH data
  FP <- str('FP') #Family Planning observation and exit data related to a specific provider
  IN <- str('IN') #Safe Injection
  IP <- str('IP') #Inpatient Unit
  LB <- str('LB') #Laboratory
  LD <- str('LD') #Labor Delivery
  MS <- str('MS') #Health Information System
  OI <- str('OI') #Outpatient/inpatient
  OP <- str('OP') #Outpatient Unit
  PH <- str('PH') #Pharmacy
  PI <- str('PI') #Personal Interview
  PM <- str('PM') #PMTCT - data related to Outpatient and Inpatient units
  PV <- str('PV') #Provider
  SC <- str('SC') #Sick Child observation and exit data related to a specific provider
  SI <- str('SI') #Sexually Transmitted Infections observation and exit data related to a specific provider
  SL <- str('SL') #Staff/Provider Listing
  TB <- str('TB') #TB data related to Outpatient and Inpatient units
  
pattern <- c(TB,LD,MS) #Modify this to determine what dataset types you are interested in                               !!!NOT DONE, FIX!!!
  
dta_files<- list.files(path=outpath1, pattern='*.dta', full.names=T, recursive=F, ignore.case=T)
length(dta_files)
  
id123 <- list.files(path=outpath1, pattern="FP", full.names=T, recursive=F, ignore.case=T)
length(important_data)
  
dta_to_csv <-function(x){
    read_data <- read_dta(x) #Load dta file
    tables <- write.table(read_data, row.names = F) #convert to matrix format
    write.csv(tables, 'csvs_here') #save as csv file in new folder
}



apply(dta_files, dta_to_csv)



print('lol it finished')

