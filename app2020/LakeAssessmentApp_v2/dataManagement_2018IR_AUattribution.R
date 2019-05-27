# This script will organize data in concise manner to attribute lat/lng to AU and appropriate lake name
#  and additional information
library(tidyverse)
library(rgdal)


# data from 2016 IR, there are 255 AU's in the 2016 IR, one additional from 2014
# table was exported from VA_ADB_2016_final.mdb, tblAllLakes_2016 table 
allLakes <- readxl::read_excel('data/tblAllLakes_2016.xlsx',sheet='tblAllLakes_2016')
unique(allLakes$WATER_NAME)
unique(allLakes$ID305B)

filter(lakeStationIDs,ID305B_1 != ID305B_2)


# 2014 WQMS GIS data brought in from U:/305b2014/GIS_Layers_2014/Monitoring_Stations/Stations_2014
#wqms_2014 <- readOGR('C:/GIS/EmmaGIS/Assessment/MonitoringStations','Stations_2014')@data
# Can't bring in 2014 bc the U drive version is only WCRO

# 2016 WQMS GIS data brought in from U:\305b2016\GIS\2016_wqms
# Local copies stored in C:\GIS\EmmaGIS\Assessment\MonitoringStations
#wqms_2016 <- readOGR('C:/GIS/EmmaGIS/Assessment/MonitoringStations/2016IR','2016_wqms')@data # nonFinal version

# Final version from X:\2016_Assessment\GIS_2016\MonitoringStations copied to a local location
wqms_2016 <- readOGR('C:/GIS/EmmaGIS/Assessment/MonitoringStations/2016IR','va_16ir_wqm_stations')@data #final version


# Connect allLakes ID305b to wqms_2016 to get StationIDs sampled in each Lake
lakeStationIDs <- filter(wqms_2016,ID305B_1 %in% unique(allLakes$ID305B))
lakeStationIDs2 <- filter(wqms_2016,ID305B_1 %in% unique(allLakes$ID305B) |
                           ID305B_2 %in% unique(allLakes$ID305B) |
                           ID305B_3 %in% unique(allLakes$ID305B) ) 
subset(data.frame(x=lakeStationIDs2$ID305B_1 %in% lakeStationIDs$ID305B_1),x==FALSE)
# There are 3 stations that ID305B_1 doesn't capture, row 150,182,199

# Make a column to join on ID305B, making the three needed corrections
lakeStationIDs2 <- dplyr::select(lakeStationIDs2,STATION_ID:VAHU6) %>%
  dplyr::mutate(ID305B=ID305B_1)
lakeStationIDs2[150,]$ID305B <- lakeStationIDs2$ID305B_3[150] #VAC-L73L_DAN07A04
lakeStationIDs2[182,]$ID305B <- lakeStationIDs2$ID305B_3[182] #VAW-L10L_BWR03A10
lakeStationIDs2[199,]$ID305B <- lakeStationIDs2$ID305B_2[199] #VAC-L79L_ROA07A98


lakeStations <- plyr::join(lakeStationIDs2,allLakes,by="ID305B")  
lakeStations <- lakeStations[,-15] # get rid of duplicate REGION column, will mess up dplyr functions



# Bring in Roger's conventionals 
conventionals <- read_csv('data/CONVENTIONALS_20171010.csv')
conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%y %H:%M")


lakeStationIDdata <- filter(conventionals,FDT_STA_ID %in% unique(lakeStations$STATION_ID))
length(unique(lakeStationIDdata$FDT_STA_ID))



# Transform 9VAC25-260-187 Individual lake standards for nutrients into a table
# from https://leg1.state.va.us/cgi-bin/legp504.exe?000+reg+9VAC25-260-187
# I copy/pasted the table into excel and saved as .csv
lakeNutStandards <- read_csv('data/9VAC25-260-187lakeNutrientStandards.csv')%>%
  mutate(SIGLAKENAME=`Man-made Lake or Reservoir Name`)
colnames(lakeNutStandards)[3:4] <- c('Chlorophyll_A_limit','TPhosphorus_limit')


lakeStations <- left_join(lakeStations,lakeNutStandards,by='SIGLAKENAME')

# Some Lake names don't match between two dataframes so fix them manually after next step


# Find StationID's associated with Lacustrine zone
# this is only for WCRO at present. I need a full list of stations statewide with this designation
# Data from U:\305b2016\305bSTAMASTER\305b.mdb, MONITOR table, exported to /data folder as excel table
# This version was last updated 11/27/2017, but lake stations do not change nor are there recent
# lake station additions, so this should be comprehensive for WCRO
monitor <- readxl::read_excel('data/MONITOR.xlsx', sheet='MONITOR')%>%
  dplyr::select(STATION,Assess_TYPE)%>%
  filter(Assess_TYPE=='LZ') %>%# just get lacustrine stations
  mutate(STATION_ID=STATION) %>%dplyr::select(-c(STATION))
lakeStations <- left_join(lakeStations,monitor,by='STATION_ID')
lakeStations$STATION_ID <- as.factor(lakeStations$STATION_ID)

#saveRDS(lakeStations,'data/lakeStations.RDS')

# Afterwards, I went in manually and populated Amanda's LZ stations by looking at her 
#   assessment determinations (if there was a nutrient assessment I called it LZ)

lakeStations <- readRDS('data/lakeStations.RDS')#%>%
 # mutate(FDT_STA_ID=STATION_ID)

fixMe <- unique(filter(lakeStations,Assess_TYPE =='LZ' & is.na(TPhosphorus_limit))$WATER_NAME)
# go in one at a time and fix in excel
write.csv(lakeStations,'data/lakeStations.csv',row.names=F)
# bring in fixed version
lakeStations <- read.csv('data/lakeStations.csv')

saveRDS(lakeStations,'data/lakeStationsFinal.RDS')






# Organize secchi data
# have to do this step because Roger's conventionals.xlsx doesn't include secchi info which
# is necessary for TSI calculations
BRRO <- readxl::read_excel('data/secchi/BlueRidge_Sechi.xlsx')
NRO <- readxl::read_excel('data/secchi/Northern_Sechi.xlsx')
PRO <- readxl::read_excel('data/secchi/Piedmont_Sechi.xlsx')
SWRO <- readxl::read_excel('data/secchi/Southwest_Sechi.xlsx')
TRO <- readxl::read_excel('data/secchi/Tidewater_Sechi.xlsx')
VRO <- readxl::read_excel('data/secchi/Valley_Sechi.xlsx')
  
secchi <- rbind(BRRO,NRO,PRO,SWRO,TRO,VRO)%>%
  dplyr::rename(FDT_STA_ID=`Station ID`,FDT_DATE_TIME=`Date Time`) # to make joining with conventionals easier
write.csv(secchi,'data/secchi_2018IR.csv',row.names = F)






##### ADD ROARING FORK RESERVOIR DATA TO CONVENTIONALS
# it was filtered out in 2018 data pull bc it had level3 code MUN
# I grabbed 2016 conventionals pull and added it to 2018 bc no additional data taken after 2012 (that fell in 2018 window)
library(lubridate)

# Bring in Roger's conventionals 
conventionals <- read_csv('data/CONVENTIONALS_20171010_updated.csv')
conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%y %H:%M")
# read in local copy of X:/2016_Assessment/Monitoring Data/2016_cedswqm_data/CONVENTIONALS.xlsx
c2016 <- readxl::read_excel('C:/HardDriveBackup/IR/IR2016/CONVENTIONALS.xlsx',sheet='CONVENTIONALS')  
rfk <- filter(c2016, FDT_STA_ID =='4ARFK000.20') %>% 
  mutate(STA_LV1_CODE='RESERV',STA_LV3_CODE='MUN',Deq_Region = 'SCRO',STA_DESC ='ROARING FK RESERV AT DAM',
         FDT_COMMENT=NA,DO=FDT_DO_PROBE,DO_RMK = FDT_DO_PROBE_RMK,FDT_SPECIFIC_CONDUCTANCE=NA, FDT_SPECIFIC_CONDUCTANCE_RMK=NA,
         AMMONIA = NA,RMK_AMMONIA=NA,NH3_DISS=NA, RMK_00608=NA, NH3_TOTAL=NA, RMK_00610=NA,SULFATE_TOTAL =NA, RMK_00945=NA, 
         SULFATE_DISS=NA, RMK_00946=NA, STA_LV2_CODE ='AMBNT',Latitude=LATITUDE, Longitude=LONGITUDE,FDT_SALINITY_RMK =NA,
         Majorbasincode=NA, Majorbasinname=NA, Basin=NA, Subbasin=NA, Huc6_Huc_8=NA, 
         Huc6_Huc_8_Name=NA, Huc6_Name=NA, Huc6_Huc_12=NA, Huc6_Huc_12_Name=NA, Huc6_Vahu5=NA, Huc6_Vahu6=NA, STA_CBP_NAME=NA) %>%
  dplyr::select(-c(FDT_CAG_CODE,FDT_DO_PROBE, FDT_DO_PROBE_RMK,LATITUDE,LONGITUDE)) %>% 
  dplyr::select(FDT_STA_ID, STA_LV3_CODE, STA_LV1_CODE, STA_REC_CODE, Deq_Region, STA_DESC,
                FDT_SSC_CODE, FDT_SPG_CODE, FDT_DATE_TIME, FDT_DEPTH, FDT_DEPTH_DESC, FDT_PERCENT_FRB, 
                FDT_COMMENT, FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK, FDT_FIELD_PH, FDT_FIELD_PH_RMK, DO, 
                DO_RMK, FDT_SPECIFIC_CONDUCTANCE, FDT_SPECIFIC_CONDUCTANCE_RMK, FDT_SALINITY,FDT_SALINITY_RMK, NITROGEN,RMK_00600,AMMONIA,
                RMK_AMMONIA,NH3_DISS, RMK_00608, NH3_TOTAL, RMK_00610, PHOSPHORUS, RMK_00665, FECAL_COLI, 
                RMK_31616, E.COLI, RMK_ECOLI, ENTEROCOCCI, RMK_31649, CHLOROPHYLL, RMK_32211, SSC, SSC_RMK, 
                NITRATE, RMK_00620, CHLORIDE, RMK_00940, SULFATE_TOTAL, RMK_00945, SULFATE_DISS, RMK_00946, 
                STA_LV2_CODE, Latitude, Longitude, Majorbasincode, Majorbasinname, Basin, Subbasin, Huc6_Huc_8, 
                Huc6_Huc_8_Name, Huc6_Name, Huc6_Huc_12, Huc6_Huc_12_Name, Huc6_Vahu5, Huc6_Vahu6, STA_CBP_NAME) %>%
  mutate(month=month(FDT_DATE_TIME),day=day(FDT_DATE_TIME),year=year(FDT_DATE_TIME),
         time=substring(gsub('.* ([0-9]+)', '\\1' , as.character(FDT_DATE_TIME)),1,5),
         FDT_DATE_TIME3 = paste(month,'/',day,'/',substring(year,3,4),' ',time,sep='')) %>%
  mutate(FDT_DATE_TIME=FDT_DATE_TIME3)%>%
  dplyr::select(-c(FDT_DATE_TIME3,month,day,year,time))
rfk$FDT_DATE_TIME2 <- as.POSIXct(as.character(rfk$FDT_DATE_TIME), format="%m/%d/%y %H:%M",tz='UTC')


conventionals2 <- rbind(conventionals,rfk)
conventionals2$FDT_PERCENT_FRB <- as.integer(as.character(conventionals2$FDT_PERCENT_FRB))


str(conventionals[60:67])
str(conventionals2[60:67])


write.csv(conventionals2,'data/conventionals08152018EVJ.csv',row.names = F)

# Add in Citizen data

# Bring in Roger's conventionals 
conventionals <- read_csv('data/CONVENTIONALS_20171010.csv')
conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%y %H:%M")

lakeStations <- readRDS('data/lakeStations.RDS')%>%
  mutate(FDT_STA_ID=STATION_ID)


# Bring in Citizen data, Paula organized it to match up with Roger's data format
cit1 <- read_csv('data/Citmon_New_Reorg_1.csv') %>%
  dplyr::select(-c(X1))
cit2 <- read_csv('data/Citmon_New_Reorg_2.csv') %>%
  dplyr::select(-c(X1))
cit <- rbind(cit1,cit2)
cit$FDT_DATE_TIME2 <- as.POSIXct(cit$FDT_DATE_TIME, format="%m/%d/%Y")

# For lake app, get rid of non level 3 citizen data
# unforturnately, this is best done by hand.
write.csv(cit,'data/citdata.csv')



# Bring back in the clean dataset (no level 1 or 2 data included)
cit3 <- read.csv('data/citdata_EVJclean.csv')

# Ferrum College doesnt report time with measures, fix so can have same datetime format for all data
cit3$FDT_DATE_TIME <- as.character(cit3$FDT_DATE_TIME)

cit3$FDT_DATE_TIME2 <- as.POSIXct(as.character(cit3$FDT_DATE_TIME), format="%m/%d/%y %H:%M")

names(cit3)[c(66,63,61,60,56)] <- c("STA_CBP_NAME","Huc6_Huc_12_Name","Huc6_Name","Huc6_Huc_8_Name","Majorbasinname"  ) # fix where arcGIS cut off full column names


# Figure out which AU citizen stations fall into to add to lakeStations
# 2016 Final AU's from X:\2016_Assessment\GIS_2016\StatewideAUs I copied locally, imported into GIS, and exported just the va_2016_aus_reservoir file
library(rgdal)
lakeAU <- readOGR('data','va_2016_aus_reservoir_prjWGS84')
lakeAU@proj4string
cit_shp <- cit# just use citizen sites, not full conventionals2 dataset
coordinates(cit_shp) <- ~Longitude+Latitude
proj4string(cit_shp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")   # WGS84 to start
# double check everything will work
identicalCRS(lakeAU,cit_shp)

cit_shp@data$ID305B_1 <- NA

for(i in 1:nrow(cit_shp@data)){
  z <- lakeAU[cit_shp[i,],]
  if(nrow(z@data) < 1 | nrow(z@data) > 1 ){
    cit_shp@data$ID305B_1[i] <- NA
  }else{
    cit_shp@data$ID305B_1[i] <- as.character(z@data$ID305B)
  }
}

# Check the broken ones in GIS
writeOGR(obj=cit_shp, dsn="data", layer="kindafixedcitdata", driver="ESRI Shapefile")

# see if paula's QA helped?

me <- readOGR('data','kindafixedcitdata')
paula <- readOGR('data/cit','kindafixedcitdata')


me@data %>%
  mutate_all(as.character)==paula@data%>%
  mutate_all(as.character)

as.character(me@data$ID305) == as.character(paula@data$ID305)


# nope. same old shit as waht I came up with
rm(paula)
rm(me)


# Figure out which AU citizen stations fall into to add to lakeStations
# 2016 Final AU's from X:\2016_Assessment\GIS_2016\StatewideAUs I copied locally, imported into GIS, and exported just the va_2016_aus_reservoir file
library(rgdal)
lakeAU <- readOGR('data','va_2016_aus_reservoir_prjWGS84')
lakeAU@proj4string
# don't lose lat/long data
cit3 <- mutate(cit3,lat=Latitude,long=Longitude)
cit_shp <- cit3# just use citizen sites, not full conventionals2 dataset
coordinates(cit_shp) <- ~long+lat
proj4string(cit_shp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")   # WGS84 to start
# double check everything will work
identicalCRS(lakeAU,cit_shp)

cit_shp@data$ID305B_1 <- NA

for(i in 1:nrow(cit_shp@data)){
  z <- lakeAU[cit_shp[i,],]
  if(nrow(z@data) < 1 | nrow(z@data) > 1 ){
    cit_shp@data$ID305B_1[i] <- NA
  }else{
    cit_shp@data$ID305B_1[i] <- as.character(z@data$ID305B)
  }
}



# Filter out stations taht don't match up with a Lake AU bc that means they aren't in fact in a lake
citCorrect <- cit_shp[!is.na(cit_shp@data$ID305B_1),]
cit4 <- filter(cit3,FDT_STA_ID %in% citCorrect@data$FDT_STA_ID)






# Add citizen lake stations to lakeStations.RDS
citFinal <- dplyr::select(citCorrect@data,FDT_STA_ID,ID305B_1,Latitude,Longitude) %>%
  dplyr::rename(STATION_ID=FDT_STA_ID,DD_LAT=Latitude,DD_LONG=Longitude)
# Map column names
n1 <- unique(names(lakeStations))
n2 <- unique(names(citFinal))
needToAdd <- n1[!(n1 %in% n2)]

citFinal[needToAdd] <- NA
# Double check everything in both
names(citFinal) %in% names(lakeStations)

citFinal <- dplyr::select(citFinal,STATION_ID, ID305B_1, ID305B_2, ID305B_3, DEPTH, REGION, STATYPE1, STATYPE2, 
                   STATYPE3, DD_LAT, DD_LONG, WATERSHED, VAHU6, ID305B, WATER_NAME, dswc, CATEGORY_ID, TROPHIC_STATUS, 
                   PUBLIC_LAKE, SEC187, SIG_LAKE, USE, OWNER, SIGLAKENAME, CYCLE, `Man-made Lake or Reservoir Name`, Location, 
                   Chlorophyll_A_limit, TPhosphorus_limit, Assess_TYPE, FDT_STA_ID)

names(citFinal) == names(lakeStations)

# Just one record per station
citFinal1 <- citFinal[!duplicated(citFinal[,c('STATION_ID')]),] %>%
  mutate(FDT_STA_ID = STATION_ID, ID305B = ID305B_1)

# Fix lake name
citFinal1$SIGLAKENAME[1] <- 'Smith Mountain Lake'
citFinal1$SIGLAKENAME[2] <- 'Smith Mountain Lake'
citFinal1$SIGLAKENAME[3] <- 'Smith Mountain Lake'
citFinal1$SIGLAKENAME[4] <- 'Smith Mountain Lake'
citFinal1$SIGLAKENAME[5] <- 'Leesville Reservoir'
citFinal1$SIGLAKENAME[6] <- 'Leesville Reservoir'




# finally combine and save
lakeStationsWithCit <- rbind(lakeStations,citFinal1)

saveRDS(lakeStationsWithCit,'data/lakeStationsWithCit.RDS')


# smash them together
cit5 <- dplyr::select(cit4,-c(lat,long))
cit5$FDT_DATE_TIME <- as.character(cit5$FDT_DATE_TIME)


conventionals2 <- rbind(conventionals,cit5)

str(conventionals[1:10])
str(conventionals2[1:10])

# fix data formats for specific columns
#conventionals2$FDT_DATE_TIME2 <- as.POSIXct(conventionals2$FDT_DATE_TIME, format="%m/%d/%y %H:%M")
conventionals2$FDT_DEPTH <- as.numeric(conventionals2$FDT_DEPTH)
conventionals2$FDT_PERCENT_FRB <- as.integer(conventionals2$FDT_PERCENT_FRB)
conventionals2$FDT_SPECIFIC_CONDUCTANCE <- as.numeric(conventionals2$FDT_SPECIFIC_CONDUCTANCE)
conventionals2$FDT_SALINITY <- as.numeric(conventionals2$FDT_SALINITY)
conventionals2$NITROGEN <- as.numeric(conventionals2$NITROGEN)
conventionals2$AMMONIA <- as.numeric(conventionals2$AMMONIA)
conventionals2$NH3_DISS <- as.numeric(conventionals2$NH3_DISS)
conventionals2$NH3_TOTAL <- as.numeric(conventionals2$NH3_TOTAL)
conventionals2$PHOSPHORUS <- as.integer(conventionals2$PHOSPHORUS)
conventionals2$FECAL_COLI <- as.integer(conventionals2$FECAL_COLI)
conventionals2$E.COLI <- as.numeric(conventionals2$E.COLI)
conventionals2$ENTEROCOCCI <- as.numeric(conventionals2$ENTEROCOCCI)
conventionals2$CHLOROPHYLL <- as.numeric(conventionals2$CHLOROPHYLL)
conventionals2$SSC <- as.numeric(conventionals2$SSC)
conventionals2$NITRATE <- as.numeric(conventionals2$NITRATE)
conventionals2$CHLORIDE <- as.numeric(conventionals2$CHLORIDE)
conventionals2$SULFATE_TOTAL <- as.numeric(conventionals2$SULFATE_TOTAL)
conventionals2$SULFATE_DISS <- as.numeric(conventionals2$SULFATE_DISS)

write.csv(conventionals2,'data/conventionalsWITHCITIZEN.csv',row.names = F)
