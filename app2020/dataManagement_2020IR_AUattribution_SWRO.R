# compare lake stations from 2020 IR (built off 2018 wqms spatial data) to processed sites from 
# 2018 IR data smashed with AU and WQS information


library(tidyverse)
library(sf)

# draft data
lakeStations2020 <- read_csv('processedStationData/final2020data/RegionalResultsLake_SWRO.csv') %>%
  mutate(ID305B = ID305B_1) # make joining column


# BRRO old data, for double checking
lakeStationsFinal <- readRDS('data/lakeStationsFinal.RDS') 


# First need each AU associated with a sig lake name

# Bring in draft 2018 reservoir spatial file to get DEQ region associated with ID305B
assessmentLayer <- st_read('GIS/newDEQRegions_VA.shp')%>%# use just big region polygons to save memory
  st_transform( st_crs(4326)) # transform to WQS84 for spatial intersection 
lakeAU <- st_read('GIS/draft2018IR_AUs/va_2018_aus_reservoir.shp')%>%
  st_transform( st_crs(4326)) # transform to WQS84 for spatial intersection

lakeAU_region <- st_join(lakeAU, assessmentLayer, join = st_intersects) 
# fix the two lake AU's that fall between regional offices, Lake Anna and Moomaw
lakeAU_region <- #mutate(lakeAU_region, OFFICE_NM = case_when(ID305B == 'VAN-F07L_NAR01A02' ~ 'NRO',
                  #                                           ID305B == 'VAW-I03L_JKS03A02' ~ 'BRRO')) %>%
  distinct(lakeAU_region, ID305B, .keep_all = TRUE) # remove duplicates



lakeStations2020_1 <- left_join(lakeStations2020, dplyr::select(lakeAU_region,ID305B, OFFICE_NM), by='ID305B')

# Now attach Significant Lake status from latest ADB (2016 at present)
# table was exported from VA_ADB_2016_final.mdb, tblAllLakes_2016 table 
allLakes <- readxl::read_excel('data/tblAllLakes_2016.xlsx',sheet='tblAllLakes_2016')

lakeStations2020_2 <- left_join(lakeStations2020_1, allLakes,by = 'ID305B')#'SIGLAKENAME')


# Now get nutrient limits for each lake

# Transform 9VAC25-260-187 Individual lake standards for nutrients into a table
# from https://leg1.state.va.us/cgi-bin/legp504.exe?000+reg+9VAC25-260-187
# I copy/pasted the table into excel and saved as .csv
lakeNutStandards <- read_csv('data/9VAC25-260-187lakeNutrientStandards.csv')%>%
  mutate(SIGLAKENAME=`Man-made Lake or Reservoir Name`)
colnames(lakeNutStandards)[3:4] <- c('Chlorophyll_A_limit','TPhosphorus_limit')


lakeStations <- left_join(lakeStations2020_2,lakeNutStandards,by='SIGLAKENAME')



# find stations that are in the lacustrine zone based on LZ designation in Depth of Martha's station
#lakeStations_3 <- left_join(lakeStations2020_2, dplyr::select(lakeStationsFinal,FDT_STA_ID, Assess_TYPE), by='FDT_STA_ID')
lakeStations_3 <- mutate(lakeStations, Assess_TYPE= 'LZ')


# Make sure we have all the columns we need to go to app
names(lakeStationsFinal) %in% names(lakeStations_3)
names(lakeStationsFinal)[!(names(lakeStationsFinal) %in% names(lakeStations_3))] # i'll make it work without these


# Fix some annoying duplicated field names
lakeStations_4 <- dplyr::select(lakeStations_3, -c(REGION.y, WATER_NAME.y))%>%
  rename('REGION'='REGION.x','WATER_NAME'='WATER_NAME.x') %>%
  dplyr::select(-geometry)


write.csv(lakeStations_4,'processedStationData/final2020data/lakeStations2020_SWRO.csv', row.names=F)
