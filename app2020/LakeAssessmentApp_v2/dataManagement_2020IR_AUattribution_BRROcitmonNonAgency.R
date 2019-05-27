# compare lake stations from 2020 IR (built off 2018 wqms spatial data) to processed sites from 
# 2018 IR data smashed with AU and WQS information


library(tidyverse)
library(sf)

# draft data
lakeStations2020 <- read_csv('processedStationData/final2020data/RegionalResultsLake_BRROCitMonNonAgencyFINAL.csv') %>%
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

lakeStations2020_2 <- left_join(lakeStations2020_1, allLakes,by = 'ID305B')

# fix lakes that did not join by ID305B
lakeStations2020_3 <- filter(lakeStations2020_2, is.na(SIGLAKENAME)) %>%
  dplyr::select(-c(REGION.y:CYCLE)) %>%
  mutate(ID305B_new = gsub('VAW', "VAC",ID305B)) %>% # VAC to VAW could be part of the problem
  left_join(mutate(allLakes, ID305B_new = ID305B), by='ID305B_new')

lakeStations2020_4 <- filter(lakeStations2020_3, is.na(SIGLAKENAME))

lakeStations2020_3 <- filter(lakeStations2020_3, !is.na(SIGLAKENAME)) %>%
  rename('ID305B'='ID305B.x', 'REGION.y'='REGION',"WATER_NAME.y" ="WATER_NAME" ) %>% 
  dplyr::select(-c(ID305B_new, ID305B.y))  

# fix last problem sites, join by GNIS_Name
lakeStations2020_5 <- dplyr::select(lakeStations2020_4,-c(ID305B_new:CYCLE)) %>%
  mutate(SIGLAKENAME=GNIS_Name) %>%
  mutate(SIGLAKENAME = recode(SIGLAKENAME, "Lake Gaston" ="Lake Gaston (Va.'s portion)" ,
                              "Leesville Lake" ="Leesville Reservoir")) %>%
  left_join(allLakes %>% distinct(SIGLAKENAME, .keep_all = T), by='SIGLAKENAME') %>%
  dplyr::select(-ID305B) %>%
  rename('ID305B'="ID305B.x", 'REGION.y'='REGION','WATER_NAME.y'='WATER_NAME') %>%
  dplyr::select(names(lakeStations2020_2))

lakeStations2020_fin <- filter(lakeStations2020_2, !(FDT_STA_ID %in% lakeStations2020_3$FDT_STA_ID)) %>%
  filter( !(FDT_STA_ID %in% lakeStations2020_5$FDT_STA_ID) ) %>%
  bind_rows(lakeStations2020_3, lakeStations2020_5)



nrow(lakeStations2020) == nrow(lakeStations2020_fin)




# Now get nutrient limits for each lake

# Transform 9VAC25-260-187 Individual lake standards for nutrients into a table
# from https://leg1.state.va.us/cgi-bin/legp504.exe?000+reg+9VAC25-260-187
# I copy/pasted the table into excel and saved as .csv
lakeNutStandards <- read_csv('data/9VAC25-260-187lakeNutrientStandards.csv')%>%
  mutate(SIGLAKENAME=`Man-made Lake or Reservoir Name`)
colnames(lakeNutStandards)[3:4] <- c('Chlorophyll_A_limit','TPhosphorus_limit')

# easy way, nothing is easy though
#lakeStations <- left_join(lakeStations2020_5,lakeNutStandards,by='SIGLAKENAME')
lakeStations <- mutate(lakeStations2020_fin, 
                       SIGLAKENAME = recode(SIGLAKENAME,
                                            "Lake Gaston (Va.'s portion)"='Lake Gaston, (Virginia portion)' ,
                                            "Martinsville (Beaver Creek) Reservoir" = "Martinsville Reservoir (Beaver Creek Reservoir)",
                                            #"Banister Lake" = , # banister lake isn't sig lake so no nut standards
                                            "Kerr Reservoir (Va.'s portion)" = "Kerr Reservoir, Virginia portion (Buggs Island Lake)",
                                            'Roaring Fork Reservoir' = "Roaring Fork" )) %>%
  left_join(lakeNutStandards,by='SIGLAKENAME')

#View(filter(lakeStations, is.na(TPhosphorus_limit))) # Timber Lake is not a 187 lake so that's cool

# LZ designation if found elsewhere
lakeStations_2 <- left_join(lakeStations, dplyr::select(lakeStationsFinal,FDT_STA_ID, Assess_TYPE), by='FDT_STA_ID')
# find stations that are in the lacustrine zone based on LZ designation in Depth of Martha's station
#lakeStations_2 <- mutate(lakeStations, Assess_TYPE= 'LZ')


# Make sure we have all the columns we need to go to app
names(lakeStationsFinal) %in% names(lakeStations_2)
names(lakeStationsFinal)[!(names(lakeStationsFinal) %in% names(lakeStations_2))] # i'll make it work without these


# Fix some annoying duplicated field names
lakeStations_3 <- dplyr::select(lakeStations_2, -c(REGION.y, WATER_NAME.y))%>%
  rename('REGION'='REGION.x','WATER_NAME'='WATER_NAME.x') %>%
  dplyr::select(-geometry)

write.csv(lakeStations_3,'processedStationData/final2020data/lakeStations2020_BRRO_citmonNonAgency.csv', row.names=F)
#write.csv(lakeStations_3,'processedStationData/final2020data/lakeStations2020_SWRO.csv', row.names=F)
