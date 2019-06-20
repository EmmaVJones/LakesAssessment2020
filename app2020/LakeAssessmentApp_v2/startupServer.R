source('global.R')


# Draft 2020 data
#conventionals <- read_csv('data/final2020data/CEDSWQM_2020_IR_DATA-CONVENTIONALS_20190305.csv') %>%
#conventionals <- read_csv('data/final2020data/conventionals_final2020_citmonNonAgency.csv') %>%

# I specified column classes this time because weird issues with data missing in fields changed (TP and eColi) happened when 
# saving as .csv and reading in using read_csv, fields turned to logical and data into NA
conventionals <- read_csv('data/final2020data/conventionals_final2020_citmonNonAgency_TP_ECOLIupdates.csv',
                          col_types = list(`PHOSPHORUS_mg/L` = col_double(), 
                                           `RMK_PHOSPHORUS` = col_character(),
                                           `ECOLI_CFU/100mL` = col_double(), 
                                           `ECOLI_RMK` = col_character() )) %>%
  filter(!is.na(Latitude)|!is.na(Longitude)) %>% # remove sites without coordinates
  rename("FDT_TEMP_CELCIUS"  ="TEMPERATURE_00010_DEGREES CENTIGRADE",
         "FDT_TEMP_CELCIUS_RMK" = "FDT_TEMP_CELCIUS_RMK",  
         "FDT_FIELD_PH" = "pH_00400_STD_UNITS" ,          
         "FDT_FIELD_PH_RMK"  ="FDT_FIELD_PH_RMK", 
         "DO" =  "DO_mg/L",       
         "DO_RMK"  ="DO_RMK",    
         "FDT_SPECIFIC_CONDUCTANCE"="SPECIFIC_CONDUCTANCE_00095_UMHOS/CM @ 25C",    
         "FDT_SPECIFIC_CONDUCTANCE_RMK" ="FDT_SPECIFIC_CONDUCTANCE_RMK" ,
         "FDT_SALINITY" = "SALINITY_00480_PPT" ,            
         "FDT_SALINITY_RMK"  ="FDT_SALINITY_RMK",  
         "NITROGEN" = "NITROGEN_mg/L" ,                    
         "AMMONIA" ="AMMONIA_mg/L",
         "PHOSPHORUS" =  "PHOSPHORUS_mg/L",
         "FECAL_COLI" = "FECAL_COLIFORM_31616_NO/100mL" ,
         "E.COLI" = "ECOLI_CFU/100mL",                       
         "ENTEROCOCCI" =  "ENTEROCOCCI_31649_NO/100mL",
         "CHLOROPHYLL" ="CHLOROPHYLL_32211_ug/L",                
         "SSC" ="SSC-TOTAL_00530_mg/L" , 
         "NITRATE" ="NITRATE_mg/L", 
         "CHLORIDE" ="CHLORIDE_mg/L",
         "SULFATE_TOTAL" ="SULFATE_TOTAL_00945_mg/L",              
         "SULFATE_DISS" ="SULFATE_DISSOLVED_00946_mg/L")
###### CHANGED 5/22/19
#conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")


lakeAU <- st_read('GIS/draft2018IR_AUs/va_2018_aus_reservoir.shp')

#lakeStations <- read_csv('processedStationData/draft2020data/lakeStations2020draft.csv')
#lakeStations <- read_csv('processedStationData/final2020data/lakeStations2020_SWRO.csv')
#lakeStations <- read_csv('processedStationData/final2020data/lakeStations2020_BRRO.csv')
