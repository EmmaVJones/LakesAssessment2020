lake_filter <- filter(lakeStations, SIGLAKENAME == 'Carvin Cove Reservoir')#'Lake Moomaw')# 'Claytor Lake')


conventionals_Lake <- filter(conventionals, FDT_STA_ID %in% unique(lake_filter$FDT_STA_ID)) %>%
  left_join(dplyr::select(lakeStations, FDT_STA_ID, SEC, CLASS, SPSTDS,PWS, ID305B_1, ID305B_2, ID305B_3,
                          STATION_TYPE_1, STATION_TYPE_2, STATION_TYPE_3, ID305B, SEC187, SIG_LAKE, USE,
                          SIGLAKENAME, Chlorophyll_A_limit, TPhosphorus_limit, Assess_TYPE), by='FDT_STA_ID')
unique(conventionals_Lake$ID305B)

AUData <- filter(conventionals_Lake, ID305B_1 %in% "VAW-I03L_JKS01A02" |#"VAW-I03L_JKS02A02" "VAW-I03L_JKS03A02"
                   ID305B_2 %in% "VAW-I03L_JKS01A02"  | 
                   ID305B_2 %in% "VAW-I03L_JKS01A02" ) %>% 
  left_join(WQSvalues, by = 'CLASS') 

AUData <- filter(conventionals_Lake, ID305B_1 %in% "VAW-N16L_NEW01A02" | #"VAW-N16L_NEW01A02" "VAW-N16L_NEW01B14" "VAW-N17L_PKC01A10" "VAW-N17L_PKC02A10"
           ID305B_2 %in% "VAW-N16L_NEW01A02" | 
           ID305B_2 %in% "VAW-N16L_NEW01A02") %>% 
    left_join(WQSvalues, by = 'CLASS') 

AUData <- filter(conventionals_Lake, ID305B_1 %in% "VAW-L05L_CRV01A02" | 
                   ID305B_2 %in% "VAW-L05L_CRV01A02" | 
                   ID305B_2 %in% "VAW-L05L_CRV01A02") %>% 
  left_join(WQSvalues, by = 'CLASS') 


# Create Data frame with all data within ID305B and stratification information
# Pool thermocline data to get 1 sample per day, not 2 with top/bottom time difference
#stationDataDailySample <- reactive({
#  req(AUData())
dat <- AUData
dat$FDT_DATE_TIME <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")

dat <- mutate(dat, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
  filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
thermo <- stratifiedLake(dat)
thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
stationDataDailySample <- plyr::join(dat,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
  mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))



stationData <- filter(stationDataDailySample, FDT_STA_ID %in% "9-NEW087.14") #"9-NEW087.14" "9-NEW089.34"
stationData <- filter(stationDataDailySample, FDT_STA_ID %in% "2-JKS044.60")# "2-JKS046.40"
stationData <- stationDataDailySample







x <- stationData


StationTableResults <- cbind(StationTableStartingData(x), tempExceedances(x),DOExceedances_Min(x),pHExceedances(x),
                             bacteriaExceedances_OLD(bacteria_Assessment_OLD(x, 'E.COLI', 126, 235),'E.COLI') %>% 
                               dplyr::rename('ECOLI_VIO_OLD' = 'E.COLI_VIO', 'ECOLI_SAMP_OLD'='E.COLI_SAMP', 'ECOLI_STAT_OLD'='E.COLI_STAT'),
                             bacteriaExceedances_NEW(x,'E.COLI', 10, 410, 126),
                             # Placeholders
                             data.frame(ENTER_VIO='Not Analyzed by App', ENTER_SAMP='Not Analyzed by App', ENTER_STAT='Not Analyzed by App', 
                                        WAT_MET_VIO='Not Analyzed by App', WAT_MET_STAT='Not Analyzed by App', WAT_TOX_VIOv='Not Analyzed by App',
                                        WAT_TOX_STAT='Not Analyzed by App', SED_MET_VIO='Not Analyzed by App', SED_MET_STAT='Not Analyzed by App', 
                                        SED_TOX_VIO='Not Analyzed by App', SED_TOX_STAT='Not Analyzed by App', FISH_MET_VIO='Not Analyzed by App', 
                                        FISH_MET_STAT='Not Analyzed by App', FISH_TOX_VIO='Not Analyzed by App', FISH_TOX_STAT='Not Analyzed by App',
                                        BENTHIC_STAT='Not Analyzed by App'),
                             TP_Exceedances(x), 
                             chlA_Exceedances(x),
                             data.frame(COMMENTS='Not Analyzed by App') )%>%
  dplyr::select(-ends_with('exceedanceRate'))

 # tempExceedances(x), DOExceedances_Min(x), pHExceedances(x),
  