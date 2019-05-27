lake_filter <- filter(lakeStations, SIGLAKENAME == 'Leesville Reservoir')#'Lake Moomaw')# 'Claytor Lake')


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

AUData <- filter(conventionals_Lake, ID305B_1 %in% 'VAW-L10L_BSA01A10') %>%#'VAW-L13L_PGG01A02') %>% 
  left_join(WQSvalues, by = 'CLASS') 

# Create Data frame with all data within ID305B and stratification information
# Pool thermocline data to get 1 sample per day, not 2 with top/bottom time difference
#stationDataDailySample <- reactive({
#  req(AUData())
dat <- AUData
dat$FDT_DATE_TIME <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")

dat <- mutate(dat, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))#%>% # Separate sampling events by day
  #filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
#thermo <- stratifiedLake(dat)
thermo <- stratifiedLake_citNonA(citmonOutOfParameterDataset(filter(dat, !is.na(FDT_TEMP_CELCIUS)), FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK) )

thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
stationDataDailySample <- plyr::join(dat,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
  mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))





stationData <- filter(stationDataDailySample, FDT_STA_ID %in% "9-NEW087.14") #"9-NEW087.14" "9-NEW089.34"
stationData <- filter(stationDataDailySample, FDT_STA_ID %in% "2-JKS044.60")# "2-JKS046.40"
stationData <- filter(stationDataDailySample, FDT_STA_ID %in% 'LVLAPGG000.47')
stationData <- stationDataDailySample



sigLake <- as.character(AUData$SIGLAKENAME)
allLakeLZstations <- as.character(filter(lakeStations,SIGLAKENAME %in% sigLake)$STATION_ID)
allLakeLZData <- filter(conventionals,FDT_STA_ID %in% allLakeLZstations)
allLakeLZData$FDT_DATE_TIME <- as.POSIXct(allLakeLZData$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")
allLakeLZData<- mutate(allLakeLZData, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
  filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
thermo <- stratifiedLake_citNonA(citmonOutOfParameterDataset(allLakeLZData, FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK) )#stratifiedLake(allLakeLZData)
thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
allLakeLZData2 <- plyr::join(allLakeLZData,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
  mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))%>%
  plyr::join(dplyr::select(lakeStations, Chlorophyll_A_limit, TPhosphorus_limit, Assess_TYPE, FDT_STA_ID), by='FDT_STA_ID')
lakeStations <- allLakeLZData2

chlA_Assessment <- function(x){
  if(nrow(x) > 0){
    holder <- list()
    for(i in 1:length(unique(x$FDT_STA_ID))){
      dat <- filter(x,FDT_STA_ID %in% unique(x$FDT_STA_ID)[i])
      holder[[i]] <-  as.data.frame(chlA_Assessment_OneStation(dat))
    }
    alldat <- do.call(rbind,holder)#%>%filter(!is.na(Year))
    return(alldat)
  }
}

exceedance_chlA <- function(x, lakeStations){
  chlA_Assess <- chlA_Assessment(x)
  if(is.null(chlA_Assess)){
    return('No Chlorophyll a data for station ')
  }
  if(nrow(chlA_Assess) < 1){
    return('No Chlorophyll a data for station ')
  }else{
    if(class(chlA_Assess$FDT_STA_ID)=="factor"){ # have to split this step up bc n stationID's affect how split performs
      chlA_Assess$FDT_STA_ID <- droplevels(chlA_Assess$FDT_STA_ID) # have to drop unused levels from factor or it messes with split function and mixes up data in each list item
    }
    dat <- split(chlA_Assess,f=chlA_Assess$FDT_STA_ID)
    holder <- list()
    for(i in 1:length(dat)){
      # Find two most recent years with >= 6 data points
      step1 <- filter(dat[[i]],samplePerYear>=6) # verify enough samples
      step2 <- filter(step1,Year %in% tail(sort(unique(step1$Year)),2)) %>% # get two most recent years from valid sample years 
        mutate(ID305B_1 = as.character(filter(lakeStations, STATION_ID %in% unique(step1$FDT_STA_ID))$ID305B_1))
      
      if(nrow(step2)>1){ # only  do this if more than 1 year of data
        if(step2$chlA_Exceedance[1]!=step2$chlA_Exceedance[2]){ # if the exceedances contradict one another in two years grab third year
          step1alt <- filter(dat[[i]],samplePerYear>=6) # verify enough samples 
          step2 <- filter(step1,Year %in% tail(sort(unique(step1$Year)),3)) %>% # get three most recent years from valid sample years
            mutate(ID305B_1 = as.character(filter(lakeStations, STATION_ID %in% unique(step1$FDT_STA_ID))$ID305B_1))
        }
      }
      holder[[i]] <-  step2
    }
    do.call(rbind,holder) # output table for user to review
  }
  
}

chlA_Exceedances <- exceedance_chlA(citmonOutOfParameterDataset(stationData, CHLOROPHYLL, RMK_32211), lakeStations)



TP_Assessment <- function(x){
  if(nrow(x) > 0){
    holder <- list()
    for(i in 1:length(unique(x$FDT_STA_ID))){
      dat <- filter(x,FDT_STA_ID %in% unique(x$FDT_STA_ID)[i])
      holder[[i]] <-  as.data.frame(TP_Assessment_OneStation(dat))
    }
    alldat <- do.call(rbind,holder)#%>%filter(!is.na(Year))
    return(alldat)
  }
  
}

exceedance_TP <- function(x){
  TP_Assess <- TP_Assessment(x)
  
  if(is.null(TP_Assess)){
    z <- data.frame(FDT_STA_ID = NA, Year = NA, samplePerYear = NA, medianTP = NA, 
                    TPhosphorus_limit_ug_L = NA, TP_Exceedance = NA, LacustrineZone = NA)[0,]
  } else {
    if(nrow(TP_Assess)==0){
      z <- data.frame(FDT_STA_ID = NA, Year = NA, samplePerYear = NA, medianTP = NA, 
                      TPhosphorus_limit_ug_L = NA, TP_Exceedance = NA, LacustrineZone = NA)[0,]
    } else {
      if(class(TP_Assess$FDT_STA_ID)=="factor"){ # have to split this step up bc n stationID's affect how split performs
        TP_Assess$FDT_STA_ID <- droplevels(TP_Assess$FDT_STA_ID) # have to drop unused levels from factor or it messes with split function and mixes up data in each list item
      }
      dat <- split(TP_Assess,f=TP_Assess$FDT_STA_ID)
      holder <- list()
      for(i in 1:length(dat)){
        # Find two most recent years with >= 6 data points
        step1 <- filter(dat[[i]],samplePerYear>=6) # verify enough samples
        step2 <- filter(step1,Year %in% tail(sort(unique(step1$Year)),2)) # get two most recent years from valid sample years 
        
        if(nrow(step2)>1){ # only  do this if more than 1 year of data
          if(step2$TP_Exceedance[1]!=step2$TP_Exceedance[2]){ # if the exceedances contradict one another in two years grab third year
            step1alt <- filter(dat[[i]],samplePerYear>=6) # verify enough samples 
            step2 <- filter(step1,Year %in% tail(sort(unique(step1$Year)),3)) # get three most recent years from valid sample years 
          }
        }
        holder[[i]] <-  step2
      }
      z <- do.call(rbind,holder) # output table for user to review
    }
  }
  
  return(z)
  
}

TP_Exceedances <- function(x){
  # get assessment
  z <- exceedance_TP(x)
  
  if(nrow(z)==0){#is.na(unique(z$FDT_STA_ID))){
    result <- data.frame(NUT_TP_VIO = NA,	NUT_TP_SAMP = NA, NUT_TP_STAT = NA)
  } else {
    result <- data.frame(NUT_TP_VIO = nrow(filter(z, TP_Exceedance== TRUE)),	NUT_TP_SAMP = nrow(z),
                         NUT_TP_STAT = ifelse(any(z[['TP_Exceedance']])==TRUE,'Review','S'))
  }
  return(result)
}
TP_Exceedances(citmonOutOfParameterDataset(stationData, PHOSPHORUS, RMK_PHOSPHORUS))


newSTDbacteriaData <- conventionalsToBacteria(stationData, 'E.COLI') 
z <- bacteriaAssessmentDecision(newSTDbacteriaData, 10, 410, 126)  %>%
  distinct(`Assessment Decision`)  # only grab 1 record



 

chlA_Exceedances_NEW <- function(stationData){
  # get assessment
  z <- exceedance_chlA(x)
  
  result <- data.frame(NUT_CHLA_VIO = nrow(filter(z, chlA_Exceedance== TRUE)),	NUT_CHLA_SAMP = nrow(z),
                       NUT_CHLA_STAT = ifelse(any(z$chlA_Exceedance)==TRUE,'Review','S'))
  return(result)
}
  