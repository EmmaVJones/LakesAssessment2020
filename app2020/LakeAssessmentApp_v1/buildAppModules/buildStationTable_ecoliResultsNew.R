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



ecoli <- filter(stationData, !is.na(`E.COLI`))


newSTDbacteriaData <- conventionalsToBacteria(stationData[0,], 'E.COLI') 
z <- bacteriaAssessmentDecision(newSTDbacteriaData, 10, 410, 126)  %>%
  distinct(`Assessment Decision`)  # only grab 1 record


stationData<- stationData
bacteriaType <- 'E.COLI'
sampleRequirement <- 10
STV <- 410
geomeanCriteria <- 126

bacteriaExceedances_NEW <- function(stationData,bacteriaType, sampleRequirement, STV, geomeanCriteria){
  # get assessment
  z <- bacteriaAssessmentDecision(conventionalsToBacteria(stationData, bacteriaType), sampleRequirement, STV, geomeanCriteria)  %>%
    distinct(`Assessment Decision`)  # only grab 1 record
  
  # Get assessment
  if(str_detect( z$`Assessment Decision`, 'Insufficient Information')){decision <- 'IN'}
  if(str_detect( z$`Assessment Decision`, 'Impaired')){decision <- 'Review'}
  if(str_detect( z$`Assessment Decision`, 'Fully Supporting')) {decision <- 'FS'}
  if(str_detect( z$`Assessment Decision`, 'Observed effect')) {decision <- 'Review'} # put this last to capture any OE's
  
  # Count samples taken in most recent 2 years
  y <- bacteriaExceedances_NewStd(conventionalsToBacteria(stationData, bacteriaType), sampleRequirement, STV, geomeanCriteria) 
  # what are the last two years sampled? They get a bit of priority
  last2years <- sort(unique(year(y$`Date Window Starts`)), TRUE)[1:2]
  x <- filter(y, (year(y$`Date Window Starts`) %in% last2years))
  
  nSTVExceedances <- nrow(filter(x, `Window Within Recreation Season` == TRUE & `STV Exceedance Rate` > 10.5))
  nGeomeanExceedances <- nrow(filter(x, `Window Within Recreation Season` == TRUE & 
                                       `Samples in 90 Day Window` >= sampleRequirement &
                                       `Geomean In Window` > geomeanCriteria))
  s <- data.frame(STV_VIO = nSTVExceedances, GEOMEAN_VIO = nGeomeanExceedances, STAT_NEW = decision)
  names(s) <- paste(bacteriaType, names(s), sep='_')
  names(s) <- gsub('[.]','',names(s))
    
  return(s)
  
}
bacteriaExceedances_NEW(stationData,bacteriaType, 10, 410, 126)
