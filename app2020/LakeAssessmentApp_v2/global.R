library(shiny)
library(shinyjs)
library(leaflet)
library(mapview)
library(tidyverse)
library(sf)
library(plotly)
library(raster)
library(DT)
library(FSA)
library(RColorBrewer)
library(lubridate)
library(testthat)

#####################################   UPDATE EACH NEW TOOL REBUILD #############################################
# Establish Assessment Period 
assessmentPeriod <- as.POSIXct(c("2013-01-01 00:00:00 UTC","2018-12-31 23:59:59 UTC"),tz='UTC')
assessmentCycle <- '2020'
##################################################################################################################


# Bring in modules
source('appModules/multipleDependentSelectizeArguments.R')
source('newBacteriaStandard_workingUpdatedRecSeason.R') # version with 2/3 samples in April-Oct




modulesToReadIn <- c('thermocline','temperature','DO','pH','Chl_a_','TP','Ecoli')#,,'SpCond','Salinity','TN','Ecoli','chlA','Enteroccoci', 'TP','sulfate',
                    # 'Ammonia', 'Chloride', 'Nitrate','metals', 'fecalColiform','SSC','Benthics')
for (i in 1:length(modulesToReadIn)){
  source(paste('appModules/',modulesToReadIn[i],'Module.R',sep=''))
}


# Loading screen
load_data <- function() {
  Sys.sleep(2)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}


WQSvalues <- tibble(CLASS = c('I',"II","II","III","IV","V","VI","VII"),
                    `Description Of Waters` = c('Open Ocean', 'Tidal Waters in the Chowan Basin and the Atlantic Ocean Basin',
                                                'Tidal Waters in the Chesapeake Bay and its tidal tributaries',
                                                'Nontidal Waters (Coastal and Piedmont Zone)','Mountainous Zone Waters',
                                                'Stockable Trout Waters','Natural Trout Waters','Swamp Waters'),
                    `Dissolved Oxygen Min (mg/L)` = c(5,4,NA,4,4,5,6,NA),
                    `Dissolved Oxygen Daily Avg (mg/L)` = c(NA,5,NA,5,5,6,7,NA),
                    `pH Min` = c(6,6,6.0,6.0,6.0,6.0,6.0,3.7),
                    `pH Max` = c(9.0,9.0,9.0,9.0,9.0,9.0,9.0,8.0),
                    `Max Temperature (C)` = c(NA, NA, NA, 32, 31, 21, 20, NA))



withinAssessmentPeriod <- function(x){
  if((range(unique(x$FDT_DATE_TIME2))[1] < assessmentPeriod[1]) | 
     (range(unique(x$FDT_DATE_TIME2))[2] > assessmentPeriod[2])){
    print('Data included that falls outside of assessment period. Review input data.')
  }else{print('All input data falls within the assessment period.')}
}

# Super Assessment function
assessmentDetermination <- function(parameterDF,parameterAssessmentDF,parameter,use){
  
  results <- data.frame(nSamples = nrow(parameterDF),nExceedance = nrow(parameterAssessmentDF))%>%
    mutate(exceedanceRate = (nExceedance/nSamples)*100)
  
  if(results$exceedanceRate > 10.5 & results$nSamples > 10){outcome <- paste('Water impaired for',parameter)}
  if(results$exceedanceRate < 10.5 & results$nSamples > 10){outcome <- paste('Water not impaired for',parameter)}
  if(results$nExceedance >= 2 & results$nSamples < 10){outcome <- paste('Water impaired for',parameter)}
  if(results$nExceedance < 2 & results$nSamples < 10){outcome <- paste('Water not impaired for',parameter)}
  
  results <- mutate(results,Assessment=outcome, Use= use)
  return(results)
}
#assessmentDetermination(temp,temp_Assess,"temperature","Aquatic Life")



######################################## NOTE DIFFERENCE IN REVIEW LOGIC IN COMPARISON TO RIVERINE APP #########################################
# Station Table building functions
quickStats <- function(parameterDataset, parameter){
  if(nrow(parameterDataset) > 0 ){
    results <- data.frame(VIO = nrow(filter(parameterDataset, exceeds == TRUE)),
                          SAMP = nrow(parameterDataset)) %>%
      mutate(exceedanceRate = as.numeric(format((VIO/SAMP)*100,digits=3)))
    
    #if(results$VIO >= 1){outcome <- 'Review'} # for Paula
    if(results$VIO >= 1 & results$SAMP > 10 & results$exceedanceRate < 10.5){outcome <- 'S'} # for Paula
    if(results$exceedanceRate >= 10.5 & results$VIO >= 2 & results$SAMP > 10){outcome <- '10.5% Exceedance'}
    if(results$VIO <= 1 &results$exceedanceRate < 10.5 & results$SAMP > 10){outcome <- 'S'}
    if(results$VIO >= 1 & results$SAMP <= 10){outcome <- 'Review'}
    if(results$VIO < 1 & results$SAMP <= 10){outcome <- 'S'}
    
    
    results <- mutate(results, STAT = outcome)
    names(results) <- c(paste(parameter,names(results)[1], sep = '_'),
                        paste(parameter,names(results)[2], sep = '_'),
                        paste(parameter,names(results)[3], sep = '_'),
                        paste(parameter,names(results)[4], sep = '_'))
    #rename based on parameter entered
    return(results)
  } else {
    z <- data.frame(VIO = NA, SAMP=NA, exceedanceRate= NA, STAT=NA)
    names(z) <- paste(parameter,names(z), sep='_')
    return(z)
  }
}
######################################## NOTE DIFFERENCE IN REVIEW LOGIC IN COMPARISON TO RIVERINE APP #########################################



concatinateUnique <- function(stuff){
  if(length(stuff)==1){
    if(is.na(stuff)){return(NA)
    }else{
      return(paste(unique(stuff), collapse= ', ')) }
  } 
  if(length(stuff) > 1){return(paste(unique(stuff), collapse= ', '))}
}

changeDEQRegionName <- function(stuff){
  # have to do this bc different places in conventionals report the assessment region over sample region
  if(length(stuff) == 1){
    if(stuff == "Valley"){return('VRO')}
    if(stuff == "Northern"){return('NRO')}
    if(stuff == "Piedmont"){return('PRO')}
    if(stuff == "Blue Ridge"){return('BRRO')}
    if(stuff == "Tidewater"){return('TRO')}
    if(stuff == "Southwest" ){return('SWRO')}
    if(stuff == 'NA'){return('NA')}
    #  if(is.na(stuff)){return("NA")}
  } else {return(concatinateUnique(stuff))}
}


StationTableStartingData <- function(x){
  data.frame(ID305B_1= concatinateUnique(x$ID305B_1), ID305B_2= concatinateUnique(x$ID305B_2), ID305B_3= concatinateUnique(x$ID305B_3),
             DEPTH = concatinateUnique(x$FDT_DEPTH_DESC), STATION_ID = concatinateUnique(x$FDT_STA_ID), REGION = changeDEQRegionName(concatinateUnique(x$Deq_Region)), 
             STATION_TYPE_1= concatinateUnique(x$STATION_TYPE_1), STATION_TYPE_2=concatinateUnique(x$STATION_TYPE_2), 
             STATION_TYPE_3= concatinateUnique(x$STATION_TYPE_3), STATION_LAT = concatinateUnique(x$Latitude), 
             STATION_LON = concatinateUnique(x$Longitude), WATERSHED_ID= substr(strsplit(as.character(concatinateUnique(x$ID305B_1)), '-')[[1]][2], 1, 3),#concatinateUnique(x$ID305B_1),
             VAHU6 = concatinateUnique(x$Huc6_Vahu6) )
  # Should be this but issues with shiny application of function          
  #WATERSHED_ID= substr(strsplit(as.character(concatinateUnique(x$ID305B_1)), '-')[[1]][2], 1, 3)
  
}




#dataFrame <-ammonia
#dateTimeColumn <- 'FDT_DATE_TIME2'
#nYears <- 3
#consecutive <- TRUE
lastXyears <- function(dataFrame, dateTimeColumn, nYears, consecutive){ # not bombproof but works for nYears=3
  uniqueYears <- dplyr::select(dataFrame, dateTimeColumn) %>% 
    mutate(sampleYear = lubridate::year(get(dateTimeColumn))) %>% 
    distinct(sampleYear) %>% 
    dplyr::pull()
  #uniqueYears <- c(2011, 2012, 2013, 2014, 2015, 2018)
  #uniqueYears <- c(2013, 2014, 2015, 2017)
  #uniqueYears <- c(2013, 2014, 2015, 2017, 2018)
  nYearsConversion <- nYears-1
  
  if(length(uniqueYears) > nYearsConversion){
    years <- sort(uniqueYears)[(length(uniqueYears)-nYearsConversion):length(uniqueYears)]
  } else { 
    years <- sort(uniqueYears)}
  
  if(consecutive == TRUE){
    recent <- years[which(years > max(years)-nYears)]
    return(recent)
    
  } else {return(years)}
}

#lastXyears(dataFrame, dateTimeColumn, 7, TRUE)





# Table of thermocline depth (if thermocline exists) for each sample date
stratifiedLake <- function(x){
  template <- data.frame(FDT_STA_ID=NA,SampleDate=NA,ThermoclineDepth=NA)
  holder <- template
  if(nrow(x)>0){
    for(k in 1:length(unique(x$FDT_STA_ID))){
      oneStation <- filter(x,FDT_STA_ID %in% unique(x$FDT_STA_ID)[k])
      for(i in 1:length(unique(oneStation$SampleDate))){
        oneProfile <- filter(oneStation,SampleDate %in% unique(oneStation$SampleDate)[i])%>%
          dplyr::select(SampleDate,FDT_DEPTH,FDT_TEMP_CELCIUS) %>%
          mutate(DepthDiff=c(NA,diff(FDT_DEPTH)),
                 TempDiff=c(NA,diff(FDT_TEMP_CELCIUS))) %>%
          filter(DepthDiff==1) %>% # get rid of changes less than 1 meter depth
          filter(TempDiff<=-1) # find where temperature changes are 1C or greater
        therm <- if(nrow(oneProfile)>0){min(oneProfile$FDT_DEPTH)-0.5}else{NaN} # subtract 0.5 to get the top of the thermocline
        holder[i,] <- cbind(oneStation$FDT_STA_ID[1],unique(oneStation$SampleDate)[i],format(therm))
      }
      template <- rbind(template,holder)
      template <- template[complete.cases(template),]
    }
    row.names(template) <- 1:nrow(template)
    return(template)
  }else{
    return(template)
  }
}

round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

stratifiedLake_citNonA <- function(x){
  template <- data.frame(FDT_STA_ID=NA,SampleDate=NA,ThermoclineDepth=NA, ThermoclineDepth_RMK= NA)
  holder <- template
  if(nrow(x)>0){
    for(k in 1:length(unique(x$FDT_STA_ID))){
      holder <- holder[0,]
      oneStation <- filter(x,FDT_STA_ID %in% unique(x$FDT_STA_ID)[k])
      for(i in 1:length(unique(oneStation$SampleDate))){
        oneProfile <- filter(oneStation,SampleDate %in% unique(oneStation$SampleDate)[i])%>%
          dplyr::select(SampleDate,FDT_DEPTH,FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK) %>%
          mutate(roundedDepth = round2(FDT_DEPTH, 0), # round to nearest interger
                 DepthDiff=c(NA,diff(roundedDepth)),
                 TempDiff=c(NA,diff(FDT_TEMP_CELCIUS))) %>%
          filter(DepthDiff==1) %>% # get rid of changes less than 1 meter depth
          filter(TempDiff<=-1) # find where temperature changes are 1C or greater
        therm <- if(nrow(oneProfile)>0){min(oneProfile$FDT_DEPTH)-0.5}else{NaN} # subtract 0.5 to get the top of the thermocline
        if(therm != 'NaN' && therm == 0 ){therm <- 1} # fix for weird citmon stuff
        holder[i,] <- cbind(oneStation$FDT_STA_ID[1],unique(oneStation$SampleDate)[i],format(therm), paste0(unique(oneStation$FDT_TEMP_CELCIUS_RMK)))
      }
      template <- rbind(template,holder)
      #template <- template[complete.cases(template[,1:3]),]
    }
    template <- template[complete.cases(template[,1:3]),]
    row.names(template) <- 1:nrow(template)
    return(template)
  }else{
    return(template)
  }
}

#thermo <- stratifiedLake_citNonA(citmonOutOfParameterDataset(dat, FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK) )
#thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)

#### Temperature Assessment Functions ---------------------------------------------------------------------------------------------------

#Max Temperature Exceedance Function
temp_Assessment <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME, FDT_DEPTH, FDT_TEMP_CELCIUS, `Max Temperature (C)`)%>% # Just get relevant columns, 
    filter(!is.na(FDT_TEMP_CELCIUS))%>% #get rid of NA's
    mutate(TemperatureExceedance=ifelse(FDT_TEMP_CELCIUS > `Max Temperature (C)`,T,F))%>% # Identify where above max Temperature, 
    filter(TemperatureExceedance==TRUE) # Only return temp measures above threshold
  temp$FDT_DATE_TIME <- as.character(temp$FDT_DATE_TIME)
  return(temp)
}

# Exceedance Rate Temperature
exceedance_temp <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME,FDT_DEPTH, FDT_TEMP_CELCIUS,`Max Temperature (C)`)%>% # Just get relevant columns, 
    filter(!is.na(FDT_TEMP_CELCIUS)) #get rid of NA's
  temp_Assess <- temp_Assessment(x)
  
  temp_results <- assessmentDetermination(temp,temp_Assess,"temperature","Aquatic Life")
  return(temp_results)
}

# For station table, presented a little differently
#Max Temperature Exceedance Function
tempExceedances <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME,FDT_TEMP_CELCIUS, `Max Temperature (C)`)%>% # Just get relevant columns, 
    filter(!is.na(FDT_TEMP_CELCIUS))%>% #get rid of NA's
    rename(parameter = !!names(.[2]), limit = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above max Temperature, 
  
  quickStats(temp, 'TEMP')
}
#tempExceedances(x)




#Max Temperature Exceedance Function
tempExceedances_MEDIAN <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME,FDT_TEMP_CELCIUS, `Max Temperature (C)`)%>% # Just get relevant columns, 
    filter(!is.na(FDT_TEMP_CELCIUS))%>% #get rid of NA's
    group_by(FDT_DATE_TIME) %>%
    mutate(parameterMedian = median(FDT_TEMP_CELCIUS)) %>%
    rename(parameter = !!names(.[4]), limit = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above max Temperature, 
  
  quickStats(temp, 'TEMP_MEDIAN')
}
#tempExceedances(x)


tempExceedances_EpiONLY <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME,FDT_TEMP_CELCIUS, `Max Temperature (C)`, LakeStratification)%>% # Just get relevant columns, 
    filter(!is.na(FDT_TEMP_CELCIUS))%>% #get rid of NA's
    filter(LakeStratification %in% c("Epilimnion")) %>%
    rename(parameter = !!names(.[2]), limit = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above max Temperature, 
  
  quickStats(temp, 'TEMP_EpiONLY')
}
#tempExceedances(x)


# For station table, presented a little differently
#Max Temperature Exceedance Function
tempExceedances_EpiMEDIAN <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME,FDT_TEMP_CELCIUS, `Max Temperature (C)`,LakeStratification)%>% # Just get relevant columns, 
    filter(!is.na(FDT_TEMP_CELCIUS))%>% #get rid of NA's
    filter(LakeStratification %in% c("Epilimnion")) %>%
    group_by(FDT_DATE_TIME) %>%
    mutate(parameterMedian = median(FDT_TEMP_CELCIUS)) %>%
    rename(parameter = !!names(.[5]), limit = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above max Temperature, 
  
  quickStats(temp, 'TEMP_EpiMEDIAN')
}
#tempExceedances(x)


# Median of all Values on same day

# x <- stationDataDailySample

# For station table, presented a little differently
#Max Temperature Exceedance Function
tempExceedances_MultiMEDIAN <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME,FDT_TEMP_CELCIUS, `Max Temperature (C)`,SampleDate)%>% # Just get relevant columns, 
    filter(!is.na(FDT_TEMP_CELCIUS))%>% #get rid of NA's
    group_by(SampleDate) %>%
    mutate(parameterMedian = median(FDT_TEMP_CELCIUS)) %>%
    distinct(parameterMedian, .keep_all = TRUE) %>%
    rename(parameter = !!names(.[5]), limit = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above max Temperature, 
  
  quickStats(temp, 'TEMP_AUdailyMEDIAN')
}
#tempExceedances(x)


# Median of each sample then median of all Values on same day

# x <- stationDataDailySample

# For station table, presented a little differently
#Max Temperature Exceedance Function
tempExceedances_MultiMEDIAN_median <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME,FDT_TEMP_CELCIUS, `Max Temperature (C)`,SampleDate)%>% # Just get relevant columns, 
    filter(!is.na(FDT_TEMP_CELCIUS))%>% #get rid of NA's
    group_by(FDT_DATE_TIME) %>% # this gets at single station on unique days
    mutate(parameterMedian = median(FDT_TEMP_CELCIUS)) %>%
    distinct(parameterMedian, .keep_all = TRUE) %>%
    ungroup() %>%
    group_by(SampleDate) %>%
    mutate(parameterMedianMedian = median(parameterMedian)) %>%
    distinct(parameterMedianMedian, .keep_all = TRUE) %>%
    rename(parameter = !!names(.[6]), limit = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above max Temperature, 
  
  quickStats(temp, 'TEMP_AUdailyMEDIANMEDIAN')
}
#tempExceedances(x)




#### Dissolved Oxygen Assessment Functions ---------------------------------------------------------------------------------------------------

#Min DO Exceedance Function
DO_Assessment <- function(x,qualifier){ # qualifier allows you to run the analysis including many or few stratification qualifications
  DO <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,DO,LakeStratification,FDT_DATE_TIME2,`Dissolved Oxygen Min (mg/L)`)%>% # Just get relevant columns, 
    filter(!is.na(DO))%>% #get rid of NA's
    filter(LakeStratification %in% qualifier)%>%
    ##filter(LakeStratification=="Epilimnion") # Only assess Epilimnion
    ##filter(LakeStratification %in% c("Epilimnion",NA))%>% # Only assess Epilimnion or NaN (no stratification)
    mutate(DOExceedance=ifelse(DO < `Dissolved Oxygen Min (mg/L)`,T,F))%>% # Identify where above max Temperature, 9VAC25-260-50 ClassIII= 32C
    filter(DOExceedance==TRUE) # Only return temp measures above threshold
  
  DO$FDT_DATE_TIME <- as.character(DO$FDT_DATE_TIME2)
  DO <- dplyr::select(DO,-c(DOExceedance,FDT_DATE_TIME2)) # Don't show user column, could be confusing to them
  
  return(DO)
}

# Exceedance Rate DO
exceedance_DO <- function(x, qualifier){
  DO <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,DO,LakeStratification,`Dissolved Oxygen Min (mg/L)`)%>% # Just get relevant columns, 
    filter(!is.na(DO))%>% #get rid of NA's
    filter(LakeStratification %in% qualifier)
  ##filter(LakeStratification %in% c("Epilimnion",NA)) # Only assess Epilimnion or NaN (no stratification)
  ##filter(LakeStratification=="Epilimnion") # Only assess Epilimnion
  DO_Assess <- DO_Assessment(x,qualifier)
  DO_results <- assessmentDetermination(DO,DO_Assess,"Dissolved Oxygen","Aquatic Life")
  return(DO_results)
}

# For station table, presented a little differently
DOExceedances_Min <- function(x){
  DO <- dplyr::select(x,FDT_DATE_TIME2,DO,`Dissolved Oxygen Min (mg/L)`,LakeStratification)%>% # Just get relevant columns, 
    filter(!is.na(DO)) %>% 
    filter(LakeStratification %in% c("Epilimnion",NA)) %>%
    rename(parameter = !!names(.[2]), limit = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(exceeds = ifelse(parameter < limit, T, F)) # Identify where below min DO 
  
  quickStats(DO, 'DO')
}
#DOExceedances_Min(x)

# For station table, presented a little differently
DOExceedances_Min_MEDIAN <- function(x){
  DO <- dplyr::select(x,FDT_DATE_TIME2,DO,`Dissolved Oxygen Min (mg/L)`,LakeStratification)%>% # Just get relevant columns, 
    filter(!is.na(DO)) %>% 
    filter(LakeStratification %in% c("Epilimnion",NA)) %>%
    group_by(FDT_DATE_TIME2) %>%
    mutate(parameterMedian = median(DO)) %>%
    rename(parameter = !!names(.[5]), limit = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(exceeds = ifelse(parameter < limit, T, F)) # Identify where below min DO 
    
  quickStats(DO, 'DO_MEDIAN')
}
#DOExceedances_Min(x)

# For station table, presented a little differently
DOExceedances_Min_EpiONLY <- function(x){
  DO <- dplyr::select(x,FDT_DATE_TIME2,DO,`Dissolved Oxygen Min (mg/L)`,LakeStratification)%>% # Just get relevant columns, 
    filter(!is.na(DO)) %>% 
    filter(LakeStratification %in% c("Epilimnion")) %>%
    rename(parameter = !!names(.[2]), limit = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(exceeds = ifelse(parameter < limit, T, F)) # Identify where below min DO 
  
  quickStats(DO, 'DO_EpiONLY')
}
#DOExceedances_Min(x)



DOExceedances_Min_EpiMEDIAN <- function(x){
  DO <- dplyr::select(x,FDT_DATE_TIME2,DO,`Dissolved Oxygen Min (mg/L)`,LakeStratification)%>% # Just get relevant columns, 
    filter(!is.na(DO)) %>% 
    filter(LakeStratification %in% c("Epilimnion")) %>%
    group_by(FDT_DATE_TIME2) %>%
    mutate(parameterMedian = median(DO)) %>%
    rename(parameter = !!names(.[5]), limit = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(exceeds = ifelse(parameter < limit, T, F)) # Identify where below min DO 
  
  quickStats(DO, 'DO_EpiMEDIAN')
}
#DOExceedances_Min(x)



# AU testing

# median of all epilimnion or NA stratification values for AU on each sample day

# x <- stationDataDaily Sample

DOExceedances_Min_MultiMEDIAN <- function(x){
  DO <- dplyr::select(x,FDT_DATE_TIME2,DO,`Dissolved Oxygen Min (mg/L)`,LakeStratification, SampleDate)%>% # Just get relevant columns, 
    filter(!is.na(DO)) %>% 
    filter(LakeStratification %in% c("Epilimnion",NA)) %>%
    group_by(SampleDate) %>% # this groups by stations each sample day
    mutate(parameterMedian = median(DO)) %>%
    distinct(parameterMedian, .keep_all = TRUE) %>%
    rename(parameter = !!names(.[6]), limit = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(exceeds = ifelse(parameter < limit, T, F)) # Identify where below min DO 
  
  quickStats(DO, 'DO_AUdailyMEDIAN')
}
#DOExceedances_Min(x)



# Median of each sample then median of all Values on same day

# x <- stationDataDailySample


DOExceedances_Min_MultiMEDIAN_median <- function(x){
  DO <- dplyr::select(x,FDT_DATE_TIME2,DO,`Dissolved Oxygen Min (mg/L)`,LakeStratification, SampleDate)%>% # Just get relevant columns, 
    filter(!is.na(DO)) %>% 
    filter(LakeStratification %in% c("Epilimnion",NA)) %>%
    group_by(FDT_DATE_TIME2) %>% # this gets at single station on unique days
    mutate(parameterMedian = median(DO)) %>%
    distinct(parameterMedian, .keep_all = TRUE) %>%
    ungroup() %>%
    group_by(SampleDate) %>%
    mutate(parameterMedianMedian = median(parameterMedian)) %>%
    distinct(parameterMedianMedian, .keep_all = TRUE) %>%
    rename(parameter = !!names(.[7]), limit = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(exceeds = ifelse(parameter < limit, T, F)) # Identify where below min DO 
  
  quickStats(DO, 'DO_AUdailyMEDIANMEDIAN')
}





#### pH Assessment Functions ---------------------------------------------------------------------------------------------------

pH_rangeAssessment <- function(x){
  pH <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,FDT_FIELD_PH,`pH Min`,`pH Max`, LakeStratification)%>% # Just get relavent columns, 
    filter(!is.na(FDT_FIELD_PH))%>% #get rid of NA's
    filter(LakeStratification %in% c("Epilimnion",NA)) %>% # As of 2018 IR pH standards only apply to epilimnion or non stratified lake samples
    mutate(pHrange=ifelse(FDT_FIELD_PH < `pH Min` | FDT_FIELD_PH > `pH Max`,TRUE, FALSE)) %>% # Identify where pH outside of assessment range
    filter(pHrange==TRUE) %>% # Only return pH measures outside of assessement range
    dplyr::select(-c(pHrange)) # Don't show user interval column, could be confusing to them, T/F in pHrange column sufficient
  pH$FDT_DATE_TIME <- as.character(pH$FDT_DATE_TIME)
  return(pH)
}


exceedance_pH <- function(x,pHrange){
  pH <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,FDT_FIELD_PH, `pH Min`,`pH Max`, LakeStratification)%>% # Just get relavent columns, 
    filter(!is.na(FDT_FIELD_PH)) #get rid of NA's
  pH_rangeAssess <- pH_rangeAssessment(x)
  
  pH_results <- assessmentDetermination(pH %>% filter(!is.na(FDT_FIELD_PH))%>% #get rid of NA's
                                          filter(LakeStratification %in% c("Epilimnion",NA)),
                                        pH_rangeAssess,"pH","Aquatic Life")
  return(pH_results)
}


# For station table, presented a little differently

pHExceedances <- function(x){
  pH <- dplyr::select(x,FDT_DATE_TIME,FDT_FIELD_PH,`pH Min`,`pH Max`, LakeStratification)%>% # Just get relevant columns, 
    filter(!is.na(FDT_FIELD_PH))%>% #get rid of NA's
    filter(LakeStratification %in% c("Epilimnion",NA)) %>%
    rowwise() %>%
    mutate(exceeds=ifelse(FDT_FIELD_PH < `pH Min` | FDT_FIELD_PH > `pH Max`,TRUE, FALSE))# Identify where pH outside of assessment range
    
  quickStats(pH, 'PH')
}
#pHExceedances(x)

pHExceedances_MEDIAN <- function(x){
  pH <- dplyr::select(x,FDT_DATE_TIME,FDT_FIELD_PH,`pH Min`,`pH Max`, LakeStratification)%>% # Just get relevant columns, 
    filter(!is.na(FDT_FIELD_PH))%>% #get rid of NA's
    filter(LakeStratification %in% c("Epilimnion",NA)) %>%
    group_by(FDT_DATE_TIME) %>%
    mutate(PH_MEDIAN = median(FDT_FIELD_PH)) %>%
    rowwise() %>% mutate(interval=findInterval(PH_MEDIAN,c(`pH Min`,`pH Max`)))%>% # Identify where pH outside of assessment range
    ungroup()%>%
    mutate(exceeds=ifelse(interval == 1, F, T)) # Highlight where pH doesn't fall into assessment range
  quickStats(pH, 'PH_MEDIAN')
}
#pHExceedances(x)

pHExceedances_EpiONLY <- function(x){
  pH <- dplyr::select(x,FDT_DATE_TIME,FDT_FIELD_PH,`pH Min`,`pH Max`, LakeStratification)%>% # Just get relevant columns, 
    filter(!is.na(FDT_FIELD_PH))%>% #get rid of NA's
    filter(LakeStratification %in% c("Epilimnion")) %>%
    rowwise() %>% mutate(interval=findInterval(FDT_FIELD_PH,c(`pH Min`,`pH Max`)))%>% # Identify where pH outside of assessment range
    ungroup()%>%
    mutate(exceeds=ifelse(interval == 1, F, T)) # Highlight where pH doesn't fall into assessment range
  quickStats(pH, 'PH_EpiONLY')
}
#pHExceedances(x)

pHExceedances_EpiMedian <- function(x){
  pH <- dplyr::select(x,FDT_DATE_TIME,FDT_FIELD_PH,`pH Min`,`pH Max`, LakeStratification)%>% # Just get relevant columns, 
    filter(!is.na(FDT_FIELD_PH))%>% #get rid of NA's
    filter(LakeStratification %in% c("Epilimnion")) %>%
    group_by(FDT_DATE_TIME) %>%
    mutate(PH_MEDIAN = median(FDT_FIELD_PH)) %>%
    rowwise() %>% mutate(interval=findInterval(PH_MEDIAN,c(`pH Min`,`pH Max`)))%>% # Identify where pH outside of assessment range
    ungroup()%>%
    mutate(exceeds=ifelse(interval == 1, F, T)) # Highlight where pH doesn't fall into assessment range
  quickStats(pH, 'PH_EpiMEDIAN')
}
#pHExceedances(x)


# AU testing

# median of all epilimnion or NA stratification values for AU on each sample day

# x <- stationDataDaily Sample


pHExceedances_MultiMEDIAN <- function(x){
  pH <- dplyr::select(x,FDT_DATE_TIME,FDT_FIELD_PH,`pH Min`,`pH Max`, LakeStratification, SampleDate)%>% # Just get relevant columns, 
    filter(!is.na(FDT_FIELD_PH))%>% #get rid of NA's
    filter(LakeStratification %in% c("Epilimnion", NA)) %>%
    group_by(SampleDate) %>% # this groups by stations each sample day
    mutate(PH_MEDIAN = median(FDT_FIELD_PH)) %>%
    distinct(PH_MEDIAN, .keep_all = TRUE) %>%
    rowwise() %>% mutate(interval=findInterval(PH_MEDIAN,c(`pH Min`,`pH Max`)))%>% # Identify where pH outside of assessment range
    ungroup()%>%
    mutate(exceeds=ifelse(interval == 1, F, T)) # Highlight where pH doesn't fall into assessment range
  quickStats(pH, 'PH_AUdailyMEDIAN')
}



# Median of each sample then median of all Values on same day

# x <- stationDataDailySample


pHExceedances_MultiMEDIAN_median <- function(x){
  pH <- dplyr::select(x,FDT_DATE_TIME2,FDT_FIELD_PH,`pH Min`,`pH Max`, LakeStratification, SampleDate)%>% # Just get relevant columns, 
    filter(!is.na(FDT_FIELD_PH))%>% #get rid of NA's
    filter(LakeStratification %in% c("Epilimnion", NA)) %>%
    group_by(FDT_DATE_TIME2) %>%  # this gets at single station on unique days
    mutate(PH_MEDIAN = median(FDT_FIELD_PH)) %>%
    distinct(PH_MEDIAN, .keep_all = TRUE) %>%
    ungroup() %>%
    group_by(SampleDate) %>%
    mutate(parameterMedianMedian = median(PH_MEDIAN)) %>%
    rowwise() %>% mutate(interval=findInterval(parameterMedianMedian,c(`pH Min`,`pH Max`)))%>% # Identify where pH outside of assessment range
    ungroup()%>%
    mutate(exceeds=ifelse(interval == 1, F, T)) # Highlight where pH doesn't fall into assessment range
  quickStats(pH, 'PH_AUdailyMEDIANMEDIAN')
}




#### Chlorophyll a Assessment Functions ---------------------------------------------------------------------------------------------------

chlA_Assessment_OneStation <- function(x){
  chlA <- filter(x, !is.na(CHLOROPHYLL))%>%
    dplyr::select(FDT_STA_ID,FDT_DEPTH,FDT_DATE_TIME, SampleDate,CHLOROPHYLL,Chlorophyll_A_limit,Assess_TYPE)%>%
    mutate(Year=format(FDT_DATE_TIME,"%Y"),Month=format(FDT_DATE_TIME,'%m'))%>%
    filter(Month %in% c('04','05','06','07','08','09','10'))%>% # make sure only assess valid sample months
    group_by(Year)%>%
    mutate(samplePerYear= n(),pct90=quantile(CHLOROPHYLL,0.9),
           chlA_Exceedance=ifelse(pct90>Chlorophyll_A_limit,T,F),
           LacustrineZone=ifelse(!is.na(unique(x$Assess_TYPE)) & unique(x$Assess_TYPE)=="LZ",TRUE,FALSE))%>%
    dplyr::select(FDT_STA_ID,Year,samplePerYear,pct90,Chlorophyll_A_limit,chlA_Exceedance,LacustrineZone)%>%
    distinct(Year,.keep_all=T)
  return(chlA)
}

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


# Station Table summary function

chlA_Exceedances <- function(x, lakeStations){
  # get assessment
  z <- exceedance_chlA(x, lakeStations)
  
  if( class(z) == 'character' && unique(z) == "No Chlorophyll a data for station "){
    result <- data.frame(NUT_CHLA_VIO = NA,	NUT_CHLA_SAMP = NA, NUT_CHLA_STAT = NA)
  } else {
    result <- data.frame(NUT_CHLA_VIO = nrow(filter(z, chlA_Exceedance== TRUE)),	NUT_CHLA_SAMP = nrow(z),
                         NUT_CHLA_STAT = ifelse(any(z$chlA_Exceedance)==TRUE,'Review','S'))
  }
  
  return(result)
}





#### Total phosphorus Assessment Functions ---------------------------------------------------------------------------------------------------


TP_Assessment_OneStation <- function(x){
  #if(!is.na(unique(x$Assess_TYPE)) & unique(x$Assess_TYPE)=="LZ"){
  TP <- filter(x, !is.na(PHOSPHORUS))%>%
    dplyr::select(FDT_STA_ID,FDT_DEPTH,FDT_DATE_TIME, SampleDate,PHOSPHORUS,TPhosphorus_limit,Assess_TYPE)%>%
    mutate(Year=format(FDT_DATE_TIME,"%Y"),Month=format(FDT_DATE_TIME,'%m'))%>%
    filter(Month %in% c('04','05','06','07','08','09','10'))%>% # make sure only assess valid sample months
    filter(FDT_DEPTH<1) %>% # make sure you do not analyze bottom sample, if any
    group_by(Year) %>%
    mutate(samplePerYear= n(),TPhosphorus_limit_ug_L=TPhosphorus_limit/1000,medianTP=median(PHOSPHORUS),
           TP_Exceedance=ifelse(medianTP>TPhosphorus_limit_ug_L,T,F),
           LacustrineZone=ifelse(!is.na(unique(x$Assess_TYPE)) & unique(x$Assess_TYPE)=="LZ",TRUE,FALSE))%>%
    dplyr::select(FDT_STA_ID,Year,samplePerYear,medianTP,TPhosphorus_limit_ug_L,TP_Exceedance,LacustrineZone)%>%
    distinct(Year,.keep_all=T)
  return(TP)
  #}else{
  #  dat <- data.frame( FDT_STA_ID=x$FDT_STA_ID[1], Year=NA, samplePerYear=NA, 
  #                     medianTP=NA, TPhosphorus_limit_ug_L=NA, TP_Exceedance=NA)
  #    return(dat)
  #}
}


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


# Station Table summary function

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




### Bacteria Old

bacteria_ExceedancesGeomeanOLD <- function(x, bacteriaType, geomeanLimit){
  if(nrow(x) > 0){
    suppressWarnings(mutate(x, SampleDate = format(FDT_DATE_TIME2,"%m/%d/%y"), # Separate sampling events by day
                            previousSample=lag(SampleDate,1),previousSampleBacteria=lag(get(bacteriaType),1)) %>% # Line up previous sample with current sample line
                       rowwise() %>% 
                       mutate(sameSampleMonth= as.numeric(strsplit(SampleDate,'/')[[1]][1])  -  as.numeric(strsplit(previousSample,'/')[[1]][1])) %>% # See if sample months are the same, e.g. more than one sample per calendar month
                       filter(sameSampleMonth == 0 | is.na(sameSampleMonth)) %>% # keep only rows with multiple samples per calendar month  or no previous sample (NA) to then test for geomean
                       # USING CALENDAR MONTH BC THAT'S HOW WRITTEN IN GUIDANCE, rolling 4 wk windows would have been more appropriate
                       mutate(sampleMonthYear = paste(month(as.Date(SampleDate,"%m/%d/%y")),year(as.Date(SampleDate,"%m/%d/%y")),sep='/')) %>% # grab sample month and year to group_by() for next analysis
                       group_by(sampleMonthYear) %>%
                       mutate(geoMeanCalendarMonth = FSA::geomean(as.numeric(get(bacteriaType))), # Calculate geomean
                              limit = geomeanLimit, samplesPerMonth = n()))
  }
  
}

bacteria_ExceedancesSTV_OLD <- function(x, STVlimit){                                    
  x %>% rename(parameter = !!names(.[2])) %>% # rename columns to make functions easier to apply
    mutate(limit = STVlimit, exceeds = ifelse(parameter > limit, T, F)) # Single Sample Maximum 
}

# How bacteria is assessed
bacteria_Assessment_OLD <- function(x, bacteriaType, geomeanLimit, STVlimit){
  bacteria <- dplyr::select(x,FDT_DATE_TIME2,bacteriaType)%>% # Just get relavent columns, 
    filter(!is.na(get(bacteriaType))) #get rid of NA's
  # Geomean Analysis (if enough n)
  if(nrow(bacteria)>0){
    bacteriaGeomean <- bacteria_ExceedancesGeomeanOLD(bacteria, bacteriaType, geomeanLimit) %>%     
      distinct(sampleMonthYear, .keep_all = T) %>%
      filter(samplesPerMonth > 4, geoMeanCalendarMonth > limit) %>% # minimum sampling rule for geomean to apply
      mutate(exceeds = TRUE) %>%
      dplyr::select(sampleMonthYear, geoMeanCalendarMonth, limit, exceeds, samplesPerMonth)
    geomeanResults <- quickStats(bacteriaGeomean, bacteriaType) %>%
      mutate(`Assessment Method` = 'Old Monthly Geomean')
    geomeanResults[,4] <- ifelse(is.na(geomeanResults[,4]),NA, dplyr::recode(geomeanResults[,4], 'Review' = paste('Review if ', bacteriaType,'_VIO > 1',sep='')))
    
    # Single Sample Maximum Analysis
    bacteriaSSM <- bacteria_ExceedancesSTV_OLD(bacteria, STVlimit) 
    SSMresults <- quickStats(bacteriaSSM, bacteriaType) %>% mutate(`Assessment Method` = 'Old Single Sample Maximum')
    return( rbind(geomeanResults, SSMresults) )
  }
  
}


# Old bacteria standard boiled down to a single line as needed to go into stations table
bacteriaExceedances_OLD <- function(results, bacteriaType){
  # If no results to report, give nothing
  if(length(results)>0){
    # if geomean applied, use those results
    if(grepl('Review',results[1,4])){
      return(results[1,1:4])}
    else{
      return(results[2,1:4])}
  }else{
    z <- data.frame(SAMP=NA, VIO = NA, exceedanceRate= NA, STAT=NA)
    names(z) <- paste(bacteriaType,names(z), sep='_')
    return(z)
  }
}



####

# Function to convert conventionals bacteria (e coli and enter) to format needed for function
#bacteriaType <- 'E.COLI'#ENTEROCOCCI
conventionalsToBacteria <- function(x, bacteriaType){
  z <- dplyr::select(x, FDT_STA_ID, FDT_DATE_TIME2, bacteriaType) %>%
    rename(ID = FDT_STA_ID, `Date Time` = FDT_DATE_TIME2, Value = bacteriaType) %>%
    filter(!is.na(Value))
  z$`Date Time` <- as.Date(z$`Date Time`)
  z$Value <- as.numeric(z$Value)
  return(z)
}


# New bacteria station table function

# New bacteria station table function

bacteriaExceedances_NEW <- function(stationData,bacteriaType, sampleRequirement, STV, geomeanCriteria){
  # get assessment
  z <- bacteriaAssessmentDecision(conventionalsToBacteria(stationData, bacteriaType), sampleRequirement, STV, geomeanCriteria)  %>%
    distinct(`Assessment Decision`)  # only grab 1 record
  
  if(is.na(z$`Assessment Decision`)){
    nSTVExceedances <- NA
    nGeomeanExceedances <- NA
    decision <- NA
  } else {
    # Get assessment
    if(str_detect( z$`Assessment Decision`, 'Insufficient Information')){decision <- 'IN'}
    if(str_detect( z$`Assessment Decision`, 'Impaired')){decision <- 'Review'}
    if(str_detect( z$`Assessment Decision`, 'Fully Supporting')) {decision <- 'FS'}
    if(str_detect( z$`Assessment Decision`, 'Observed effect')) {decision <- 'Review'} # put this last to capture any OE's
    
    # Samples taken in most recent 2 years
    y <- bacteriaExceedances_NewStd(conventionalsToBacteria(stationData, bacteriaType), sampleRequirement, STV, geomeanCriteria) 
    # what are the last two years sampled? They get a bit of priority
    last2years <- sort(unique(year(y$`Date Window Starts`)), TRUE)[1:2]
    x <- filter(y, (year(y$`Date Window Starts`) %in% last2years))
    nonOverlappingExceedanceResults <- nonOverlappingIntervals(x, TRUE, last2years)
    
    nSTVExceedances <- nrow(filter(nonOverlappingExceedanceResults, `Window Within Recreation Season` == TRUE & `STV Exceedance Rate` > 10.5 &
                                     `STV Exceedances In Window` > 1))
    nGeomeanExceedances <- nrow(filter(nonOverlappingExceedanceResults, `Window Within Recreation Season` == TRUE & 
                                         `Samples in 90 Day Window` >= sampleRequirement &
                                         `Geomean In Window` > geomeanCriteria))
    
  }
  
  s <- data.frame(STV_VIO = nSTVExceedances, GEOMEAN_VIO = nGeomeanExceedances, STAT_NEW = decision)
  names(s) <- if(bacteriaType == 'ENTEROCOCCI'){
    paste("ENTER", names(s), sep='_')} else {paste(bacteriaType, names(s), sep='_')}
  names(s) <- gsub('[.]','',names(s))
  
  return(s)
  
  
  
}




## Getting rid of citmon data

citmonOutOfParameterDataset <- function(x, parameter, parameterRMK){
  
  parameter1 <- enquo(parameter)
  parameterRMK1 <- enquo(parameterRMK)
  
  # catch for James not including level but not dropping DEQ data
  if(all(x$STA_LV3_CODE %in% c("CMON", "NONA", "NONA "))){
    # first get rid of no remarks
    filter(x, !is.na( !!parameterRMK1)) %>%
      # then get rid of level I and II data
      filter( !( !!parameterRMK1  %in% c('Level II', 'Level I')) )
  } else {
    filter(x, !( !!parameterRMK1  %in% c('Level II', 'Level I')) )
  }
  #dplyr:: select(x, FDT_DATE_TIME2, !!parameter1, !!parameterRMK1) %>% # Just get relavent columns,
}  

#citmonOutOfParameterDataset(dat, FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK) ### Note the non quoted variables, makes a big difference 








#### Station Table stuff for citmon

countSamplesAtLevels <- function(x){
  parameterName <- unique(names(x)[1])
  x1 <- x %>% rename(measure = !!names(.[1])) %>% filter(!is.na(measure)) %>% group_by_at(2) %>% summarize(count = n()) %>% rename(parameter = !!names(.[1])) %>% mutate(together = paste(parameter,':',count))
  if( nrow(x1) > 0){ 
    return(paste(parameterName," (", paste0(x1$together, collapse='; '), ")"))
  } else{ return(NA)}
}

#testpH <- data.frame(FDT_FIELD_PH= c(7,3,4,5,5,3,2,3,4,5),
#                     FDT_FIELD_PH_RMK = c('Level I', "Level III", "Level II","Level II","Level II","Level II","Level III","Level III", "Level I", "Level I"))
#testDO <- data.frame(DO= c(7,3,4,5,5,3,2,3,NA,5),
#                     DO_RMK = c('Level I', "Level III", "Level II","Level II","Level II","Level II","Level III","Level III", "Level I", "Level I"))
#countSamplesAtLevels(testpH)
#countSamplesAtLevels(testDO)
#countSamplesAtLevels(x[,c(16:17)])



collapseAllDataByLevel <- function(x, colNumbers){
  # Only do this if citmon or NonAgency data
  if(unique(x$STA_LV3_CODE) %in% c("CMON", "NONA", "NONA ")){
    #skip every other column (remarks)
    a <- colNumbers#12:117#(ncol(stationData)-2)
    n <- length(a)
    
    holder <- NA
    
    for(i in a[seq(1,n,2)]){ 
      x <- x[,c(i, i+1)]
      holder[i] <- countSamplesAtLevels(x)
    }
    
    return(paste0(holder[!is.na(holder)], collapse = '; ')) 
  } 
}

#collapseAllDataByLevel(stationData, 12:117)




# consolidate lake counts
lakeConsolidation <- function(x){
  parameterName <- strsplit(names(x)[1], '_')[[1]][1]  
  if(parameterName == 'NUT'){
    parameterName <- strsplit(names(x)[1], '_')[[1]][2] 
  }
  ratio <- paste(x[,1],'/',x[,2], sep='')
  return( paste(parameterName, ': ', ratio, sep='' ))
  
}
