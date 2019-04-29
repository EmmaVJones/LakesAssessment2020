library(shiny)
library(shinyjs)
library(leaflet)
library(mapview)
library(tidyverse)
library(plotly)
library(raster)
library(DT)
library(FSA)


##-------------------------------------------------------------------------------------------------------------------------------------
## Multiple DependentSelectizeArguments.R script


#' Safe subset
#'
#' @param df Dataframe
#' @param column One name of column to subset within
#' @param subset Vector of entries in column to subset to
#'
#' If column not in df, returns back the df
safeSubset <- function(df, column, subset){
  
  testthat::expect_is(df, "data.frame")
  testthat::expect_is(column, "character")
  testthat::expect_equal(length(column), 1)
  
  if(!is.null(subset)){
    testthat::expect_is(subset, "character")
  } else {
    message("Subset is NULL, returning original")
    out <- df
  }
  
  message(" # subsetting # original rows: ",nrow(df) ," column:", column, " by ", paste(subset, collapse = ", "))
  
  col <- df[[column]]
  
  if(!is.null(col)){
    out <- df[col %in% subset,]
    message("Subset rows: ", nrow(out))
  } else {
    message("Column not found:", column)
    out <- df
  }
  
  out
  
}


#' Dynamical Update of a selectInput
#'
#' Shiny Module: useage details at \link{dynamicSelect}
#'
#' @param id shiny id
#'
#' @return dynamicSelectInput
#' @export
dynamicSelectInput <- function(id, label, multiple = FALSE){
  
  ns <- shiny::NS(id)
  
  shiny::selectInput(ns("dynamic_select"), label,
                     choices = NULL, multiple = multiple, width = "100%")
  
}

#' Dynamical Update of a selectInput
#'
#' Shiny Module
#'
#' Use via \code{callModule(dynamicSelect, "name_select", the_data, "cyl")}
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param the_data data.frame containing column of choices
#' @param column The column to select from
#' @param default_select The choices to select on load
#'
#' @seealso \link{dynamicSelectInput}
#'
#' @return the_data filtered to the choice
#' @export
dynamicSelect <- function(input, output, session, the_data, column, default_select = NULL){
  
  ns <- session$ns
  
  ## update input$dynamic_select
  observe({
    shiny::validate(
      shiny::need(the_data(),"Fetching data")
    )
    dt <- the_data()
    
    testthat::expect_is(dt, "data.frame")
    testthat::expect_is(column, "character")
    
    choice <- sort(unique(dt[[column]]))
    
    updateSelectInput(session, "dynamic_select",
                      choices = choice,
                      selected = default_select)
    
  })
  
  new_data <- reactive({
    shiny::validate(
      shiny::need(input$dynamic_select,"Select data"),
      shiny::need(the_data(), "Waiting for data")
    )
    
    sd <- the_data()
    selected <- input$dynamic_select
    
    ## will return sd even if column is NULL
    safeSubset(sd, column, selected)
    
  })
  
  return(new_data)
  
}


##-------------------------------------------------------------------------------------------------------------------------------------


# CONVENTIONALS.XLSX is a tricky one. First format the PHOSPHORUS column as numeric, 2 decimal places.
# Then, save Roger's pull as a .csv. Then copy the original FDT_DATE_TIME from the .xlsx file
# and overwrite that field in the .csv. Last, convert the field in the .csv to Date -> 3/14/01 13:30
# It is better to use read_csv over read_excel bc memory and time loading app
conventionals <- suppressWarnings(read_csv('data/conventionals08152018EVJ.csv'))
conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%y %H:%M")



lakeStations <- readRDS('data/lakeStationsFinal.RDS')%>%
  mutate(FDT_STA_ID=STATION_ID)
lakeStations_shp <- lakeStations
coordinates(lakeStations_shp) <- ~DD_LONG+DD_LAT
proj4string(lakeStations_shp) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")                                 









sigLakeID305B <- subset(lakeStations,SIG_LAKE=="Y")$ID305B

# Loading screen
load_data <- function() {
  Sys.sleep(2)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}


# Map 
stationMapUI <- function(id){
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"), height =400, width = 650))
}

stationMap <- function(input, output, session, userDataset){
  ns <- session$ns
  
  output$map <- renderLeaflet({
    
    leaflet(userDataset()) %>% setView(-79.2,37.7,zoom=7)%>%
      addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
      addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
      addProviderTiles(providers$Stamen.TerrainBackground,group='Stamen Terrain Background')%>%
      addCircleMarkers(data=userDataset(),~DD_LONG,~DD_LAT,radius=5,color='yellow',fillColor="orange",
                       fillOpacity = 1, opacity=1, weight=2, stroke=TRUE, 
                       group="Lake Monitoring Stations",layerId=~STATION_ID,
                       popup=popupTable(userDataset(),zcol=c(1,6,7,13,14,16)))%>%
      addCircleMarkers(data=lakeStations_shp,radius=3,color='orange',fillColor="orange",fillOpacity = 1,stroke=0,
                       group="All Lake Monitoring Stations",layerId=~lakeStations_shp@data$DD_LAT,
                       popup=popupTable(lakeStations_shp@data,zcol=c(1,6,7,13,14,16)))%>%hideGroup('All Lake Monitoring Stations')%>%
      addLayersControl(baseGroups=c('Open Street Map','Esri World Imagery','Stamen Terrain Background'),
                       overlayGroups=c('Lake Monitoring Stations','All Lake Monitoring Stations'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')%>%
      addHomeButton(extent(lakeStations_shp),"All Lake Monitoring Stations")%>%
      mapview::addMouseCoordinates(style='basic')
  })
}






# Establish Assessment Period
assessmentPeriod <- as.POSIXct(c("2011-01-01 00:00:00 UTC","2016-12-31 23:59:59 UTC"),tz='UTC')



withinAssessmentPeriod <- function(x){
  #print(unique(x$FDT_DATE_TIME))
  
  if((range(unique(x$FDT_DATE_TIME2))[1] < assessmentPeriod[1]) | 
     (range(unique(x$FDT_DATE_TIME2))[2] > assessmentPeriod[2])){
    print('Lake data included that falls outside of assessment period. Review input data.')
  }else{print('All input lake data falls within the assessment period.')}
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



#pHrange <- c(6,9)

pH_rangeAssessment <- function(x, pHrange){
  pH <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,FDT_FIELD_PH,LakeStratification)%>% # Just get relavent columns, 
    filter(!is.na(FDT_FIELD_PH))%>% #get rid of NA's
    filter(LakeStratification %in% c("Epilimnion",NA)) %>% # As of 2018 IR pH standards only apply to epilimnion or non stratified lake samples
    mutate(interval=findInterval(FDT_FIELD_PH,pHrange))%>% # Identify where pH outside of assessment range
    mutate(pHrange=ifelse(interval==1,T,F))%>% # Highlight where pH doesn't fall into assessment range
    filter(pHrange==FALSE)%>% # Only return pH measures outside of assessement range
    dplyr::select(-c(interval,pHrange)) # Don't show user interval column, could be confusing to them, T/F in pHrange column sufficient
  
  return(pH)
}


exceedance_pH <- function(x,pHrange){
  pH <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,FDT_FIELD_PH,LakeStratification)%>% # Just get relavent columns, 
    filter(!is.na(FDT_FIELD_PH)) #get rid of NA's
  pH_rangeAssess <- pH_rangeAssessment(x,pHrange)
  
  pH_results <- assessmentDetermination(pH %>% filter(!is.na(FDT_FIELD_PH))%>% #get rid of NA's
                                          filter(LakeStratification %in% c("Epilimnion",NA)),
                                        pH_rangeAssess,"pH","Aquatic Life")
  #data.frame(nSamples = nrow(pH),nExceedance = nrow(pH_rangeAssess))%>%
  #mutate(exceedanceRate = (nExceedance/nSamples)*100,
  #       pH_Determination = ifelse(exceedanceRate > 10.5 & nSamples > 2, 
  #                                 'Water impaired for pH','Water not imparied for pH'))
  return(pH_results)
}


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

#Max Temperature Exceedance Function
temp_Assessment <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME,FDT_DEPTH,FDT_TEMP_CELCIUS)%>% # Just get relavent columns, 
    filter(!is.na(FDT_TEMP_CELCIUS))%>% #get rid of NA's
    mutate(TemperatureExceedance=ifelse(FDT_TEMP_CELCIUS>32,T,F))%>% # Identify where above max Temperature, 9VAC25-260-50 ClassIII= 32C
    filter(TemperatureExceedance==TRUE) # Only return temp measures above threshold
  return(temp)
}

# Exceedance Rate Temperature
exceedance_temp <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME,FDT_DEPTH,FDT_TEMP_CELCIUS)%>% # Just get relavent columns, 
    filter(!is.na(FDT_TEMP_CELCIUS)) #get rid of NA's
  temp_Assess <- temp_Assessment(x)
  
  temp_results <- assessmentDetermination(temp,temp_Assess,"temperature","Aquatic Life")
  #data.frame(nSamples = nrow(temp),nExceedance = nrow(temp_Assess))%>%
  #mutate(exceedanceRate = (nExceedance/nSamples)*100,
  #       temperature_Determination = ifelse(exceedanceRate > 10.5 & nSamples > 2, 
  #                                          'Water impaired for temperature','Water not imparied for temperature'))
  return(temp_results)
}

#Min DO Exceedance Function
DO_Assessment <- function(x,qualifier){ # qualifier allows you to run the analysis including many or few stratification qualifications
  DO <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,DO,LakeStratification,FDT_DATE_TIME2)%>% # Just get relevant columns, 
    filter(!is.na(DO))%>% #get rid of NA's
    filter(LakeStratification %in% qualifier)%>%
    ##filter(LakeStratification=="Epilimnion") # Only assess Epilimnion
    ##filter(LakeStratification %in% c("Epilimnion",NA))%>% # Only assess Epilimnion or NaN (no stratification)
    mutate(DOExceedance=ifelse(DO<4,T,F))%>% # Identify where above max Temperature, 9VAC25-260-50 ClassIII= 32C
    filter(DOExceedance==TRUE) # Only return temp measures above threshold
  
  DO$FDT_DATE_TIME <- as.character(DO$FDT_DATE_TIME2)
  DO <- dplyr::select(DO,-c(DOExceedance,FDT_DATE_TIME2)) # Don't show user column, could be confusing to them
  
  return(DO)
}

# Exceedance Rate DO
exceedance_DO <- function(x, qualifier){
  DO <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,DO,LakeStratification)%>% # Just get relevant columns, 
    filter(!is.na(DO))%>% #get rid of NA's
    filter(LakeStratification %in% qualifier)
  ##filter(LakeStratification %in% c("Epilimnion",NA)) # Only assess Epilimnion or NaN (no stratification)
  ##filter(LakeStratification=="Epilimnion") # Only assess Epilimnion
  DO_Assess <- DO_Assessment(x,qualifier)
  DO_results <- assessmentDetermination(DO,DO_Assess,"Dissolved Oxygen","Aquatic Life")
  return(DO_results)
}



# How bacteria is assessed
bacteria_Assessment <- function(x){
  bacteria <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,E.COLI,SampleDate)%>% # Just get relavent columns, 
    filter(!is.na(E.COLI)) #get rid of NA's
  if(nrow(bacteria)>0){
    bacteria <- mutate(bacteria,singleSampleMaximum=ifelse(E.COLI>235,T,F), # Find any single sample exceedances
                       previousSample=lag(SampleDate,1),previousSampleECOLI=lag(E.COLI,1))%>% # Line up previous sample with current sample line
      rowwise() %>% 
      mutate(sameSampleMonth= as.numeric(strsplit(SampleDate,'/')[[1]][1])  -  as.numeric(strsplit(previousSample,'/')[[1]][1])) %>% # See if sample months are the same, e.g. more than one sample per calendar month
      filter(singleSampleMaximum == T | sameSampleMonth == 0 | is.na(sameSampleMonth)) %>% # keep only rows with single sample exceedances or 
      # multiple samples per calendar month  or no previous sample (NA) to then test for geomean
      rowwise() %>% 
      mutate(geoMeanCalendarMonth=ifelse(!is.na(sameSampleMonth),FSA::geomean(c(E.COLI,previousSampleECOLI)),NA))%>% # Calculate geomean only if sameSampleMonth != NA, otherwise will bomb out
      filter(singleSampleMaximum == T | geoMeanCalendarMonth > 126) %>% #find exceedances to either rule
      dplyr::select(FDT_STA_ID ,SampleDate, E.COLI) # only keep columns that will be important to assessors
    return(bacteria)}else{
      return('No bacteria data for station.')
    }
}



# How bacteria should be assessed
#bacteria_Assessment <- function(x){
#  bacteria <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,E.COLI)%>% # Just get relavent columns, 
#    filter(!is.na(E.COLI))%>% #get rid of NA's
#    mutate(singleSampleMaximum=ifelse(E.COLI>235,T,F), # Find any single sample exceedances
#           previousSampleDate=lag(FDT_DATE_TIME,1),previousSampleECOLI=lag(E.COLI,1), # Line up previous sample with current sample line
#           timeDiff=as.numeric(FDT_DATE_TIME-previousSampleDate))%>% # Calculate n days between sampling events
#    filter(singleSampleMaximum==T | timeDiff < 30)%>% # keep only rows with single sample exceedances or <30day samples to then test for geomean
#    rowwise() %>% mutate(geoMean30day=FSA::geomean(c(E.COLI,previousSampleECOLI)))%>% # Calculate geomean
#    filter(singleSampleMaximum==T | geoMean30day > 126) %>% #find exceedances to either rule
#    dplyr::select(-c(FDT_DEPTH,singleSampleMaximum,  previousSampleDate, previousSampleECOLI ))
#  return(bacteria)
#}

# Exceedance Rate Bacteria
exceedance_bacteria <- function(x){
  bacteria <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,E.COLI)%>% # Just get relavent columns, 
    filter(!is.na(E.COLI))
  if(nrow(bacteria)>0){
    bacteria_Assess <- bacteria_Assessment(x)
    bacteria_results <- assessmentDetermination(bacteria,bacteria_Assess,"E.Coli","Recreation")
    return(bacteria_results)
  }else{
    return('No bacteria data for station.')
  }
  
}


chlA_Assessment_OneStation <- function(x){
  #if(!is.na(unique(x$Assess_TYPE)) & unique(x$Assess_TYPE)=="LZ"){
  chlA <- filter(x, !is.na(CHLOROPHYLL))%>%
    dplyr::select(FDT_STA_ID,FDT_DEPTH,FDT_DATE_TIME, SampleDate,CHLOROPHYLL,71,Assess_TYPE)%>%
    mutate(Year=format(FDT_DATE_TIME,"%Y"),Month=format(FDT_DATE_TIME,'%m'))%>%
    filter(Month %in% c('04','05','06','07','08','09','10'))%>% # make sure only assess valid sample months
    group_by(Year)%>%
    mutate(samplePerYear= n(),pct90=quantile(CHLOROPHYLL,0.9),
           chlA_Exceedance=ifelse(pct90>Chlorophyll_A_limit,T,F),
           LacustrineZone=ifelse(!is.na(unique(x$Assess_TYPE)) & unique(x$Assess_TYPE)=="LZ",TRUE,FALSE))%>%
    dplyr::select(FDT_STA_ID,Year,samplePerYear,pct90,Chlorophyll_A_limit,chlA_Exceedance,LacustrineZone)%>%
    distinct(Year,.keep_all=T)
  return(chlA)
  #}else{
  #  dat <- data.frame( FDT_STA_ID=x$FDT_STA_ID[1], Year=NA, samplePerYear=NA, 
  #                     pct90=NA, Chlorophyll_A_limit=NA, chlA_Exceedance=NA)
  #  
  #  return(dat)
  #}
}

chlA_Assessment <- function(x){
  holder <- list()
  for(i in 1:length(unique(x$FDT_STA_ID))){
    dat <- filter(x,FDT_STA_ID %in% unique(x$FDT_STA_ID)[i])
    holder[[i]] <-  as.data.frame(chlA_Assessment_OneStation(dat))
  }
  alldat <- do.call(rbind,holder)#%>%filter(!is.na(Year))
  return(alldat)
}


exceedance_chlA <- function(x){
  chlA_Assess <- chlA_Assessment(x)
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
  holder <- list()
  for(i in 1:length(unique(x$FDT_STA_ID))){
    dat <- filter(x,FDT_STA_ID %in% unique(x$FDT_STA_ID)[i])
    holder[[i]] <-  as.data.frame(TP_Assessment_OneStation(dat))
  }
  alldat <- do.call(rbind,holder)#%>%filter(!is.na(Year))
  return(alldat)
}


exceedance_TP <- function(x){
  TP_Assess <- TP_Assessment(x)
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
  do.call(rbind,holder) # output table for user to review
}













temperatureSubTabUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             h5(strong("Thermocline Analysis")),
             DT::dataTableOutput(ns('thermoclineResults'))),
      column(6,
             uiOutput(ns('stationDateSelection')),
             plotlyOutput(ns('thermoclinePlotly'))))
  )
}


temperatureSubTab <- function(input,output,session, oneStationData){
  ns <- session$ns
  
  # Thermocline Analysis Table
  output$thermoclineResults <- DT::renderDataTable({
    req(oneStationData())
    dat <- dplyr::select(oneStationData(),SampleDate,ThermoclineDepth)%>%
      distinct(SampleDate,.keep_all = TRUE)
    dat[is.nan(dat$ThermoclineDepth),2] <- 'No Thermocline'
    
    
    datatable(dat, extensions = 'Buttons', escape=F, rownames = F,
              options=list(dom='Bt',
                           buttons=list('copy'),
                           pageLength=length(unique(dat$SampleDate)),
                           scrollY = "300px"))%>%
      formatStyle("ThermoclineDepth",backgroundColor=styleEqual('No Thermocline',"red"))
  })
  
  
  # Thermocline date option dropdown
  output$stationDateSelection <- renderUI({
    req(oneStationData())
    selectInput(ns('thermoDateSelection'),strong("Choose a Sample Date to Plot"),choices = unique(oneStationData()$SampleDate))
  })
  
  
  
  # Thermoncline plotly based on user date input
  output$thermoclinePlotly <- renderPlotly({
    req(oneStationData(),input$thermoDateSelection)
    dat <- dplyr::filter(oneStationData(),SampleDate %in% input$thermoDateSelection)
    
    if(dat$ThermoclineDepth[1] != 'NaN'){
      suppressWarnings(
        plot_ly(data=dat)%>%
          add_lines(x=~FDT_TEMP_CELCIUS,y=~ThermoclineDepth, mode='line',line = list(color = '#E50606'),
                    hoverinfo = "Thermocline", name="Thermocline")%>%
          add_markers(x= ~FDT_TEMP_CELCIUS, y= ~FDT_DEPTH,mode = 'scatter', name="Temperature",
                      color=~LakeStratification, colors=c('#BF382A', '#0C4B8E'),
                      hoverinfo="text",text=~paste(sep="<br>",paste("Depth: ",FDT_DEPTH, "m"),
                                                   paste("Temperature:",FDT_TEMP_CELCIUS,"C"),
                                                   paste("LakeStratification: ",LakeStratification)))%>%
          layout(xaxis = list(autorange = "reversed",title="Temperature (Celcius)"),
                 yaxis = list(autorange = "reversed",title="Depth (m)"),
                 showlegend=FALSE
          ))
    }else{
      suppressWarnings(
        plot_ly(data=dat,y=~FDT_DEPTH,x=~FDT_TEMP_CELCIUS,type='scatter',mode='markers',
                hoverinfo="text",text=~paste(sep="<br>",paste("Depth: ",FDT_DEPTH, "m"),
                                             paste("Temperature:",FDT_TEMP_CELCIUS,"C")))%>%
          layout(xaxis = list(autorange = "reversed",title="Temperature (Celcius)"),
                 yaxis = list(autorange = "reversed",title="Depth (m)"),
                 showlegend=FALSE))}
  })
  
  
}




DOSubTabUI <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns('DOplotly')),
    fluidRow(column(3),column(4,actionButton(ns('zoomPlotDO'),"Zoomed Plot by Sample Date",class='btn-block'))),
    br(),hr(),br(),
    h5('All DO records that fall below the criteria for the ',span(strong('selected site')),' are highlighted below.'),
    div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('DOrangeTableSingleSite')))
  )
}

DOSubTab <- function(input,output,session, oneStationData){
  ns <- session$ns
  
  output$DOplotly <- renderPlotly({
    req(oneStationData)
    dat <- mutate(oneStationData(),bottom=4)
    dat$SampleDate <- as.POSIXct(dat$SampleDate, format="%m/%d/%y")
    if(any(is.na(dat$LakeStratification))){
      dat[is.na(dat$LakeStratification),]$LakeStratification <- 'NONE'
    }
    dat$LakeStratification <- as.factor(dat$LakeStratification)
    dat$LakeStratification <- factor(dat$LakeStratification,levels=c("Epilimnion","NONE","Hypolimnion"))#,ordered=T)
    
    plot_ly(data=dat)%>%
      add_lines(x=~SampleDate,y=~bottom, mode='line',line = list(color = '#E50606'),
                hoverinfo = "none", name="DO Standard")%>%
      add_markers(x= ~SampleDate, y= ~DO,mode = 'scatter', name="Dissolved Oxygen",
                  color=~LakeStratification,# colors=c('#BF382A', '#0C4B8E','#C4C4C4'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Dissolved Oxygen: ",DO,"mg/L"),
                                               paste("LakeStratification: ",LakeStratification)))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Dissolved Oxygen (mg/L)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))
  })
  
  
  observeEvent(input$zoomPlotDO,{
    showModal(modalDialog(
      title="Zoomed Plot for Single Sample Date",
      uiOutput(ns('stationDateSelectionDO')),
      hr(),
      plotlyOutput(ns('DOplotlyByDate')),
      easyClose = TRUE))
  })
  
  # DO date option dropdown, inside modal
  output$stationDateSelectionDO <- renderUI({
    req(oneStationData())
    selectInput(ns('DOdateSelection'),"Choose a Sample Date to Plot",choices = unique(oneStationData()$SampleDate))
  })
  
  # Plotly DO by single sample date
  output$DOplotlyByDate <- renderPlotly({
    req(input$DOdateSelection)
    dat <- filter(oneStationData(),SampleDate %in% input$DOdateSelection)%>%
      mutate(bottom=4)
    
    if(dat$ThermoclineDepth[1] != 'NaN'){
      plot_ly(data=dat)%>%
        add_lines(x=~FDT_TEMP_CELCIUS,y=~bottom, mode='line',line = list(color = '#E50606'),
                  hoverinfo = "none", name="DO Standard")%>%
        add_markers(x= ~FDT_TEMP_CELCIUS, y= ~DO,mode = 'scatter', name="Dissolved Oxygen",
                    color=~LakeStratification, colors=c('#BF382A', '#0C4B8E'),
                    hoverinfo="text",text=~paste(sep="<br>",paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Dissolved Oxygen:",DO,"mg/L"),
                                                 paste("LakeStratification: ",LakeStratification)))%>%
        layout(xaxis = list(autorange = "reversed",title="Temperature (C)",tickfont = list(size = 10)),
               showlegend=FALSE,
               yaxis=list(title="Dissolved Oxygen (mg/L)"))
      
    }else{
      plot_ly(data=dat)%>%
        add_lines(x=~FDT_TEMP_CELCIUS,y=~bottom, mode='line',line = list(color = '#E50606'),
                  hoverinfo = "none", name="DO Standard")%>%
        add_markers(x= ~FDT_TEMP_CELCIUS, y= ~DO,mode = 'scatter', name="Dissolved Oxygen",
                    hoverinfo="text",text=~paste(sep="<br>",paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Dissolved Oxygen:",DO,"mg/L"),
                                                 paste("LakeStratification: None")))%>%
        layout(xaxis = list(autorange = "reversed",title="Temperature (C)",tickfont = list(size = 10)),
               showlegend=FALSE,
               yaxis=list(title="Dissolved Oxygen (mg/L)"))        }
    
  })
  
  # Highlight records that don't meet DO criteria
  output$DOrangeTableSingleSite <- renderTable({
    req(oneStationData())
    DO_Assessment(oneStationData(),c("Epilimnion",NA))%>%dplyr::select(-FDT_STA_ID)})
}







pHSubTabUI <- function(id){
  ns <- NS(id)
  tagList( plotlyOutput(ns('pHplot'),height = "400px"))
}




pHSubTab <-  function(input,output,session, oneStationData, pHrange){
  ns <- session$ns
  
  output$pHplot <- renderPlotly({
    req(oneStationData())
    dat <- mutate(oneStationData(),top=pHrange()[2],bottom=pHrange()[1])
    dat$FDT_DATE_TIME <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y %H:%M")
    
    if(any(is.na(dat$LakeStratification))){ dat[is.na(dat$LakeStratification),]$LakeStratification <- 'NONE' }
    if(any(is.nan(dat$LakeStratification))){ dat[is.nan(dat$LakeStratification),]$LakeStratification <- 'NONE'}
    
    dat$LakeStratification <- as.factor(dat$LakeStratification)
    dat$LakeStratification <- factor(dat$LakeStratification,levels=c("Epilimnion","NONE","Hypolimnion"))#,ordered=T)
    
    suppressWarnings(
      plot_ly(dat)%>%
        add_lines(x=~FDT_DATE_TIME,y=~top, mode='line',line = list(color = '#E50606'),
                  hoverinfo = "none", name="pH Standard")%>%
        add_lines(x=~FDT_DATE_TIME,y=~bottom, mode='line',line = list(color = '#E50606'),
                  hoverinfo = "none", name="pH Standard")%>%
        add_markers(x= ~FDT_DATE_TIME, y= ~FDT_FIELD_PH,mode = 'scatter', name="pH",
                    color=~LakeStratification, colors=c('#BF382A', '#0C4B8E'),
                    hoverinfo="text",text=~paste(sep="<br>",paste("Date: ",FDT_DATE_TIME),
                                                 paste(FDT_FIELD_PH,"standard units"),
                                                 paste("Depth: ",FDT_DEPTH),
                                                 paste("LakeStratification: ",LakeStratification)))%>%
        layout(showlegend = FALSE,
               xaxis = list(title = "Date",tickfont = list(size = 10)),
               yaxis = list(title = "pH (standard units)")))
  })
}



bacteriaSubTabUI <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns('bacteriaPlotly')))
}


bacteriaSubTab <- function(input,output,session, oneStationData){
  ns <- session$ns
  
  output$bacteriaPlotly <- renderPlotly({
    req(oneStationData())
    dat <- mutate(oneStationData(),top=235)%>%filter(!is.na(E.COLI))
    
    if(nrow(dat)>0){
      dat$SampleDate <- as.POSIXct(dat$SampleDate, format="%m/%d/%y")
      
      plot_ly(data=dat)%>%
        add_lines(x=~SampleDate,y=~top, mode='line',line = list(color = '#E50606'),
                  hoverinfo = "none", name="E.coli Single Sample Maximum")%>%
        add_markers(x= ~SampleDate, y= ~E.COLI,mode = 'scatter', name="E.coli",
                    colors='blue', 
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("E.coli: ",E.COLI,"cfu/100mL")))%>%
        layout(showlegend=FALSE,
               yaxis=list(title="E.coli (cfu/100mL)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10)))
    }else{
      # dummy plot bc no data
      dat <- mutate(oneStationData(),top=235)
      plot_ly(data=dat)%>%
        add_lines(x=~SampleDate,y=~top, mode='line',line = list(color = '#E50606'),
                  hoverinfo = "none", name="E.coli Single Sample Maximum")%>%
        layout(showlegend=FALSE,
               yaxis=list(title="E.coli (cfu/100mL)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10)))
    }
    
  })
}





chlaSingleStationSubTabUI <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns('chlaPlotly')),
    h5('Annual Chlorophyll a 90th percentiles for the selected site are reported below.'),
    helpText('If no data is reported, the station does not fall within the lacustrine zone
             for the lake and is thus not assessed for Chlorophyll a.'),
    div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('singleStationAnnualChla90thTable')))
    )
}




chlaSingleStationSubTab <- function(input,output,session, oneStationData){
  ns <- session$ns
  
  output$chlaPlotly <- renderPlotly({
    req(oneStationData())
    dat <- oneStationData()
    dat$SampleDate <- as.POSIXct(dat$SampleDate, format="%m/%d/%y")
    
    plot_ly(data=dat)%>%
      add_lines(x=~SampleDate,y=~Chlorophyll_A_limit, mode='line',line = list(color = '#E50606'),
                hoverinfo = "text", name="Lake Specific Chlorophyll a Limit")%>%
      add_markers(x= ~SampleDate, y= ~CHLOROPHYLL,mode = 'scatter', name="Chlorophyll a",
                  color='blue',
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Chlorophyll a: ",CHLOROPHYLL,"ug/L")))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Chlorophyll a (ug/L)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))
  })
  
  output$singleStationAnnualChla90thTable <- renderTable({
    req(oneStationData())
    chlA_Assessment_OneStation(oneStationData()) })
}



chlaSubTabUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             helpText('The table below summarizes the last two years of Chlorophyll a sampling data.
                      All StationIDs are first analyzed by sampling window (April - October), then 
                      each year is verified that there are enough samples (> 6) within the window before the 90th 
                      percentile of Chlorophyll a measures within the top meter is calculated. Only StationIDs that
                      fall within the lacustrine zone are used for this analysis. The criteria are based
                      on individual limits specified for each Section 187 Lake (9VAC25-260-187). If the 
                      assessment of the last two years conflict, the last three years are analyzed and 
                      reported below.'),
             fluidRow(
               h5(strong('Chlorophyll a Annual Summary for All Stations in Lake'))),
             #downloadButton("downloadChlaData", "Download all data used for Chlorophyll a assessment")),
             div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('chla90thTable')))),
      column(6,
             helpText('The exceedance analysis below is based on data reported in the Chlorophyll a
                      Annual Summary table immediately to the left. This exceedance calculation
                      pools all data that meets the above mentioned requirements across all lake sites
                      in the lacustrine zone.'),
             wellPanel(
               h5('Station Exceedance Rate:'),
               uiOutput(ns('stationChlaExceedanceRateSelect_UI')),
               tableOutput(ns('stationChlaExceedanceRate'))),
             h5('Lake Exceedance Rate ',span(strong('(ALL STATIONS):'))),
             tableOutput(ns('chla_exceedanceRateALL')),
             h5('Lake Exceedance Rate ',span(strong('(ONLY LACUSTRINE ZONE STATIONS):'))),
             tableOutput(ns('chla_exceedanceRate'))))
    
      )
}


chlaSubTab <- function(input,output,session, allLakeLZData,stationData,stationDataDailySample){
  ns <- session$ns
  
  output$chla90thTable <- renderTable({
    req(allLakeLZData())
    exceedance_chlA(allLakeLZData())})
  
  #output$downloadChlaData <- downloadHandler(filename=function(){paste('ChlaData_',Sys.Date(),'.csv',sep='')},
  #                                           content=function(file){
  #                                             write.csv(allLakeLZData(),file)})
  
  # Chlorophyll a Exceedance Results by Station
  output$stationChlaExceedanceRateSelect_UI <- renderUI({
    req(stationData())
    selectInput(session$ns('stationChlaExceedanceRateSelect'),strong('Select Station to Review for individual Chlorophyll a exceedance statistics'),
                choices=unique(stationData())$FDT_STA_ID,width='300px')})
  
  output$stationChlaExceedanceRate <- renderTable({
    req(stationDataDailySample(),input$stationChlaExceedanceRateSelect)
    LZ <- exceedance_chlA(filter(stationDataDailySample(),FDT_STA_ID %in% input$stationChlaExceedanceRateSelect))
    
    if(class(LZ) == 'character'){
      return("No Chlorophyll a data to assess.")
    }else{
      LZ <- LZ$LacustrineZone[1]
      assessmentDetermination(exceedance_chlA(filter(stationDataDailySample(),FDT_STA_ID %in% input$stationChlaExceedanceRateSelect)),
                              filter(exceedance_chlA(filter(stationDataDailySample(),FDT_STA_ID %in% input$stationChlaExceedanceRateSelect)),
                                     chlA_Exceedance ==T),'Chlorophyll a','Aquatic Life')%>%
        mutate(LacustrineZone=LZ) %>%# for chla and TP only, show whether or not in lacustrine zone with single station exceedance
        dplyr::select(nSamples,nExceedance,exceedanceRate, LacustrineZone) # don't give assessment determination for single station}})
    }
  })
  
  
  # All Lake Chlorophyll a exceedance rate REGARDLESS OF WHETHER OR NOT STATIONS ARE IN LACUSTRINE ZONE 
  output$chla_exceedanceRateALL <- renderTable({
    req(allLakeLZData())
    z <- exceedance_chlA(allLakeLZData())
    if(class(z)=='character'){
      return("No Chlorophyll a data to assess.")
    }else{
      assessmentDetermination(z,filter(exceedance_chlA(allLakeLZData()),chlA_Exceedance ==T),'Chlorophyll a','Aquatic Life')}})
  
  
  
  # All Lake LACUSTRINE ZONE Chlorophyll a exceedance rate
  output$chla_exceedanceRate <- renderTable({
    req(allLakeLZData())
    z <- exceedance_chlA(allLakeLZData())
    if(class(z)=='character'){
      return("No Chlorophyll a data to assess.")
    }else{
      assessmentDetermination(filter(exceedance_chlA(allLakeLZData()), LacustrineZone == TRUE),
                              filter(exceedance_chlA(allLakeLZData()),chlA_Exceedance ==T & LacustrineZone == TRUE),'Chlorophyll a','Aquatic Life')}})
}






TPSingleStationSubTabUI <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns('TPplotly')),
    h5('Annual median Total Phosphorus for the selected site are reported below.'),
    div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('singleStationAnnualMedianTPtable')))
  )
}




TPSingleStationSubTab <- function(input,output,session, oneStationData){
  ns <- session$ns
  
  output$TPplotly <- renderPlotly({
    req(oneStationData())
    dat <- mutate(oneStationData(),TPhosphorus_limit_ug_L=TPhosphorus_limit/1000)
    dat$SampleDate <- as.POSIXct(dat$SampleDate, format="%m/%d/%y")
    
    plot_ly(data=dat)%>%
      add_lines(x=~SampleDate,y=~TPhosphorus_limit_ug_L, mode='line',line = list(color = '#E50606'),
                hoverinfo = "text", name="Lake Specific Total Phosphorus Limit")%>%
      add_markers(x= ~SampleDate, y= ~PHOSPHORUS,mode = 'scatter', name="Total Phosphorus",
                  color='blue',
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Total Phosphorus: ",PHOSPHORUS,"ug/L")))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Total Phosphorus (ug/L)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))
  })
  
  output$singleStationAnnualMedianTPtable <- renderTable({
    req(oneStationData())
    TP_Assessment_OneStation(oneStationData()) })
}



TPSubTabUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             helpText('The table below summarizes the last two years of Total Phosphorus sampling data.
                      All StationIDs are first analyzed by sampling window (April - October), then 
                      each year is verified that there are enough samples (> 6) within the window before the  
                      median of Total Phosphorus measures within the top meter is calculated. Only StationIDs that
                      fall within the lacustrine zone are used for this analysis. The criteria are based
                      on individual limits specified for each Section 187 Lake (9VAC25-260-187). If the 
                      assessment of the last two years conflict, the last three years are analyzed and 
                      reported below.'),
             h5(strong('Total Phosphorus Annual Summary for All Lacustrine Stations in Lake')),
             div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('medianTPtable'))),
             helpText("To view the data analyzed for the lacustrine Total Phosphorus analysis, click the
                      'Download all data used for Chlorophyll a and Total Phosphorus assessment' button 
                      in the Chlorophyll a section.")),
      column(6,
             helpText('The exceedance analysis below is based on data reported in the Total Phosphorus
                      Annual Summary table immediately to the left. This exceedance calculation
                      pools all data that meets the above mentioned requirements across all lake sites
                      in the lacustrine zone.'),
             wellPanel(
               h5('Station Exceedance Rate:'),
               uiOutput(ns('stationTPExceedanceRateSelect_UI')),
               tableOutput(ns('stationTPExceedanceRate'))),
             h5('Lake Exceedance Rate ',span(strong('(ALL STATIONS):'))),
             tableOutput(ns('TP_exceedanceRateALL')),
             h5('Lake Exceedance Rate ',span(strong('(ONLY LACUSTRINE ZONE STATIONS):'))),
             tableOutput(ns('TP_exceedanceRate'))))
             )
}

TPSubTab <- function(input,output,session, allLakeLZData,stationData,stationDataDailySample){
  ns <- session$ns
  
  output$medianTPtable <- renderTable({
    req(allLakeLZData())
    exceedance_TP(allLakeLZData())})
  
  # Total Phosphorus Exceedance Results by Station
  output$stationTPExceedanceRateSelect_UI <- renderUI({
    req(stationData())
    selectInput(session$ns('stationTPExceedanceRateSelect'),strong('Select Station to Review for individual E.coli exceedance statistics'),
                choices=unique(stationData())$FDT_STA_ID,width='300px')})
  
  output$stationTPExceedanceRate <- renderTable({
    req(stationDataDailySample(),input$stationTPExceedanceRateSelect)
    LZ <- exceedance_TP(filter(stationDataDailySample(),FDT_STA_ID %in% input$stationTPExceedanceRateSelect))$LacustrineZone[1]
    assessmentDetermination(exceedance_TP(filter(stationDataDailySample(),FDT_STA_ID %in% input$stationTPExceedanceRateSelect)),
                            filter(exceedance_TP(filter(stationDataDailySample(),FDT_STA_ID %in% input$stationTPExceedanceRateSelect)),TP_Exceedance ==T),
                            'Total Phosphorus','Aquatic Life')%>%
      mutate(LacustrineZone=LZ) %>%# for chla and TP only, show whether or not in lacustrine zone with single station exceedance
      dplyr::select(nSamples,nExceedance,exceedanceRate, LacustrineZone) # don't give assessment determination for single station
  })
  
  # All Lake Total Phosphorus exceedance rate REGARDLESS OF WHETHER OR NOT STATIONS ARE IN LACUSTRINE ZONE 
  output$TP_exceedanceRateALL <- renderTable({
    req(allLakeLZData())
    assessmentDetermination(exceedance_TP(allLakeLZData()),filter(exceedance_TP(allLakeLZData()),TP_Exceedance ==T),'Total Phosphorus','Aquatic Life')})
  
  
  # All Lake LACUSTRINE ZONE Total Phosphorus Exceedance Results 
  output$TP_exceedanceRate <- renderTable({
    req(allLakeLZData())
    assessmentDetermination(filter(exceedance_TP(allLakeLZData()), LacustrineZone == TRUE),
                            filter(exceedance_TP(allLakeLZData()),TP_Exceedance ==T & LacustrineZone == TRUE),'Total Phosphorus','Aquatic Life')})
  
}






































ui <- shinyUI(fluidPage(theme="yeti.css",
                  shinyjs::useShinyjs(),
                  div(
                    id = "loading_page",
                    h1("Loading...")
                  ),
                  hidden(
                    div(
                      id = "main_content",
                      navbarPage("VDEQ 2018 Lake Assessment Tool",
                                 
                                 tabPanel('Lake Selection',
                                          sidebarPanel(h4('Instructions:'),
                                                       p("Use the drop down box to select an Assessment Unit (AU) to assess. All AU's are organized by lake. 
                                                         The map will update based on your selection. Once you have reviewed the data below the map, proceed 
                                                         to the 'Lake Assessment' Tab to begin analyzing the AU."),
                                                       dynamicSelectInput("lakeSelection", "Select Lake to Assess", multiple = FALSE),
                                                       dynamicSelectInput("AUselection", "Select Assessment Unit to Assess", multiple = FALSE)
                                                       ),
                                          mainPanel(
                                            stationMapUI("lakeMap"),
                                            DT::dataTableOutput("lakeDetails",width=650)
                                          )),
                                 tabPanel('Lake Assessment',
                                          fluidRow(column(4,h4('User Selected ID305B:')),
                                                   column(4,h4(textOutput({'ID305Bselected'}))),
                                                   column(4,actionButton('retrieveData',label='Retrieve Data'))),
                                          DT::dataTableOutput('lakeDetails2'),
                                          
                                          br(),br(),
                                          tabsetPanel(
                                            tabPanel('Data',br(),
                                                     DT::dataTableOutput('stationDataTable'),
                                                     h4('Data Summary'),
                                                     h5('Records Retrieved in Assessment Unit:'),
                                                     fluidRow(column(1),column(10,textOutput('stationDataTableRecords'))),
                                                     h5('Individual Station Records Retrieved within Assessment Unit'),
                                                     fluidRow(column(1),column(10,tableOutput('uniqueStationDataTableRecords'))),
                                                     h5('Assessment Window:'),
                                                     fluidRow(column(1),column(10,textOutput('stationDataTableAssessmentWindow'))),br(),br()),
                                            tabPanel('Temperature',
                                                     helpText('Review each site using the single site visualization section, then 
                                                              proceed to the bottom of the page to find exceedance rate for the entire assessment unit.',br(),
                                                              span(strong('NOTE: The temperature exceedance analysis results at the bottom of the page include data
                                                                          from ALL stations within the assessment unit.'))),
                                                     wellPanel(
                                                       h4(strong('Single Station Data Visualization')),
                                                       uiOutput('oneStationSelectionUI'),
                                                       temperatureSubTabUI('temp')),
                                                     br(),hr(),br(),
                                                     h5(strong("Temperature Exceedance Analysis")),
                                                     fluidRow(
                                                       column(6,
                                                              h5('All temperature records that exceed the threshold for the',span(strong('assessment unit')),' are highlighted below. 
                                                                 If no records are presented in the table below, then no data exceedes the temperature threshold.'),
                                                              tableOutput('tempRangeTable')),
                                                       column(6,
                                                              wellPanel(
                                                                h5('Station Exceedance Rate:'),
                                                                uiOutput('stationTempExceedanceRateSelect_UI'),
                                                                tableOutput("stationTempExceedanceRate")),
                                                              hr(),
                                                              h5('Assessment Unit Exceedance Rate:'),
                                                              tableOutput("tempExceedanceRate")))),
                                            #tableOutput("tempExceedanceRate")),
                                            tabPanel('Dissolved Oxygen',
                                                     helpText('Review each site using the single site visualization section, then 
                                                              proceed to the bottom of the page to find exceedance rate for the entire assessment unit.',br(),
                                                              span(strong('NOTE: The dissolved oxygen exceedance analysis results at the bottom of the page include data
                                                                          from ALL stations within the assessment unit.'),br(), 'The data is run as both a
                                                                   stratified and unstratified lake. See the table for a full results report.')),
                                                     wellPanel(
                                                       h4(strong('Single Station Data Visualization')),
                                                       uiOutput('DO_oneStationSelectionUI'),
                                                       DOSubTabUI('DO')),
                                                     br(),hr(),br(),
                                                     h5(strong("Dissolved Oxygen Exceedance Analysis")),
                                                     fluidRow(
                                                       column(6,
                                                              h5('All DO records that fall below the criteria for the ',span(strong('assessment unit')),' are highlighted below.'),
                                                              div(style = 'height:300px;overflow-y: scroll', tableOutput('DOrangeTable'))),
                                                       column(6,
                                                              wellPanel(
                                                                h5('Station Exceedance Rate:'),
                                                                uiOutput('stationDOExceedanceRateSelect_UI'),
                                                                h6("Results for selected station data in epilimnion throughout sampling season."),
                                                                tableOutput('stationDOexceedanceRate_epi'),
                                                                h6("Results for selected station data in epilimnion and unstratified sample profiles throughout sampling season."),
                                                                tableOutput('stationDOexceedanceRate_epiAndNA'),
                                                                h6("Results for selected station data throughout sampling season."),
                                                                tableOutput('stationDOexceedanceRate_noStrat')),
                                                              hr(),
                                                              h5('Assessment Unit Exceedance Rate:'),
                                                              h6("Results for all data in epilimnion throughout sampling season."),
                                                              tableOutput('DOexceedanceRate_epi'),
                                                              h6("Results for all data in assessment unit in epilimnion and unstratified sample profiles throughout sampling season."),
                                                              tableOutput('DOexceedanceRate_epiAndNA'),
                                                              h6("Results for all data in assessment unit throughout sampling season."),
                                                              tableOutput('DOexceedanceRate_noStrat')))),
                                            tabPanel('pH',
                                                     helpText('Review each site using the single site visualization section, then 
                                                              proceed to the bottom of the page to find exceedance rate for the entire assessment unit.',br(),
                                                              span(strong('NOTE: The pH exceedance analysis results at the bottom of the page include data
                                                                          from ALL stations within the assessment unit.'))),
                                                     wellPanel(
                                                       h4(strong('Single Station Data Visualization')),
                                                       fluidRow(column(6,uiOutput('pH_oneStationSelectionUI')),
                                                                column(6,selectInput('pHrange',strong("Select the appropriate pH range for the assessment unit"),
                                                                                     choices=c('6 - 9','6.5 - 9.5'),width='350px'))),
                                                       pHSubTabUI('pH')),
                                                     br(),hr(),br(),
                                                     h5(strong("pH Exceedance Analysis")),
                                                     fluidRow(
                                                       column(6,
                                                              h5('All pH records that fall out of the user selected pH range for the ',span(strong('assessment unit')),' are highlighted below.'),
                                                              tableOutput('pHRangeTable'),
                                                              h5(strong("Assessment Guidance:")),
                                                              p('In cases where the applicable nutrient criteria are met for teh man-made lakes/reservoirs listed in Section 187 but
                                                                the maximum pH criterion is exceeded, the lake or resrvoir should be classified as Category 4C and recommended for a 
                                                                WQS review due to natural pH fluctuations. In lakes that are not in Section 187, the waterbody would be listed as impaired
                                                                (Category 5A). See lakes/reservoirs assessment flowchart for additional guidance.')),
                                                       column(6,
                                                              wellPanel(
                                                                h5('Station Exceedance Rate:'),
                                                                uiOutput('stationpHExceedanceRateSelect_UI'),
                                                                tableOutput('stationpHexceedanceRate')),
                                                              h5('Assessment Unit Exceedance Rate:'),
                                                              tableOutput('pHexceedanceRate')))),
                                            tabPanel('Bacteria',
                                                     helpText('Review each site using the single site visualization section, then 
                                                              proceed to the bottom of the page to find exceedance rate for the entire assessment unit.',br(),
                                                              span(strong('NOTE: The E.coli exceedance analysis results at the bottom of the page include data
                                                                          from ALL stations within the assessment unit.'))),
                                                     wellPanel(
                                                       h4(strong('Single Station Data Visualization')),
                                                       uiOutput('bacteria_oneStationSelectionUI'),
                                                       bacteriaSubTabUI('bacteria')),
                                                     br(),hr(),br(),
                                                     h5(strong("E.coli Exceedance Analysis")),
                                                     fluidRow(
                                                       column(6,
                                                              h5('All E.coli individual records or monthly geometric means within the ',span(strong('assessment unit')),
                                                                 ' that exceed the lake-specific criterion are highlighted below.'),
                                                              tableOutput('bacteriaRangeTable')),
                                                       column(6,
                                                              wellPanel(
                                                                h5('Station Exceedance Rate:'),
                                                                uiOutput('stationBacteriaExceedanceRateSelect_UI'),
                                                                tableOutput('stationBacteriaExceedanceRate')),
                                                              h5('Assessment Unit Exceedance Rate:'),
                                                              tableOutput('bacteriaExceedanceRate')))),
                                            tabPanel('Nutrients',
                                                     helpText('Select the appropriate assessment methodology based on whether or not the assessment unit is within
                                                              a Section 187 lake.'),
                                                     radioButtons("radio",h5("Assess as:"),choices=c("Section 187 Lake","Non 187 Lake"),selected=NULL),
                                                     conditionalPanel(condition="input.radio == 'Section 187 Lake'",
                                                                      h4('Chlorophyll a Analysis'),
                                                                      wellPanel(h5('Single Station Data Visualization'),
                                                                                uiOutput('chla_oneStationSelectionUI'),
                                                                                chlaSingleStationSubTabUI('chlaPlot')),
                                                                      chlaSubTabUI('chlaAllLake'),br(),
                                                                      downloadButton("downloadChlaData", "Download all data used for Chlorophyll a and Total Phosphorus assessment"),
                                                                      br(),hr(),br(),
                                                                      
                                                                      
                                                                      
                                                                      helpText('Were there algaecides utilized during the growing season on the lake?'),
                                                                      checkboxInput('algaecide','Check if algaecides used during growing season'),
                                                                      conditionalPanel("input.algaecide",
                                                                                       h4('Total Phosphorus Analysis'),
                                                                                       wellPanel(h5('Single Station Data Visualization'),
                                                                                                 uiOutput('TP_oneStationSelectionUI'),
                                                                                                 TPSingleStationSubTabUI('TPplot')),
                                                                                       TPSubTabUI('TPallLake'),br()
                                                                      )),
                                                     conditionalPanel(condition="input.radio == 'Non 187 Lake'",p('build out with TSI'))
                                                     
                                                     )
                                            
                                            
                                            
                                            
                                            
                                            
                                                              )),
                                 
                                 
                                 
                                 
                                 
                                 tabPanel("About",fluidRow(column(10,
                                                                  h4("This app was created to assist Virginia Department of Environmental Quality
                                                                     Water Quality Assessors with data manipulation, analysis, and reporting for
                                                                     the 2018 Integrated Report."))))
                                                                  )))
                                                                  ))








































server <- shinyServer(function(input, output, session) {
  
  # display the loading feature until data loads into app
  load_data()
  
  # Query AU's By Selectize arguments
  the_data <- reactive({lakeStations})
  lake_filter <- shiny::callModule(dynamicSelect, "lakeSelection", the_data, "SIGLAKENAME" )
  au_filter <- shiny::callModule(dynamicSelect, "AUselection", lake_filter, "ID305B" )
  
  
  # Station Map
  callModule(stationMap, "lakeMap",au_filter)
  
  # Output info on user selected station based on map marker click or station selectInput
  output$lakeDetails <- DT::renderDataTable({
    DT::datatable(au_filter(),extensions = 'Buttons', escape=F, rownames = F,
                  options=list(dom='Bt',scrollX = TRUE, buttons=list('copy')))})
  
  
  
  
  ### Lake Assessment Tab ###
  
  # Show user site(s) one more time
  output$ID305Bselected <- renderPrint({cat(as.character(au_filter()$ID305B[1]))}) # unlist this part
  output$lakeDetails2 <- DT::renderDataTable({DT::datatable(au_filter(),rownames = F,options=list(dom='t',scrollX = TRUE))})
  
  # Pull data from conventionals based on user selection from station selectInput
  stationData <- eventReactive(input$retrieveData, {
    req(au_filter)
    filter(conventionals,FDT_STA_ID %in% au_filter()$STATION_ID)})
  
  
  ## Data Tab ##------------------------------------------------------------------------------------------------------
  
  # Display data 
  output$stationDataTable <- DT::renderDataTable({
    DT::datatable(stationData(),extensions = c('Buttons','Scroller'), escape=F, rownames = F,
                  options=list(deferRender = TRUE, scrollY = 200, scroller = TRUE, dom='Bftp',scrollX = TRUE,
                               buttons=list('copy',
                                            list(extend='csv',filename=paste('LakeData_',paste(input$station, collapse = "_"),Sys.Date(),sep='')),
                                            list(extend='excel',filename=paste('LakeData_',paste(input$station, collapse = "_"),Sys.Date(),sep='')))))})
  
  # Summarize data
  output$stationDataTableRecords <- renderText({
    req(stationData())
    paste(nrow(stationData()), 'records were retrieved for',as.character(au_filter()$ID305B[1]),sep=' ')})
  output$uniqueStationDataTableRecords <- renderTable({
    req(stationData())
    plyr::count(stationData(), vars = c("FDT_STA_ID"))%>%dplyr::rename('Number of Records'='freq')})
  output$stationDataTableAssessmentWindow <- renderText({
    req(stationData())
    withinAssessmentPeriod(stationData())})
  
  # Create Data frame with all data within ID305B and stratification information
  # Pool thermocline data to get 1 sample per day, not 2 with top/bottom time difference
  stationDataDailySample <- reactive({
    req(stationData())
    dat <- stationData()
    dat$FDT_DATE_TIME <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y %H:%M")
    
    dat <- mutate(dat, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
      filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
    thermo <- stratifiedLake(dat)
    thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
    dat2 <- plyr::join(dat,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
      mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))%>%
      plyr::join(lakeStations[,c(28:31)],by='FDT_STA_ID')
    return(dat2)
  })
  
  
  ## Temperature Tab ##------------------------------------------------------------------------------------------------------
  
  # Select One station for individual review
  output$oneStationSelectionUI <- renderUI({
    req(stationData())
    selectInput('oneStationSelection',strong('Select Station to Review'),choices=unique(stationData())$FDT_STA_ID,width='300px')})
  
  oneStation <- reactive({
    req(input$oneStationSelection)
    filter(stationDataDailySample(),FDT_STA_ID %in% input$oneStationSelection)})
  
  callModule(temperatureSubTab,'temp',oneStation)
  
  # Temperature Raw Exceedance Results (all AU)
  output$tempRangeTable <- renderTable({
    req(stationData())
    temp_Assessment(stationData())})
  
  # Temperature Station Exceedance Rate
  output$stationTempExceedanceRateSelect_UI <- renderUI({
    req(stationData())
    selectInput('stationTempExceedanceRateSelect',strong('Select Station to Review for individual temperature exceedance statistics'),
                choices=unique(stationData())$FDT_STA_ID,width='300px')})
  
  output$stationTempExceedanceRate <- renderTable({
    req(stationData(),input$stationTempExceedanceRateSelect)
    z <- filter(stationData(),FDT_STA_ID %in% input$stationTempExceedanceRateSelect)
    exceedance_temp(z) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  # Temperature AU Exceedance Rate
  output$tempExceedanceRate <- renderTable({
    req(stationData())
    exceedance_temp(stationData())})
  
  ## Dissolved Oxygen Tab ## ------------------------------------------------------------------------------------------------------
  
  # Select One station for individual review
  output$DO_oneStationSelectionUI <- renderUI({
    req(stationData())
    selectInput('DO_oneStationSelection',strong('Select Station to Review'),choices=unique(stationData())$FDT_STA_ID,width='300px')})
  
  DO_oneStation <- reactive({
    req(input$DO_oneStationSelection)
    filter(stationDataDailySample(),FDT_STA_ID %in% input$DO_oneStationSelection)})
  
  callModule(DOSubTab,'DO',DO_oneStation)
  
  # Highlight records that don't meet DO criteria
  output$DOrangeTable <- renderTable({
    req(stationDataDailySample())
    DO_Assessment(stationDataDailySample(),c("Epilimnion",NA))})
  
  # Dissolved Oxygen Exceedance Results by Station
  output$stationDOExceedanceRateSelect_UI <- renderUI({
    req(stationData())
    selectInput('stationDOExceedanceRateSelect',strong('Select Station to Review for individual dissolved oxygen exceedance statistics'),
                choices=unique(stationData())$FDT_STA_ID,width='300px')})
  output$stationDOexceedanceRate_epi <- renderTable({
    req(stationDataDailySample(),input$stationDOExceedanceRateSelect)
    exceedance_DO(filter(stationDataDailySample(),FDT_STA_ID %in% input$stationDOExceedanceRateSelect),"Epilimnion") %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  output$stationDOexceedanceRate_epiAndNA <- renderTable({
    req(stationDataDailySample(),input$stationDOExceedanceRateSelect)
    exceedance_DO(filter(stationDataDailySample(),FDT_STA_ID %in% input$stationDOExceedanceRateSelect),c("Epilimnion",NA)) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  output$stationDOexceedanceRate_noStrat <- renderTable({
    req(stationDataDailySample(),input$stationDOExceedanceRateSelect)
    exceedance_DO(filter(stationDataDailySample(),FDT_STA_ID %in% input$stationDOExceedanceRateSelect),c("Epilimnion",NA,"Hypolimnion")) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  # Dissolved Oxygen Exceedance Results by Assessment Unit
  output$DOexceedanceRate_epi <- renderTable({
    req(stationDataDailySample())
    exceedance_DO(stationDataDailySample(),"Epilimnion")})
  output$DOexceedanceRate_epiAndNA <- renderTable({
    req(stationDataDailySample())
    exceedance_DO(stationDataDailySample(),c("Epilimnion",NA))})
  output$DOexceedanceRate_noStrat <- renderTable({
    req(stationDataDailySample())
    exceedance_DO(stationDataDailySample(),c("Epilimnion",NA,"Hypolimnion"))})
  
  
  
  
  ## pH Tab ## ------------------------------------------------------------------------------------------------------
  
  # Select One station for individual review
  output$pH_oneStationSelectionUI <- renderUI({
    req(stationDataDailySample())
    selectInput('pH_oneStationSelection',strong('Select Station to Review'),choices=unique(stationData())$FDT_STA_ID,width='300px')})
  
  pH_oneStation <- reactive({
    req(input$pH_oneStationSelection)
    filter(stationDataDailySample(),FDT_STA_ID %in% input$pH_oneStationSelection)})
  
  callModule(pHSubTab,'pH',pH_oneStation,pHrange)
  
  output$test <- renderPrint({pHrange()})
  pHrange <- reactive({switch(input$pHrange,'6 - 9' = c(6,9),'6.5 - 9.5' = c(6.5,9.5))})
  
  # pH Exceedance Results by Station
  output$stationpHExceedanceRateSelect_UI <- renderUI({
    req(stationData())
    selectInput('stationpHExceedanceRateSelect',strong('Select Station to Review for individual pH exceedance statistics'),
                choices=unique(stationData())$FDT_STA_ID,width='300px')})
  output$stationpHexceedanceRate <- renderTable({
    req(stationDataDailySample(),input$stationpHExceedanceRateSelect,pHrange())
    exceedance_pH(filter(stationDataDailySample(),FDT_STA_ID %in% input$stationpHExceedanceRateSelect),pHrange()) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  # pH Exceedance Results for Assessment Unit
  output$pHRangeTable <- renderTable({
    req(stationDataDailySample(),pHrange())
    pH_rangeAssessment(stationDataDailySample(),pHrange())})
  output$pHexceedanceRate <- renderTable({
    req(stationDataDailySample(),pHrange())
    exceedance_pH(stationDataDailySample(),pHrange())})
  
  
  ## Bacteria Tab ## ------------------------------------------------------------------------------------------------------
  
  # Select One station for individual review
  output$bacteria_oneStationSelectionUI <- renderUI({
    req(stationDataDailySample())
    selectInput('bacteria_oneStationSelection',strong('Select Station to Review'),choices=unique(stationData())$FDT_STA_ID,width='300px')})
  
  bacteria_oneStation <- reactive({
    req(input$bacteria_oneStationSelection)
    filter(stationDataDailySample(),FDT_STA_ID %in% input$bacteria_oneStationSelection)})
  
  callModule(bacteriaSubTab,'bacteria',bacteria_oneStation)
  
  # Bacteria Exceedance Results by Station
  output$stationBacteriaExceedanceRateSelect_UI <- renderUI({
    req(stationData())
    selectInput('stationBacteriaExceedanceRateSelect',strong('Select Station to Review for individual E.coli exceedance statistics'),
                choices=unique(stationData())$FDT_STA_ID,width='300px')})
  
  output$stationBacteriaExceedanceRate <- renderTable({
    req(stationDataDailySample(),input$stationBacteriaExceedanceRateSelect)
    z <- exceedance_bacteria(filter(stationDataDailySample(),FDT_STA_ID %in% input$stationBacteriaExceedanceRateSelect))
    if(class(z) != 'character'){
      z %>% dplyr::select(nSamples,nExceedance,exceedanceRate) # don't give assessment determination for single station
    }else{return(z)}
  })
  
  # Bacteria Exceedance Results By Assessment Unit
  output$bacteriaRangeTable <- renderTable({
    req(stationDataDailySample())
    bacteria_Assessment(stationDataDailySample())})
  output$bacteriaExceedanceRate <- renderTable({
    req(stationDataDailySample())
    exceedance_bacteria(stationDataDailySample())})
  
  
  
  ## Nutrients Tab ##------------------------------------------------------------------------------------------------------
  
  # Select One station for individual review, CHL A
  output$chla_oneStationSelectionUI <- renderUI({
    req(stationDataDailySample())
    selectInput('chla_oneStationSelection',strong('Select Station to Review'),choices=unique(stationData())$FDT_STA_ID,width='300px')})
  
  chla_oneStation <- reactive({
    req(input$chla_oneStationSelection)
    filter(stationDataDailySample(),FDT_STA_ID %in% input$chla_oneStationSelection)})
  
  callModule(chlaSingleStationSubTab,'chlaPlot',chla_oneStation)
  
  # Filter all lake LZ data to make pooled assessment, for CHL A and TP
  allLakeLZdata <- reactive({
    req(stationDataDailySample())
    sigLake <- as.character(au_filter()$SIGLAKENAME)
    allLakeLZstations <- as.character(subset(lakeStations,SIGLAKENAME==sigLake)$STATION_ID)
    allLakeLZData <- filter(conventionals,FDT_STA_ID %in% allLakeLZstations)
    allLakeLZData$FDT_DATE_TIME <- as.POSIXct(allLakeLZData$FDT_DATE_TIME, format="%m/%d/%y %H:%M")
    allLakeLZData<- mutate(allLakeLZData, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
      filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
    thermo <- stratifiedLake(allLakeLZData)
    thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
    allLakeLZData2 <- plyr::join(allLakeLZData,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
      mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))%>%
      plyr::join(lakeStations[,c(28:31)],by='FDT_STA_ID')
    return(allLakeLZData2)
  })
  
  callModule(chlaSubTab,'chlaAllLake',allLakeLZdata,stationData,stationDataDailySample)
  
  output$downloadChlaData <- downloadHandler(filename=function(){paste('ChlaData_',Sys.Date(),'.csv',sep='')},
                                             content=function(file){
                                               write.csv(allLakeLZdata(),file)})
  
  
  # Select One station for individual review, TP
  output$TP_oneStationSelectionUI <- renderUI({
    req(stationDataDailySample())
    selectInput('TP_oneStationSelection',strong('Select Station to Review'),choices=unique(stationData())$FDT_STA_ID,width='300px')})
  
  TP_oneStation <- reactive({
    req(input$TP_oneStationSelection)
    filter(stationDataDailySample(),FDT_STA_ID %in% input$TP_oneStationSelection) %>%
      filter(FDT_DEPTH<1)})# make sure you do not analyze bottom sample, if any
  
  callModule(TPSingleStationSubTab,'TPplot',TP_oneStation)
  
  callModule(TPSubTab,'TPallLake',allLakeLZdata,stationData,stationDataDailySample)
  
  
})



shinyApp(ui,server)
