library(shiny)
library(shinyjs)
library(leaflet)
library(mapview)
library(tidyverse)
library(plotly)
library(raster)
library(DT)

# Get data to test module
sml <- subset(lakeStations,SIGLAKENAME=='Smith Mountain Lake')%>%
  filter(ID305B=='VAW-L07L_ROA03A10')
smlData <- filter(conventionals, FDT_STA_ID %in% sml$STATION_ID)


# Data management to get test dataset to look like stationDataDailySample()
allsml <- subset(lakeStations,SIGLAKENAME=='Smith Mountain Lake')#Lake Moomaw')#
allsmlData <- filter(conventionals,FDT_STA_ID %in% allsml$STATION_ID)
allsmlData$FDT_DATE_TIME <- as.POSIXct(allsmlData$FDT_DATE_TIME, format="%m/%d/%y %H:%M")
allsmlData<- mutate(allsmlData, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
  filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
thermo <- stratifiedLake(allsmlData)
thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
allsmlData2 <- plyr::join(allsmlData,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
  mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))%>%
  plyr::join(lakeStations[,c(28:31)],by='FDT_STA_ID')
x <- filter(allsmlData2,FDT_STA_ID=="4AROA167.34")#4AROA180.21")

x2 <- filter(conventionals, FDT_STA_ID=='4AROA180.21')%>%
  filter(FDT_DEPTH==0.3)%>%
  dplyr::select(FDT_DATE_TIME,PHOSPHORUS)

# for testing
TP[34:39,5] <- c(45,34,12,34,54,54)


TP_Assessment_OneStation <- function(x){
  if(!is.na(unique(x$Assess_TYPE)) & unique(x$Assess_TYPE)=="LZ"){
    TP <- filter(x, !is.na(PHOSPHORUS))%>%
      dplyr::select(FDT_STA_ID,FDT_DEPTH,FDT_DATE_TIME, SampleDate,PHOSPHORUS,TPhosphorus_limit,Assess_TYPE)%>%
      mutate(Year=format(FDT_DATE_TIME,"%Y"),Month=format(FDT_DATE_TIME,'%m'))%>%
      filter(Month %in% c('04','05','06','07','08','09','10'))%>% # make sure only assess valid sample months
      filter(FDT_DEPTH<1) %>% # make sure you do not analyze bottom sample, if any
      group_by(Year) %>%
      mutate(samplePerYear= n(),TPhosphorus_limit_ug_L=TPhosphorus_limit/1000,medianTP=median(PHOSPHORUS),
             TP_Exceedance=ifelse(medianTP>TPhosphorus_limit_ug_L,T,F))%>%
      dplyr::select(FDT_STA_ID,Year,samplePerYear,medianTP,TPhosphorus_limit_ug_L,TP_Exceedance)%>%
      distinct(Year,.keep_all=T)
    return(TP)
  }else{
    dat <- data.frame( FDT_STA_ID=x$FDT_STA_ID[1], Year=NA, samplePerYear=NA, 
                       medianTP=NA, TPhosphorus_limit_ug_L=NA, TP_Exceedance=NA)
    
    return(dat)
  }
}
TP_Assessment_OneStation(x)


TP_Assessment <- function(x){
  holder <- list()
  for(i in 1:length(unique(x$FDT_STA_ID))){
    dat <- filter(x,FDT_STA_ID %in% unique(x$FDT_STA_ID)[i])
    holder[[i]] <-  as.data.frame(TP_Assessment_OneStation(dat))
  }
  alldat <- do.call(rbind,holder)%>%filter(!is.na(Year))
  return(alldat)
}

TP_Assessment(x)
allsmlData3 <- allsmlData2
allsmlData3$PHOSPHORUS[c(2225,2227,2252,2279,2305,2335)] <- c(220,345,212,341,222,122)
TP_Assessment(allsmlData3)
x <- allsmlData3

# Exceedance Rate Total Phosphorus
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
    
    # for testing
    #step2$TP_Exceedance[2] <- TRUE
    
    if(step2$TP_Exceedance[1]!=step2$TP_Exceedance[2]){ # if the exceedances contradict one another in two years grab third year
      step1alt <- filter(dat[[i]],samplePerYear>=6) # verify enough samples 
      step2 <- filter(step1,Year %in% tail(sort(unique(step1$Year)),3)) # get three most recent years from valid sample years 
    }
    holder[[i]] <-  step2
  }
  do.call(rbind,holder) # output table for user to review
}
exceedance_TP(allsmlData2)
exceedance_TP(allsmlData3)


# How assessment actually made:
assessmentDetermination(exceedance_TP(allsmlData3),filter(exceedance_TP(allsmlData3),TP_Exceedance ==T),'Total Phosphorus','Aquatic Life')





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
    req(oneStationData)
    dat <- oneStationData
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
    req(oneStationData)
    TP_Assessment_OneStation(oneStationData) })
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
             div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('medianTPtable')))),
      column(6,
             helpText('The exceedance analysis below is based on data reported in the Total Phosphorus
                      Annual Summary table immediately to the left. This exceedance calculation
                      pools all data that meets the above mentioned requirements across all lake sites
                      in the lacustrine zone.'),
             h5('Lake Exceedance Rate:'),
             tableOutput(ns('TP_exceedanceRate'))))
      )
}

TPSubTab <- function(input,output,session, allLakeLZData){
  ns <- session$ns
  
  output$medianTPtable <- renderTable({
    req(allLakeLZData)
    exceedance_TP(allLakeLZData)})
  
  output$TP_exceedanceRate <- renderTable({
    req(allLakeLZData)
    assessmentDetermination(exceedance_TP(allLakeLZData),filter(exceedance_TP(allLakeLZData),TP_Exceedance ==T),'Total Phosphorus','Aquatic Life')})
}


