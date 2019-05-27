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
smlData$FDT_DATE_TIME <- as.POSIXct(smlData$FDT_DATE_TIME, format="%m/%d/%y %H:%M")
smlData<- mutate(smlData, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
  filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
thermo <- stratifiedLake(smlData)
thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
smlData2 <- plyr::join(smlData,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
  mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))%>%
  plyr::join(lakeStations[,c(28:31)],by='FDT_STA_ID')
x <- filter(smlData2, FDT_STA_ID =='4AROA180.21')

#for testing purposes
#x <- smlData2
#x$Assess_TYPE <- "LZ"
#x$Chlorophyll_A_limit <- 10
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


chlA_Assessment_OneStation <- function(x){
  if(!is.na(unique(x$Assess_TYPE)) & unique(x$Assess_TYPE)=="LZ"){
    chlA <- filter(x, !is.na(CHLOROPHYLL))%>%
      dplyr::select(FDT_STA_ID,FDT_DEPTH,FDT_DATE_TIME, SampleDate,CHLOROPHYLL,71,Assess_TYPE)%>%
      mutate(Year=format(FDT_DATE_TIME,"%Y"),Month=format(FDT_DATE_TIME,'%m'))%>%
      filter(Month %in% c('04','05','06','07','08','09','10'))%>% # make sure only assess valid sample months
      group_by(Year)%>%
      mutate(samplePerYear= n(),pct90=quantile(CHLOROPHYLL,0.9),
             chlA_Exceedance=ifelse(pct90>Chlorophyll_A_limit,T,F))%>%
      dplyr::select(FDT_STA_ID,Year,samplePerYear,pct90,Chlorophyll_A_limit,chlA_Exceedance)%>%
      distinct(Year,.keep_all=T)
    return(chlA)
  }else{
    dat <- data.frame( FDT_STA_ID=x$FDT_STA_ID[1], Year=NA, samplePerYear=NA, 
                       pct90=NA, Chlorophyll_A_limit=NA, chlA_Exceedance=NA)
    
    return(dat)
  }
}
chlA_Assessment_OneStation(x)

chlA_Assessment <- function(x){
  holder <- list()
  for(i in 1:length(unique(x$FDT_STA_ID))){
    dat <- filter(x,FDT_STA_ID %in% unique(x$FDT_STA_ID)[i])
    holder[[i]] <-  as.data.frame(chlA_Assessment_OneStation(dat))
  }
  alldat <- do.call(rbind,holder)%>%filter(!is.na(Year))
  return(alldat)
}

chlA_Assessment(x)
chlA_Assessment(allsmlData2)
allsmlData3 <- allsmlData2
#allsmlData3 <- dplyr::select(allsmlData2,FDT_STA_ID,FDT_DEPTH,SampleDate,CHLOROPHYLL)
allsmlData3$CHLOROPHYLL[c(2225,2227,2252,2279,2305,2335)] <- c(220,345,212,341,222,122)
x <- allsmlData3  
z <- filter(allsmlData3,FDT_STA_ID=='4ABWR002.50')%>%dplyr::select(FDT_DEPTH,SampleDate,CHLOROPHYLL)


# Exceedance Rate Chlorophyll A
exceedance_chlA <- function(x){
  chlA_Assess <- chlA_Assessment(x)
  if(class(chlA_Assess$FDT_STA_ID)=="factor"){ # have to split this step up bc n stationID's affect how split performs
    chlA_Assess$FDT_STA_ID <- droplevels(chlA_Assess$FDT_STA_ID) # have to drop unused levels from factor or it messes with split function and mixes up data in each list item
    }
  dat <- split(chlA_Assess,f=chlA_Assess$FDT_STA_ID)
  holder <- list()
  for(i in 1:length(dat)){
    # Find two most recent years with >= 6 data points
    step1 <- filter(dat[[i]],samplePerYear>=6) # verify enough samples
    step2 <- filter(step1,Year %in% tail(sort(unique(step1$Year)),2)) # get two most recent years from valid sample years 
    
    # for testing
    #step2$chlA_Exceedance[2] <- TRUE
    
    if(step2$chlA_Exceedance[1]!=step2$chlA_Exceedance[2]){ # if the exceedances contradict one another in two years grab third year
      step1alt <- filter(dat[[i]],samplePerYear>=6) # verify enough samples 
      step2 <- filter(step1,Year %in% tail(sort(unique(step1$Year)),3)) # get three most recent years from valid sample years 
      }
    holder[[i]] <-  step2
  }
  do.call(rbind,holder) # output table for user to review
  #alldat <- do.call(rbind,holder)
  #assessmentDetermination(alldat,filter(alldat,chlA_Exceedance ==T),'Chlorophyll A','Aquatic Life')
}
exceedance_chlA(allsmlData2)
exceedance_chlA(allsmlData3)


# How assessment actually made:
#assessmentDetermination(alldat,filter(alldat,chlA_Exceedance ==T),'Chlorophyll A','Aquatic Life')
assessmentDetermination(exceedance_chlA(allsmlData3),filter(exceedance_chlA(allsmlData3),chlA_Exceedance ==T),'Chlorophyll A','Aquatic Life')%>%
  dplyr::select(nSamples,nExceedance,exceedanceRate) # don't give assessment determination for single station}


chlaSingleStationSubTabUI <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns('chlaPlotly')),
    h5('Annual Chlorophyll a 90th percentiles for the selected site are reported below.'),
    div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('singleStationAnnualChla90thTable')))
    
  )
    
}




chlaSingleStationSubTab <- function(input,output,session, oneStationData){
  ns <- session$ns
  
  output$chlaPlotly <- renderPlotly({
    req(oneStationData)
    dat <- oneStationData
    dat$SampleDate <- as.POSIXct(dat$SampleDate, format="%m/%d/%y")
    
    plot_ly(data=dat)%>%
      add_lines(x=~SampleDate,y=~Chlorophyll_A_limit, mode='line',line = list(color = '#E50606'),
                hoverinfo = "none", name="Lake Specific Chlorophyll a Limit")%>%
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
    req(oneStationData)
    chlA_Assessment_OneStation(oneStationData) })
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
               h5(strong('Chlorophyll a Annual Summary for All Lacustrine Stations in Lake'))),
               #downloadButton("downloadChlaData", "Download all data used for Chlorophyll a assessment")),
               #actionButton(ns('zoomDataForChla'),"Table of all pooled data",class='btn-block')),
             div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('chla90thTable')))),
      column(6,
             helpText('The exceedance analysis below is based on data reported in the Chlorophyll a
                      Annual Summary table immediately to the left. This exceedance calculation
                      pools all data that meets the above mentioned requirements across all lake sites
                      in the lacustrine zone.'),
             h5('Lake Exceedance Rate:'),
             tableOutput(ns('chla_exceedanceRate'))))
                    
  )
}

chlaSubTab <- function(input,output,session, allLakeLZData){
  ns <- session$ns
  
  output$chla90thTable <- renderTable({
    req(allLakeLZData)
    exceedance_chlA(allLakeLZData)})
  
  # Doesn't work with reactive object coming into modal, need to keep workign on this part
  # temporary fix is to place download button in actual ui/server files and not in modal
  
  #output$downloadChlaData <- downloadHandler(
  #  filename = function() {
  #    paste("ChlaData_",Sys.Date(), ".csv", sep = "")
  #  },
  #  content = function(file) {
  #    write.csv(allLakeLZData, file, row.names = FALSE)
  #  }
  #)
  
  
  #observeEvent(input$zoomDataForChla,{
  #  showModal(modalDialog(
  #    title="Table of all data used for Chlorophyll a assessment",
  #    DT::dataTableOutput(ns('allLakeChlaData'),width=550),
  #    easyClose = TRUE))
  #})
  
  #output$allLakeChlaData <- DT::renderDataTable({
  #  req(input$zoomDataForChla)
  #  DT::datatable(allLakeLZData,extensions = 'Buttons', escape=F, rownames = F,
  #                  options=list(dom='Btp',pageLength = 5,scrollX = TRUE,scrollY = TRUE, 
  #                               buttons=list('copy',
  #                                            list(extend='csv',filename=paste('ChlaData_',Sys.Date(),sep='')),
  #                                            list(extend='excel',filename=paste('ChlaData_',Sys.Date(),sep=''))
  #                                            )))})
  
  output$chla_exceedanceRate <- renderTable({
    req(allLakeLZData)
    assessmentDetermination(exceedance_chlA(allLakeLZData),filter(exceedance_chlA(allLakeLZData),chlA_Exceedance ==T),'Chlorophyll a','Aquatic Life')})
}


ui <- fluidPage(
  chlaSubTabUI('chlaAllLake'))

server <- function(input,output,session){
  # Filter all lake LZ data to make pooled assessment
  allLakeLZdata <- reactive({
    #req(stationDataDailySample())
    sigLake <- as.character('Lake Moomaw')#au_filter()$SIGLAKENAME)
    allLakeLZstations <- as.character(subset(lakeStations,SIGLAKENAME==sigLake)$STATION_ID)
    allLakeLZdata <- filter(conventionals,FDT_STA_ID %in% allLakeLZstations)
    allLakeLZdata$FDT_DATE_TIME <- as.POSIXct(allLakeLZdata$FDT_DATE_TIME, format="%m/%d/%y %H:%M")
    allLakeLZdata<- mutate(allLakeLZdata, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
      filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
    thermo <- stratifiedLake(allLakeLZdata)
    thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
    allLakeLZdata2 <- plyr::join(allLakeLZdata,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
      mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))%>%
      plyr::join(lakeStations[,c(28:31)],by='FDT_STA_ID')
    return(allLakeLZdata2)
  })
  
  
  callModule(chlaSubTab,'chlaAllLake',allLakeLZdata())
}

shinyApp(ui,server)









sigLake <- as.character('Smith Mountain Lake')#au_filter()$SIGLAKENAME)
allLakeLZstations <- as.character(subset(lakeStations,SIGLAKENAME==sigLake)$STATION_ID)
allLakeLZdata <- filter(conventionals,FDT_STA_ID %in% allLakeLZstations)
allLakeLZdata$FDT_DATE_TIME <- as.POSIXct(allLakeLZdata$FDT_DATE_TIME, format="%m/%d/%y %H:%M")
allLakeLZdata<- mutate(allLakeLZdata, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
  filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
thermo <- stratifiedLake(allLakeLZdata)
thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
allLakeLZdata2 <- plyr::join(allLakeLZdata,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
  mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))%>%
  plyr::join(lakeStations[,c(28:31)],by='FDT_STA_ID')
allLakeLZdata <- allLakeLZdata2
