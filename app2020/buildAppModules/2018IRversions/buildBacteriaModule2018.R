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
  filter(ID305B=='VAW-L07L_ROA04A10')
smlData <- filter(conventionals, FDT_STA_ID %in% sml$STATION_ID)


x <- filter(conventionals,FDT_STA_ID=='4AROA192.94')

# Data management to get test dataset to look like stationDataDailySample()
smlData$FDT_DATE_TIME <- as.POSIXct(smlData$FDT_DATE_TIME, format="%m/%d/%y %H:%M")
smlData<- mutate(smlData, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
  filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
thermo <- stratifiedLake(smlData)
thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
smlData2 <- plyr::join(smlData,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
  mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))%>%
  plyr::join(lakeStations[,c(28:31)],by='FDT_STA_ID')
x <- filter(smlData2, FDT_STA_ID =='4AROA192.94')




bacteriaSubTabUI <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns('bacteriaPlotly')))
}


bacteriaSubTab <- function(input,output,session, oneStationData){
  ns <- session$ns
  
  output$bacteriaPlotly <- renderPlotly({
    req(oneStationData)
    dat <- mutate(oneStationData,top=235)%>%filter(!is.na(E.COLI))
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
  })
}





ui <- fluidPage(
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
           h5('All E.coli individual records or monthly geometric means that fall out of range are highlighted below.'),
           tableOutput('bacteriaRangeTable'))))#,
  #  column(6,
  #         h5('Exceedance Rate:'),
  #         tableOutput('bacteriaExceedanceRate'))))

server <- function(input,output,session){
  # Select One station for individual review
  #output$bacteria_oneStationSelectionUI <- renderUI({
  #  req(stationDataDailySample())
  #  selectInput('bacteria_oneStationSelection',strong('Select Station to Review'),choices=unique(stationData())$FDT_STA_ID,width='300px')})
  
  #bacteria_oneStation <- reactive({
  #  req(input$bacteria_oneStationSelection)
  #  filter(stationDataDailySample(),FDT_STA_ID %in% input$bacteria_oneStationSelection)})
  
  callModule(bacteriaSubTab,'bacteria',x)#bacteria_oneStation)
  
  # Bacteria Exceedance Results
  output$bacteriaRangeTable <- renderTable({
    req(x)#req(stationDataDailySample())
    bacteria_Assessment(x)})#stationDataDailySample())})
 # output$bacteriaExceedanceRate <- renderTable({
#    req(stationDataDailySample())
 #   exceedance_bacteria(stationDataDailySample())})
  
}

shinyApp(ui,server)












# How bacteria is assessed
bacteria_Assessment <- function(x){
  bacteria <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,E.COLI,SampleDate)%>% # Just get relavent columns, 
    filter(!is.na(E.COLI))%>% #get rid of NA's
    mutate(singleSampleMaximum=ifelse(E.COLI>235,T,F), # Find any single sample exceedances
           previousSample=lag(SampleDate,1),previousSampleECOLI=lag(E.COLI,1))%>% # Line up previous sample with current sample line
    rowwise() %>% 
    mutate(sameSampleMonth= as.numeric(strsplit(SampleDate,'/')[[1]][1])  -  as.numeric(strsplit(previousSample,'/')[[1]][1])) %>% # See if sample months are the same, e.g. more than one sample per calendar month
    filter(singleSampleMaximum == T | sameSampleMonth == 0) %>% # keep only rows with single sample exceedances or multiple samples per calendar month to then test for geomean
    rowwise() %>% mutate(geoMeanCalendarMonth=FSA::geomean(c(E.COLI,previousSampleECOLI)))%>% # Calculate geomean
    filter(singleSampleMaximum == T | geoMeanCalendarMonth > 126) %>% #find exceedances to either rule
    dplyr::select(FDT_STA_ID ,SampleDate, E.COLI) # only keep columns that will be important to assessors
  return(bacteria)
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

# Exceedance Rate DO
exceedance_bacteria <- function(x){
  bacteria <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,E.COLI)%>% # Just get relavent columns, 
    filter(!is.na(E.COLI))
  
  bacteria_Assess <- bacteria_Assessment(x)
  bacteria_results <- assessmentDetermination(bacteria,bacteria_Assess,"E.Coli","Recreation")
  return(bacteria_results)
}

bacteria_Assessment(x)
exceedance_bacteria(x)
