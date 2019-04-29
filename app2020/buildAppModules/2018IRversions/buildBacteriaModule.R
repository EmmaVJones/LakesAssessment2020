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
    plotlyOutput(ns('bacteriaPlotly')),
    #fluidRow(column(3),column(4,actionButton(ns('zoomPlotDO'),"Zoomed Plot by Sample Date",class='btn-block'))),
    br(),hr(),br(),
    fluidRow(
      column(6,
             h5(strong('All E.coli records that fall below the criteria are highlighted below.')),
             tableOutput(ns('bacteriaRangeTable')),
             column(6,
                    h5('Exceedance Rate:'),
                    h6("This analysis considers all data throughout sampling season."),
                    tableOutput(ns('bacteriaExceedanceRate')))))
  )
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
  bacteriaSubTabUI('bacteria'))

server <- function(input,output,session){
  callModule(bacteriaSubTab,'bacteria',x)#mooData2)
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
