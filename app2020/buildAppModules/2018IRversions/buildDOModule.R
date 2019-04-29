library(shiny)
library(shinyjs)
library(leaflet)
library(mapview)
library(tidyverse)
library(plotly)
library(raster)
library(DT)

# Get data to test module
moo <- subset(lakeStations,SIGLAKENAME=='Lake Moomaw')%>%
  filter(ID305B=='VAW-I03L_JKS01A02')
mooData <- filter(conventionals, FDT_STA_ID %in% moo$STATION_ID)

# Data management to get test dataset to look like stationDataDailySample()
mooData$FDT_DATE_TIME <- as.POSIXct(mooData$FDT_DATE_TIME, format="%m/%d/%y %H:%M")
mooData<- mutate(mooData, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
  filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
thermo <- stratifiedLake(mooData)
thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
mooData2 <- plyr::join(mooData,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
  mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))%>%
  plyr::join(lakeStations[,c(28:31)],by='FDT_STA_ID')
x <- filter(mooData2, FDT_STA_ID =='2-JKS044.60')



DOSubTabUI <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns('DOplotly')),
    fluidRow(column(3),column(4,actionButton(ns('zoomPlotDO'),"Zoomed Plot by Sample Date",class='btn-block'))),
    br(),hr(),br(),
    fluidRow(
      column(6,
             h5(strong('All DO records that fall below the criteria are highlighted below.')),
             tableOutput(ns('DOrangeTable')),
      column(6,
             h5('Exceedance Rate:'),
             h6("This analysis considers all data in epilimnion throughout sampling season."),
             tableOutput(ns('DOexceedanceRate')))))
  )
}
dat <- oneStationData

DOSubTab <- function(input,output,session, oneStationData){
  ns <- session$ns
  
  output$DOplotly <- renderPlotly({
    req(oneStationData)
    dat <- mutate(oneStationData,bottom=4)
    dat$SampleDate <- as.POSIXct(dat$SampleDate, format="%m/%d/%y")
    dat[is.na(dat$LakeStratification),]$LakeStratification <- 'NONE'
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
    req(oneStationData)
    selectInput(ns('DOdateSelection'),"Choose a Sample Date to Plot",choices = unique(oneStationData()$SampleDate))
  })
  
  
  # Plotly DO by single sample date
  output$DOplotlyByDate <- renderPlotly({
    req(oneStationData)
    dat <- filter(oneStationData,SampleDate %in% input$DOdateSelection)%>%
      mutate(bottom=4)
    
    plot_ly(data=dat)%>%
      add_lines(x=~FDT_TEMP_CELCIUS,y=~bottom, mode='line',line = list(color = '#E50606'),
                hoverinfo = "none", name="DO Standard")%>%
      add_markers(x= ~FDT_TEMP_CELCIUS, y= ~DO,mode = 'scatter', name="Dissolved Oxygen",
                  color=~LakeStratification, colors=c('#BF382A', '#0C4B8E'),
                  hoverinfo="text",text=~paste(sep="<br>",paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Dissolved Oxygen:",DO,"mg/L"),
                                               paste("LakeStratification: ",LakeStratification)))%>%
      layout(#xaxis = list(autorange = "reversed"),
        showlegend=FALSE,
        yaxis=list(title="Dissolved Oxygen (mg/L)"),
        xaxis=list(title="Temperature (C)",
                   tickfont = list(size = 10)))
  })
  
  
}


ui <- fluidPage(
  DOSubTabUI('DO'))

server <- function(input,output,session){
  callModule(DOSubTab,'DO',mooData2)
}

shinyApp(ui,server)