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
station1 <- filter(conventionals,FDT_STA_ID=='2-JKS044.60')
station2 <- filter(conventionals,FDT_STA_ID=='2-JKS046.40')



temperatureSubTabUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             h5(strong("Thermocline Analysis")),
             DT::dataTableOutput(ns('thermoclineResults'))),
      column(6,
             uiOutput(ns('stationDateSelection')),
             plotlyOutput(ns('thermoclinePlotly'))
             ))
  )
}
  

temperatureSubTab <- function(input,output,session, oneStationData){
  ns <- session$ns
  
  # Pool thermocline data to get 1 sample per day, not 2 with top/bottom time difference
  stationDataDailySample <- reactive({
    req(oneStationData())
    dat <- oneStationData
    dat$FDT_DATE_TIME <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y %H:%M")
    
    dat <- mutate(dat, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
      filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
    thermo <- stratifiedLake(dat)
    thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
    dat2 <- plyr::join(dat,thermo,by='SampleDate')%>%
      mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))
    return(dat2)
  })
  
  # Thermocline Analysis Table
  output$thermoclineResults <- DT::renderDataTable({
    req(stationDataDailySample())
    dat <- dplyr::select(stationDataDailySample(),SampleDate,ThermoclineDepth)%>%
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
    req(stationDataDailySample())
    selectInput(ns('thermoDateSelection'),"Choose a Sample Date to Plot",choices = unique(stationDataDailySample()$SampleDate))
  })
  

  
  # Thermoncline plotly based on user date input
  output$thermoclinePlotly <- renderPlotly({
    req(stationDataDailySample())
    dat <- dplyr::filter(stationDataDailySample(),SampleDate %in% input$thermoDateSelection)
    
    if(dat$ThermoclineDepth[1] != 'NaN'){
      plot_ly(data=dat)%>%
        add_lines(x=~FDT_TEMP_CELCIUS,y=~ThermoclineDepth, mode='line',line = list(color = '#E50606'),
                  hoverinfo = "Thermocline", name="Thermocline")%>%
        add_markers(x= ~FDT_TEMP_CELCIUS, y= ~FDT_DEPTH,mode = 'scatter', name="Temperature",
                    color=~LakeStratification, colors=c('#BF382A', '#0C4B8E'),
                    hoverinfo="text",text=~paste(sep="<br>",paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Temperature:",FDT_TEMP_CELCIUS,"C"),
                                                 paste("LakeStratification: ",LakeStratification)))%>%
        layout(xaxis = list(autorange = "reversed"),
               yaxis = list(autorange = "reversed"),
               showlegend=FALSE,
               yaxis=list(title="Depth (m)"),
               xaxis=list(title="Temperature (Celcius)"))
    }else{
      plot_ly(data=dat,y=~FDT_DEPTH,x=~FDT_TEMP_CELCIUS,type='scatter',mode='markers')%>%
        layout(xaxis = list(autorange = "reversed"),
               yaxis = list(autorange = "reversed"),
               showlegend=FALSE,
               yaxis=list(title="Depth (m)"),
               xaxis=list(title="Temperature (Celcius)"))}
  })
  
  
}



ui <- fluidPage(
  temperatureSubTabUI('temp'))

server <- function(input,output,session){
  callModule(temperatureSubTab,'temp',station1)
}

shinyApp(ui,server)