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


pHSubTabUI <- function(id){
  ns <- NS(id)
  tagList( plotlyOutput(ns('pHplot'),height = "400px"))
}



pHSubTab <-  function(input,output,session, oneStationData){
  ns <- session$ns
  
  output$pHplot <- renderPlotly({
    req(oneStationData)
    dat <- mutate(oneStationData,top=9,bottom=6)
    dat$FDT_DATE_TIME <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y %H:%M")
    
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


output$pHRangeTable <- renderTable({
  req(stationData())
  pH_rangeAssessment(stationData())
})

output$pHexceedanceRate <- renderTable({
  req(stationData())
  exceedance_pH(stationData())
})




ui <- fluidPage(
  pHSubTabUI('pH'))

server <- function(input,output,session){
  callModule(pHSubTab,'pH',mooData1)
}

shinyApp(ui,server)
