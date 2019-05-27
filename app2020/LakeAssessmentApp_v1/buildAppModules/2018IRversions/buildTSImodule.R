library(shiny)
library(shinyjs)
library(leaflet)
library(mapview)
library(tidyverse)
library(plotly)
library(raster)
library(DT)

# Get data to test module
lex <- subset(lakeStations,SIGLAKENAME=='Lake Orange')
lexData <- filter(conventionals, FDT_STA_ID %in% lex$STATION_ID)


# Data management to get test dataset to look like stationDataDailySample()
lexData$FDT_DATE_TIME <- as.POSIXct(lexData$FDT_DATE_TIME, format="%m/%d/%y %H:%M")
lexData<- mutate(lexData, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
  filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
thermo <- stratifiedLake(lexData)
thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
lexData2 <- plyr::join(lexData,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
  mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))%>%
  plyr::join(lakeStations[,c(28:31)],by='FDT_STA_ID')
x <- lexData2


nonSigLake <- filter(lakeStations,SIG_LAKE == 'N')

# Bring in Secchi Depth Data statewide
secchi <- as.data.frame(read_csv('data/secchi_2018IR.csv'))
secchi$FDT_DATE <- as.Date(secchi$FDT_DATE_TIME, format="%m/%d/%Y")



TSI_Assessment_OneStation <- function(x,secchi){
  # Significant Lake?
  if(suppressWarnings(unique(left_join(x,nonSigLake,by='FDT_STA_ID')$SIG_LAKE))=="N"){ # Make sure it isn't a 187 Lake
    x$FDT_DATE <- as.Date(x$FDT_DATE_TIME) # need this to merge by dates
    TSI <- filter(x,!is.na(PHOSPHORUS) | !is.na(CHLOROPHYLL) ) %>% 
      left_join(dplyr::select(secchi,FDT_DATE,`Secchi Depth`,FDT_STA_ID), by=c('FDT_STA_ID','FDT_DATE')) %>%
      # Filter dates between June15 - Sept15 for TSI
      mutate(monthday = format(FDT_DATE_TIME, "%m%d"), year = format(FDT_DATE_TIME, "%Y")) %>%
      filter(monthday >= '0615' & monthday <= '0915', # make sure only assess valid sample monthdays,
             !is.na(`Secchi Depth`), FDT_DEPTH == '0.3', # Need secchi depths at all locations and surface sample
             LakeStratification == 'Epilimnion') %>% # need to be considered Epilimnion (stratified lakes only)
      dplyr::group_by(FDT_STA_ID,year) %>% # group by years to get unique sample seasons for TSI calculation since you need average of sample season
              
      # TSI EQUATIONS
      mutate(TSI_SD = 10*(6 - (log(mean(`Secchi Depth`)) / log(2))), # TSI for Secchi Depth, note in R, log() = ln() *natural log whereas log10() = log() base 10
             TSI_CA = 10*(6 - ((2.04 - 0.68 * (log(mean(`CHLOROPHYLL`)))) / (log(2)))), # TSI Chlorophyll a
             TSI_TP = 10*(6 - (log(48 / (1000 * mean(`PHOSPHORUS`))) / log(2)))) # TSI Phosphorus, Phosphorus reported in mg/L and calc needs ug/L so convert in equation
      TSIsummary <- summarise(TSI,`StationID` = unique(FDT_STA_ID), `Sample Year` = head(year,1), `Valid Samples` = n_distinct(monthday),
                              TSI_SD = head(TSI_SD,1), TSI_CA = head(TSI_CA,1), TSI_TP = head(TSI_TP,1)) %>%
        dplyr::select(-c(year))
      
    return(list(TSI,TSIsummary))
   }else{
    return(print('This is a Section 187 Lake and should be assessed using nutrient criteria'))
  }
}


TSIassessment <- TSI_Assessment_OneStation(x,secchi)

trophicStatus <- data.frame(`Trophic Index Trophic State` = c('Hypereutrophic','Eutrophic','Mesotrophic','Oligotrophic','Unknown'),
                            `Carlson Trophic State Index` = c('80 - 100','60 - <80','40 - <60','0 - <40','Insufficient Data'),
                            CTSI_upperBound = c(100,80,60,40,NA), CTST_lowerBound = c(80,60,40,0,NA),
                            `ADB Category` = c('5A','5A','4C','4C','3A'))

assessmentDeterminationTSI <- function(TSIassessment,trophicStatus){
  if(TSIassessment$TSI_SD >= 60 | TSIassessment$TSI_CA >= 60 | TSIassessment$TSI_TP >= 60){
    ADBcategory <- '5A'  
    CarlsonTSI <- ifelse(TSIassessment$TSI_SD >= 80 | TSIassessment$TSI_CA >= 80 | TSIassessment$TSI_TP >= 80,
                         'Hypereutrophic','Eutrophic')}
  
  
  
  assessmentTSI <- mutate(TSIassessment, CarlsonTSI = )
}






TSISingleStationSubTabUI <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns('TSIparameter'),'Choose parameter to plot: ', 
                   choices = c('Secchi Depth (m)','Chlorophyll a (ug/L)','Total Phosphorus (ug/L)'),selected = NULL),
    plotlyOutput(ns('TSIplotly')),
    h5('Trophic State Index equations for secchi depth, Chlorophyl a, and total phosphorus'),
    div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('singleStationAnnualTSImetrics')))
    
  )
  
}

TSISingleStationSubTab <- function(input,output,session, oneStationData){
  ns <- session$ns

  output$TSIplotly <- renderPlotly({
    req(oneStationData)
    dat <- TSI_Assessment_OneStation(oneStationData,secchi)[[1]] %>%
      dplyr::rename('Secchi Depth (m)' = "Secchi Depth",'Chlorophyll a (ug/L)' = "CHLOROPHYLL", 'Total Phosphorus (ug/L)' = "PHOSPHORUS") %>%
      mutate(`Total Phosphorus (ug/L)` = `Total Phosphorus (ug/L)` * 1000)
    dat$SampleDate <- as.POSIXct(dat$SampleDate, format="%m/%d/%y")
    dat <- dat[,c("SampleDate",input$TSIparameter)]
    
    # reverse y axis for Secchi Depth only
    if(input$TSIparameter == ('Secchi Depth (m)')){
      plot_ly(data=dat)%>%
        add_markers(x= ~SampleDate, y= ~get(input$TSIparameter),
                    mode = 'scatter', name=input$TSIparameter,#color='blue',
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Measure: ",get(input$TSIparameter))))%>%
        layout(showlegend=FALSE,
               yaxis=list(title=input$TSIparameter,autorange = "reversed"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10)))
      }else{
        plot_ly(data=dat)%>%
          add_markers(x= ~SampleDate, y= ~get(input$TSIparameter),
                      mode = 'scatter', name=input$TSIparameter,#color='blue',
                      hoverinfo="text",text=~paste(sep="<br>",
                                                   paste("Date: ",SampleDate),
                                                   paste("Measure: ",get(input$TSIparameter))))%>%
          layout(showlegend=FALSE,
                 yaxis=list(title=input$TSIparameter),
                 xaxis=list(title="Sample Date",tickfont = list(size = 10)))}
  })
  
  output$singleStationAnnualTSImetrics <- renderTable({
    req(oneStationData)
    z <- TSI_Assessment_OneStation(oneStationData,secchi)[[1]] %>% ungroup() %>%
      dplyr::select(FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,FDT_COMMENT,PHOSPHORUS,CHLOROPHYLL,
                    `Secchi Depth`,TSI_TP,TSI_CA,TSI_SD)
    z$FDT_DATE_TIME <- as.character(z$FDT_DATE_TIME)
    return(z)})
  }

TSISubTabUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             helpText("The table below summarizes the last six years of Total Phosphorus, Chlorophyll a,
                      and Secchi Depth sampling data in the assessment unit. Prior to analyzing for TSI,
                      all StationIDs are first filtered by sampling window (June 15 - Sept 15 if each year), then 
                      each measure is verified that are within the epilimnion before applying the appropriate
                      Trophic State Index equation (see assessment guidance for calculation details)."),br(),
             helpText("For each monitoring station, if one or more of the TSIs >= 60, the non-187 
                      lake/reservoir will be assessed as impaired partially due to one or more pollutants from 
                      anthropogenic sources. The assessment unit or entire lake/reservoir will be placed in 
                      Category 5A for TMDL development."),br(),
             helpText("For each monitoring station, if each of the TSIs < 60, the lake/reservoir will be assessed
                      as impaired due to pollution from natural sorces and placed in Category 4C. A TMDL is not
                      needed for the assessment unit represented by the monitoring station(s) and appropriate DO
                      criteria will be developed for the hypolimnion."),
             h5(strong('Trophic State Index Annual Summary')),
             div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('TSIsummaryTable')))),
      column(6,
             helpText('The tropic state assessment determination below is based on the data reported in the 
                      Trophic State Index Annual Summary table immediately to the left. These ranges are available
                      in the assessment guidance. The trophic state reported in ADB should be reflective of the 
                      entire lake/reservoir, thus assessors should use best professional judgement if the multiple
                      stations report different trophic states.'),
             h5('Lake Trophic State:'),
             tableOutput(ns('TSI_exceedanceRate'))))
             )
}

TSISubTab <- function(input,output,session, allLakeLZData){
  ns <- session$ns
  
  output$TSIsummaryTable <- renderTable({
    req(allLakeLZData())
    exceedance_TP(allLakeLZData())})
  
  output$TP_exceedanceRate <- renderTable({
    req(allLakeLZData())
    assessmentDetermination(exceedance_TP(allLakeLZData()),filter(exceedance_TP(allLakeLZData()),TP_Exceedance ==T),'Total Phosphorus','Aquatic Life')})
}


ui <- fluidPage(
  TSISingleStationSubTabUI('TSIonestation'))

server <- function(input,output,session){
  callModule(TSISingleStationSubTab,'TSIonestation',x)
}

shinyApp(ui,server)

