lakeStations <- read_csv('processedStationData/final2020data/lakeStations2020_BRRO_citmonNonAgency.csv')


lake_filter <- filter(lakeStations, SIGLAKENAME == 'Leesville Reservoir')


conventionals_Lake <- filter(conventionals, FDT_STA_ID %in% unique(lake_filter$FDT_STA_ID)) %>%
  left_join(dplyr::select(lakeStations, FDT_STA_ID, SEC, CLASS, SPSTDS,PWS, ID305B_1, ID305B_2, ID305B_3,
                          STATION_TYPE_1, STATION_TYPE_2, STATION_TYPE_3, ID305B, SEC187, SIG_LAKE, USE,
                          SIGLAKENAME, Chlorophyll_A_limit, TPhosphorus_limit, Assess_TYPE), by='FDT_STA_ID')



AUData <- filter(conventionals_Lake, ID305B_1 %in% "VAW-L13L_ROA01A18") %>% 
  left_join(WQSvalues, by = 'CLASS') 

stationData <- filter(AUData, FDT_STA_ID %in% "LVLAROA140.66") #"9-NEW087.14" "9-NEW089.34"



# Create Data frame with all data within ID305B and stratification information
# Pool thermocline data to get 1 sample per day, not 2 with top/bottom time difference
#stationDataDailySample <- reactive({
#  req(AUData())
dat <- AUData
dat$FDT_DATE_TIME <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")

dat <- mutate(dat, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
  filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
#thermo <- stratifiedLake(dat)
thermo <- stratifiedLake_citNonA(citmonOutOfParameterDataset(dat, FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK) )
thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
stationDataDailySample <- plyr::join(dat,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
  mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))




DOPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(6,uiOutput(ns('DO_oneStationSelectionUI'))),
               column(6,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      
      plotlyOutput(ns('DOplotly')),
      fluidRow(column(3),column(4,actionButton(ns('zoomPlotDO'),"Zoomed Plot by Sample Date",class='btn-block'))),
      br(),hr(),br(),
      fluidRow(
        column(8, h5('All DO records that fall below the criteria for the ',span(strong('selected site')),' in the ',
                     span(strong('epilimnion or unstratified samples')),' are highlighted below.'),
               div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('DOrangeTableSingleSite')))),
        column(4, h5('Dissolved oxygen exceedance rate for the ', span(strong('selected station.'))),
               h6("Results for the ", span(strong('selected station in the epilimnion')), " throughout sampling season."),
               tableOutput(ns('stationDOexceedanceRate_epi')),
               h6("Results for the ", span(strong('selected station in the epilimnion and unstratified sample profiles'))," throughout sampling season."),
               tableOutput(ns('stationDOexceedanceRate_epiAndNA')),
               h6("Results for the ", span(strong('selected station'))," data throughout sampling season."),
               tableOutput(ns('stationDOexceedanceRate_noStrat'))  ) ) ),
    br(),hr(),br(),
    fluidRow(
      column(8,
             h5('All DO records that fall below the criteria for the ',span(strong('assessment unit')),' in the ',
                span(strong('epilimnion or unstratified samples')),' are highlighted below.'),
             div(style = 'height:300px;overflow-y: scroll', tableOutput(ns('DOrangeTable')))),
      column(4,
             h5('Dissolved oxygen exceedance rate for the ', span(strong('assessment unit.'))),
             h6("Results for ", span(strong('all data in the epilimnion')), " throughout sampling season."),
             tableOutput(ns('DOexceedanceRate_epi')),
             h6("Results for ", span(strong('all data in the epilimnion and unstratified sample profiles'))," throughout sampling season."),
             tableOutput(ns('DOexceedanceRate_epiAndNA')),
             h6("Results for ", span(strong('all data'))," throughout sampling season."),
             tableOutput(ns('DOexceedanceRate_noStrat')))),
    br(),br(), br()
    
  )
}


DOPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$DO_oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('DO_oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='300px', selected = stationSelectedAbove())})
  
  DO_oneStation <- reactive({
    req(ns(input$DO_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$DO_oneStationSelection) %>%
      filter(!is.na(DO))})
  
  # Button to visualize modal table of available parameter data
  observeEvent(input$reviewData,{
    showModal(modalDialog(
      title="Review Raw Data for Selected Station and Parameter",
      helpText('This table subsets the conventionals raw data by station selected in Single Station Visualization Section drop down and
               parameter currently reviewing. Scroll right to see the raw parameter values and any data collection comments. Data analyzed
               by app is highlighted in gray (all DEQ data and non agency/citizen monitoring Level III), data counted by app and noted in
               comment fields is highlighed in yellow (non agency/citizen monitoring Level II), and data NOT CONSIDERED in app is noted in
               orange (non agency/citizen monitoring Level I).'),
      h4(span(strong("Note that all citizen and non agency depth data are rounded to the nearest interger for thermocline analysis."), style="color:red")),
      DT::dataTableOutput(ns('parameterData')),
      easyClose = TRUE))  })
  
  # modal parameter data
  output$parameterData <- DT::renderDataTable({
    req(DO_oneStation())
    parameterFilter <- dplyr::select(DO_oneStation(), FDT_STA_ID:FDT_DEPTH_DESC, DO, DO_RMK, ThermoclineDepth_RMK, LakeStratification)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t')) %>%
      formatStyle(c('DO','DO_RMK'), 'DO_RMK', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  # Overall plot with DO standard
  output$DOplotly <- renderPlotly({
    req(DO_oneStation())
    dat <- DO_oneStation() %>%
      mutate(LakeStratification = replace_na(LakeStratification,"NONE"))
    
    #dat[is.na(dat$LakeStratification),]$LakeStratification <- 'NONE'
    dat$SampleDate <- as.POSIXct(dat$SampleDate, format="%m/%d/%y")
    dat$LakeStratification <- as.factor(dat$LakeStratification)
    dat$LakeStratification <- factor(dat$LakeStratification,levels=c("Epilimnion","NONE","Hypolimnion"))#,ordered=T)
    
    plot_ly(data=dat)%>%
      add_lines(x=~SampleDate,y=~`Dissolved Oxygen Min (mg/L)`, mode='line',line = list(color = '#E50606'),
                hoverinfo = "text", text= paste("DO Standard:",unique(dat$`Dissolved Oxygen Min (mg/L)`),' (mg/L)',sep=''), name = 'DO Standard')%>%
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
  
  #### DO MODAL ----------------------------------------------------------------------------------------------------------
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
    req(DO_oneStation)
    selectInput(ns('DOdateSelection'),"Choose a Sample Date to Plot",choices = unique(DO_oneStation()$SampleDate))
  })
  
  oneSampleDate <- reactive({
    req(input$DOdateSelection, DO_oneStation())
    dat <- filter(DO_oneStation(),SampleDate %in% input$DOdateSelection) %>%
      mutate(LakeStratification = replace_na(LakeStratification,"NONE"))
    
    #dat[is.na(dat$LakeStratification),]$LakeStratification <- 'NONE'
    dat$LakeStratification <- as.factor(dat$LakeStratification)
    dat$LakeStratification <- factor(dat$LakeStratification,levels=c("Epilimnion","NONE","Hypolimnion"))#,ordered=T)
    return(dat)
  })
  
  # Plotly DO by single sample date
  output$DOplotlyByDate <- renderPlotly({
    req(oneSampleDate())
    dat <- oneSampleDate()
    
    plot_ly(data=dat)%>%
      add_lines(x=~FDT_TEMP_CELCIUS,y=~`Dissolved Oxygen Min (mg/L)`, mode='line',line = list(color = '#E50606'),
                hoverinfo = "text",text= paste("DO Standard:",unique(dat$`Dissolved Oxygen Min (mg/L)`),' (mg/L)',sep=''), name = 'DO Standard') %>%
      add_markers(x= ~FDT_TEMP_CELCIUS, y= ~DO,mode = 'scatter', name="Dissolved Oxygen",
                  color=~LakeStratification, colors=c('#BF382A', '#0C4B8E'),
                  hoverinfo="text",text=~paste(sep="<br>",paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Dissolved Oxygen:",DO,"mg/L"),
                                               paste("LakeStratification: ",LakeStratification)))%>%
      layout(xaxis = list(autorange = "reversed"),
             showlegend=FALSE,
             yaxis=list(title="Dissolved Oxygen (mg/L)"),
             xaxis=list(title="Temperature (C)",
                        tickfont = list(size = 10)))
  })
  #### END MODAL -------------------------------------------------------------------------------------------
  
  
  # Highlight records that don't meet DO criteria for STATION
  output$DOrangeTableSingleSite <- renderTable({
    req(DO_oneStation())
    DO_Assessment(citmonOutOfParameterDataset(DO_oneStation(), DO, DO_RMK),c("Epilimnion",NA)) %>%
      dplyr::select(-c(FDT_STA_ID))})
  
  # Epilimnion one station
  output$stationDOexceedanceRate_epi <- renderTable({
    req(DO_oneStation())
    exceedance_DO(citmonOutOfParameterDataset(DO_oneStation(), DO, DO_RMK),"Epilimnion") %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  # Epilimnion and no stratification samples, one station
  output$stationDOexceedanceRate_epiAndNA <- renderTable({
    req(DO_oneStation())
    exceedance_DO(citmonOutOfParameterDataset(DO_oneStation(), DO, DO_RMK),c("Epilimnion",NA)) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  # All samples, one station
  output$stationDOexceedanceRate_noStrat <- renderTable({
    req(DO_oneStation())
    exceedance_DO(citmonOutOfParameterDataset(DO_oneStation(), DO, DO_RMK),c("Epilimnion",NA,"Hypolimnion")) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  
  # Highlight records that don't meet DO criteria, ASSESSMENT UNIT
  output$DOrangeTable <- renderTable({
    req(AUdata())
    DO_Assessment(citmonOutOfParameterDataset(AUdata(), DO, DO_RMK),c("Epilimnion",NA))})
  
  # Epilimnion ASSESSMENT UNIT
  output$DOexceedanceRate_epi <- renderTable({
    req(AUdata())
    exceedance_DO(citmonOutOfParameterDataset(AUdata(), DO, DO_RMK),"Epilimnion") %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  
  # Epilimnion and no stratification samples ASSESSMENT UNIT
  output$DOexceedanceRate_epiAndNA <- renderTable({
    req(AUdata())
    exceedance_DO(citmonOutOfParameterDataset(AUdata(), DO, DO_RMK),c("Epilimnion",NA)) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  
  # All samples ASSESSMENT UNIT
  output$DOexceedanceRate_noStrat <- renderTable({
    req(AUdata())
    exceedance_DO(citmonOutOfParameterDataset(AUdata(), DO, DO_RMK),c("Epilimnion",NA,"Hypolimnion")) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
}





ui <- fluidPage(
  helpText('Review each site using the single site visualization section'),
  selectInput('stationSelection', 'Station Selection', choices = unique(AUData$FDT_STA_ID)),
  DOPlotlySingleStationUI('DO')
)

server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  AUData <- reactive({filter(conventionals_Lake, ID305B_1 %in% "VAW-L13L_ROA01A18") %>% 
      left_join(WQSvalues, by = 'CLASS') })
  
  # Create Data frame with all data within ID305B and stratification information
  # Pool thermocline data to get 1 sample per day, not 2 with top/bottom time difference
  stationDataDailySample <- reactive({
    req(AUData())
    dat <- AUData()
    dat$FDT_DATE_TIME <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")
    
    dat <- mutate(dat, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
      filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
    thermo <- stratifiedLake_citNonA(citmonOutOfParameterDataset(dat, FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK) ) #thermo <- stratifiedLake(dat)
    thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
    dat2 <- plyr::join(dat,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
      mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))
    return(dat2)
  })
  
  
  
  callModule(DOPlotlySingleStation,'DO', stationDataDailySample, stationSelected)
  
  
}

shinyApp(ui,server)
