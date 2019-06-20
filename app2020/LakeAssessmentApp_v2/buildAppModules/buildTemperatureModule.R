lakeStations <- read_csv('processedStationData/final2020data/lakeStations2020_BRRO_citmonNonAgency.csv')


lake_filter <- filter(lakeStations, SIGLAKENAME == "Lake Gaston, (Virginia portion)")#'Smith Mountain Lake')#'Leesville Reservoir')


conventionals_Lake <- filter(conventionals, FDT_STA_ID %in% unique(lake_filter$FDT_STA_ID)) %>%
  left_join(dplyr::select(lakeStations, FDT_STA_ID, SEC, CLASS, SPSTDS,PWS, ID305B_1, ID305B_2, ID305B_3,
                          STATION_TYPE_1, STATION_TYPE_2, STATION_TYPE_3, ID305B, SEC187, SIG_LAKE, USE,
                          SIGLAKENAME, Chlorophyll_A_limit, TPhosphorus_limit, Assess_TYPE), by='FDT_STA_ID')



AUData <- filter(conventionals_Lake, ID305B_1 %in%  'VAW-L79L_ROA07A98')%>%#'VAW-L10L_BSA01A10') %>% # "VAW-L13L_ROA01A18") %>% 
  left_join(WQSvalues, by = 'CLASS') 

stationData <- filter(AUData, FDT_STA_ID %in% "LVLAROA140.66") #"9-NEW087.14" "9-NEW089.34"



# Create Data frame with all data within ID305B and stratification information
# Pool thermocline data to get 1 sample per day, not 2 with top/bottom time difference
#stationDataDailySample <- reactive({
#  req(AUData())
dat <- AUData
dat$FDT_DATE_TIME <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")

dat <- mutate(dat, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))#%>% # Separate sampling events by day
 # filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
#thermo <- stratifiedLake(dat)
thermo <- stratifiedLake_citNonA(citmonOutOfParameterDataset(filter(dat,!is.na(FDT_TEMP_CELCIUS)), FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK) )
thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
stationDataDailySample <- plyr::join(dat,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
  mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))



temperaturePlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(6,uiOutput(ns('temperature_oneStationSelectionUI'))),
               column(6,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      plotlyOutput(ns('temperatureplotly')),
      br(),hr(),br(),
      fluidRow(
        column(8, h5('All temperature records that are above the criteria for the ',span(strong('selected site')),' are highlighted below.'),
               div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('TempRangeTableSingleSite')))),
        column(4, h5('Individual temperature exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               tableOutput(ns("stationTempExceedanceRate")))
      )),
    br(),hr(), br(),
    fluidRow(
      column(8, h5('All temperature records that are above the criteria for the ',span(strong('assessment unit')),' are highlighted below.'),
             div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('TempRangeTableAU')))),
      column(4, h5('Temperature exceedance statistics for the ',span(strong('assessment unit')),' are highlighted below.'),
             tableOutput(ns("AUTempExceedanceRate")))
    ),
    br(),br(), br()
  )
}




temperaturePlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$temperature_oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('temperature_oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='300px', selected = stationSelectedAbove())})
  
  temperature_oneStation <- reactive({
    req(ns(input$temperature_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$temperature_oneStationSelection)%>%
      filter(!is.na(FDT_TEMP_CELCIUS))})
  
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
    req(temperature_oneStation())
    parameterFilter <- dplyr::select(temperature_oneStation(), FDT_STA_ID:FDT_DEPTH_DESC, FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK, ThermoclineDepth_RMK, LakeStratification)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t')) %>%
      formatStyle(c('FDT_TEMP_CELCIUS','FDT_TEMP_CELCIUS_RMK'), 'ThermoclineDepth_RMK', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  output$temperatureplotly <- renderPlotly({
    req(input$temperature_oneStationSelection, temperature_oneStation())
    
    dat <- mutate(temperature_oneStation(), LakeStratification = replace_na(LakeStratification,"NONE"))
    
    dat$SampleDate <- as.POSIXct(dat$SampleDate, format="%m/%d/%y")
    dat$LakeStratification <- as.factor(dat$LakeStratification)
    dat$LakeStratification <- factor(dat$LakeStratification,levels=c("Epilimnion","NONE","Hypolimnion"))#,ordered=T)
    
    suppressWarnings(
      plot_ly(data=dat)%>%
        add_markers(x=~SampleDate,y=~FDT_TEMP_CELCIUS, mode = 'scatter', name="Temperature",
                    color=~LakeStratification,# colors=c('#BF382A', '#0C4B8E'),
                    hoverinfo="text",text=~paste(sep="<br>",paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Temperature:",FDT_TEMP_CELCIUS,"C"),
                                                 paste("LakeStratification: ",LakeStratification))) %>%
        add_lines(x=~SampleDate, y=~`Max Temperature (C)`, mode='line',line = list(color = 'black'),
                  hoverinfo = "TemperatureSTD", text= paste("Temperature Standard (C):",unique(dat$`Max Temperature (C)`),sep=''), name = 'Temperature Standard')%>%
        
        layout(showlegend=FALSE,
               yaxis=list(title="Temperature (Celsius)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10))
        ))
  })
  
  
  # Station temperature exceedances
  output$TempRangeTableSingleSite <- renderTable({
    req(temperature_oneStation())
    temp_Assessment(citmonOutOfParameterDataset(temperature_oneStation(), FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK))})
  
  # Station temperature Exceedance Rate
  output$stationTempExceedanceRate <- renderTable({
    req(ns(input$temperature_oneStationSelection), temperature_oneStation())
    
    exceedance_temp(citmonOutOfParameterDataset(temperature_oneStation(), FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK)) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  # AU temperature exceedances
  output$TempRangeTableAU <- renderTable({
    req(AUdata())
    temp_Assessment(citmonOutOfParameterDataset(AUdata(), FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK))})
  
  # AU temperature Exceedance Rate
  output$AUTempExceedanceRate <- renderTable({
    req(AUdata())
    
    exceedance_temp(citmonOutOfParameterDataset(AUdata(), FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK)) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
}




ui <- fluidPage(
  helpText('Review each site using the single site visualization section'),
  selectInput('stationSelection', 'Station Selection', choices = unique(AUData$FDT_STA_ID)),
  temperaturePlotlySingleStationUI('temperature')
)

server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  AUData <- reactive({filter(conventionals_Lake, ID305B_1 %in% c('VAW-L79L_ROA07A98','VAW-L80L_ROA08A04')) %>% #'VAW-L10L_BSA01A10') %>% #"VAW-L13L_ROA01A18") %>% 
      left_join(WQSvalues, by = 'CLASS') })
  
  # Create Data frame with all data within ID305B and stratification information
  # Pool thermocline data to get 1 sample per day, not 2 with top/bottom time difference
  stationDataDailySample <- reactive({
    req(AUData())
    dat <- AUData()
    dat$FDT_DATE_TIME <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")

    dat <- mutate(dat, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y")) # Separate sampling events by day
     # filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
    thermo <- stratifiedLake_citNonA(citmonOutOfParameterDataset(filter(dat, !is.na(FDT_TEMP_CELCIUS)), FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK) )
    #Old way but can accidentally lose stations if no temperature data
    #thermo <- stratifiedLake_citNonA(citmonOutOfParameterDataset(dat, FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK) ) #thermo <- stratifiedLake(dat)
    thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
    dat2 <- plyr::join(dat,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
      mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))
    return(dat2)
  })
  
  
  
  callModule(temperaturePlotlySingleStation,'temperature', stationDataDailySample, stationSelected)
  

  
}

shinyApp(ui,server)


  