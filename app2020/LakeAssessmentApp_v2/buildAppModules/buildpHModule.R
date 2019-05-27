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



pHPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(6,uiOutput(ns('pH_oneStationSelectionUI'))),
               column(6,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      plotlyOutput(ns('pHplotly')),
      br(),hr(),br(),
      fluidRow(
        column(8, h5('All pH records that fall outside the criteria for the ',span(strong('selected site')),' in the ',
                     span(strong('epilimnion or unstratified samples')),' are highlighted below.'),div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('pHRangeTableSingleSite')))),
        column(4, h5('Individual pH exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               tableOutput(ns("stationpHExceedanceRate"))))
    ),
    br(),hr(),br(),
    fluidRow(
      column(8, h5('All pH records that fall outside the criteria for the ',span(strong('assessment unit')),' in the ',
                   span(strong('epilimnion or unstratified samples')),' are highlighted below.'),
             div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('pHRangeTableAU'))),
             h5(strong("Assessment Guidance:")),
             p('In cases where the applicable nutrient criteria are met for the man-made lakes/reservoirs listed in §187 
               but the pH criterion range is not met, the lake or reservoir should be classified as Category 5C and recommended 
               for a WQS review due to natural pH fluctuations. In lakes that are not in §187, the waterbody would be listed as 
               impaired (Category 5C), as well. See lakes/reservoir assessment flowchart below. See lakes/reservoirs assessment 
               flowchart for additional guidance.')),
      column(4, h5('Individual pH exceedance statistics for the ',span(strong('assessment unit')),' are highlighted below.'),
             tableOutput(ns("AUpHExceedanceRate")))),
    br(),br(), br()
  )
}

pHPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$pH_oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('pH_oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='300px', selected = stationSelectedAbove())})
  
  pH_oneStation <- reactive({
    req(ns(input$pH_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$pH_oneStationSelection)%>%
      filter(!is.na(FDT_FIELD_PH))})
  
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
    req(pH_oneStation())
    parameterFilter <- dplyr::select(pH_oneStation(), FDT_STA_ID:FDT_DEPTH_DESC, FDT_FIELD_PH, FDT_FIELD_PH_RMK,  ThermoclineDepth_RMK, LakeStratification)

    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t')) %>%
      formatStyle(c('FDT_FIELD_PH','FDT_FIELD_PH_RMK'), 'FDT_FIELD_PH_RMK', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))

  })
  
  
  output$pHplotly <- renderPlotly({
    req(input$pH_oneStationSelection, pH_oneStation())
    dat <- mutate(pH_oneStation(),top = `pH Max`, bottom = `pH Min`)  %>%
      mutate(LakeStratification = replace_na(LakeStratification,"NONE"))
    
    dat$SampleDate <- as.POSIXct(dat$SampleDate, format="%m/%d/%y")
    dat$LakeStratification <- as.factor(dat$LakeStratification)
    dat$LakeStratification <- factor(dat$LakeStratification,levels=c("Epilimnion","NONE","Hypolimnion"))#,ordered=T)
    
    
    plot_ly(data=dat)%>%
      add_lines(data=dat, x=~SampleDate,y=~top, mode='line',line = list(color = 'black'),
                hoverinfo = "text",text="pH Standard", name="pH Standard") %>%
      add_lines(data=dat, x=~SampleDate,y=~bottom, mode='line',line = list(color = 'black'),
                hoverinfo = "text", text="pH Standard", name="pH Standard") %>%
      add_markers(data=dat, x= ~SampleDate, y= ~FDT_FIELD_PH,mode = 'scatter', name="pH (unitless)", 
                  color=~LakeStratification, #colors=c('#BF382A', '#0C4B8E'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("pH: ",FDT_FIELD_PH," (unitless)"),
                                               paste("LakeStratification: ",LakeStratification)))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="pH (unitless)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))
  })
  
  # pH Exceedance Results by Station
  output$pHRangeTableSingleSite <- renderTable({
    req(pH_oneStation(),input$pH_oneStationSelection)
    pH_rangeAssessment(citmonOutOfParameterDataset(pH_oneStation(), FDT_FIELD_PH, FDT_FIELD_PH_RMK))})
  
  output$stationpHExceedanceRate <- renderTable({
    req(pH_oneStation(),input$pH_oneStationSelection)
    exceedance_pH(citmonOutOfParameterDataset(pH_oneStation(), FDT_FIELD_PH, FDT_FIELD_PH_RMK)) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  # pH Exceedance Results for Assessment Unit
  output$pHRangeTableAU <- renderTable({
    req(AUdata())
    pH_rangeAssessment(citmonOutOfParameterDataset(AUdata(), FDT_FIELD_PH, FDT_FIELD_PH_RMK))})
  
  output$AUpHExceedanceRate <- renderTable({
    req(AUdata())
    exceedance_pH(citmonOutOfParameterDataset(AUdata(), FDT_FIELD_PH, FDT_FIELD_PH_RMK))%>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination 
  
  
  
}






ui <- fluidPage(
  helpText('Review each site using the single site visualization section'),
  selectInput('stationSelection', 'Station Selection', choices = unique(AUData$FDT_STA_ID)),
  pHPlotlySingleStationUI('pH')
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
  
  callModule(pHPlotlySingleStation,'pH', stationDataDailySample, stationSelected)
  
  
}

shinyApp(ui,server)

