TPPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('TP_oneStationSelectionUI')),
      plotlyOutput(ns('TPplotly')),
      br(),hr(),br(),
      fluidRow(
        column(6, h5('Annual median Total Phosphorus for the ',span(strong('most recent two years at the selected site')),' are reported below.'),
               div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('singleStationAnnualMedianTPtable')))),
        column(6,  h5('Total phosphorus exceedance statistics for the ',span(strong('selected site')),' are highlighted below. Note
                      whether or not the station meets the lacustrine zone requirement before making any final assessments.'),
               tableOutput(ns('stationTPExceedanceRate'))))
      ), br(),hr(), br(),
    fluidRow(
      column(6,
             helpText('The table below summarizes the last two years of Total Phosphorus sampling data.
                      All StationIDs are first analyzed by sampling window (April - October), then 
                      each year is verified that there are enough samples (> 6) within the window before the  
                      median of Total Phosphorus measures within the top meter is calculated. Only StationIDs that
                      fall within the lacustrine zone are used for this analysis. The criteria are based
                      on individual limits specified for each Section 187 Lake (9VAC25-260-187). If the 
                      assessment of the last two years conflict, the last three years are analyzed and 
                      reported below.'),
             h5(strong('Total Phosphorus Annual Summary for All Lacustrine Stations in Lake')),
             div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('medianTPtable')))),
      column(6,
             helpText('The exceedance analysis below is based on data reported in the Total Phosphorus
                      Annual Summary table immediately to the left. This exceedance calculation
                      pools all data that meets the above mentioned requirements across all lake sites
                      in the lacustrine zone.'),
             h5('Total phosphorus exceedance statistics for the ',span(strong('lake')),' are highlighted below. ',span(strong('(ONLY LACUSTRINE ZONE STATIONS):'))),
             tableOutput(ns('TP_exceedanceRate')),
             h5('Total phosphorus exceedance statistics for the ',span(strong('lake')),' are highlighted below. ',span(strong('(ALL STATIONS):'))),
             tableOutput(ns('TP_exceedanceRateALL'))  )),
    br(),br(), br()
    
      )
}


TPPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove, allLakeLZData){
  ns <- session$ns
  
  # Select One station for individual review
  output$TP_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('TP_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),
                
                width='300px', selected = stationSelectedAbove())})
  
  TP_oneStation <- reactive({
    req(ns(input$TP_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$TP_oneStationSelection)})
  
  output$TPplotly <- renderPlotly({
    req(input$TP_oneStationSelection, TP_oneStation())
    dat <- mutate(TP_oneStation(),TPhosphorus_limit_ug_L=TPhosphorus_limit/1000)
    
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    
    plot_ly(data=dat)%>%
      add_lines(data=dat, x=~SampleDate,y=~TPhosphorus_limit_ug_L, mode='line',line = list(color = 'black'),
                hoverinfo = "text",text=paste("Total Phosphorus limit:",unique(dat$TPhosphorus_limit_ug_L),'ug/L'), 
                name="Total Phosphorus limit") %>%
      add_markers(x= ~SampleDate, y= ~PHOSPHORUS,mode = 'scatter', name="Total Phosphorus (mg/L)",marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Total Phosphorus: ",PHOSPHORUS,"ug/L")))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Total Phosphorus (ug/L)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) })
  
  output$singleStationAnnualMedianTPtable <- renderTable({
    req(TP_oneStation())
    TP_Assessment_OneStation(TP_oneStation()) })
  
  output$stationTPExceedanceRate <- renderTable({
    req(TP_oneStation())
    LZ <- exceedance_TP(TP_oneStation())$LacustrineZone[1]
    assessmentDetermination(exceedance_TP(TP_oneStation()),
                            filter(exceedance_TP(TP_oneStation()),TP_Exceedance ==T),
                            'Total Phosphorus','Aquatic Life')%>%
      mutate(LacustrineZone=LZ) %>%# for chla and TP only, show whether or not in lacustrine zone with single station exceedance
      dplyr::select(nSamples,nExceedance,exceedanceRate, LacustrineZone) # don't give assessment determination for single station
  })
  
  output$medianTPtable <- renderTable({
    req(allLakeLZData())
    exceedance_TP(allLakeLZData())})
  
  # All Lake LACUSTRINE ZONE Total Phosphorus Exceedance Results 
  output$TP_exceedanceRate <- renderTable({
    req(allLakeLZData())
    assessmentDetermination(filter(exceedance_TP(allLakeLZData()), LacustrineZone == TRUE),
                            filter(exceedance_TP(allLakeLZData()),TP_Exceedance ==T & LacustrineZone == TRUE),'Total Phosphorus','Aquatic Life')%>%
      dplyr::select(nSamples, nExceedance, exceedanceRate)})
  
  
  
  # All Lake Total Phosphorus exceedance rate REGARDLESS OF WHETHER OR NOT STATIONS ARE IN LACUSTRINE ZONE 
  output$TP_exceedanceRateALL <- renderTable({
    req(allLakeLZData())
    assessmentDetermination(exceedance_TP(allLakeLZData()),filter(exceedance_TP(allLakeLZData()),TP_Exceedance ==T),'Total Phosphorus','Aquatic Life')%>%
      dplyr::select(nSamples, nExceedance, exceedanceRate)})
  
}
