chlAPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(6, uiOutput(ns('chlA_oneStationSelectionUI'))),
               column(6,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      plotlyOutput(ns('chlAplotly')),
      br(),hr(),br(),
      fluidRow(
        column(6, h5('Annual Chlorophyll a 90th percentiles for the ',span(strong('most recent two years at the selected site')),' are reported below.'),
               div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('singleStationAnnualChla90thTable')))),
        column(6,  h5('Chlorophyll a exceedance statistics for the ',span(strong('selected site')),' are highlighted below. Note
                      whether or not the station meets the lacustrine zone requirement before making any final assessments.'),
               tableOutput(ns('stationChlaExceedanceRate'))))
    ), br(),hr(), br(),
    fluidRow(
      column(6,
             helpText('The table below summarizes the last two years of Chlorophyll a sampling data.
                      All StationIDs are first analyzed by sampling window (April - October), then 
                      each year is verified that there are enough samples (> 6) within the window before the 90th 
                      percentile of Chlorophyll a measures within the top meter is calculated. Only StationIDs that
                      fall within the lacustrine zone are used for this analysis. The criteria are based
                      on individual limits specified for each Section 187 Lake (9VAC25-260-187). If the 
                      assessment of the last two years conflict, the last three years are analyzed and 
                      reported below.'),
             h5(strong('Chlorophyll a Annual Summary for All Stations in Lake')),
             div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('chla90thTable')))),
      column(6,
             helpText('The exceedance analysis below is based on data reported in the Chlorophyll a
                      Annual Summary table immediately to the left. This exceedance calculation
                      pools all data that meets the requirements outlined in the table header.'),
             h5('Chlorophyll a exceedance statistics for the ',span(strong('lake')),' are highlighted below. ',span(strong('(ONLY LACUSTRINE ZONE STATIONS):'))),
             tableOutput(ns('chla_exceedanceRate')),
             h5('Chlorophyll a exceedance statistics for the ',span(strong('lake')),' are highlighted below. ',span(strong('(ALL STATIONS):'))),
             tableOutput(ns('chla_exceedanceRateALL'))   )),
    br(),br(), br()
  )
}


chlAPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove, allLakeLZData, lakeStations){
  ns <- session$ns
  
  # Select One station for individual review
  output$chlA_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('chlA_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),
                
                width='300px', selected = stationSelectedAbove())})
  
  chlA_oneStation <- reactive({
    req(ns(input$chlA_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$chlA_oneStationSelection) %>%
      filter(!is.na(CHLOROPHYLL))})
  
  
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
    req(chlA_oneStation())
    parameterFilter <- dplyr::select(chlA_oneStation(), FDT_STA_ID:FDT_DEPTH_DESC, CHLOROPHYLL, RMK_32211, ThermoclineDepth_RMK, LakeStratification)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t')) %>%
      formatStyle(c('CHLOROPHYLL','RMK_32211'), 'RMK_32211', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  output$chlAplotly <- renderPlotly({
    req(input$chlA_oneStationSelection, chlA_oneStation())
    dat <- chlA_oneStation() %>%
      mutate(LakeStratification = replace_na(LakeStratification,"NONE"))
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    dat$LakeStratification <- as.factor(dat$LakeStratification)
    dat$LakeStratification <- factor(dat$LakeStratification,levels=c("Epilimnion","NONE","Hypolimnion"))#,ordered=T)
    
    
    plot_ly(data=dat)%>%
      add_lines(data=dat, x=~SampleDate,y=~Chlorophyll_A_limit, mode='line',line = list(color = 'black'),
                hoverinfo = "text",text=paste("Chlorophyll a limit:",unique(dat$Chlorophyll_A_limit),'ug/L'), 
                name="Chlorophyll a limit") %>%
      add_markers(x= ~SampleDate, y= ~CHLOROPHYLL,mode = 'scatter', name="Chlorophyll a (ug/L)",
                  color=~LakeStratification, #marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Chlorophyll a: ",CHLOROPHYLL,"ug/L"),
                                               paste("LakeStratification: ",LakeStratification)))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Chlorophyll a (ug/L)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) })
  
  # selected station chl a 90th table
  output$singleStationAnnualChla90thTable <- renderTable({
    req(chlA_oneStation())
    chlA_Assessment_OneStation(citmonOutOfParameterDataset(chlA_oneStation(), CHLOROPHYLL, RMK_32211)) })
  
  # selected station exceedance rate
  output$stationChlaExceedanceRate <- renderTable({
    req(chlA_oneStation())
    LZ <- exceedance_chlA(citmonOutOfParameterDataset(chlA_oneStation(), CHLOROPHYLL, RMK_32211),lakeStations)
    
    if(class(LZ) == 'character'){
      return("No Chlorophyll a data to assess.")
    }else{
      LZ <- LZ$LacustrineZone[1]
      assessmentDetermination(exceedance_chlA(citmonOutOfParameterDataset(chlA_oneStation(), CHLOROPHYLL, RMK_32211), lakeStations),
                              filter(exceedance_chlA(citmonOutOfParameterDataset(chlA_oneStation(), CHLOROPHYLL, RMK_32211), lakeStations), 
                                     chlA_Exceedance ==T),'Chlorophyll a','Aquatic Life') %>%
        mutate(LacustrineZone=LZ) %>%# for chla and TP only, show whether or not in lacustrine zone with single station exceedance
        dplyr::select(nSamples,nExceedance,exceedanceRate, LacustrineZone) # don't give assessment determination for single station}})
    }
  })
  
  
  
  # Lake Chl a 90th percentiles
  output$chla90thTable <- renderTable({
    req(allLakeLZData())
    exceedance_chlA(citmonOutOfParameterDataset(allLakeLZData(), CHLOROPHYLL, RMK_32211), lakeStations)})
  
  
  # All Lake LACUSTRINE ZONE Chlorophyll a exceedance rate
  output$chla_exceedanceRate <- renderTable({
    req(allLakeLZData())
    z <- exceedance_chlA(citmonOutOfParameterDataset(allLakeLZData(), CHLOROPHYLL, RMK_32211), lakeStations)
    if(class(z)=='character'){
      return("No Chlorophyll a data to assess.")
    }else{
      assessmentDetermination(filter(exceedance_chlA(citmonOutOfParameterDataset(allLakeLZData(), CHLOROPHYLL, RMK_32211), lakeStations), 
                                     LacustrineZone == TRUE),
                              filter(exceedance_chlA(citmonOutOfParameterDataset(allLakeLZData(), CHLOROPHYLL, RMK_32211), lakeStations),
                                     chlA_Exceedance ==T & LacustrineZone == TRUE),'Chlorophyll a','Aquatic Life')%>%
        dplyr::select(nSamples, nExceedance, exceedanceRate)}})
  
  
  # All Lake Chlorophyll a exceedance rate REGARDLESS OF WHETHER OR NOT STATIONS ARE IN LACUSTRINE ZONE 
  output$chla_exceedanceRateALL <- renderTable({
    req(allLakeLZData())
    z <- exceedance_chlA(citmonOutOfParameterDataset(allLakeLZData(), CHLOROPHYLL, RMK_32211), lakeStations)
    if(class(z)=='character'){
      return("No Chlorophyll a data to assess.")
    }else{
      assessmentDetermination(z,filter(exceedance_chlA(citmonOutOfParameterDataset(allLakeLZData(), CHLOROPHYLL, RMK_32211), lakeStations),
                                       chlA_Exceedance ==T),'Chlorophyll a','Aquatic Life')%>%
        dplyr::select(nSamples, nExceedance, exceedanceRate)}})
}