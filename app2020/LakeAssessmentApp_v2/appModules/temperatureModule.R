

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

