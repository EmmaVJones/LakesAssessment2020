
DOPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('DO_oneStationSelectionUI')),
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
    filter(AUdata(),FDT_STA_ID %in% input$DO_oneStationSelection)})
  
  
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
    DO_Assessment(DO_oneStation(),c("Epilimnion",NA))%>%dplyr::select(-c(FDT_STA_ID))})
  
  # Epilimnion one station
  output$stationDOexceedanceRate_epi <- renderTable({
    req(DO_oneStation())
    exceedance_DO(DO_oneStation(),"Epilimnion") %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  # Epilimnion and no stratification samples, one station
  output$stationDOexceedanceRate_epiAndNA <- renderTable({
    req(DO_oneStation())
    exceedance_DO(DO_oneStation(),c("Epilimnion",NA)) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  # All samples, one station
  output$stationDOexceedanceRate_noStrat <- renderTable({
    req(DO_oneStation())
    exceedance_DO(DO_oneStation(),c("Epilimnion",NA,"Hypolimnion")) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  
  # Highlight records that don't meet DO criteria, ASSESSMENT UNIT
  output$DOrangeTable <- renderTable({
    req(AUdata())
    DO_Assessment(AUdata(),c("Epilimnion",NA))})
  
  # Epilimnion ASSESSMENT UNIT
  output$DOexceedanceRate_epi <- renderTable({
    req(AUdata())
    exceedance_DO(AUdata(),"Epilimnion") %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  
  # Epilimnion and no stratification samples ASSESSMENT UNIT
  output$DOexceedanceRate_epiAndNA <- renderTable({
    req(AUdata())
    exceedance_DO(AUdata(),c("Epilimnion",NA)) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  
  # All samples ASSESSMENT UNIT
  output$DOexceedanceRate_noStrat <- renderTable({
    req(AUdata())
    exceedance_DO(AUdata(),c("Epilimnion",NA,"Hypolimnion")) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
}
