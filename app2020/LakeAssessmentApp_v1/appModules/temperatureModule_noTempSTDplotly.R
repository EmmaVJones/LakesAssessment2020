
temperaturePlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('temperature_oneStationSelectionUI')),
      fluidRow(
        column(6,
               h5(strong("Thermocline Analysis")),
               DT::dataTableOutput(ns('thermoclineResults'))),
        column(6,
               uiOutput(ns('stationDateSelection')),
               plotlyOutput(ns('thermoclinePlotly')))),
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
    )
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
    filter(AUdata(),FDT_STA_ID %in% input$temperature_oneStationSelection)})
  
  
  # Thermocline Analysis Table
  output$thermoclineResults <- DT::renderDataTable({
    req(temperature_oneStation())
    dat <- dplyr::select(temperature_oneStation(),SampleDate,ThermoclineDepth)%>%
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
    req(temperature_oneStation())
    selectInput(ns('thermoDateSelection'),strong("Choose a Sample Date to Plot"),choices = unique(temperature_oneStation()$SampleDate))
  })
  
  
  # Thermoncline plotly based on user date input
  output$thermoclinePlotly <- renderPlotly({
    req(temperature_oneStation(),input$thermoDateSelection)
    dat <- dplyr::filter(temperature_oneStation(),SampleDate %in% input$thermoDateSelection)
    
    if(dat$ThermoclineDepth[1] != 'NaN'){
      suppressWarnings(
        plot_ly(data=dat)%>%
          add_lines(x=~FDT_TEMP_CELCIUS,y=~ThermoclineDepth, mode='line',line = list(color = '#E50606'),
                    hoverinfo = "Thermocline", text= paste("Thermocline Depth:",unique(dat$ThermoclineDepth),sep=''), name = 'Thermocline')%>%
          add_markers(x= ~FDT_TEMP_CELCIUS, y= ~FDT_DEPTH,mode = 'scatter', name="Temperature",
                      color=~LakeStratification, colors=c('#BF382A', '#0C4B8E'),
                      hoverinfo="text",text=~paste(sep="<br>",paste("Depth: ",FDT_DEPTH, "m"),
                                                   paste("Temperature:",FDT_TEMP_CELCIUS,"C"),
                                                   paste("LakeStratification: ",LakeStratification)))%>%
          layout(xaxis = list(autorange = "reversed",title="Temperature (Celcius)"),
                 yaxis = list(autorange = "reversed",title="Depth (m)"),
                 showlegend=FALSE
          ))
    }else{
      suppressWarnings(
        plot_ly(data=dat,y=~FDT_DEPTH,x=~FDT_TEMP_CELCIUS,type='scatter',mode='markers',
                hoverinfo="text",text=~paste(sep="<br>",paste("Depth: ",FDT_DEPTH, "m"),
                                             paste("Temperature:",FDT_TEMP_CELCIUS,"C")))%>%
          layout(xaxis = list(autorange = "reversed",title="Temperature (Celcius)"),
                 yaxis = list(autorange = "reversed",title="Depth (m)"),
                 showlegend=FALSE))}
  })
  
  
  # Station temperature exceedances
  output$TempRangeTableSingleSite <- renderTable({
    req(temperature_oneStation())
    temp_Assessment(temperature_oneStation())})
  
  # Station temperature Exceedance Rate
  output$stationTempExceedanceRate <- renderTable({
    req(ns(input$temperature_oneStationSelection), temperature_oneStation())
    
    exceedance_temp(temperature_oneStation()) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  # AU temperature exceedances
  output$TempRangeTableAU <- renderTable({
    req(AUdata())
    temp_Assessment(AUdata())})
  
  # AU temperature Exceedance Rate
  output$AUTempExceedanceRate <- renderTable({
    req(AUdata())
    
    exceedance_temp(AUdata()) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
}