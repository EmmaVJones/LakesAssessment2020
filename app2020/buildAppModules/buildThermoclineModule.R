lake_filter <- filter(lakeStations, SIGLAKENAME == 'Talbott Reservoir')#'Claytor Lake')


conventionals_Lake <- filter(conventionals, FDT_STA_ID %in% unique(lake_filter$FDT_STA_ID)) %>%
  left_join(dplyr::select(lakeStations, FDT_STA_ID, SEC, CLASS, SPSTDS,PWS, ID305B_1, ID305B_2, ID305B_3,
                          STATION_TYPE_1, STATION_TYPE_2, STATION_TYPE_3, ID305B, SEC187, SIG_LAKE, USE,
                          SIGLAKENAME, Chlorophyll_A_limit, TPhosphorus_limit, Assess_TYPE), by='FDT_STA_ID')



AUData <- filter(conventionals_Lake, ID305B_1 %in% "VAW-L42L_DAN01A02" | #"VAW-N16L_NEW01A02" "VAW-N16L_NEW01B14" "VAW-N17L_PKC01A10" "VAW-N17L_PKC02A10"
                   ID305B_2 %in% "VAW-L42L_DAN01A02" | 
                   ID305B_2 %in% "VAW-L42L_DAN01A02") %>% 
  left_join(WQSvalues, by = 'CLASS') 

stationData <- filter(AUData, FDT_STA_ID %in% "4ADAN194.10") #"9-NEW087.14" "9-NEW089.34"



# Create Data frame with all data within ID305B and stratification information
# Pool thermocline data to get 1 sample per day, not 2 with top/bottom time difference
#stationDataDailySample <- reactive({
#  req(AUData())
  dat <- AUData()
  dat$FDT_DATE_TIME <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")
  
  dat <- mutate(dat, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
    filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
  thermo <- stratifiedLake(dat)
  thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
  stationDataDailySample <- plyr::join(dat,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
    mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))




thermoclinePlotlySingleStationUI <- function(id){
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
               fluidRow(
                 column(6,uiOutput(ns('stationDateSelection'))),
                 column(6,uiOutput(ns('stationTempStandardPlotly')))),
               plotlyOutput(ns('thermoclinePlotly'))))),
    br(),br(), br()
    )
}




thermoclinePlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
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
  
  output$stationTempStandardPlotly <- renderUI({
    checkboxInput(ns('tempStd'),strong("Plot Temperature Standard (C)"))
  })
  
  # Thermoncline plotly based on user date input
  output$thermoclinePlotly <- renderPlotly({
    req(temperature_oneStation(),input$thermoDateSelection)
    dat <- dplyr::filter(temperature_oneStation(),SampleDate %in% input$thermoDateSelection)
    
    if(dat$ThermoclineDepth[1] != 'NaN'){
      if(input$tempStd == TRUE){
        suppressWarnings(
          plot_ly(data=dat)%>%
            add_lines(x=~FDT_TEMP_CELCIUS,y=~ThermoclineDepth, mode='line',line = list(color = '#E50606'),
                      hoverinfo = "Thermocline", text= paste("Thermocline Depth:",unique(dat$ThermoclineDepth),sep=''), name = 'Thermocline')%>%
            add_lines(x=~`Max Temperature (C)`,y=~FDT_DEPTH, mode='line',line = list(color = 'black'),
                      hoverinfo = "TemperatureSTD", text= paste("Temperature Standard (C):",unique(dat$`Max Temperature (C)`),sep=''), name = 'Temperature Standard')%>%
            add_markers(x= ~FDT_TEMP_CELCIUS, y= ~FDT_DEPTH,mode = 'scatter', name="Temperature",
                        color=~LakeStratification, colors=c('#BF382A', '#0C4B8E'),
                        hoverinfo="text",text=~paste(sep="<br>",paste("Depth: ",FDT_DEPTH, "m"),
                                                     paste("Temperature:",FDT_TEMP_CELCIUS,"C"),
                                                     paste("LakeStratification: ",LakeStratification)))%>%
            layout(xaxis = list(autorange = "reversed",title="Temperature (Celcius)"),
                   yaxis = list(autorange = "reversed",title="Depth (m)"),
                   showlegend=FALSE
            ))
      } else {
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
      }
      
    }else{
      if(input$tempStd == TRUE){
        suppressWarnings(
          plot_ly(data=dat,y=~FDT_DEPTH,x=~FDT_TEMP_CELCIUS,type='scatter',mode='markers',
                  hoverinfo="text",text=~paste(sep="<br>",paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Temperature:",FDT_TEMP_CELCIUS,"C")))%>%
            add_lines(x=~`Max Temperature (C)`,y=~FDT_DEPTH, mode='line',line = list(color = 'black'),
                      hoverinfo = "TemperatureSTD", text= paste("Temperature Standard (C):",unique(dat$`Max Temperature (C)`),sep=''), name = 'Temperature Standard')%>%
            layout(xaxis = list(autorange = "reversed",title="Temperature (Celcius)"),
                   yaxis = list(autorange = "reversed",title="Depth (m)"),
                   showlegend=FALSE))
      } else {
        suppressWarnings(
          plot_ly(data=dat,y=~FDT_DEPTH,x=~FDT_TEMP_CELCIUS,type='scatter',mode='markers',
                  hoverinfo="text",text=~paste(sep="<br>",paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Temperature:",FDT_TEMP_CELCIUS,"C")))%>%
            layout(xaxis = list(autorange = "reversed",title="Temperature (Celcius)"),
                   yaxis = list(autorange = "reversed",title="Depth (m)"),
                   showlegend=FALSE))}
        }
      
      
  })
  
}




ui <- fluidPage(
  helpText('Review each site using the single site visualization section'),
  selectInput('stationSelection', 'Station Selection', choices = unique(AUData$FDT_STA_ID)),
  thermoclinePlotlySingleStationUI('temperature')
)

server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  AUData <- reactive({filter(conventionals_Lake, ID305B_1 %in% "VAW-L42L_DAN01A02" | #"VAW-N16L_NEW01A02" "VAW-N16L_NEW01B14" "VAW-N17L_PKC01A10" "VAW-N17L_PKC02A10"
                               ID305B_2 %in% "VAW-L42L_DAN01A02" | 
                               ID305B_2 %in% "VAW-L42L_DAN01A02") %>% 
      left_join(WQSvalues, by = 'CLASS') })
  
  # Create Data frame with all data within ID305B and stratification information
  # Pool thermocline data to get 1 sample per day, not 2 with top/bottom time difference
  stationDataDailySample <- reactive({
    req(AUData())
    dat <- AUData()
    dat$FDT_DATE_TIME <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")
    
    dat <- mutate(dat, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
      filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
    thermo <- stratifiedLake(dat)
    thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
    dat2 <- plyr::join(dat,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
      mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))
    return(dat2)
  })
  
  callModule(thermoclinePlotlySingleStation,'temperature', stationDataDailySample, stationSelected)
  

  
}

shinyApp(ui,server)


  