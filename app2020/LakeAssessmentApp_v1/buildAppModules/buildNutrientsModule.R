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




nutrientsPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('sec187'))
    #wellPanel(
    #  h4(strong('Single Station Data Visualization')),
    #  uiOutput(ns('pH_oneStationSelectionUI')),
    #  plotlyOutput(ns('pHplotly')),
    #  br(),hr(),br(),
    #  fluidRow(
    #    column(8, h5('All pH records that fall outside the criteria for the ',span(strong('selected site')),' in the ',
    #                 span(strong('epilimnion or unstratified samples')),' are highlighted below.'),div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('pHRangeTableSingleSite')))),
    #    column(4, h5('Individual pH exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
    #           tableOutput(ns("stationpHExceedanceRate"))))
    #)
  )
}
    
chlAPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('chlA_oneStationSelectionUI')),
      plotlyOutput(ns('chlAplotly')),
      br(),hr(),br(),
      h5('Annual Chlorophyll a 90th percentiles for the ',span(strong('selected site')),' selected site are reported below.'),
      div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('pHRangeTableSingleSite')))
    )
  )
}


chlAPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$chlA_oneStationSelectionUI <- renderUI({
    req(#AUdata, 
      stationSelectedAbove)
    #print(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID)))
    selectInput(ns('chlA_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  chlA_oneStation <- reactive({
    req(ns(input$chlA_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$chlA_oneStationSelection)})
  
  output$chlAplotly <- renderPlotly({
    req(input$chlA_oneStationSelection, chlA_oneStation())
    dat <- chlA_oneStation()
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    plot_ly(data=dat)%>%
      add_markers(x= ~SampleDate, y= ~CHLOROPHYLL,mode = 'scatter', name="Chlorophyll a (ug/L)",marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Chlorophyll a: ",CHLOROPHYLL,"ug/L")))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Chlorophyll a (ug/L)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) })
}



nutrientsPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$nuts_oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('pH_oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='300px', selected = stationSelectedAbove())})
  
  nuts_oneStation <- reactive({
    req(ns(input$pH_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$pH_oneStationSelection)})
  
  output$sec187 <- renderUI({
    req(nuts_oneStation())
    if(unique(nuts_oneStation()$SEC187) == "Y"){
      chlAPlotlySingleStationUI(ns('chlA'))
    } #else {
      
    #}
  })
  
}








ui <- fluidPage(
  helpText('Review each site using the single site visualization section'),
  selectInput('stationSelection', 'Station Selection', choices = unique(AUData$FDT_STA_ID)),
  nutrientsPlotlySingleStationUI('nutrients')
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
  
  callModule(nutrientsPlotlySingleStationUI,'nutrients', stationDataDailySample, stationSelected)
  
  
}

shinyApp(ui,server)

