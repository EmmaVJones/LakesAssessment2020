
lake_filter <- filter(lakeStations, SIGLAKENAME == 'Banister Lake')# 'Lake Moomaw')#'Talbott Reservoir')#'Claytor Lake')


conventionals_Lake <- filter(conventionals, FDT_STA_ID %in% unique(lake_filter$FDT_STA_ID)) %>%
  left_join(dplyr::select(lakeStations, FDT_STA_ID, SEC, CLASS, SPSTDS,PWS, ID305B_1, ID305B_2, ID305B_3,
                          STATION_TYPE_1, STATION_TYPE_2, STATION_TYPE_3, ID305B, SEC187, SIG_LAKE, USE,
                          SIGLAKENAME, Chlorophyll_A_limit, TPhosphorus_limit, Assess_TYPE), by='FDT_STA_ID')

AUData <- filter(conventionals_Lake, ID305B_1 %in% "VAW-I03L_JKS01A02" |#"VAW-I03L_JKS02A02" "VAW-I03L_JKS03A02"
                   ID305B_2 %in% "VAW-I03L_JKS01A02"  | 
                   ID305B_2 %in% "VAW-I03L_JKS01A02" ) %>% 
  left_join(WQSvalues, by = 'CLASS') 


AUData <- filter(conventionals_Lake, ID305B_1 %in% "VAW-L42L_DAN01A02" | #"VAW-N16L_NEW01A02" "VAW-N16L_NEW01B14" "VAW-N17L_PKC01A10" "VAW-N17L_PKC02A10"
                   ID305B_2 %in% "VAW-L42L_DAN01A02" | 
                   ID305B_2 %in% "VAW-L42L_DAN01A02") %>% 
  left_join(WQSvalues, by = 'CLASS') 

AUData <- conventionals_Lake %>% 
  left_join(WQSvalues, by = 'CLASS') 

stationData <- filter(stationDataDailySample, FDT_STA_ID %in% "2-JKS046.40")#"4ADAN194.10") #"9-NEW087.14" "9-NEW089.34"



# Create Data frame with all data within ID305B and stratification information
# Pool thermocline data to get 1 sample per day, not 2 with top/bottom time difference
#stationDataDailySample <- reactive({
#  req(AUData())
dat <- AUData
dat$FDT_DATE_TIME <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")

dat <- mutate(dat, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
  filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
thermo <- stratifiedLake(dat)
thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
stationDataDailySample <- plyr::join(dat,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
  mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))

# Filter all lake LZ data to make pooled assessment, for CHL A and TP
#allLakeLZdata <- reactive({
#  req(stationDataDailySample())
sigLake <- as.character(AUData$SIGLAKENAME)
allLakeLZstations <- as.character(filter(lakeStations,SIGLAKENAME %in% sigLake)$STATION_ID)
allLakeLZData <- filter(conventionals,FDT_STA_ID %in% allLakeLZstations)
allLakeLZData$FDT_DATE_TIME <- as.POSIXct(allLakeLZData$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")
allLakeLZData<- mutate(allLakeLZData, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
  filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
thermo <- stratifiedLake(allLakeLZData)
thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
allLakeLZData2 <- plyr::join(allLakeLZData,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
  mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))%>%
  plyr::join(dplyr::select(lakeStations, Chlorophyll_A_limit, TPhosphorus_limit, Assess_TYPE, FDT_STA_ID), by='FDT_STA_ID')
allLakeLZData <- allLakeLZData2



TPPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('TP_oneStationSelectionUI')),
      plotlyOutput(ns('TPplotly')),
      br(),hr(),br(),
      fluidRow(
        column(6, h5('Annual median Total Phosphorus for the ',span(strong('selected site')),' are reported below.'),
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
    dat <- mutate(TP_oneStation(),TPhosphorus_limit_ug_L=TPhosphorus_limit/1000,
                  LakeStratification = replace_na(LakeStratification,"NONE"))
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    dat$LakeStratification <- as.factor(dat$LakeStratification)
    dat$LakeStratification <- factor(dat$LakeStratification,levels=c("Epilimnion","NONE","Hypolimnion"))#,ordered=T)
    
    
    plot_ly(data=dat)%>%
      add_lines(data=dat, x=~SampleDate,y=~TPhosphorus_limit_ug_L, mode='line',line = list(color = 'black'),
                hoverinfo = "text",text=paste("Total Phosphorus limit:",unique(dat$TPhosphorus_limit_ug_L),'ug/L'), 
                name="Total Phosphorus limit") %>%
      add_markers(x= ~SampleDate, y= ~PHOSPHORUS,mode = 'scatter', name="Total Phosphorus (mg/L)",
                  color=~LakeStratification, #marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Total Phosphorus: ",PHOSPHORUS,"ug/L"),
                                               paste("LakeStratification: ",LakeStratification)))%>%
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


ui <- fluidPage(
  helpText('Review each site using the single site visualization section'),
  selectInput('stationSelection', 'Station Selection', choices = unique(AUData$FDT_STA_ID)),
  helpText('Review each site using the single site visualization section. The results from this analysis are reflected
           in the NUT_TP_VIO, NUT_TP_SAMP, NUT_TP_STAT, NUT_CHLA_VIO, NUT_CHLA_SAMP, and NUT_CHLA_STAT columns 
           in the station table.'),
  tabsetPanel(
    #tabPanel('Chlorophyll a',br()),
             #chlAPlotlySingleStationUI('chlA')),
    tabPanel('Total Phosphorus',br(),
             h5('Chlorophyll a and total phosphorus are used when algaecides are applied within any zone of the lake/reservoir.'),
             TPPlotlySingleStationUI("TP"))
  )
  
  )

server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({conventionals_Lake %>% 
      left_join(WQSvalues, by = 'CLASS') })
  
  #AUData <- reactive({filter(conventionals_Lake, ID305B_1 %in% "VAW-I03L_JKS01A02" |#"VAW-I03L_JKS02A02" "VAW-I03L_JKS03A02"
  #                             ID305B_2 %in% "VAW-I03L_JKS01A02"  | 
  #                             ID305B_2 %in% "VAW-I03L_JKS01A02" ) %>% 
  #    left_join(WQSvalues, by = 'CLASS')  })
  
  
  #AUData <- reactive({filter(conventionals_Lake, ID305B_1 %in% "VAW-L42L_DAN01A02" | #"VAW-N16L_NEW01A02" "VAW-N16L_NEW01B14" "VAW-N17L_PKC01A10" "VAW-N17L_PKC02A10"
  #                             ID305B_2 %in% "VAW-L42L_DAN01A02" | 
  #                             ID305B_2 %in% "VAW-L42L_DAN01A02") %>% 
  #    left_join(WQSvalues, by = 'CLASS') })
  
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
  
  # Filter all lake LZ data to make pooled assessment, for CHL A and TP
  allLakeLZdata <- reactive({
    req(stationDataDailySample())
    sigLake <- as.character(AUData()$SIGLAKENAME)
    allLakeLZstations <- as.character(filter(lakeStations,SIGLAKENAME %in% sigLake)$STATION_ID)
    allLakeLZData <- filter(conventionals,FDT_STA_ID %in% allLakeLZstations)
    allLakeLZData$FDT_DATE_TIME <- as.POSIXct(allLakeLZData$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")
    allLakeLZData<- mutate(allLakeLZData, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
      filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
    thermo <- stratifiedLake(allLakeLZData)
    thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
    allLakeLZData2 <- plyr::join(allLakeLZData,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
      mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))%>%
      plyr::join(dplyr::select(lakeStations, Chlorophyll_A_limit, TPhosphorus_limit, Assess_TYPE, FDT_STA_ID), by='FDT_STA_ID')
    return(allLakeLZData2)
  })
  
  
  callModule(TPPlotlySingleStation,'TP', stationDataDailySample, stationSelected, allLakeLZdata)
  
  
}

shinyApp(ui,server)





