lake_filter <- filter(lakeStations, SIGLAKENAME == 'Lake Moomaw')#'Talbott Reservoir')#'Claytor Lake')


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






chlAPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('chlA_oneStationSelectionUI')),
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
    filter(AUdata(),FDT_STA_ID %in% input$chlA_oneStationSelection)})
  
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
    chlA_Assessment_OneStation(chlA_oneStation()) })
  
  # selected station exceedance rate
  output$stationChlaExceedanceRate <- renderTable({
    req(chlA_oneStation())
    LZ <- exceedance_chlA(chlA_oneStation(), lakeStations())
    
    if(class(LZ) == 'character'){
      return("No Chlorophyll a data to assess.")
    }else{
      LZ <- LZ$LacustrineZone[1]
      assessmentDetermination(exceedance_chlA(chlA_oneStation(), lakeStations()),
                              filter(exceedance_chlA(chlA_oneStation(), lakeStations()), chlA_Exceedance ==T),'Chlorophyll a','Aquatic Life')%>%
        mutate(LacustrineZone=LZ) %>%# for chla and TP only, show whether or not in lacustrine zone with single station exceedance
        dplyr::select(nSamples,nExceedance,exceedanceRate, LacustrineZone) # don't give assessment determination for single station}})
    }
  })
  
  
  # Lake Chl a 90th percentiles
  output$chla90thTable <- renderTable({
    req(allLakeLZData())
    exceedance_chlA(allLakeLZData(), lakeStations())})
  
  
  # All Lake LACUSTRINE ZONE Chlorophyll a exceedance rate
  output$chla_exceedanceRate <- renderTable({
    req(allLakeLZData())
    z <- exceedance_chlA(allLakeLZData(), lakeStations())
    if(class(z)=='character'){
      return("No Chlorophyll a data to assess.")
    }else{
      assessmentDetermination(filter(exceedance_chlA(allLakeLZData(), lakeStations()), LacustrineZone == TRUE),
                              filter(exceedance_chlA(allLakeLZData(), lakeStations()),chlA_Exceedance ==T & LacustrineZone == TRUE),'Chlorophyll a','Aquatic Life')%>%
        dplyr::select(nSamples, nExceedance, exceedanceRate)}})
  
  
  # All Lake Chlorophyll a exceedance rate REGARDLESS OF WHETHER OR NOT STATIONS ARE IN LACUSTRINE ZONE 
  output$chla_exceedanceRateALL <- renderTable({
    req(allLakeLZData())
    z <- exceedance_chlA(allLakeLZData(), lakeStations())
    if(class(z)=='character'){
      return("No Chlorophyll a data to assess.")
    }else{
      assessmentDetermination(z,filter(exceedance_chlA(allLakeLZData(), lakeStations()),chlA_Exceedance ==T),'Chlorophyll a','Aquatic Life')%>%
        dplyr::select(nSamples, nExceedance, exceedanceRate)}})
}




ui <- fluidPage(
  fileInput('stationsTable','Choose your Regional Stations Table 2.0.',
            accept = c(".csv")),
  sidebarPanel(
    h4('Instructions:'),
    p("Use the drop down box to select a lake/reservoir to assess. All AU's are organized by lake/reservoir. 
      The map will update based on your selection. Once you have reviewed the data below the map, proceed 
      to the 'Assessment Unit Review' Tab to begin analyzing the lake/reservoir."),
    #uiOutput("lake_filterUI")#,
    
    #dynamicSelectInput('regionSelection', "Select DEQ Region to Assess", multiple = FALSE),
    dynamicSelectInput("lakeSelection", "Select Lake to Assess", multiple = FALSE)#,
    ),
  mainPanel(
    leafletOutput("lakeMap"),br(),
    h5(strong('Assessment units in selected lake')),
    DT::dataTableOutput('AUSummary'), br(),
    h5(strong('Stations in selected lake that were sampled in current window')),
    DT::dataTableOutput('stationSummary')),
  fluidRow(column(9, DT::dataTableOutput('selectedLake')),
           column(3,br(),actionButton('pullLakeData','Select lake for analysis')#,
                  #helpText('If the button above is disabled, there are no AUs in the selected lakes.')
           )),
  hr(),
  verbatimTextOutput('verbatim'),
  uiOutput('AUSelection_'),
  h5(strong('AU information from last cycle')),
  DT::dataTableOutput('selectedAU'),br(),
  uiOutput('stationSelection_'),
  helpText('Review each site using the single site visualization section'),
  selectInput('stationSelection', 'Station Selection', choices = unique(AUData$FDT_STA_ID)),
  helpText('Review each site using the single site visualization section. The results from this analysis are reflected
                                                              in the NUT_TP_VIO, NUT_TP_SAMP, NUT_TP_STAT, NUT_CHLA_VIO, NUT_CHLA_SAMP, and NUT_CHLA_STAT columns 
           in the station table.'),
  tabsetPanel(
    tabPanel('Chlorophyll a',br(),
             chlAPlotlySingleStationUI('chlA')),
    tabPanel('Total Phosphorus',br())
  )
  
)

server <- function(input,output,session){
  
  # display the loading feature until data loads into app
  load_data()
  
  
  ### Lake Selection Tab
  
  lakeStations <- reactive({
    req(input$stationsTable)
    inFile <- input$stationsTable
    read_csv(inFile$datapath) 
  })
  
  # Query AU's By Selectize arguments
  #the_data <- reactive({lakeStations})
  the_data <- reactive({lakeStations()})
  #region_filter <- shiny::callModule(dynamicSelect, 'regionSelection', the_data, "OFFICE_NM")
  lake_filter <- shiny::callModule(dynamicSelect, "lakeSelection", the_data, "SIGLAKENAME" )
  
  
  
  
  #output$lake_filterUI <- uiOutput({
  #  req(lakeStations())
  #  selectInput('lakeSelection','Select Lake to Assess', 
  #              choices= unique(lakeStations()$SIGLAKENAME), multiple = FALSE)
  #  
  #})
  
  #lake_filter <- reactive({
  #  req(input$lakeSelection)
  #  filter(lakeStations(), SIGLAKENAME %in% input$lakeSelection)
  #})
  
  
  # Station Map
  output$lakeMap <- renderLeaflet({
    req(lake_filter())
    points_sf <- lake_filter() %>%
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = F, # don't remove these lat/lon cols from df
               crs = 4326) # add coordinate reference system, needs to be geographic for now bc entering lat/lng, 
    AUs <- filter(lakeAU, ID305B %in% as.character( points_sf $ID305B_1) |
                    ID305B %in% as.character( points_sf $ID305B_2) |
                    ID305B %in% as.character( points_sf $ID305B_3))
    AUs$ID305B <- factor(AUs$ID305B) # drop extra factor levels so colors come out right
    
    map1 <- mapview(AUs,zcol = 'ID305B', label= AUs$ID305B, layer.name = 'Assessment Unit',
                    popup= popupTable(AUs, zcol=c("ID305B","Acres","CYCLE","WATER_NAME"))) + 
      mapview(points_sf, color='yellow',lwd=5,
              stroke=TRUE,label= points_sf$FDT_STA_ID, layer.name = c('Selected Stations'),
              popup= popupTable(points_sf, zcol=c("FDT_STA_ID","STA_DESC","ID305B_1", "ID305B_2", "ID305B_3")))
    map1@map })
  
  # Table of AUs in selected lake
  output$AUSummary <-  DT::renderDataTable({ 
    req(lake_filter())
    z <- dplyr::select(lake_filter(),ID305B, WATERSHED_ID, VAHU6, GNIS_ID:Assess_TYPE) %>%
      distinct(ID305B, .keep_all = T)
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "200px", dom='t')) 
  })
  
  # Table of stations in selected lake
  output$stationSummary <- DT::renderDataTable({
    req(lake_filter())
    z <- dplyr::select(lake_filter(), FDT_STA_ID:STA_REC_CODE, Latitude:VAHU6, CATEGORY_ID:Assess_TYPE)
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "200px", dom='t'))  })
  
  
  
  #### Assessment Unit Review Tab
  
  # Show selected AU
  output$selectedLake <- DT::renderDataTable({
    datatable(lake_filter() %>% dplyr::select(VAHU6, SIGLAKENAME, ID305B, SEC187 ) %>% distinct(ID305B, .keep_all = T),
              rownames = FALSE, options= list(pageLength = 1, scrollY = "35px", dom='t'))})
  
  # Pull Conventionals data for selected Lake on click
  conventionals_Lake <- eventReactive( input$pullLakeData, {
    z <- filter(conventionals, FDT_STA_ID %in% unique(lake_filter()$FDT_STA_ID)) %>%
      left_join(dplyr::select(lakeStations(), FDT_STA_ID, SEC, CLASS, SPSTDS,PWS, ID305B_1, ID305B_2, ID305B_3,
                              STATION_TYPE_1, STATION_TYPE_2, STATION_TYPE_3, ID305B, SEC187, SIG_LAKE, USE, SIGLAKENAME, 
                              Chlorophyll_A_limit, TPhosphorus_limit, Assess_TYPE ), by='FDT_STA_ID') })
  
  
  output$AUSelection_ <- renderUI({ 
    req(conventionals_Lake())
    selectInput('AUSelection', 'Assessment Unit Selection', choices = unique(conventionals_Lake()$ID305B)) })
  
  output$selectedAU <- DT::renderDataTable({
    req(conventionals_Lake(),input$AUSelection)
    z <- filter(lakeAU, ID305B %in% input$AUSelection) %>% st_set_geometry(NULL) %>% as.data.frame()
    datatable(z, rownames = FALSE, 
              options= list(pageLength = nrow(z),scrollX = TRUE, scrollY = "200px", dom='t'))})
  
  output$stationSelection_ <- renderUI({ 
    req(conventionals_Lake(), input$AUSelection)
    z <- filter(conventionals_Lake(), ID305B_1 %in% input$AUSelection | 
                  ID305B_2 %in% input$AUSelection | 
                  ID305B_2 %in% input$AUSelection) %>%
      distinct(FDT_STA_ID)
    fluidRow(selectInput('stationSelection', 'Station Selection', choices = unique(z$FDT_STA_ID)),
             helpText("The stations available in the drop down are limited to stations with an ID305B_1 designation equal 
                      to the selected AU. All AU's associated with the selected station can be viewed in the map below."))})
  
  
  
  AUData <- eventReactive( input$AUSelection, {
    filter(conventionals_Lake(), ID305B_1 %in% input$AUSelection | 
             ID305B_2 %in% input$AUSelection | 
             ID305B_2 %in% input$AUSelection) %>% 
      left_join(WQSvalues, by = 'CLASS') }) 
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData(), FDT_STA_ID %in% input$stationSelection) })
  
  
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
    allLakeLZstations <- as.character(filter(lakeStations(),SIGLAKENAME %in% sigLake)$STATION_ID)
    allLakeLZData <- filter(conventionals,FDT_STA_ID %in% allLakeLZstations)
    allLakeLZData$FDT_DATE_TIME <- as.POSIXct(allLakeLZData$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")
    allLakeLZData<- mutate(allLakeLZData, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
      filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
    thermo <- stratifiedLake(allLakeLZData)
    thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
    allLakeLZData2 <- plyr::join(allLakeLZData,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
      mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))%>%
      plyr::join(dplyr::select(lakeStations(), Chlorophyll_A_limit, TPhosphorus_limit, Assess_TYPE, FDT_STA_ID), by='FDT_STA_ID')
    return(allLakeLZData2)
  })
  
  
  # Need this as a reactive to regenerate below modules when user changes station 
  stationSelected <- reactive({input$stationSelection})


  callModule(chlAPlotlySingleStation,'chlA', stationDataDailySample, stationSelected, allLakeLZdata, lakeStations)
  #callModule(chlAPlotlySingleStation,'chlA', stationDataDailySample, stationSelected, allLakeLZdata)
  
  
}

shinyApp(ui,server)





