# Run in R 3.5.2
source('global.R')


ui <- shinyUI(fluidPage(theme="yeti.css",
                        shinyjs::useShinyjs(),
                        div(
                          id = "loading_page",
                          h1("Loading...")
                        ),
                        hidden(
                          div(
                            id = "main_content",
                            # suppress error messages as data loads, hacky
                            tags$style(type="text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"
                            ),
                            navbarPage(paste("VDEQ ",assessmentCycle," IR Lacustrine Assessment Tool", sep=''),
                                       
                                       tabPanel('Lake Selection',
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
                                                  DT::dataTableOutput('stationSummary'))
                                                ),
                                       tabPanel('Assessment Unit Review',
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
                                                fluidRow(column(4, DT::dataTableOutput('stationInfo')),
                                                         column(4, leafletOutput('stationMap', height = 300, width = 300),
                                                                helpText("The AUs displayed on the map above represent all AUs associated with the selected
                                                                         station (listed in a station's ID305B_1/ID305B_2/ID305B_3 fields) for context. ")),
                                                         column(4, DT::dataTableOutput('stationHistoricalInfo'))),
                                                hr(),
                                                h3('Station Results for Review'),
                                                helpText('This table outputs the site specific results for direct export to the Station Table. It also serves to highlight
                                                         where exceedances are present and should be reviewed in the individual parameter visualization tabs below.'),
                                                h4('Official Station Results Table'),
                                                helpText('Note that WAT_TOX_VIO AND WAT_TOX_STAT are only reflecting ammonia analysis. Additionally, parameters are highlighted
                                                         in different colors to indicate further review may be necessary. Parameters highlighted in yellow have at least one 
                                                         violation of a standard. Parameters highlighted in red exceed the 10.5% exceedance rate. Both scenarios warrant further
                                                         investigation and may requre comments in the Station Table and ADB.'),
                                                DT::dataTableOutput('stationTableDataSummary'), br(), 
                                                #h4('PWS violations'),
                                                #helpText("Any PWS violations should noted in a station's COMMENT field of the Stations Table. The table below organizes 
                                                #          PWS information to expedite the comment process."),
                                                # DT::dataTableOutput('PWStable'),
                                                br(),hr(),br(),
                                                h3('Assessment Unit Raw Data Review and Visualization'),
                                                tabsetPanel(
                                                  tabPanel('Conventionals Data',
                                                           tabsetPanel(
                                                             tabPanel('Raw Data',br(),
                                                                      DT::dataTableOutput('AURawData'),
                                                                      h4('Data Summary'),
                                                                      h5('Records Retrieved in Assessment Unit:'),
                                                                      fluidRow(column(1),column(10,textOutput('stationDataTableRecords'))),
                                                                      h5('Field and Lab Data in Assessment Window:'),
                                                                      fluidRow(column(1),column(10,tableOutput('uniqueStationDataTableRecords'))),
                                                                      h5('Assessment Window:'),
                                                                      fluidRow(column(1),column(10,textOutput('stationDataTableAssessmentWindow'))), br(),br()),
                                                             tabPanel('Temperature',
                                                                      helpText('Review each site using the single site visualization section. The results from this analysis are reflected
                                                                               in the TEMP_VIO, TEMP_SAMP, and TEMP_STAT columns in the station table.'),
                                                                      temperaturePlotlySingleStationUI('temperature')),
                                                             tabPanel('Dissolved Oxygen',
                                                                      helpText('Review each site using the single site visualization section. The results from this analysis are reflected
                                                                               in the DO_VIO, DO_SAMP, and DO_STAT columns in the station table.'),
                                                                      DOPlotlySingleStationUI('DO')),
                                                             tabPanel('pH',
                                                                      helpText('Review each site using the single site visualization section. The results from this analysis are reflected
                                                                               in the PH_VIO, PH_SAMP, and PH_STAT columns in the station table.'),
                                                                      pHPlotlySingleStationUI('pH')),
                                                             tabPanel('Bacteria',
                                                                      helpText('Review each site using the single site visualization section. The results from this analysis are reflected
                                                                        in the ECOLI_VIO, ECOLI_SAMP, and ECOLI_STAT columns in the station table.'),
                                                                      EcoliPlotlySingleStationUI("Ecoli")),
                                                             tabPanel('Nutrients',
                                                                      helpText('Review each site using the single site visualization section. The results from this analysis are reflected
                                                              in the NUT_TP_VIO, NUT_TP_SAMP, NUT_TP_STAT, NUT_CHLA_VIO, NUT_CHLA_SAMP, and NUT_CHLA_STAT columns 
                                                              in the station table.'),
                                                                      tabsetPanel(
                                                                        tabPanel('Chlorophyll a',br(),
                                                                                 chlAPlotlySingleStationUI('chlA')),
                                                                        tabPanel('Total Phosphorus',br(),
                                                                                 h5("Total phosphorus is assessed only when algaecides are applied within any zone of the lake/reservoir.
                                                                   It is the assessor's responsibility to remove the Chlorophyll a assessment from the autogenerated stations
                                                                   table if algaecides are noted anywhere in the lake/reservoir."),
                                                                                 TPPlotlySingleStationUI("TP"))
                                                                      ))
                                                             
                                                             )))
                                                
                                                           )
                                                )))
                                                  )
                                                )              
              
server <- shinyServer(function(input, output, session) {
  
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
  
  
  output$stationInfo <- DT::renderDataTable({ 
    req(stationData())
    z <- dplyr::select(stationData()[1,], FDT_STA_ID:STA_REC_CODE, Latitude:`Max Temperature (C)`) %>% 
      t() %>% as.data.frame() %>% rename(`Station and WQS Information` = 1)
    DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "250px", dom='t'))  })
  
  output$stationMap <- renderLeaflet({
    req(stationData())
    point <- dplyr::select(stationData()[1,],  FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:ID305B_3, Latitude, Longitude ) %>%
      st_as_sf(coords = c("Longitude", "Latitude"), 
               remove = F, # don't remove these lat/lon cols from df
               crs = 4269) # add projection, needs to be geographic for now bc entering lat/lng
    AU <- filter(lakeAU, ID305B %in% as.character(point$ID305B_1) |
                   ID305B %in% as.character(point$ID305B_2) |
                   ID305B %in% as.character(point$ID305B_3))
    map1 <- mapview(AU,zcol = 'ID305B', label= AU$ID305B, layer.name = 'Assessment Unit (ID305B_1)',
                    popup= popupTable(AU, zcol=c("ID305B","Acres","CYCLE","WATER_NAME"))) + 
      mapview(point, color = 'yellow', lwd = 5, label= point$FDT_STA_ID, layer.name = c('Selected Station'),
              popup= popupTable(point, zcol=c("FDT_STA_ID","STA_DESC","ID305B_1", "ID305B_2", "ID305B_3")))
    map1@map
    
  })
  
  output$stationHistoricalInfo <- DT::renderDataTable({ 
    req(stationData())
    z <- filter(lakeStations(), FDT_STA_ID == input$stationSelection) %>% 
      dplyr::select(STATION_ID:COMMENTS) %>%
      t() %>% as.data.frame() %>% rename(`Station Information From Last Cycle` = 1)
    DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "250px", dom='t'))  })
  
  
  ## Station Table View Section
  
  StationTableStuff <- reactive({
    req(stationDataDailySample(),input$stationSelection)
    
    x <- filter(stationDataDailySample(), FDT_STA_ID %in% input$stationSelection)
    
    #chlA_Exceedances(x, lakeStations())
    chlA_Exceedances <- exceedance_chlA(x, lakeStations())
    
    if( class(chlA_Exceedances) == 'character' && unique(chlA_Exceedances) == "No Chlorophyll a data for station "){
      chlA_Exceedances_result <- data.frame(NUT_CHLA_VIO = NA,	NUT_CHLA_SAMP = NA, NUT_CHLA_STAT = NA)
    } else {
      chlA_Exceedances_result <- data.frame(NUT_CHLA_VIO = nrow(filter(chlA_Exceedances, chlA_Exceedance== TRUE)),	NUT_CHLA_SAMP = nrow(chlA_Exceedances),
                           NUT_CHLA_STAT = ifelse(any(chlA_Exceedances$chlA_Exceedance)==TRUE,'Review','S'))
    }
    
    cbind(StationTableStartingData(x), tempExceedances(x),DOExceedances_Min(x),pHExceedances(x),
          bacteriaExceedances_OLD(bacteria_Assessment_OLD(x, 'E.COLI', 126, 235),'E.COLI') %>% 
            dplyr::rename('ECOLI_VIO_OLD' = 'E.COLI_VIO', 'ECOLI_SAMP_OLD'='E.COLI_SAMP', 'ECOLI_STAT_OLD'='E.COLI_STAT'),
          bacteriaExceedances_NEW(x,'E.COLI', 10, 410, 126),
          # Placeholders
          data.frame(ENTER_VIO='Not Analyzed by App', ENTER_SAMP='Not Analyzed by App', ENTER_STAT='Not Analyzed by App', 
                     ENTER_STV_VIO='Not Analyzed by App', ENTER_GEOMEAN_VIA='Not Analyzed by App', ENTER_STAT_NEW='Not Analyzed by App', 
                     WAT_MET_VIO='Not Analyzed by App', WAT_MET_STAT='Not Analyzed by App', WAT_TOX_VIO='Not Analyzed by App',
                     WAT_TOX_STAT='Not Analyzed by App', SED_MET_VIO='Not Analyzed by App', SED_MET_STAT='Not Analyzed by App', 
                     SED_TOX_VIO='Not Analyzed by App', SED_TOX_STAT='Not Analyzed by App', FISH_MET_VIO='Not Analyzed by App', 
                     FISH_MET_STAT='Not Analyzed by App', FISH_TOX_VIO='Not Analyzed by App', FISH_TOX_STAT='Not Analyzed by App',
                     BENTHIC_STAT='Not Analyzed by App'),
          TP_Exceedances(x), 
          chlA_Exceedances_result,
          #chlA_Exceedances(x, lakeStations()),
          data.frame(COMMENTS='Not Analyzed by App') )%>%
      dplyr::select(-ends_with('exceedanceRate'))
  })
  
  output$stationTableDataSummary <- DT::renderDataTable({
    req(StationTableStuff())
    DT::datatable(StationTableStuff(), extensions = 'Buttons', escape=F, rownames = F, 
                  options= list(scrollX = TRUE, pageLength = nrow(StationTableStuff),
                                dom='Bt', buttons=list('copy',
                                                       list(extend='csv',filename=paste('AssessmentResults_',paste(assessmentCycle,input$stationSelection, collapse = "_"),Sys.Date(),sep='')),
                                                       list(extend='excel',filename=paste('AssessmentResults_',paste(assessmentCycle,input$stationSelection, collapse = "_"),Sys.Date(),sep=''))))) %>% 
      formatStyle(c('TEMP_SAMP','TEMP_VIO','TEMP_STAT'), 'TEMP_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('DO_SAMP','DO_VIO','DO_STAT'), 'DO_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('PH_SAMP','PH_VIO','PH_STAT'), 'PH_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('ECOLI_SAMP_OLD','ECOLI_VIO_OLD','ECOLI_STAT_OLD'), 'ECOLI_STAT_OLD', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('ECOLI_STV_VIO','ECOLI_GEOMEAN_VIO','ECOLI_STAT_NEW'), 'ECOLI_STAT_NEW', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('NUT_TP_VIO','NUT_TP_SAMP','NUT_TP_STAT'), 'NUT_TP_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('NUT_CHLA_VIO','NUT_CHLA_SAMP','NUT_CHLA_STAT'), 'NUT_CHLA_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red')))
    
    
  })
  
  ## End station table section
  
  #### Data Sub Tab ####---------------------------------------------------------------------------------------------------
  
  # Display Data 
  output$AURawData <- DT::renderDataTable({ 
    AUData()
    DT::datatable(AUData(), extensions = 'Buttons', escape=F, rownames = F, 
                  options= list(scrollX = TRUE, pageLength = nrow(AUData()), scrollY = "300px", 
                                dom='Btf', buttons=list('copy',
                                                        list(extend='csv',filename=paste('AUData_',paste(input$stationSelection, collapse = "_"),Sys.Date(),sep='')),
                                                        list(extend='excel',filename=paste('AUData_',paste(input$stationSelection, collapse = "_"),Sys.Date(),sep='')))))})
  # Summarize data
  output$stationDataTableRecords <- renderText({
    req(AUData())
    paste(nrow(AUData()), 'records were retrieved for',as.character(input$AUSelection),sep=' ')})
  output$uniqueStationDataTableRecords <- renderTable({
    req(AUData())
    plyr::count(AUData(), vars = c("FDT_STA_ID"))%>%dplyr::rename('Number of Records'='freq')})
  output$stationDataTableAssessmentWindow <- renderText({
    req(AUData())
    withinAssessmentPeriod(AUData())})
  
  
  
  
  
  
  
  # Need this as a reactive to regenerate below modules when user changes station 
  stationSelected <- reactive({input$stationSelection})
  
  
  
  ## Temperature Sub Tab ##------------------------------------------------------------------------------------------------------
  
  callModule(temperaturePlotlySingleStation,'temperature', stationDataDailySample, stationSelected)
  
  
  ## DO Sub Tab ##------------------------------------------------------------------------------------------------------
  
  callModule(DOPlotlySingleStation,'DO', stationDataDailySample, stationSelected)
  
  ## pH Sub Tab ##------------------------------------------------------------------------------------------------------
  
  callModule(pHPlotlySingleStation,'pH', stationDataDailySample, stationSelected)
  
  ## Chlorophyll a Sub Tab ##------------------------------------------------------------------------------------------------------
  
  callModule(chlAPlotlySingleStation,'chlA', stationDataDailySample, stationSelected, allLakeLZdata, lakeStations)
  
  ## Total Phosphorus Sub Tab ##------------------------------------------------------------------------------------------------------
  
  callModule(TPPlotlySingleStation,'TP', stationDataDailySample, stationSelected, allLakeLZdata)
  
  ## Ecoli Sub Tab ##------------------------------------------------------------------------------------------------------
  
  callModule(EcoliPlotlySingleStation,'Ecoli', stationDataDailySample, stationSelected)
  
  
})


shinyApp(ui, server)
