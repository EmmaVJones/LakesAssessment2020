lakeStations <- read_csv('processedStationData/final2020data/lakeStations2020_BRRO_citmonNonAgency.csv')


lake_filter <- filter(lakeStations, SIGLAKENAME == 'Leesville Reservoir')


conventionals_Lake <- filter(conventionals, FDT_STA_ID %in% unique(lake_filter$FDT_STA_ID)) %>%
  left_join(dplyr::select(lakeStations, FDT_STA_ID, SEC, CLASS, SPSTDS,PWS, ID305B_1, ID305B_2, ID305B_3,
                          STATION_TYPE_1, STATION_TYPE_2, STATION_TYPE_3, ID305B, SEC187, SIG_LAKE, USE,
                          SIGLAKENAME, Chlorophyll_A_limit, TPhosphorus_limit, Assess_TYPE), by='FDT_STA_ID')



AUData <- filter(conventionals_Lake, ID305B_1 %in% "VAW-L13L_ROA01A18") %>% 
  left_join(WQSvalues, by = 'CLASS') 

stationData <- filter(AUData, FDT_STA_ID %in% "LVLAROA140.66") #"9-NEW087.14" "9-NEW089.34"



# Create Data frame with all data within ID305B and stratification information
# Pool thermocline data to get 1 sample per day, not 2 with top/bottom time difference
#stationDataDailySample <- reactive({
#  req(AUData())
dat <- AUData
dat$FDT_DATE_TIME <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")

dat <- mutate(dat, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
  filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
#thermo <- stratifiedLake(dat)
thermo <- stratifiedLake_citNonA(citmonOutOfParameterDataset(dat, FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK) )
thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
stationDataDailySample <- plyr::join(dat,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
  mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))





EcoliPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(6,uiOutput(ns('Ecoli_oneStationSelectionUI'))),
               column(6,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      plotlyOutput(ns('Ecoliplotly')),br(),hr(),br(),
      fluidRow(
        column(6, h5('All E. coli records that are ',span(strong('above the criteria')),' for the ',
                     span(strong('selected site')),' are highlighted below.',
                     span(strong('If no data are reflected in below tables then no data exceeded the respective criteria.'))),
               h4(strong('Old Standard (Monthly Geomean = 126 CFU / 100 mL)')),
               DT::dataTableOutput(ns('EcoliexceedancesOldStdTableSingleSitegeomean')),
               h4(strong('Old Standard (Single Sample Maximum = 235 CFU / 100 mL)')),
               DT::dataTableOutput(ns('EcoliexceedancesOldStdTableSingleSiteSTV')), br(),  br(), hr(), br(), br(), 
               h4(strong('New Standard (STV= 410 CFU / 100 mL, geomean = 126 CFU / 100 mL with additional sampling requirements)')),
               helpText('The below table highlights all analyzed windows that have either STV violations OR geomean violations. Note
                        the number of samples in the window, STV Assessment, and Geomean Assessment columns for context. These violations
                        are important to understand the dataset, but assessment decision in the table to the right is where one should look
                        for assistance choosing which of the potential violations are driving the decision. Explore the dataset in 
                        90 day windows in the interactive graph below and the full dataset with assessment decisions paired with each window
                        in the bottom-most table.'),
               DT::dataTableOutput(ns('EcoliexceedancesNEWStdTableSingleSite'))),
        column(6, h5('Individual E. coli exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               h4(strong('Old Standard (Single Sample Maximum = 235 CFU / 100 mL, geomean = 126 CFU / 100 mL)')), 
               DT::dataTableOutput(ns("EcoliOldStdTableSingleSite")), br(), br(), br(),  br(), br(),br(), br(),br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), hr(), br(), br(),
               h4(strong('New Standard (STV= 410 CFU / 100 mL, geomean = 126 CFU / 100 mL with additional sampling requirements)')), 
               DT::dataTableOutput(ns("EcoliNEWStdTableSingleSite")),
               h4(strong('See below section for detailed analysis with new recreation standard.')))),
      hr(),br(),
      h4(strong('New Recreation Standard Analysis')),
      helpText('Review the 90 day windows (identified by each sample date) for STV and geomean exceedances.
               Comments are specific to each row of data. To view the dataset within each 90 day window, use
               the drop down box to select the start of the window in question.'),
      fluidRow(
        column(6, helpText('Below is the raw data associated with the ',span('selected site'),'.'), 
               h5(strong('Raw Data')),DT::dataTableOutput(ns('rawData'))),
        column(6, uiOutput(ns('windowChoice')),
               plotlyOutput(ns('EcoliplotlyZoom')))),
      br(), br(),
      h5(strong('Analyzed Data (Each window with an individual assessment decision)')),
      DT::dataTableOutput(ns('analysisTable'))),
    br(),hr(),br(),
    h4('AU assessment results'),
    h5('Bacteria in lakes is assessed by pooling the same day samples in an assessment unit, calculating the median, and then 
       comparing the median to the standard. The daily AU median E. coli is presented in a table below, followed by both 
       assessment methods for the AU.'),
    dataTableOutput(ns('dailyMedianAUbacteria')),
    hr(),
    fluidRow(
      column(6, 
             h5('All E. coli records that fall outside the criteria for the ',span(strong('assessment unit')),' using the ',
                span(strong('Old Standard')),' are highlighted below.'),
             DT::dataTableOutput(ns('AUdailyMedianTable')),
             h4(strong('Old Standard (Monthly Geomean = 126 CFU / 100 mL)')),
             DT::dataTableOutput(ns('AU_EcoliexceedancesOldStdTableSingleSitegeomean')),
             h4(strong('Old Standard (Single Sample Maximum = 235 CFU / 100 mL)')),
             DT::dataTableOutput(ns('AU_EcoliexceedancesOldStdTableSingleSiteSTV')),br(),  br(), hr(), br(), br(), 
             h4(strong('New Standard (STV= 410 CFU / 100 mL, geomean = 126 CFU / 100 mL with additional sampling requirements)')),
             helpText('The below table highlights all analyzed windows ',strong('of daily median AU data'),' that have either STV violations OR geomean violations. Note
                      the number of samples in the window, STV Assessment, and Geomean Assessment columns for context. These violations
                      are important to understand the dataset, but assessment decision in the table to the right is where one should look
                      for assistance choosing which of the potential violations are driving the decision. Explore the dataset in 
                      90 day windows in the interactive graphs for each station in the AU above and the full dataset with assessment decisions paired with each window
                      in the bottom-most table.'),
             DT::dataTableOutput(ns('AU_EcoliexceedancesNEWStdTableSingleSite'))),
      column(6, h5('Individual E. coli exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
             h4(strong('Old Standard (Single Sample Maximum = 235 CFU / 100 mL, geomean = 126 CFU / 100 mL)')), 
             DT::dataTableOutput(ns("AU_EcoliOldStdTableSingleSite")), br(), br(), br(),  br(), br(),br(), br(),br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), hr(), br(), br(),
             h4(strong('New Standard (STV= 410 CFU / 100 mL, geomean = 126 CFU / 100 mL with additional sampling requirements)')), 
             DT::dataTableOutput(ns("AU_EcoliNEWStdTableSingleSite")))),
    h5(strong('Analyzed Data for entire AU (Each window with an individual assessment decision)')),
    DT::dataTableOutput(ns('AU_analysisTable')),
    br(),br(), br()
  )
  
}


EcoliPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$Ecoli_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('Ecoli_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})
  
  Ecoli_oneStation <- reactive({
    req(ns(input$Ecoli_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$Ecoli_oneStationSelection) %>%
      filter(!is.na(E.COLI))})
  
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
    req(Ecoli_oneStation())
    parameterFilter <- dplyr::select(Ecoli_oneStation(), FDT_STA_ID:FDT_DEPTH_DESC, E.COLI, ECOLI_RMK, ThermoclineDepth_RMK, LakeStratification)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t')) %>%
      formatStyle(c('E.COLI','ECOLI_RMK'), 'ECOLI_RMK', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  output$Ecoliplotly <- renderPlotly({
    req(input$Ecoli_oneStationSelection, Ecoli_oneStation())
    dat <- Ecoli_oneStation() %>%
      mutate(newSTV = 410, geomean = 126, oldSTV = 235)
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    plot_ly(data=dat) %>%
      add_markers(x= ~SampleDate, y= ~E.COLI,mode = 'scatter', name="E. coli (CFU / 100 mL)", marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("E. coli: ",E.COLI,"CFU / 100 mL")))%>%
      add_lines(data=dat, x=~SampleDate,y=~newSTV, mode='line', line = list(color = '#484a4c',dash = 'dot'),
                hoverinfo = "text", text= "New STV: 410 CFU / 100 mL", name="New STV: 410 CFU / 100 mL") %>%
      add_lines(data=dat, x=~SampleDate,y=~oldSTV, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text= "Old SSM: 235 CFU / 100 mL", name="Old SSM: 235 CFU / 100 mL") %>%
      add_lines(data=dat, x=~SampleDate,y=~geomean, mode='line', line = list(color = 'black', dash= 'dash'),
                hoverinfo = "text", text= "Geomean: 126 CFU / 100 mL", name="Geomean: 126 CFU / 100 mL") %>%
      layout(showlegend=FALSE,
             yaxis=list(title="E. coli (CFU / 100 mL)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
  })
  
  output$EcoliexceedancesOldStdTableSingleSitegeomean <- DT::renderDataTable({
    req(Ecoli_oneStation())
    z <- bacteria_ExceedancesGeomeanOLD(citmonOutOfParameterDataset(Ecoli_oneStation(), E.COLI, ECOLI_RMK) %>% 
                                          dplyr::select(FDT_DATE_TIME2,E.COLI)%>% # Just get relavent columns, 
                                          filter(!is.na(E.COLI)), #get rid of NA's
                                        'E.COLI', 126) %>%
      dplyr::select(FDT_DATE_TIME2, E.COLI, sampleMonthYear, geoMeanCalendarMonth, limit, samplesPerMonth) %>%
      rename(FDT_DATE_TIME = FDT_DATE_TIME2) %>%# for user view consistency, same data, just different format for R purposes
      filter(samplesPerMonth > 4, geoMeanCalendarMonth > limit) # minimum sampling rule for geomean to apply
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t')) 
    
  })
  
  output$EcoliexceedancesOldStdTableSingleSiteSTV <- DT::renderDataTable({
    req(Ecoli_oneStation())
    z <- bacteria_ExceedancesSTV_OLD(citmonOutOfParameterDataset(Ecoli_oneStation(), E.COLI, ECOLI_RMK) %>%
                                       dplyr::select(FDT_DATE_TIME2,E.COLI)%>% # Just get relavent columns, 
                                       filter(!is.na(E.COLI)) #get rid of NA's
                                     , 235 ) %>%
      filter(exceeds == T) %>%
      mutate(FDT_DATE_TIME = as.character(FDT_DATE_TIME2), E.COLI = parameter) %>%
      dplyr::select(FDT_DATE_TIME, E.COLI, limit, exceeds)
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t')) 
  })
  
  output$EcoliOldStdTableSingleSite <- DT::renderDataTable({
    req(Ecoli_oneStation())
    z <- bacteria_Assessment_OLD(citmonOutOfParameterDataset(Ecoli_oneStation(), E.COLI, ECOLI_RMK), 'E.COLI', 126, 235) %>% 
      dplyr::select(`Assessment Method`,everything())
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'))})
  
  ### AU results
  
  AUdailyMedian <- reactive({
    req(AUdata())
    filter(citmonOutOfParameterDataset(AUdata(), E.COLI, ECOLI_RMK), !is.na(E.COLI)) %>%
      dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, E.COLI,SampleDate, FDT_DATE_TIME2) %>%
      group_by(SampleDate) %>%
      mutate(dailyAUmedianE.COLI = median(E.COLI)) %>%
      arrange(SampleDate) %>%
      dplyr::select(SampleDate, FDT_DATE_TIME, FDT_STA_ID, E.COLI, dailyAUmedianE.COLI, everything())
  })
  
  output$dailyMedianAUbacteria <- DT::renderDataTable({
    req(AUdailyMedian())
    z <- AUdailyMedian()
    z$FDT_DATE_TIME <- as.character(z$FDT_DATE_TIME)
    DT::datatable(
      dplyr::select(z, SampleDate:dailyAUmedianE.COLI), rownames = FALSE, 
      options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "200px",dom='t')) 
  })
  
  output$AU_EcoliexceedancesOldStdTableSingleSitegeomean <-  DT::renderDataTable({
    req(AUdailyMedian())
    z <- AUdailyMedian() %>%
      ungroup() %>%
      distinct(SampleDate, .keep_all = T) %>%
      dplyr::select(FDT_DATE_TIME2,dailyAUmedianE.COLI)%>% # Just get relavent columns, 
      filter(!is.na(dailyAUmedianE.COLI)) %>%
      rename("E.COLI"="dailyAUmedianE.COLI")
    
    z1 <- bacteria_ExceedancesGeomeanOLD(z ,'E.COLI', 126) %>%
      dplyr::select(FDT_DATE_TIME2, E.COLI, sampleMonthYear, geoMeanCalendarMonth, limit, samplesPerMonth) %>%
      rename(FDT_DATE_TIME = FDT_DATE_TIME2, "dailyAUmedianE.COLI"='E.COLI') %>%# for user view consistency, same data, just different format for R purposes
      filter(samplesPerMonth > 4, geoMeanCalendarMonth > limit) # minimum sampling rule for geomean to apply
    
    DT::datatable(z1, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z1), scrollY = "250px", dom='t'))  })
  
  output$AU_EcoliexceedancesOldStdTableSingleSiteSTV <- DT::renderDataTable({
    req(AUdailyMedian())
    z <- bacteria_ExceedancesSTV_OLD(AUdailyMedian() %>%
                                       ungroup() %>%
                                       distinct(SampleDate, .keep_all = T) %>%
                                       dplyr::select(FDT_DATE_TIME2,dailyAUmedianE.COLI)%>% # Just get relavent columns, 
                                       filter(!is.na(dailyAUmedianE.COLI)) #get rid of NA's
                                     , 235 ) %>%
      filter(exceeds == T) %>%
      mutate(FDT_DATE_TIME = as.character(FDT_DATE_TIME2), dailyAUmedianE.COLI = parameter) %>%
      dplyr::select(FDT_DATE_TIME, dailyAUmedianE.COLI, limit, exceeds)
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'))  })
  
  output$AU_EcoliOldStdTableSingleSite <- DT::renderDataTable({
    req(AUdailyMedian())
    
    z <- AUdailyMedian() %>%
      ungroup() %>%
      distinct(SampleDate, .keep_all = T) %>%
      dplyr::select(FDT_DATE_TIME2,dailyAUmedianE.COLI) %>% # Just get relavent columns, 
      filter(!is.na(dailyAUmedianE.COLI)) %>%
      dplyr::rename('E.COLI'= 'dailyAUmedianE.COLI')
    
    z1 <- bacteria_Assessment_OLD(z, 'E.COLI', 126, 235) %>% 
      dplyr::select(`Assessment Method`,everything()) %>%
      rename('dailyAUmedianE.COLI_VIO' = 'E.COLI_VIO', 'dailyAUmedianE.COLI_SAMP' = 'E.COLI_SAMP',
             'dailyAUmedianE.COLI_exceedanceRate'= 'E.COLI_exceedanceRate', 'dailyAUmedianE.COLI_STAT' = 'E.COLI_STAT')
    
    DT::datatable(z1, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z1), scrollY = "250px", dom='t'))})
  
  
  
  
  
  
  ### New standard ----------------------------------------------------------------------------------
  newSTDbacteriaData <- reactive({
    req(Ecoli_oneStation())
    conventionalsToBacteria(citmonOutOfParameterDataset(Ecoli_oneStation(), E.COLI, ECOLI_RMK), 'E.COLI')})  
  
  newSTDbacteriaData_AU <- reactive({
    req(AUdailyMedian())
    z <- AUdailyMedian() %>%
      ungroup() %>%
      distinct(SampleDate, .keep_all = T) %>%
      dplyr::select(SampleDate, dailyAUmedianE.COLI, FDT_DATE_TIME, FDT_DATE_TIME2) %>%
      rename('Value'='dailyAUmedianE.COLI') %>%
      mutate(ID='AUmedianData',`Date Time`= FDT_DATE_TIME2) %>%
      dplyr::select(ID, `Date Time`, Value) %>%
      filter(!is.na(Value))
    z$`Date Time` <- as.Date(z$`Date Time`)
    z$Value <- as.numeric(z$Value)
    return(z)  })
  
  output$EcoliexceedancesNEWStdTableSingleSite <- DT::renderDataTable({
    req(Ecoli_oneStation(),newSTDbacteriaData())
    z <- bacteriaExceedances_NewStd(newSTDbacteriaData(), 10, 410, 126) %>% 
      filter(`STV Exceedances In Window` > 0 | `Geomean In Window` > 126) %>%
      dplyr::select(-associatedData) # remove embedded tibble to make table work
    z$`Date Window Starts` <- as.character(z$`Date Window Starts`)
    z$`Date Window Ends` <- as.character(z$`Date Window Ends`)
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t')) 
  })
  
  
  output$EcoliNEWStdTableSingleSite <- DT::renderDataTable({
    req(Ecoli_oneStation(),newSTDbacteriaData())
    z <- bacteriaAssessmentDecision(newSTDbacteriaData(), 10, 410, 126)  %>%
      distinct(`Assessment Decision`) %>% # only grab 1 record
      mutate(`Assessment Method`= 'New Recreation Standard') %>%
      dplyr::select(`Assessment Method`, `Assessment Decision`) #only grab decision
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t')) 
  })
  
  output$windowChoice <- renderUI({
    req(Ecoli_oneStation(),newSTDbacteriaData())
    fluidRow(
      column(4, selectInput(ns('windowChoice_'),'Select 90 day window start date',
                            choices = unique(newSTDbacteriaData()$`Date Time`), width = '100%')),
      column(8, helpText('Orange line corresponds to the window geomean; wide black dashed line
                         corresponds to the geomean criteria; thin black dashed line corresponds
                         to the STV limit.')))})
  
  output$EcoliplotlyZoom <- renderPlotly({
    req(input$windowChoice_, Ecoli_oneStation(),newSTDbacteriaData())
    windowStart <- bacteriaExceedances_NewStd(newSTDbacteriaData(), 10, 410, 126) %>%
      filter(`Date Window Starts` == input$windowChoice_) #'2011-02-17')#
    
    windowData <- dplyr::select(windowStart, associatedData) %>%
      unnest() %>%
      mutate(`Date Window Starts` = as.POSIXct(unique(windowStart$`Date Window Starts`, format="%m/%d/%y")),
             `Date Window Ends` = as.POSIXct(unique(windowStart$`Date Window Ends`, format="%m/%d/%y")),
             newSTV = 410, geomean = 126)
    windowData$`Date Time` <- as.POSIXct(strptime(windowData$`Date Time`, format="%Y-%m-%d"))#as.POSIXct(windowData$`Date Time`, format="%Y-%m-%d", tz='GMT') + as.difftime(1, units="days")
    
    plot_ly(data=windowData) %>%
      add_markers(x= ~`Date Time`, y= ~Value,mode = 'scatter', name="E. coli (CFU / 100 mL)", marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",`Date Time`),
                                               paste("E. coli: ",Value,"CFU / 100 mL"))) %>%
      add_lines(data=windowData, x=~`Date Time`, y=~E.COLI_geomean, mode='line', line = list(color = 'orange', dash= 'dash'),
                hoverinfo = "text", text= ~paste("Window Geomean: ", format(E.COLI_geomean,digits=3)," CFU / 100 mL", sep=''), 
                name="Window Geomean") %>%
      add_lines(data=windowData, x=~`Date Time`,y=~newSTV, mode='line', line = list(color = '#484a4c',dash = 'dot'),
                hoverinfo = "text", text= "New STV: 410 CFU / 100 mL", name="New STV: 410 CFU / 100 mL") %>%
      add_lines(data=windowData, x=~`Date Time`,y=~geomean, mode='line', line = list(color = 'black', dash= 'dash'),
                hoverinfo = "text", text= "Geomean: 126 CFU / 100 mL", name="Geomean: 126 CFU / 100 mL") %>%
      layout(showlegend=FALSE,
             yaxis=list(title="E. coli (CFU / 100 mL)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
  })
  
  output$rawData <- DT::renderDataTable({
    req(input$windowChoice_, Ecoli_oneStation(),newSTDbacteriaData())
    z <- dplyr::select(citmonOutOfParameterDataset(Ecoli_oneStation(), E.COLI, ECOLI_RMK), FDT_STA_ID, FDT_DATE_TIME, E.COLI ,RMK_ECOLI) %>%
      filter(!is.na(E.COLI))
    z$FDT_DATE_TIME <- as.character(z$FDT_DATE_TIME)
    
    DT::datatable(z, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "400px", dom='t'))
  })
  
  output$analysisTable <- DT::renderDataTable({
    req(Ecoli_oneStation(),newSTDbacteriaData())
    z <- bacteriaExceedances_NewStd(newSTDbacteriaData(), 10, 410, 126) %>%
      dplyr::select(-associatedData) # remove embedded tibble to make table work
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "400px", dom='t'))
  })
  
  
  
  
  
  
  output$AU_EcoliexceedancesNEWStdTableSingleSite <- DT::renderDataTable({
    req(newSTDbacteriaData_AU())
    z1 <- bacteriaExceedances_NewStd(newSTDbacteriaData_AU(), 10, 410, 126) %>% 
      filter(`STV Exceedances In Window` > 0 | `Geomean In Window` > 126) %>%
      dplyr::select(-associatedData) # remove embedded tibble to make table work
    z1$`Date Window Starts` <- as.character(z1$`Date Window Starts`)
    z1$`Date Window Ends` <- as.character(z1$`Date Window Ends`)
    DT::datatable(z1, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z1), scrollY = "250px", dom='t')) 
  })
  
  output$AU_EcoliNEWStdTableSingleSite <- DT::renderDataTable({
    req(newSTDbacteriaData_AU())
    
    z <- bacteriaAssessmentDecision(newSTDbacteriaData(), 10, 410, 126)  %>%
      distinct(`Assessment Decision`) %>% # only grab 1 record
      mutate(`Assessment Method`= 'New Recreation Standard') %>%
      dplyr::select(`Assessment Method`, `Assessment Decision`) #only grab decision
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t')) 
  })
  
  output$AU_analysisTable <- DT::renderDataTable({
    req(newSTDbacteriaData_AU())
    z <- bacteriaExceedances_NewStd(newSTDbacteriaData_AU(), 10, 410, 126) %>%
      dplyr::select(-associatedData) # remove embedded tibble to make table work
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "400px", dom='t'))
  })
  
  
  
}






ui <- fluidPage(
  helpText('Review each site using the single site visualization section'),
  selectInput('stationSelection', 'Station Selection', choices = unique(AUData$FDT_STA_ID)),
  helpText('Review each site using the single site visualization section. The results from this analysis are reflected
           in the NUT_TP_VIO, NUT_TP_SAMP, NUT_TP_STAT, NUT_CHLA_VIO, NUT_CHLA_SAMP, and NUT_CHLA_STAT columns 
           in the station table.'),
  EcoliPlotlySingleStationUI("Ecoli")
)
  

server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  AUData <- reactive({filter(conventionals_Lake, ID305B_1 %in% "VAW-L13L_ROA01A18") %>% 
      left_join(WQSvalues, by = 'CLASS') })
  
  # Create Data frame with all data within ID305B and stratification information
  # Pool thermocline data to get 1 sample per day, not 2 with top/bottom time difference
  stationDataDailySample <- reactive({
    req(AUData())
    dat <- AUData()
    dat$FDT_DATE_TIME <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")
    
    dat <- mutate(dat, SampleDate=format(FDT_DATE_TIME,"%m/%d/%y"))%>% # Separate sampling events by day
      filter(!is.na(FDT_TEMP_CELCIUS))# remove any NA values to keep thermocline function happy
    thermo <- stratifiedLake_citNonA(citmonOutOfParameterDataset(dat, FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK) ) #thermo <- stratifiedLake(dat)
    thermo$ThermoclineDepth <- as.numeric(thermo$ThermoclineDepth)
    dat2 <- plyr::join(dat,thermo,by=c('FDT_STA_ID','SampleDate'))%>%
      mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))
    return(dat2)
  })
  
  
  
  callModule(EcoliPlotlySingleStation,'Ecoli', stationDataDailySample, stationSelected)
  
  
}

shinyApp(ui,server)

























