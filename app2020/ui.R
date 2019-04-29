# Run in R 3.5.2
source('global.R')

shinyUI(fluidPage(theme="yeti.css",
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
                                 tabPanel('Data Upload',
                                          h3('Tool Overview'),
                                          p("The Lacustrine Assessment Tool is designed to expedite analysis, assessment
                                            decisions, and quality assurance/quality control (QA/QC) procedures for Virginia's
                                            contribution to the 2020 Integrated Report (IR). The data window analyzed covers 
                                            January 1, 2013 to December 31, 2018. Tool users can expect significant time savings
                                            on repetitive procedures including: raw data organization from disparate databases, 
                                            geospatial organization of stations by assessment unit, standard/criteria calculations, 
                                            and data visualization."),
                                          p('The application represents the first iteration of an automated assessment tool. The datasets
                                            and parameters chosen for analysis readily lend themselves to automated processing. Future versions
                                            of the tool may include additional datasets and analyses.'),
                                          p('For feedback, troubleshooting, and questions regarding analyses or missing data, please contact 
                                            Emma Jones (emma.jones@deq.virginia.gov)'),
                                          br(),
                                          h3('Tool Inputs'),br(),
                                          h4('Prepopulated Tool Inputs'),
                                          tags$ul(
                                            tags$li('Conventionals- CEDS data pulled and analyzed by Roger Stewart (roger.stewart@deq.virginia.gov) for each 
                                                    Integrated Report data window.'), 
                                            tags$li("Statewide Assessment (spatial) layer- The spatial dataset that identifies each regional office's lakes
                                                    or reservoirs that require assessment."),
                                            tags$li("Statewide Lacustrine Assessment Units (spatial) layer- The spatial dataset that organizes assessment units
                                                    by each lake or reservoir.")),
                                          br(),
                                          h4('User Defined Tool Inputs'),
                                          p('In order to reduce processing time and facilitate peristent data storage, users must
                                            upload certain datasets that follow a specified template. These include their regional
                                            Stations Table 2.0, limited to lake/reservoir sites. The dataProcessing_Statewide.Rmd walks
                                            users through organizing the station table and splitting up riverine and lacustrine stations.'),
                                          h5('Stations Table 2.0'),
                                          helpText('This dataset is derived before any Lacustrine Assessment Tool analysis 
                                                   procedures can commence using the ',span(strong('dataProcessing_Statewide.Rmd.')), 
                                                   'After completing the requisite analyses from the ',
                                                   span(strong('dataProcessing_Statewide.Rmd')),'once, users can 
                                                   upload their results to the Lacustrine Assessment Tool each time they open
                                                   the tool for analysis.'),
                                          fileInput('stationsTable','Choose your Regional Stations Table 2.0.',
                                                    accept = c(".csv")),
                                          br(),br(),br()),
                      tabPanel('Lake Selection',
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
                                            tabPanel('Thermocline',
                                                     helpText('Review each site using the single site visualization section. The results from this analysis directly affect
                                                                        the dissolved oxygen, and pH assessment. '),
                                                     thermoclinePlotlySingleStationUI('thermocline')),
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