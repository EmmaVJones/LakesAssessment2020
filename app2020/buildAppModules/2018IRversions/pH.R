





pHSubTabUI <- function(id){
  ns <- NS(id)
  tagList( plotlyOutput(ns('pHplot'),height = "400px"))
}



pHSubTab <-  function(input,output,session, oneStationData){
  ns <- session$ns
  
  output$pHplot <- renderPlotly({
    req(oneStationData())
    dat <- mutate(oneStationData(),top=9,bottom=6)
    dat$FDT_DATE_TIME <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y %H:%M")
    
    suppressWarnings(
      plot_ly(dat)%>%
        add_lines(x=~FDT_DATE_TIME,y=~top, mode='line',line = list(color = '#E50606'),
                  hoverinfo = "none", name="pH Standard")%>%
        add_lines(x=~FDT_DATE_TIME,y=~bottom, mode='line',line = list(color = '#E50606'),
                  hoverinfo = "none", name="pH Standard")%>%
        add_markers(x= ~FDT_DATE_TIME, y= ~FDT_FIELD_PH,mode = 'scatter', name="pH",
                    marker = list(color="#598BE2"),
                    hoverinfo="text",text=~paste(sep="<br>",paste("Date: ",FDT_DATE_TIME),
                                                 paste(FDT_FIELD_PH,"standard units"),
                                                 paste("Depth: ",FDT_DEPTH)))%>%
        layout(showlegend = FALSE,
               xaxis = list(title = "Date",tickfont = list(size = 10)),
               yaxis = list(title = "pH (standard units)")))
  })
  
}









tabPanel('pH',
         helpText('Review each site using the single site visualization section, then 
                                                            proceed to the bottom of the page to find exceedance rate for the entire assessment unit.',br(),
                  span(strong('NOTE: The pH exceedance analysis results at the bottom of the page include data
                                                                        from ALL stations within the assessment unit.'))),
         wellPanel(
           h4(strong('Single Station Data Visualization')),
           uiOutput('pH_oneStationSelectionUI'),
           temperatureSubTabUI('pH')),
         br(),hr(),br(),
         h5(strong("pH Exceedance Analysis")),
         tableOutput("pHExceedanceRate")),



## pH Tab ## ------------------------------------------------------------------------------------------------------

# Select One station for individual review
output$pH_oneStationSelectionUI <- renderUI({
  req(stationData())
  selectInput('pH_oneStationSelection',strong('Select Station to Review'),choices=unique(stationData())$FDT_STA_ID,width='300px')})

pH_oneStation <- reactive({
  req(input$pH_oneStationSelection)
  filter(stationDataDailySample(),FDT_STA_ID %in% input$DO_oneStationSelection)})

callModule(pHSubTab,'pH',pH_oneStation)



