#-------------------------------------------------------------------------------------------------------------------
## Lake Selection Tab ##
#-------------------------------------------------------------------------------------------------------------------
marker_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=5, color="orange", fillColor="yellow", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
sameID305B_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=5, color="orange", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected1")


# Basic Lake Map
output$lakeMap <- renderLeaflet({
  leaflet()%>%
    addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
    addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
    addProviderTiles(providers$Stamen.TerrainBackground,group='Stamen Terrain Background')%>%
    addCircleMarkers(data=lakeStations_shp,radius=5,color='blue',fillColor="blue",fillOpacity = 1,stroke=0,
                     group="Lake Monitoring Stations",layerId=~STATION_ID,
                     popup=popupTable(lakeStations_shp, zcol=c(1,6,7,13,14,16)))%>%#hideGroup('Lake Monitoring Stations')%>%
    addLayersControl(baseGroups=c('Open Street Map','Esri World Imagery','Stamen Terrain Background'),
                     overlayGroups=c('Lake Monitoring Stations'),
                     options=layersControlOptions(collapsed=T),
                     position='topleft')%>%
    addHomeButton(extent(wqms_Lshp),"All Lake Monitoring Stations")%>%
    mapview::addMouseCoordinates(style='basic')
})

# Update the selected map marker and view on user changes to station selectInput
observeEvent(input$station, { 
  p <- input$lakeMap_marker_click
  p2 <- subset(lakeStations, ID305B %in% au_filter()$ID305B)
  proxy <- leafletProxy("lakeMap")
  
  if(nrow(p2@data)==0){
    proxy %>% removeMarker(layerId="Selected")
  } else if(length(p$id) && au_filter()$ID305B!=p$id){
    proxy %>% setView(lng=mean(p2$DD_LONG), lat=mean(p2$DD_LAT), 11) %>% clearGroup("Selected ID305B")%>%
      addCircleMarkers(data=p2,radius=5, color="orange", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId=~STATION_ID, group="Selected ID305B",
                       popup=paste(sep="<br>",paste(strong('StationID: '),p2@data$STATION_ID),
                                   paste(strong('ID305B: '),p2@data$ID305B_1)))
  } else if(!length(p$id)){
    proxy %>% setView(lng=mean(p2$DD_LONG), lat=mean(p2$DD_LAT), 11) %>% clearGroup("Selected ID305B")%>%
      addCircleMarkers(data=p2,radius=5, color="orange", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId=~STATION_ID, group="Selected ID305B",
                       popup=paste(sep="<br>",paste(strong('StationID: '),p2@data$STATION_ID),
                                   paste(strong('ID305B: '),p2@data$ID305B_1)))
  }
})

