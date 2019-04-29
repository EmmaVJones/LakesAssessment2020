userDataset <- filter(lakeStations, SIGLAKENAME == 'Claytor Lake')


stationMapUI <- function(id){
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"), height =400, width = 650))
}

stationMap <- function(input, output, session, userDataset){
  ns <- session$ns
  
  output$map <- renderLeaflet({
    #print(userDataset())
    userDataset_sf <- suppressWarnings(suppressMessages(userDataset() %>%
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = F, # don't remove these lat/lon cols from df
               crs = 4326))) # add coordinate reference system, needs to be geographic for now bc entering lat/lng, 
    AUs <- filter(lakeAU, ID305B %in% as.character( points_sf $ID305B_1) |
                    ID305B %in% as.character( points_sf $ID305B_2) |
                    ID305B %in% as.character( points_sf $ID305B_3))
    
    
    suppressWarnings(suppressMessages(leaflet(userDataset_sf) %>% setView(lng=unique(userDataset_sf$Longitude)[1],lat=unique(userDataset_sf$Latitude)[1],zoom=12) %>%
      addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
      addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
      addCircleMarkers(data=userDataset_sf,~Longitude, ~Latitude,radius=5,color='yellow',fillColor="orange",
                       fillOpacity = 1, opacity=1, weight=2, stroke=TRUE, 
                       group="Lake Monitoring Stations", layerId=~FDT_STA_ID,
                       popup=popupTable(userDataset_sf,
                                        zcol=c("FDT_STA_ID","STA_DESC","ID305B_1", "ID305B_2", "ID305B_3")))%>%
      addPolygons(data = AUs, )
      addCircleMarkers(data=lakeStations_sf,radius=3,color='orange',fillColor="orange",fillOpacity = 1,stroke=0,
                       group="All Lake Monitoring Stations",layerId=~lakeStations_sf$Longitude,
                       popup=popupTable(lakeStations_sf,zcol=c("FDT_STA_ID","STA_DESC","Deq_Region","ID305B_1","ID305B_2","ID305B_3"))) %>% hideGroup('All Lake Monitoring Stations')%>%
      addLayersControl(baseGroups=c('Open Street Map','Esri World Imagery'),
                       overlayGroups=c('Lake Monitoring Stations','All Lake Monitoring Stations'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')%>%
      addHomeButton(extent(lakeStations_sf),"All Lake Monitoring Stations")%>%
      mapview::addMouseCoordinates(style='basic')))
  })
}


