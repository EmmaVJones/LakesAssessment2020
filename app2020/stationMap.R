# Station Map
#callModule(stationMap, "lakeMap",lake_filter)
output$lakeMap <- renderLeaflet({
  req(lake_filter())
  points_sf <- lake_filter() %>%
    st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
             remove = F, # don't remove these lat/lon cols from df
             crs = 4326) # add coordinate reference system, needs to be geographic for now bc entering lat/lng, 
  print(points_sf)
  
  
  
  # AUs <- filter(lakeAU, ID305B %in% as.character( points_sf $ID305B_1) |
  #                      ID305B %in% as.character( points_sf $ID305B_2) |
  #                      ID305B %in% as.character( points_sf $ID305B_3))
  #  AUs$ID305B <- factor(AUs$ID305B) # drop extra factor levels so colors come out right
  #  print(AUs)
  
  map1 <- #mapview(AUs,zcol = 'ID305B', label= AUs$ID305B, layer.name = 'Assessment Unit',
    #        popup= popupTable(AUs, zcol=c("ID305B","Acres","CYCLE","WATER_NAME"))) #+ 
    mapview(points_sf, color='yellow',lwd=5,
            stroke=TRUE,label= points_sf$FDT_STA_ID, layer.name = c('Selected Stations'),
            popup= popupTable(points_sf, zcol=c("FDT_STA_ID","STA_DESC","ID305B_1", "ID305B_2", "ID305B_3")))
  map1@map
})
