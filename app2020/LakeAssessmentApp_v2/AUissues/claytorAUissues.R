lakeStations <- read_csv('processedStationData/final2020data/lakeStations2020_BRRO_citmonNonAgency.csv')

lake_filter <- filter(lakeStations, SIGLAKENAME == 'Claytor Lake')# 'Timber Lake')

points_sf <- lake_filter %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) # add coordinate reference system, needs to be geographic for now bc entering lat/lng, 
AUs <- filter(lakeAU, ID305B %in% as.character( points_sf $ID305B_1)) # | # removing all options for more than one ID305B to be associated with a single station
#ID305B %in% as.character( points_sf $ID305B_2) |
#ID305B %in% as.character( points_sf $ID305B_3))
AUs$ID305B <- factor(AUs$ID305B) # drop extra factor levels so colors come out right

map1 <- mapview(AUs,zcol = 'ID305B', label= AUs$ID305B, layer.name = 'Assessment Unit',
                popup= popupTable(AUs, zcol=c("ID305B","Acres","CYCLE","WATER_NAME"))) + 
  mapview(points_sf, color='yellow',lwd=5,
          stroke=TRUE,label= points_sf$FDT_STA_ID, layer.name = c('Selected Stations'),
          popup= popupTable(points_sf, zcol=c("FDT_STA_ID","STA_DESC","ID305B_1", "ID305B_2", "ID305B_3")))
map1@map 

z <- filter(lakeAU, WATER_NAME %in% c('Claytor Lake (New River)','Claytor Lake (Peak Creek)'))


map2 <- mapview(z,zcol = 'ID305B', label= z$ID305B, layer.name = 'Assessment Unit',
                popup= popupTable(z, zcol=c("ID305B","Acres","CYCLE","WATER_NAME"))) + 
  mapview(points_sf, color='yellow',lwd=5,
          stroke=TRUE,label= points_sf$FDT_STA_ID, layer.name = c('Selected Stations'),
          popup= popupTable(points_sf, zcol=c("FDT_STA_ID","STA_DESC","ID305B_1", "ID305B_2", "ID305B_3")))
map2@map 

filter(lakeAU, ID305B %in% 'VAW-N16L_NEW06A02')

