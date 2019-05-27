lake_filter <- filter(lakeStations, SIGLAKENAME == 'Claytor Lake')


conventionals_Lake <- filter(conventionals, FDT_STA_ID %in% unique(lake_filter$FDT_STA_ID)) %>%
  left_join(dplyr::select(lakeStations, FDT_STA_ID, SEC, CLASS, SPSTDS,PWS, ID305B_1, ID305B_2, ID305B_3,
                          STATION_TYPE_1, STATION_TYPE_2, STATION_TYPE_3, ID305B, SEC187, SIG_LAKE, USE,
                          SIGLAKENAME, Chlorophyll_A_limit, TPhosphorus_limit, Assess_TYPE), by='FDT_STA_ID')



AUData <- filter(conventionals_Lake, ID305B_1 %in% "VAW-N16L_NEW01A02" | #"VAW-N16L_NEW01A02" "VAW-N16L_NEW01B14" "VAW-N17L_PKC01A10" "VAW-N17L_PKC02A10"
           ID305B_2 %in% "VAW-N16L_NEW01A02" | 
           ID305B_2 %in% "VAW-N16L_NEW01A02") %>% 
    left_join(WQSvalues, by = 'CLASS') 

stationData <- filter(AUData, FDT_STA_ID %in% "9-NEW087.14") #"9-NEW087.14" "9-NEW089.34"


point <- dplyr::select(stationData[1,],  FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:ID305B_3, Latitude, Longitude ) %>%
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
       
