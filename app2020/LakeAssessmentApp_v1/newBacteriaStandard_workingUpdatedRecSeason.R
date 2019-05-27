# New standard logic, can substitute in E.coli or Enter. limits and play with sampling
# requirements
bacteriaExceedances_NewStd <- function(x, # input dataframe with bacteria data
                                       sampleRequirement, # minimum n samples in 90 day window needed to apply geomean
                                       STV, # unique for ecoli/enter
                                       geomeanCriteria# unique for ecoli/enter
){
  # Output tiblle to organize results, need list object to save associate data
  out <- tibble(`StationID` = as.character(NA),
                `Window Within Recreation Season` = as.logical(NA),
                `Date Window Starts` = as.Date(NA), `Date Window Ends` = as.Date(NA), 
                `Samples in 90 Day Window` = as.numeric(NA), 
                `STV Exceedances In Window` = as.numeric(NA), 
                `STV Exceedance Rate` = as.numeric(NA), 
                `STV Assessment` = as.character(NA),
                `Geomean In Window` = as.numeric(NA),
                `Geomean Assessment` = as.character(NA),
                associatedData = list()) 
  
  # Loop through each row of input df to test 90 day windows against assessment criteria
  for( i in 1 : nrow(x)){
    time1 <- x$`Date Time`[i]
    timePlus90 <- ymd(x$`Date Time`[i]) + days(90)
  
    # Organize prerequisites to decision process
    z <- filter(x, `Date Time` >= time1 & `Date Time` <= timePlus90) %>% 
      mutate(nSamples = n(), # count number of samples in 90 day window
             STVhit = ifelse(Value > STV, TRUE, FALSE), # test values in window against STV
             E.COLI_geomean = ifelse(nSamples > 1, FSA::geomean(Value), NA), # calculate geomean of samples if nSamples>1
             geomeanCriteriaHit = ifelse(E.COLI_geomean > geomeanCriteria, TRUE, FALSE)) # test geomean against geomean Criteria
    
    # Recreation Season, first flag samples that do not comply with recreation season requirements
    # this is the more lax version of rule that allows samples outside of April-October as long
    # as the % within April-October > 2/3
    # To be considered a "recreational season 90-day period", at least 2/3 of sample days of the period 
    # must be within the April-October time frame.  
    recSeasonMath <- filter(z, month(`Date Time`) >= 4 & month(`Date Time`) < 11) %>% #just samples in rec period
      mutate(nSamplesRec = n(), `Rec%Samples` = (nSamplesRec / unique(z$nSamples))*100)
    recSeason <- ifelse(nrow(recSeasonMath) > 0 , ifelse(unique(recSeasonMath$`Rec%Samples`) > 66.6, TRUE, FALSE), FALSE)
    # report T if nSamples in rec season > 2/3 of samples collected in the 90 day window
    
    
    # First level of testing: any STV hits in dataset? Want this information for all scenarios
    nSTVhitsInWindow <- nrow(filter(z, STVhit == TRUE))
    STVexceedanceRate <- ( nSTVhitsInWindow / unique(z$nSamples)) * 100 # STV exceedance rate calculation
    if(nSTVhitsInWindow == 0){
      `STV Assessment` <- 'No STV violations within 90 day window' } 
    if(nSTVhitsInWindow == 1){
      `STV Assessment` <- paste(nSTVhitsInWindow, ' STV violation(s) with ', format(STVexceedanceRate, digits = 3), 
                                '% exceedance rate in 90 day window | Insufficient Information (Prioritize for follow up monitoring)',sep='')}
    if(nSTVhitsInWindow >= 2){
      `STV Assessment` <- paste(nSTVhitsInWindow, ' STV violation(s) with ', format(STVexceedanceRate, digits = 3), 
                                '% exceedance rate in 90 day window | Impaired: ', nSTVhitsInWindow,' hits in the same 90-day period',sep='') }
    
    # Second level of testing: only if minimum geomean sampling requirements met in 90 day period
    if(unique(z$nSamples) >= sampleRequirement){
      # Geomean Hit
      if(unique(z$geomeanCriteriaHit) == TRUE){
        `Geomean Assessment` <- paste('Geomean: ', format(unique(z$E.COLI_geomean), digits = 3), 
                                      ' | Impaired: geomean exceeds criteria in the 90-day period', sep='')  
      } else{
        `Geomean Assessment` <-  paste('Geomean: ', format(unique(z$E.COLI_geomean), digits = 3), 
                                       ' | Geomean criteria met, hold assessment decision for further testing', sep= '')} 
    } else { # minimum geomean sampling requirements NOT met in 90 day period
      `Geomean Assessment` <- 'Insufficient Information: geomean sampling criteria not met'  }
    
    out[i,] <-  tibble(`StationID` = unique(x$ID),
                       `Window Within Recreation Season` = recSeason,
                       `Date Window Starts` = time1, `Date Window Ends` = timePlus90, 
                       `Samples in 90 Day Window` = unique(z$nSamples), 
                       `STV Exceedances In Window` = nSTVhitsInWindow, 
                       `STV Exceedance Rate` = STVexceedanceRate,
                       `STV Assessment` = `STV Assessment`,
                       `Geomean In Window` = ifelse(unique(z$nSamples) >= sampleRequirement, unique(z$E.COLI_geomean), NA), # avoid excitement, only give geomean result if 10+ samples
                       `Geomean Assessment` = `Geomean Assessment`,
                       associatedData = list(z)) 
  } #end for loop
  return(out) 
}
# really just a building block, one probably wouldn't run this function independently
#results <- ecoliExceedances_NewStd(tish1, 10, 410, 126)   

# Function to identify non overlapping windows in a given dataset.
# this ONLY returns the first non overlapping datasets and gives NO PRIORITY
# to datasets that may exceed more/less
# I'm not 100% happy with this function so far. Needs more testing to see utility
#last2years = 2016

nonOverlappingIntervals <- function(results, withinRec, last2years){
  if(withinRec == T){ results <- filter(results, `Window Within Recreation Season` == withinRec) }
  #if(!any(is.na(last2years))){ 
  results <- mutate(results, yr= year(`Date Window Starts`)) %>%
    filter(yr %in% last2years) %>% dplyr::select(-yr)
  #if(any(is.na(last2years))){ finalResults <- results}
  
  if(nrow(results) > 0){
    results$newInt <- NA
    x <- results$`Date Window Ends`[1]
    
    for(i in 1:nrow(results)){
      if(results$`Date Window Starts`[i] < x){results$newInt[i] <- as.character(x)
      }else{
        x <- results$`Date Window Ends`[i]
        results$newInt[i] <- as.character(x)}  }
    
    uniqueInt <- unique(results$newInt)
    finalResults <- suppressWarnings(filter(results, `Date Window Ends` %in% as.Date(unique(results$newInt))) %>%
                                       dplyr::select(-newInt))
  } else {
    finalResults <- results
  }
  
  
  suppressWarnings(return(finalResults))
}


#Function to see if any 90 day windows have 2+ STV exceedances
STVexceedance_noWindowSize <- function(df, STV){
  morethan1STVexceedanceInAnyWindow <- filter(df, `STV Exceedances In Window` >= 2)
  if(nrow(morethan1STVexceedanceInAnyWindow) > 0){
    return('| Data outside last two years Observed Effect: 2 or more STV exceedances in a 90 day window |')
  }
}

# function to test geomean exceedances
geomeanExceedance <- function(df, geomeanCriteria){
  geomeanExceedances <- filter(df, `Geomean In Window` > geomeanCriteria)
  if(nrow(geomeanExceedances) > 0){
    return('| Data outside last two years Observed Effect: geometric means calculated for the 90-day periods represented by 10+ samples do not meet the GM criterion |')
  }
}


# Function to summarize results into decisions
# This function returns all potential issues with priorty on geomean results IF there
# are enough samples to run geomean
# NO filtering of recreation season, so in theory this could run the citizen data, too.
# Recreation season is noted as boolean with April 1 - October 31 compliance
bacteriaAssessmentDecision <- function(x, sampleRequirement, STV, geomeanCriteria){
  
  if(nrow(x)>0){
    # Run assessment function
    z <- bacteriaExceedances_NewStd(x,  sampleRequirement, STV, geomeanCriteria)   
    
    # what are the last two years sampled? They get a bit of priority
    last2years <- sort(unique(year(z$`Date Window Starts`)), TRUE)[1:2]
    
    # Deal with data not in last two years separately
    dataNotInLast2years <- filter(z, !(year(z$`Date Window Starts`) %in% last2years))
    OE <- ifelse(nrow(dataNotInLast2years) > 0, 
                 paste( STVexceedance_noWindowSize(dataNotInLast2years, STV), 
                        geomeanExceedance(dataNotInLast2years, geomeanCriteria)), NA)  
    
    z <- filter(z, year(z$`Date Window Starts`) %in% last2years)
    
    
    # Then make sure at least 2 independent 90 day windows present in dataset before testing for other things
    nonOverlappingExceedanceResults <- nonOverlappingIntervals(z, FALSE, last2years)
    if(nrow(nonOverlappingExceedanceResults) < 2){
      # If 2+ STV hits in 90 days or geomean calculated on 90 day window exceed geomean criteria then impaired
      if(!is.null(STVexceedance_noWindowSize(z, STV)) | !is.null(geomeanExceedance(z, geomeanCriteria))){
        statement1 <- ifelse(!is.null(STVexceedance_noWindowSize(z, STV)),  '| 2 or more STV exceedances in a 90 day window |', '')
        statement2 <- ifelse(!is.null(geomeanExceedance(z, STV)),  '| geomean exceedances in a 90 day window |', '')
        statement <- paste('Dataset lacks two independent 90 day windows: Impaired',statement1, statement2)
        return(mutate(z,`Assessment Decision` = ifelse(is.na(OE), statement, paste(statement, OE)))) 
      } else {
        statement <- 'Dataset lacks two independent 90 day windows: Insufficient Information (Prioritize for follow-up Monitoring)'
        return(mutate(z,`Assessment Decision` = ifelse(is.na(OE), statement, paste(statement, OE))))  
      }
    } else {
      # See if geomean method applies
      geomeanApplies <- filter(z, `Samples in 90 Day Window` >= sampleRequirement)
      
      if(nrow(geomeanApplies) > 1){
        geomeanExceedances <- filter(geomeanApplies, `Geomean In Window` > geomeanCriteria)
        
        # If any geomean exceedances then return all windows where geomean exceeds
        if(nrow(geomeanExceedances) > 0){#} & any(year(geomeanExceedances$`Date Window Starts`) %in% last2years)){
          return(mutate(geomeanExceedances,`Assessment Decision` = 
                          'Impaired: geometric means calculated for the 90-day periods represented by 10+ samples do not meet the GM criterion'))
        } else{ # If no geomean exceedances then dig into STV exceedances
          
          # Any STV exceedance rates > 10.5% in a 90 day window with 10+ samples?
          STVexceedanceAfterPassingGeomean <- filter(z, `STV Exceedance Rate` >= 10.5)
          if(nrow(STVexceedanceAfterPassingGeomean) > 0){
            
            # If any STV exceedance rate > 10.5% from 90 day windows with 10+ samples then impaired
            STVexceedanceAnd10Samples <- filter(STVexceedanceAfterPassingGeomean, `Samples in 90 Day Window` >= 10)
            if(nrow(STVexceedanceAnd10Samples) > 0 ){
              return(mutate(STVexceedanceAnd10Samples, 
                            `Assessment Decision` = 'Impaired: at least one 90 day window with 10+ samples and STV exceedance rate > 10.5%'))  }
            
            # If any STV exceedance rate > 10.5% from 90 day windows with <10 samples then test more
            
            # If 2+ STV exceedances in any size window
            morethan1STVexceedanceInAnyWindow <- filter(STVexceedanceAfterPassingGeomean, `STV Exceedances In Window` >= 2)
            if(nrow(morethan1STVexceedanceInAnyWindow) > 0){
              
              
              return(mutate(morethan1STVexceedanceInAnyWindow, `Assessment Decision` = paste('Impaired: 2 or more STV exceedances in a 90 day window')))  
            } else {
              # Only other option is only 1 STV exceedance in any size window
              nonOverlappingIntervalsPassingGeomeanButHave1STV <- nonOverlappingIntervals(geomeanApplies, TRUE, last2years) # EVJ edit to test passing geomean windows for independence
              if(nrow(nonOverlappingIntervalsPassingGeomeanButHave1STV) >=2){
                statement <- 'Fully Supporting: no geomean exceedances in 2+ non overlapping 90 day windows within the Recreation Season; Observed effect: window(s) present with < 10 samples and 1 STV Exceedance'
                return(mutate(nonOverlappingIntervalsPassingGeomeanButHave1STV, 
                              `Assessment Decision` = ifelse(is.na(OE), statement, paste(statement, OE))))  
              } else {
                statement <- 'Insufficient Information: meets geomean criteria but lacks at least two independent 90 day windows; Observed Effect: 1 STV exceedance in 90 day window with <= 10 samples (Prioritize for follow up monitoring)'
                return(mutate(z, `Assessment Decision` = ifelse(is.na(OE), statement, paste(statement, OE)))) }
            }
          } else { 
            # If no geomean exceedances and no STV exceedances in 90 day window 
            
            # First see if two independent 90 day windows passing geomean can render Fully Supporting
            nonOverlappingIntervalsPassingGeomean <- nonOverlappingIntervals(geomeanApplies, TRUE, last2years)
            if(nrow(nonOverlappingIntervalsPassingGeomean) >= 2 ){
              # Means two or more nonoverlapping windows that fall in rec season
              # Test for any observed effects before spitting out answer
              STVexceedanceInDataset <- filter(z, `STV Exceedances In Window` > 0)
              if(nrow(STVexceedanceInDataset) > 0){
                statement <- 'Fully Supporting: no geomean exceedances in 2+ non overlapping 90 day windows within the Recreation Season; Observed Effect: window(s) present with 1 STV Exceedance'
                return(mutate(nonOverlappingIntervalsPassingGeomean, 
                              `Assessment Decision` = ifelse(is.na(OE), statement, paste(statement, OE))))  
              } else {
                statement <- 'Fully Supporting: no geomean exceedances in 2+ non overlapping 90 day windows within the Recreation Season'
                return(mutate(nonOverlappingIntervalsPassingGeomean, 
                              `Assessment Decision` = ifelse(is.na(OE), statement, paste(statement, OE))))  }
            } else { # If no independent windows meeting geomean and STV needs additional sampling
              # Test for any observed effects before spitting out answer
              STVexceedanceInDataset <- filter(z, `STV Exceedances In Window` > 0)
              if(nrow(STVexceedanceInDataset) > 0){
                statement <- 'Insufficient Information: no geomean exceedances but STV exceedances noted. Dataset lacks 2+ non overlapping 90 day windows within Recreation Season'
                return(mutate(z,  `Assessment Decision` = ifelse(is.na(OE), statement, paste(statement, OE))))    
              } else {
                statement <- 'Insufficient Information: no geomean or STV exceedances but dataset lacks 2+ non overlapping 90 day windows within Recreation Season'
                return(mutate(z, `Assessment Decision` = ifelse(is.na(OE), statement, paste(statement, OE))))   }
            }
          }
        }
      } else { # These scenarios have no geomean criteria bc sampling < sampleRequirements
        exceedsSTVrate <- filter(z, `STV Exceedance Rate` >= 10.5)
        # First deal with no STV exceedance rate issues AND not enough data to assess geomean
        if(nrow(exceedsSTVrate) == 0 ){
          statement <- 'Insufficient Information: no 90 day windows with > 10.5% exceedance of STV but not enough samples in any 90 day window to assess geomean (Prioritize for follow up monitoring)'
          return(mutate(z,`Assessment Decision` = ifelse(is.na(OE), statement, paste(statement, OE)))) 
        } else { # These have STV exceedance rate issues AND not enough data to assess geomean
          moreThan2STVhitsInWindow <- filter(exceedsSTVrate, `STV Exceedances In Window` >= 2)
          if(nrow(moreThan2STVhitsInWindow) >= 1){
            # these have 2 or more hits in any given 90 day window
            return(mutate(moreThan2STVhitsInWindow,`Assessment Decision` = 
                            paste('Impaired: ',nrow(moreThan2STVhitsInWindow), '90 day window(s) with 2+ exceedances of STV but not enough samples in any 90 day window to assess geomean.',
                                  nrow(nonOverlappingIntervals(moreThan2STVhitsInWindow, TRUE, last2years)),'of the 90 day window(s) with 2+ exceedances of STV were nonoverlapping and
                                  within the Recreation Season.')))   
          } else {
            # These have 1 hit in any 90 day window but < 10 samples in said window(s) i.e. not enough info for geomean assessment
            statement <- 'Insufficient Information: no 90 day windows with > 10.5% exceedance of STV but not enough samples in any 90 day window to assess geomean (Prioritize for follow up monitoring)'
            return(mutate(z,`Assessment Decision` = ifelse(is.na(OE), statement, paste(statement, OE))))
          }
        }
        
      }
    }
  } else {
    return(tibble(StationID=NA, `Window Within Recreation Season`=NA, `Date Window Starts`=NA, `Date Window Ends`=NA,
                  `Samples in 90 Day Window`=NA, `STV Exceedances In Window`=NA, `STV Exceedance Rate`=NA, `STV Assessment`=NA, 
                  `Geomean In Window`=NA, `Geomean Assessment`=NA, associatedData=NA, `Assessment Decision`=NA))
  }
  
 
}
