
organizeAllGageData <- function(gageInfo, 
                                reportDurationStart,
                                reportDurationEnd){
  
  
  # fancy way to skip every other gage (bc it is the downstream gage) to roll through data pulls efficiently
  a <- 1:(nrow(gageInfo)-2)
  n <- length(a)
  
  # Where to store everything
  Results <- list()
  
  # make a template in case no data comes in for joining later
  template <- tibble(.rows = 0,
                     agency_cd = character(),
                     site_no = character(),
                     dateTime = as.POSIXct(character(), tz = 'America/New_York'),
                     Wtemp_Inst = numeric(),
                     Wtemp_Inst_cd = character(),
                     #GH_Inst = numeric(),
                     #GH_Inst_cd = character(),
                     SpecCond_Inst = numeric(),
                     SpecCond_Inst_cd = character(),
                     DO_Inst = numeric(),
                     DO_Inst_cd = character(),
                     pH_Inst = numeric(),
                     pH_Inst_cd = character(),
                     Turb_Inst = numeric(),
                     Turb_Inst_cd = character(),
                     tz_cd = character()  )
  
  # the actual data pull
  for(j in a[seq(1,n,2)]){ 
    print(j)
    #j = 1
    
    upstreamData <- NWISpull(paste(0,gageInfo$`USGS Station ID`[j],sep=''),
                             reportDurationStart, reportDurationEnd)
    # Special step to get gage height from Lafayette gage for Roanoke gage pair only
    # Gage height from Lafayette gage not available before 11/28/2017
    if(paste(0,gageInfo$`USGS Station ID`[j],sep='')=='0205450393'){
      upstreamDataLafayette <- NWISpull("02054500",reportDurationStart, reportDurationEnd) %>%
        dplyr::select(dateTime,GH_Inst,GH_Inst_cd)
      upstreamData <- full_join(upstreamData,upstreamDataLafayette,by='dateTime') %>% 
        dplyr::select(agency_cd,site_no,dateTime,Wtemp_Inst,Wtemp_Inst_cd,GH_Inst,GH_Inst_cd,everything())
      rm(upstreamDataLafayette)
    }
    downstreamData <- NWISpull(paste(0,gageInfo$`USGS Station ID`[j+1],sep=''),
                               reportDurationStart, reportDurationEnd)
    
    # Before joining, make sure all columns exists in case no data comes back from one (or both) gages
    upstreamData <- bind_rows(template, upstreamData)
    downstreamData <- bind_rows(template, downstreamData)
    
    together <- full_join(upstreamData,downstreamData,by=c('agency_cd','dateTime')) %>%
    # and add in appropriate information if missing data in join
      mutate(site_no.x = case_when(is.na(site_no.x) ~ paste0(0,gageInfo$`USGS Station ID`[j]),
                                   TRUE ~ site_no.x),
             site_no.y = case_when(is.na(site_no.y) ~ paste0(0,gageInfo$`USGS Station ID`[j+1]),
                                   TRUE ~ site_no.y))
    
    
    gageResults <- suppressWarnings(
      dataScan(upstreamData, downstreamData, 
               WQclassGage1 = gageInfo$WQS_Class[j],
               WQclassGage2 = gageInfo$WQS_Class[j+1],
               pHspecialStandards = gageInfo$pH_SpecialStandards[j],
               pHrangeAllowance = gageInfo$pH_RangeAllowance[j],
               SpCond_Designation = gageInfo$SpCond_Designation[j],
               turbidityBaseline = gageInfo$Turbidity_Baseline[j],
               turbidity99th1 = gageInfo$Turbidity_99th[j],
               turbidity99th2 = gageInfo$Turbidity_99th[j+1])
    )
    
    
    #To be fair, start analysis time when both gages are up and running,
    # replace data that is out of USGS accepted range
    firstUpstreamReading <- min(filter(together,!is.na(site_no.x))$dateTime)
    firstDownstreamReading <- min(filter(together,!is.na(site_no.y))$dateTime)
    
    fair <- filter(together,dateTime>max(firstUpstreamReading,firstDownstreamReading))%>%
      mutate(Wtemp_Inst.x = replace(Wtemp_Inst.x, Wtemp_Inst.x < 0 | Wtemp_Inst.x > 40, NA ),
             Wtemp_Inst.y = replace(Wtemp_Inst.y, Wtemp_Inst.y < 0 | Wtemp_Inst.y > 40, NA ),
             DO_Inst.x = replace(DO_Inst.x, DO_Inst.x < 0 | DO_Inst.x > 20, NA ),
             DO_Inst.y = replace(DO_Inst.y, DO_Inst.y < 0 | DO_Inst.y > 20, NA ),
             pH_Inst.x = replace(pH_Inst.x, pH_Inst.x < 0 | pH_Inst.x  > 14, NA ),
             pH_Inst.y = replace(pH_Inst.y, pH_Inst.y < 0 | pH_Inst.y  > 14, NA ),
             SpecCond_Inst.x = replace(SpecCond_Inst.x, SpecCond_Inst.x < 0 | SpecCond_Inst.x  > 50000, NA ),
             SpecCond_Inst.y = replace(SpecCond_Inst.y, SpecCond_Inst.y < 0 | SpecCond_Inst.y  > 50000, NA ),
             Turb_Inst.x = replace(Turb_Inst.x, Turb_Inst.x < 0 | Turb_Inst.x > 1500, NA ),
             Turb_Inst.y = replace(Turb_Inst.y, Turb_Inst.y < 0 | Turb_Inst.y > 1500, NA ))
    
    
    
    # How much time is data missing upstream vs downstream
    #missingDataAnalysis <- eventReactive(input$adjustWindow, {
    parametersToScan <- which(colnames(together) %in%
                                names(select(together,contains('Wtemp_Inst'),
                                             contains('SpecCond_Inst'),
                                             contains('DO_Inst'),
                                             contains('pH_Inst'),
                                             contains('Turb_Inst'),
                                             contains('GH_Inst')) %>%                               
                                        select(-contains('_cd'))))
    
    dataWindow <- filter(fair, dateTime >= format(reportDurationStart,tz="EST5EDT") & 
                           dateTime <= format(reportDurationEnd,tz="EST5EDT"))
    
    
    missingData <- data.frame(parameter=NA,pctMissingData=NA)
    for(i in parametersToScan){
      if(length(grep('.x',names(dataWindow)[i]))==1){prettyName <- sub('.x','_upstream',names(dataWindow)[i])
      }else{prettyName <- sub('.y','_downstream',names(dataWindow)[i])}
      missingData[i,] <- cbind(prettyName,(sum(is.na(dataWindow[,i]))/nrow(dataWindow))*100)
      missingData <- missingData[complete.cases(missingData),]
    }
    missingData <- missingData[order(missingData$parameter),]
    
    
    minmaxData <- select(gageResults,dateTime,site_no.x,site_no.y,T_upstreamMaxViolation,
                         T_downstreamMaxViolation,T_riseAboveNaturalViolation,DO_upstreamMinViolation,
                         DO_downstreamMinViolation,DO_upDownDifferenceViolation,
                         pH_upstreamViolation,pH_downstreamViolation,pH_upDownDifferenceViolation,
                         spCond_upstreamViolation,spCond_downstreamViolation,
                         spCond_upDownNumericDifferenceViolation,spCond_upDownPercentDifferenceViolation) %>%
      filter(dateTime>max(firstUpstreamReading,firstDownstreamReading))
    
    
    dataWindow <- filter(minmaxData,dateTime >= format(reportDurationStart,tz="EST5EDT") & 
                           dateTime <= format(reportDurationEnd,tz="EST5EDT"))
    
    exceedThreshold <- data.frame(parameter=NA,pctExceedCurrentThreshold=NA, nTweetsSent=NA)
    
    for(k in 4:length(minmaxData)){# start at 4 bc first columns are gage numbers and date
      # calculate how many tweets were sent hourly and then how many total sent
      z <- dataWindow[which(dataWindow[,k]==1),c(1,k)]
      if(nrow(z)>0){
        z1 <- data.frame(table(cut(z$dateTime,breaks = 'hour'))) %>%
          filter(Freq>0)
        totalTweetsSent <- nrow(z1)
      }else{totalTweetsSent <- 0}
      #save results to see n violations per hour????
      exceedThreshold[k,] <- cbind(names(dataWindow)[k],
                                   (length(which(dataWindow[,k]==1))/nrow(dataWindow))*100,
                                   totalTweetsSent)
      exceedThreshold <- exceedThreshold[complete.cases(exceedThreshold),]
    }
    
    turbidityResults <- turbidityWholeRecord(upstreamData, downstreamData, 'Turb_Inst',
                                             turbidity99th1 = gageInfo$`Turbidity_99th`[j],
                                             turbidity99th2=gageInfo$`Turbidity_99th`[j+1],
                                             time1=reportDurationStart,
                                             time2=reportDurationEnd)
    
    
    # Count up the tweets sent per hour, this summarizes violations per hour (strictly 8:00-9:00 even though the
    #  server splits the cron jobs between first 20minutes of hour). If an hour has >1 violation then it is 
    #  considered to have sent a tweet. More than one violation (or the same parameter) within a single hour
    #  does not trigger more tweets.
    
    if(nrow(turbidityResults[['upDownNumericExceed']])==0){
      # give it empty dataframe with no rows
      nTweets_upDownNumericExceed <- data.frame(Var1=as.factor(character()),Freq=as.integer(character()))
    }else{
      nTweets_upDownNumericExceed <- data.frame(table(cut(turbidityResults[['upDownNumericExceed']]$dateTime,breaks = 'hour'))) %>%
        filter(Freq>0)
    }
    if(nrow(turbidityResults[['upDownPercentExceed']])==0){
      # give it empty dataframe with no rows
      nTweets_upDownPercentExceed <- data.frame(Var1=as.factor(character()),Freq=as.integer(character()))
    }else{
      nTweets_upDownPercentExceed <- data.frame(table(cut(turbidityResults[['upDownPercentExceed']]$dateTime,breaks = 'hour'))) %>%
        filter(Freq>0)
    }
    
    if(nrow(turbidityResults[['upstreamExceed99']])==0){
      # give it empty dataframe with no rows
      nTweets_upstreamExceed99  <- data.frame(Var1=as.factor(character()),Freq=as.integer(character()))
    }else{
      nTweets_upstreamExceed99 <- data.frame(table(cut(turbidityResults[['upstreamExceed99']]$dateTime,breaks = 'hour'))) %>%
        filter(Freq>0)
    }
    
    if(nrow(turbidityResults[['downstreamExceed99']])==0){
      # give it empty dataframe with no rows
      nTweets_downstreamExceed99   <- data.frame(Var1=as.factor(character()),Freq=as.integer(character()))
    }else{
      nTweets_downstreamExceed99 <- data.frame(table(cut(turbidityResults[['downstreamExceed99']]$dateTime,breaks = 'hour'))) %>%
        filter(Freq>0)
    }
    
    
    exceedThreshold2 <- cbind(turbidityResults[['exceedThreshold']],
                              nTweetsSent=c(nrow(nTweets_upDownNumericExceed),
                                            nrow(nTweets_upDownPercentExceed),
                                            nrow(nTweets_upstreamExceed99),
                                            nrow(nTweets_downstreamExceed99)))
    exceedThreshold <- rbind(exceedThreshold,exceedThreshold2)
    
    
    # save out
    prettyColumnNames <- sub('.x','_upstream',names(together))
    prettyColumnNames <- c(prettyColumnNames[1],sub('.y','_downstream',prettyColumnNames[2:length(prettyColumnNames)]))
    names(together) <- prettyColumnNames
    
    out <- list(`Stream Name`= gageInfo$`Stream Name`[j],
                `Raw Data` = mutate(together,
                                    `Stream Name` = gageInfo$`Stream Name`[j]) %>%
                  dplyr::select(`Stream Name`, everything()),
                gageResults = mutate(gageResults,
                                     `Stream Name` = gageInfo$`Stream Name`[j]) %>%
                  dplyr::select(`Stream Name`, everything()),
                `Missing Data` = mutate(missingData,
                                        `Stream Name` = gageInfo$`Stream Name`[j]) %>%
                  dplyr::select(`Stream Name`, everything()),
                `Threshold Exceedances` = mutate(exceedThreshold,
                                                 `Stream Name` = gageInfo$`Stream Name`[j]) %>%
                  dplyr::select(`Stream Name`, everything()))
    Results[[`Stream Name`= gageInfo$`Stream Name`[j]]] <- out
    
  }
  
  return(Results)
}

#saveRDS(Results, paste0('Results_',reportDurationEnd,'.RDS'))
