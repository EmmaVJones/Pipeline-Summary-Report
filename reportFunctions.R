rawDataPlot <- function(plottingDataset,upstreamParameterName,downstreamParameterName,prettyParameterName,units){
  plot_ly(plottingDataset)%>%
    add_trace(x = ~dateTime, y = ~get(upstreamParameterName), mode = 'scatter', name=paste("Upstream",prettyParameterName),
              hoverinfo="text",text=~paste("Date: ",dateTime,":",get(upstreamParameterName),units)) %>%
    add_trace(x = ~dateTime, y = ~get(downstreamParameterName), mode = 'scatter', name=paste("Downstream",prettyParameterName),
              hoverinfo="text",text=~paste("Date: ",dateTime,":",get(downstreamParameterName),units))%>%
    layout(showlegend = T,
           xaxis = list(title = "Date"),
           yaxis = list(title = paste(prettyParameterName,units)))}

differencePlot <- function(plottingDataset,prettyParameterName,units){
  plot_ly(plottingDataset)%>%
    add_trace(x = ~dateTime, y = ~absDiff, mode = 'scatter', name=paste("Absolute",prettyParameterName,"Difference"),
              hoverinfo="text",text=~paste("Date: ",dateTime,":",absDiff,units))%>%
    add_trace(x = ~dateTime, y = ~pctDiff, mode = 'scatter', name=paste("Percent", prettyParameterName,"Difference"),
              hoverinfo="text",text=~paste("Date: ",dateTime,":",pctDiff,"%"))%>%
    layout(showlegend = T,
           xaxis = list(title = "Date"),
           yaxis = list(title = paste("Difference (",units,"or %)")))}


medianNArm <- function(x){
  return(median(x,na.rm = T))
}



# Turbidity Whole Record analysis
turbidityWholeRecord <- function(upstreamData, downstreamData, parameter, turbidity99th1, turbidity99th2, time1, time2){
  # only run function if Turbidity data came from both datasets
  if(unique(c('Turb_Inst',"Turb_Inst_cd") %in% names(upstreamData))){
    UP <- dplyr::select(upstreamData,agency_cd,site_no,dateTime,parameter)%>%rename(upstream=!!names(.[4])) # change parameter to general name to make further manipulations easier
    # If gage height data available at gage then grab that, too
    if("GH_Inst" %in% names(upstreamData)){
      if(unique(upstreamData$site_no)[!is.na(unique(upstreamData$site_no))]=="0205450393"){
        gh <- dplyr::select(upstreamData,agency_cd,site_no,dateTime,GH_Inst)%>%
          mutate(site_noGH='02054500')%>% dplyr::select(agency_cd,site_noGH,everything(),-site_no)
      }else{
        gh <- dplyr::select(upstreamData,agency_cd,site_no,dateTime,GH_Inst)%>%
          rename(site_noGH=!!names(.[2]))}
      
    }else{gh <- dplyr::select(upstreamData,agency_cd,dateTime)%>%
      mutate(site_noGH=NA,GH_Inst=NA)%>%dplyr::select(agency_cd,site_noGH,dateTime,GH_Inst)}
  }else{UP <- select(upstreamData,agency_cd,site_no,dateTime)%>%mutate(upstream=NA)}
  if(unique(c('Turb_Inst',"Turb_Inst_cd") %in% names(downstreamData))){
    DOWN <- dplyr::select(downstreamData,agency_cd,site_no,dateTime,parameter)%>%rename(downstream=!!names(.[4])) # change parameter to general name to make further manipulations easier
    # If gage height data available at gage then grab that, too
    if("GH_Inst" %in% names(downstreamData)){
      gh <- dplyr::select(downstreamData,agency_cd,site_no,dateTime,GH_Inst) %>%
        rename(site_noGH=!!names(.[2]))
    }else{
      if(unique(upstreamData$site_no)[!is.na(unique(upstreamData$site_no))]=="0205450393"){
        gh <- gh}
      if(exists('gh')){gh <- gh
      }else{
        gh <- dplyr::select(downstreamData,agency_cd,dateTime)%>%
          mutate(site_noGH=NA,GH_Inst=NA)%>%dplyr::select(agency_cd,site_noGH,dateTime,GH_Inst)}}
    
  }else{DOWN <- select(downstreamData,agency_cd,site_no,dateTime)%>%mutate(downstream=NA)}
  
  together <- full_join(UP,DOWN,by=c('agency_cd','dateTime'))%>%
    full_join(gh,by=c('agency_cd','dateTime'))
  
  dataWindow <- subset(together,dateTime >= format(time1,tz="EST5EDT") & 
                         dateTime <= format(time2,tz="EST5EDT")) %>%
    # Ensure data is realistic, replace wonky probe readings with NA so they are skipped
    # Filtering out bad data is not a good idea because you will lose entire rows of data and subsequently
    #  throw off other analyses down the line
    mutate(upstream = replace(upstream, upstream < 0 | upstream > 1500, NA ),
           downstream = replace(downstream, downstream < 0 | downstream > 1500, NA )) %>%
    mutate(parameter=parameter,
           numericDiff=downstream-upstream,
           pctDiff=(numericDiff/downstream)*100,
           window= lag(dateTime,6),
           turbidity_valid30minuteWindow=dateTime-lag(dateTime,6)) # lag 6 bc lag already grabs 1 row above
  
  # Calculate Baseline Turbidity for 2hr window 
  together <- dataWindow %>%
    tq_mutate(
      select     = upstream,
      mutate_fun = rollapply, 
      # rollapply args
      width      = 28,
      align      = "right",
      by.column  = FALSE,
      FUN        = medianNArm,
      col_rename = 'upstreamMedian')%>%
    tq_mutate(
      select     = downstream,
      mutate_fun = rollapply, 
      # rollapply args
      width      = 28,
      align      = "right",
      by.column  = FALSE,
      FUN        = medianNArm,
      col_rename = 'downstreamMedian')%>%
    # Apply both functions and choose result to use after the fact with median
    tq_mutate(
      select     = numericDiff,
      mutate_fun = rollapply, 
      # rollapply args
      width      = 7,
      align      = "right",
      by.column  = FALSE,
      FUN        = turbidityNumericThreshold,
      # FUN args
      threshold  = 5.999999999) %>% 
    dplyr::rename(NumericTurbidity_Exceedance=turbidity_Exceedance)%>%
    tq_mutate(
      select     = pctDiff,
      mutate_fun = rollapply, 
      # rollapply args
      width      = 7,
      align      = "right",
      by.column  = FALSE,
      FUN        = turbidityPercentThreshold, 
      # FUN args
      threshold  = 14.99999999)%>% #,col_rename = 'PercentExceedance')%>% # col_rename doesnt workwith tibbles
    dplyr::rename(PercentTurbidity_Exceedance=turbidity_Exceedance)%>%
    rowwise()%>%select(-turbidity_NAs..1)%>%
    # for < 3.6.0
    #rowwise()%>%select(-turbidity_NAs.1)%>%
    mutate(baselineTurbidity=max(upstreamMedian,downstreamMedian,na.rm=T),
           # the money part           
           exceedance=ifelse(baselineTurbidity<=40,NumericTurbidity_Exceedance,
                             PercentTurbidity_Exceedance),
           exceedanceType=ifelse(baselineTurbidity<=40,'Numeric',"Percent")) %>%
    tq_mutate(
      select     = upstream,
      mutate_fun = rollapply, 
      # rollapply args
      width      = 7,
      align      = "right",
      by.column  = FALSE,
      FUN        = turbidityNumericThreshold,
      # FUN args
      threshold  = turbidity99th1) %>%
    rename(turbidity_upstreamExceed99th=turbidity_Exceedance,turbidity_upstreamNAs=turbidity_NAs..1)%>%
    # for < R version 3.6.0
    #rename(turbidity_upstreamExceed99th=turbidity_Exceedance,turbidity_upstreamNAs=turbidity_NAs.1)%>%
    tq_mutate(
      select     = downstream,
      mutate_fun = rollapply, 
      # rollapply args
      width      = 7,
      align      = "right",
      by.column  = FALSE,
      FUN        = turbidityNumericThreshold,
      # FUN args
      threshold  = turbidity99th2) %>%
    rename(turbidity_downstreamExceed99th=turbidity_Exceedance,turbidity_downstreamNAs=turbidity_NAs..1) 
    # for < R version 3.6.0
    #rename(turbidity_downstreamExceed99th=turbidity_Exceedance,turbidity_downstreamNAs=turbidity_NAs.1) 
  
  
  validData <- filter(together,turbidity_valid30minuteWindow==30)
  # make a list object to output multiple dataframes of important data
  savedData <- list()
  savedData[['upDownNumericExceed']] <- filter(validData,exceedance>0 & exceedanceType=='Numeric' & turbidity_NAs <= 3) # Only allow up to 3 missing turbidity reading per half hour window
  savedData[['upDownPercentExceed']] <- filter(validData,exceedance>0 & exceedanceType=='Percent' & turbidity_NAs <= 3)
  savedData[['upstreamExceed99']] <- filter(validData,turbidity_upstreamExceed99th>0  & turbidity_upstreamNAs <= 3)
  savedData[['downstreamExceed99']] <- filter(validData,turbidity_downstreamExceed99th>0  & turbidity_downstreamNAs <= 3)
  
  
  savedData[['exceedThreshold']] <- data.frame(parameter=c('turbidity_upDownNumericExceed','turbidity_upDownPercentExceed',
                                                           'turbidity_upstreamExceed99','turbidity_downstreamExceed99'),
                                               pctExceedCurrentThreshold=c((nrow(savedData[['upDownNumericExceed']])/nrow(together))*100,
                                                                           (nrow(savedData[['upDownPercentExceed']])/nrow(together))*100,
                                                                           (nrow(savedData[['upstreamExceed99']])/nrow(together))*100,
                                                                           (nrow(savedData[['downstreamExceed99']])/nrow(together))*100))
  
  return(savedData)
}


#  upDownNumericExceed <- filter(validData,exceedance>0 & exceedanceType=='Numeric' & turbidity_NAs <= 3) # Only allow up to 3 missing turbidity reading per half hour window
#  upDownPercentExceed <- filter(validData,exceedance>0 & exceedanceType=='Percent' & turbidity_NAs <= 3)
#  upstreamExceed99 <- filter(validData,turbidity_upstreamExceed99th>0  & turbidity_upstreamNAs <= 3)
#  downstreamExceed99 <- filter(validData,turbidity_downstreamExceed99th>0  & turbidity_downstreamNAs <= 3)
  
  
#  exceedThreshold <- data.frame(parameter=c('turbidity_upDownNumericExceed','turbidity_upDownPercentExceed',
#                                            'turbidity_upstreamExceed99','turbidity_downstreamExceed99'),
#                                pctExceedCurrentThreshold=c((nrow(upDownNumericExceed)/nrow(together))*100,
#                                                            (nrow(upDownPercentExceed)/nrow(together))*100,
#                                                            (nrow(upstreamExceed99)/nrow(together))*100,
#                                                            (nrow(downstreamExceed99)/nrow(together))*100))
  
#  return(exceedThreshold)
  
#}