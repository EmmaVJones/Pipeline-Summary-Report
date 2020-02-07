library(shiny)
library(plotly)
library(tidyverse)
library(purrr)
library(DT)

# Pull 7 days of gage data
#library(tidyquant)
#library(DT)
#library(dataRetrieval)

#gageInfo <- read_csv('data/gageInfoEVJ.csv')
#source('parameterFunctions[rmd2r].R')
#source('reportFunctions.R')
#source('pipelineSummaryFunctions.R')

# Daily or Weekly report?
#reportType <- 'Weekly' #'Daily' 
#reportDurationStart <- if(reportType == 'Daily'){
#  Sys.Date()-2
#} else {
#  Sys.Date()-8
#}

#reportDurationEnd <- Sys.Date()-1

#Results <- suppressMessages(
#  suppressWarnings(organizeAllGageData(gageInfo, reportDurationStart, reportDurationEnd)))

Results <- readRDS('Results.RDS')

missingData <- map_df(Results, 'Missing Data') %>%
  mutate(pctMissingData = as.numeric(pctMissingData)) 
  
  
#map_df(Results, 'Missing Data') %>%
#  mutate(pctMissingData = as.numeric(pctMissingData)) %>%
#  group_by(`Stream Name`) %>%
#  summarize(meanMissing = mean(pctMissingData, na.rm = T)) %>%
#  plot_ly() %>%
#  add_bars(x = ~`Stream Name`, y = ~meanMissing, hoverinfo = 'text',
#           text = ~paste0(`Stream Name`," Missing on Average ", format(meanMissing, digits = 2), "%")) %>%
#  layout(yaxis = list(title = 'Average Missing Data'),
#         xaxis = list(title = 'Stream Name'))

#map_df(Results, 'Missing Data') %>%
#  mutate(pctMissingData = as.numeric(pctMissingData)) %>%
#  filter(`Stream Name` %in% c('Cowpasture River')) %>%
#  plot_ly(name = 'Stream Name') %>%
#  add_bars(x = ~`parameter`, y = ~pctMissingData)

paramAxisName <- data.frame(raw = c('Wtemp_Inst', 'DO_Inst', 'pH_Inst', 
                                    'SpecCond_Inst', 'Turb_Inst'),
                            axis = c("Temperature (Celsius)", "Dissolved Oxygen (mg/L)",
                                     "pH (Standard Units)", "Specific Conductivity (uS/cm)",
                                     "Turbidity (FNU)"))


scatterPlot <- function( Results, paramAxisName, streamName, parameter){
  parameter <- strsplit(sub('(^[^_]+_[^_]+)_(.*)$', '\\1 \\2', parameter), ' ')[[1]][1]
  paramAxisName <- filter(paramAxisName, raw == parameter)[,2]

  map_df(Results, 'Raw Data') %>%
    filter(`Stream Name` %in% streamName) %>%
    dplyr::select(dateTime, GH_Inst, starts_with(parameter)) %>%
    dplyr::select(-contains("cd")) %>%
    plot_ly(name = streamName) %>%
    add_lines(x = ~dateTime, y = ~GH_Inst,yaxis = "y2", name = 'Gage Height',
                hoverinfo="text",text=~paste("Date: ",dateTime,"<br>",GH_Inst,"ft")) %>%
    
    add_lines(x = ~dateTime, y = ~get(paste0(parameter,"_upstream")), name = 'Upstream') %>%
    add_lines(x = ~dateTime, y = ~get(paste0(parameter,"_downstream")), name = 'Downstream') %>%
    layout(showlegend = TRUE,
           title = streamName,
           xaxis = list(title = "Date"),
           yaxis = list(title = paramAxisName),
           yaxis2 = list(title = 'Gage Height (ft)',
                         overlaying = "y",
                         side = 'right'))
}
#scatterPlot( Results, paramAxisName, 'Little Stony Creek', 'Turb_Inst_upstream')
  






### The app

ui <- fluidPage(plotlyOutput("plot"), uiOutput("back"), #verbatimTextOutput('test'),
                uiOutput('tableOrPlot')
                )

server <- function(input, output, session) {
  
  selections <- reactiveVal()
  
  # show population by continent by default, but if there is a selected continent
  # show population by country within that continent
  output$plot <- renderPlotly({
    nSelections <- length(selections())
    if (nSelections == 0) {
      missingData %>%
        group_by(`Stream Name`) %>%
        summarize(meanMissing = mean(pctMissingData, na.rm = T)) %>%
        plot_ly() %>%
        add_bars(x = ~`Stream Name`, y = ~meanMissing, hoverinfo = 'text',
                 text = ~paste0(`Stream Name`," Missing on Average ", 
                                format(meanMissing, digits = 2), "%")) %>%
        layout(yaxis = list(title = 'Average Missing Data'),
               xaxis = list(title = 'Stream Name'))
    } else {
      missingData %>%
        filter(`Stream Name` %in% selections()) %>%
        plot_ly(name = 'Stream Name') %>%
        add_bars(x = ~`parameter`, y = ~pctMissingData)
    }
  })
  
  observeEvent(event_data("plotly_click"), {
    new <- event_data("plotly_click")$x
    old <- selections()
    selections(c(old, new))
  })
  
  # populate back button if category is chosen
  output$back <- renderUI({
    if (length(selections())) 
      actionButton("clear", "Back", icon("chevron-left"))
  })
  
  # clear the chosen category on back button press
  observeEvent(input$clear, selections(NULL))


  tableData <- reactive({
    if(is.null(event_data("plotly_click")$x))
      return(NULL)
    
    if(event_data("plotly_click")$x %in% missingData$`Stream Name`){
      z <- filter(missingData, `Stream Name` %in% event_data("plotly_click")$x)
    } else {
      # to compare all like parameters regardless of stream
      #z <- filter(missingData, parameter %in% event_data("plotly_click")$x)
      z <- filter(missingData, `Stream Name` %in% selections()[1]) %>%
        filter( parameter %in% event_data("plotly_click")$x)
    }
  })
  
  output$tableOrPlot <- renderUI({
    req(tableData())
    
    if(event_data("plotly_click")$x %in% missingData$`Stream Name`){
      DT::dataTableOutput('table')
    }else{
        plotlyOutput('plotlyRaw')
      }
      
  })
  
  output$table <- DT::renderDataTable({
    if(is.null(selections()))
      return(NULL)
    DT::datatable(tableData(), rownames = F)
    })
    
  output$plotlyRaw <- renderPlotly({
    if(is.null(selections()))
      return(NULL)
    scatterPlot( Results, paramAxisName, selections()[1], event_data("plotly_click")$x)
  })

}

shinyApp(ui, server)
