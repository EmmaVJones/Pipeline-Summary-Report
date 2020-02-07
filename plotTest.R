library(plotly)

dat <- readRDS('data/littleStony.RDS')#Results[["Little Stony Creek"]][["Raw Data"]]


plot_ly(dat)%>%
  add_markers(x = ~dateTime, y = ~pH_Inst_upstream, #mode = 'markers', 
              name="pH upstream",
            hoverinfo="text",text=~paste("Date: ",dateTime,":",pH_Inst_upstream,"standard units"))%>%
  add_markers(x = ~dateTime, y = ~pH_Inst_downstream, #mode = 'markers', 
              name="pH downstream",
            hoverinfo="text",text=~paste("Date: ",dateTime,":",pH_Inst_downstream,"standard units"))%>%
  add_lines(x=~dateTime,y=9,# mode='lines',
            line = list(color = '#E50606'),
            hoverinfo = "none", name="pH Standard")%>%
  add_lines(x=~dateTime,y=6,# mode='lines',
            line = list(color = '#E50606'),
            hoverinfo = "none", name="pH Standard")%>%
  add_markers(x=~dateTime, y = ~GH_Inst,# mode = 'markers', 
              name="Gage Height",yaxis = "y2",
            hoverinfo="text",text=~paste("Date: ",dateTime,"<br>",GH_Inst,"ft"))%>%
  layout(showlegend = TRUE,
         xaxis = list(title = "Date"),
         yaxis = list(title = "pH (standard units)"),
         yaxis2 = list(title = 'Gage Height (ft)',
                       overlaying = "y",
                       side = 'right'))
