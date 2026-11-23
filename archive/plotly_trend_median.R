plotly_trend_median <- function(data){
plot_ly(data,
        x = ~fy, y = ~median_diagnosis_to_contact, 
        hovertext = ~geog,
        type = 'scatter', mode = "lines+markers", color = ~geog, colors = phs_colours_46,
        hovertemplate = paste0("%{hovertext} <br>", "%{x} <br>", 
                               "Average (median) days from diagnosis to first contact: %{y}<extra></extra>")
      ) %>% 
  layout(clickmode = "none", showlegend = TRUE, legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.3),
         margin = list(l = -5, b = 10, t = 40),
         xaxis = list(title = "Financial Year of Diagnosis", showgrid = FALSE,
                      categoryorder = "trace", tickangle = 0, titlefont = list(size = 16)
                      ),
         yaxis = list(title = "Median Wait (days)", linecolor = "#b3b3b3", titlefont = list(size = 16), 
                      rangemode = "tozero")
  ) %>%
  config(displayModeBar = TRUE, doubleClick = F,
         modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                       'zoomOut2d', 'autoScale2d', 
                                       'toggleSpikelines', 
                                       'hoverCompareCartesian', 
                                       'hoverClosestCartesian', 'toImage'), 
         displaylogo = F, editable = F)

}