everyother <- function(x) x[seq_along(x) %% 2 != 0]

plot_trend_rate <- function(data, measure, measure_text, y = NULL, scales = NULL){
  
  plot <-  ggplot(data) +
    
    geom_point(aes(x = fy, y = pop_rate_10000, group = {{measure}}, colour = {{measure}},
                   text = paste0(geog, "<br>",
                                 measure_text, {{measure}}, "<br>",
                                 fy, "<br>",
                                 pop_rate_10000)
    )
    ) +
    
    geom_line(aes(x = fy, y = pop_rate_10000, group = {{measure}}, colour = {{measure}}))+
    
    geom_point(aes(x = fy, y = scot_pop_rate_10000, group = {{measure}}, colour = {{measure}},
                   text = paste0("Scotland", "<br>",
                                 measure_text, {{measure}}, "<br>",
                                 fy, "<br>",
                                 scot_pop_rate_10000)
    )
    ) +
    
    geom_line(aes(x = fy, y = scot_pop_rate_10000, group = {{measure}}, colour = {{measure}}),
              linetype = 2) +
    
    labs(title = "",
         x = "",
         y = y,
         colour = NULL,
         group = NULL
    ) +
    
    scale_y_continuous(limits = c(0, NA)
    ) +
    
    scale_x_discrete(breaks = everyother) +
    
    scale_colour_manual(values = phs_colours_32) +
    
    
    facet_wrap(vars({{measure}}), ncol = 5, scales = scales) + 
    
    theme_dementia(xangle = 45) +
    
    theme(strip.background = element_rect(fill = "#DFDDE3"),
          strip.text.x = element_text(size = 9),
          legend.position = "none",
          axis.title.y = element_text(margin = margin(r = 10))
    ) 
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, doubleClick = F,
           modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                         'zoomOut2d', 'autoScale2d', 
                                         'toggleSpikelines', 
                                         'hoverCompareCartesian', 
                                         'hoverClosestCartesian', 'toImage'), 
           displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.6 , y = 1.1,
                         xanchor = "center", yanchor = "top")) %>% 
    layout(margin = list(l = -5, b = 50, t = 40) # to avoid labels getting cut out
    ) 
  
}



plot_trend_rate_legend_1 <- function(data) {
  
  legend <- data %>% ggplot(aes(x = x, y = y, fill = geog)
  ) +
    
    geom_line(aes(linetype = geog)) +
    
    labs(fill = NULL, linetype = NULL) +
    
    scale_fill_manual(values = c("black","black")) +
    
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = c(0.5, 0.5), # move the legend to the center
          panel.grid = element_blank(),
          panel.border = element_rect(colour = "white", fill='white', size=0)
    )
  
  legend_plotly<-ggplotly(legend, tooltip = NULL)
  
  config(legend_plotly, staticPlot = TRUE) %>%
    layout(legend = list(orientation = "h",
                         x = 0.5, y = 0.5, xanchor = "center", yanchor = "right" )#,
           # margin = list(r = -500, l = -500, b = -500, t = 40)
    )  
}

plot_trend_rate_legend_2 <- function(data, measure, measure_text) {
  
  legend <- data %>% ggplot(aes(x = fy, y = 0, group = {{measure}}, colour = {{measure}})
  ) +
    geom_point() +
    
    labs(colour = measure_text) +
    
    scale_colour_manual(values = phs_colours_32) +
    
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = c(0.5, 0.5), # move the legend to the center
          panel.grid = element_blank(),
          panel.border = element_rect(colour = "white", fill='white', size=0)
    )
  
  legend_plotly<-ggplotly(legend, tooltip = NULL)
  
  config(legend_plotly, staticPlot = TRUE) %>%
    layout(legend = list(orientation = "h",
                         x = 0.5, y = 0.5, xanchor = "center", yanchor = "right" )#,
           # margin = list(r = -500, l = -500, b = -500, t = 40)
    )  
}


