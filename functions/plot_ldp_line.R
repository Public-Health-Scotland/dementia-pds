everyother <- function(x) x[seq_along(x) %% 2 != 0]

plot_ldp_line <- function(data, measure, measure_text, ncol = 5, nrow = NULL, colours = phs_colours_32){
  
  plot <-  ggplot(data) +
    
    geom_point(aes(x = fy, y = perc_met, group = {{measure}}, colour = {{measure}},
                   text = paste0(geog, "<br>",
                                 fy, "<br>",
                                 measure_text, {{measure}}, "<br>",
                                 paste0(perc_met, "%"))
    )
    ) +
    
    geom_line(aes(x = fy, y = perc_met, group = {{measure}}, colour = {{measure}}))+
    
    geom_point(aes(x = fy, y = scot_perc_met, group = {{measure}}, colour = {{measure}},
                   text = paste0("Scotland", "<br>",
                                 fy, "<br>",
                                 measure_text, {{measure}}, "<br>",
                                 paste0(perc_met, "%"))
    )
    ) +
    
    geom_line(aes(x = fy, y = scot_perc_met, group = {{measure}}, colour = {{measure}}),
              linetype = 2) +
    
    labs(title = "",
         x = "",
         y = "",
         colour = NULL,
         group = NULL
    ) +
    
    scale_y_continuous(limits = c(0, NA),  labels=function(x) paste0(x,"%")
    ) +
    
    scale_x_discrete(breaks = everyother
    ) +
    
    scale_colour_manual(values = colours) +
    
    
    facet_wrap(vars({{measure}}), ncol = ncol, nrow = nrow) + 
    
    theme_dementia(xangle = 45) +
    
    theme(strip.background = element_rect(fill = "#DFDDE3"),
          strip.text.x = element_text(size = 9),
          legend.position = "none"
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



plot_ldp_line_legend_1 <- function(data) {
  
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


plot_ldp_line_legend_2 <- function(data, measure, measure_text) {
  
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


