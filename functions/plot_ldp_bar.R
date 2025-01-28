plot_ldp_bar <- function(data, measure, measure_text, ncol = 10, nrow = NULL){
  
  plot <-  ggplot(data) +
    
    geom_col(aes(x = {{measure}}, y = perc_met, group = {{measure}}, fill = {{measure}},
                   text = paste0(geog, "<br>",
                                 measure_text, {{measure}}, "<br>",
                                 fy, "<br>",
                                 paste0(perc_met, "%"))
    ),  position = position_identity()
    ) +
    
    
    labs(title = "",
         x = "",
         y = "",
         colour = NULL,
         group = NULL,
    ) +
    
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, 101),
                       labels=function(x) paste0(x,"%")
    ) +
    
    scale_fill_manual(values = phs_colours_32) +
  
   facet_wrap(vars(fy), ncol = ncol, nrow = nrow) + 
    
    theme_dementia(xangle = 45) +
    
    theme(strip.background = element_blank(),
          axis.ticks.x = element_blank(),
          strip.text.x = element_blank(),
          axis.text.x = element_blank(),
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
    #layout(legend = list(orientation = "h", x = 0.6 , y = 1.1,
                       #  xanchor = "center", yanchor = "top")) %>% 
    layout(margin = list(l = -5, b = 50, t = 40) # to avoid labels getting cut out
    ) 
  
}


plot_ldp_bar_legend <- function(data, measure, measure_text) {
  
  legend <- data %>% ggplot(aes(x = {{measure}}, y = 0, group = {{measure}}, fill = {{measure}})
  ) +
    geom_col() +
    
    labs(fill = measure_text) +
    
    scale_fill_manual(values = phs_colours_32) +
    
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position.inside = c(0.5, 0.5), # move the legend to the center
          panel.grid = element_blank(),
          panel.border = element_rect(colour = "white", fill='white', linewidth =0)
    )
  
  legend_plotly<-ggplotly(legend, tooltip = NULL)
  
  config(legend_plotly, staticPlot = TRUE) %>%
    layout(legend = list(orientation = "h",
                         x = 0.5, y = 0.5, xanchor = "center", yanchor = "right" )#,
           # margin = list(r = -500, l = -500, b = -500, t = 40)
    )  
}


plot_ldp_bar_labels <- function(data, measure, ncol = 10, nrow = NULL){
  
  label <-  ggplot(data) +
    
    geom_col(aes(x = {{measure}}, y = 0, group = {{measure}}, fill = {{measure}}
    ),  position = position_identity()
    ) +
    
    
    labs(title = "",
         x = "",
         y = "",
         colour = NULL,
         group = NULL,
    ) +
    
    facet_wrap(~ fy, strip.position = "top", ncol = ncol, nrow = nrow) + 
    
    theme(axis.title = element_blank(),
          legend.position = "none",
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_rect(colour = "white", fill='white', linewidth =0),
          axis.text.x = element_text(colour = "white", size = 10, angle = 45, vjust = 0.5, hjust = 0.5),
    ) 
  
  label_plotly<-ggplotly(label, tooltip = NULL)
  
  config(label_plotly, staticPlot = TRUE)
  
}