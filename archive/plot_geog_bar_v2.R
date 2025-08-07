plot_geog_bar_v2 <- function(data, y_value, measure, measure_text, measure_2,
                             y_value_text = NULL, x = NULL, y = NULL,
                             ylabel = waiver(), ylimits = c(0, NA), symbol = NULL,
                             colours = phs_colours_core){
  
  plot <-  data %>% ggplot() +
    
    geom_col(aes(x = geog, y = {{y_value}}, fill = {{measure}},
                 text = paste0(geog, "<br>",
                               fy, "<br>",
                               measure_text, {{measure}}, "<br>",
                               y_value_text, {{y_value}}, symbol
                 )),
             position = position_dodge()) +
    
    labs(title = "",
         x = x,
         y = y,
         fill = NULL
    ) +
    
    scale_y_continuous(expand = c(0, 0),
                       limits = ylimits,
                       labels = ylabel
    ) +
    
    scale_fill_manual(values = colours) +
    
    facet_wrap(vars(fy, {{measure_2}}), ncol = 1, scales = "free_y") + 
    
    theme_dementia(xangle = 45) +
    
    theme(strip.background = element_blank(),
          strip.text.x = element_blank(),
          legend.position = "none",
          axis.title.x = element_text(size = 13,
                                      #face = "bold",
                                      margin = margin(t = 30)),
          axis.title.y = element_text(margin = margin(r = 10))
    ) +
    
    theme(panel.spacing = unit(1000, "pt")) #this is for 'stacking' facets so only the selected one is visible
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, doubleClick = F,
           modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                         'zoomOut2d', 'autoScale2d', 
                                         'toggleSpikelines', 
                                         'hoverCompareCartesian', 
                                         'hoverClosestCartesian', 'toImage'), 
           displaylogo = F, editable = F) %>%
    layout(clickmode = "none", margin = list(l = -5, b = 10, t = 40) # to avoid labels getting cut out
    ) 
  
}

plot_geog_bar_legend <- function(data, measure, colours = phs_colours_32) {

  legend <- data %>% ggplot(aes(x = geog, y = 0, fill = {{measure}})) +

    geom_col(width = 0) +

    labs(fill = NULL) +

    scale_fill_manual(values = colours) +

    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_rect(colour = "white", fill='white', linewidth=0)
    )

  legend_plotly<-ggplotly(legend, tooltip = NULL)

  config(legend_plotly, staticPlot = TRUE) %>%
    layout(legend = list(orientation = "h",
                         x = 0.5, y = 1, xanchor = "center", yanchor = "top" )
    )
}





