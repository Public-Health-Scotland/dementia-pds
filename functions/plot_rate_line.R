plot_rate_line <- function(data, measure, measure_text, y = NULL, scales = NULL, colours = phs_colours_core, facet_space = 6){
  
  plot <-  ggplot(data) +
    
    geom_point(aes(x = fy, y = pop_rate_10000, group = {{measure}}, colour = {{measure}},
                   text = paste0(geog, "<br>",
                                 fy, "<br>",
                                 measure_text, {{measure}}, "<br>",
                                 "Rate per 10,000 population: ", pop_rate_10000)
    )
    ) +
    
    geom_line(aes(x = fy, y = pop_rate_10000, group = {{measure}}, colour = {{measure}}))+
    
    geom_point(aes(x = fy, y = scot_pop_rate_10000, group = {{measure}}, colour = {{measure}},
                   text = paste0("Scotland", "<br>",
                                 fy, "<br>",
                                 measure_text, {{measure}}, "<br>",
                                 "Rate per 10,000 population: ", scot_pop_rate_10000)
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
    
    scale_colour_manual(values = colours) +
    
    
    facet_wrap(vars({{measure}}), ncol = 5, scales = scales) + 
    
    theme_dementia(xangle = 45) +
    
    theme(strip.background = element_rect(fill = "#DFDDE3"),
          strip.text.x = element_text(size = 9),
          legend.position = "none",
          axis.title.y = element_text(margin = margin(r = 10)),
          panel.spacing = unit(facet_space, "pt")
    ) 
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, doubleClick = F,
           modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                         'zoomOut2d', 'autoScale2d', 
                                         'toggleSpikelines', 
                                         'hoverCompareCartesian', 
                                         'hoverClosestCartesian', 'toImage'), 
           displaylogo = F, editable = F) %>%
    layout(clickmode = "none", legend = list(orientation = "h", x = 0.6 , y = 1.1,
                         xanchor = "center", yanchor = "top")) %>% 
    layout(margin = list(l = -5, b = 50, t = 40) # to avoid labels getting cut out
    ) 
  
}

plot_rate_line_legend  <- function(data) {
  
  legend_plot <- data %>% ggplot(aes(x = x, y = y, 
                                     linetype = legend)
  ) +
    geom_line()+
   
    scale_linetype_manual(values = c(c(rep("solid", 45), "dashed"))) +
    
    labs(fill = NULL, linetype = NULL) +
    
       theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = c(0.5, 0.5), # move the legend to the center
          panel.grid = element_blank(),
          panel.border = element_rect(colour = "white", fill='white', size=0)
    )
  
  legend_plotly<-ggplotly(legend_plot, tooltip = NULL)
  
  config(legend_plotly, staticPlot = TRUE) %>%
    layout(legend = list(orientation = "h",
                         x = 0.5, y = 0.5, xanchor = "center", yanchor = "right" )
          
    )  
}









# <- function(data) {
#   
#   legend_plot <- data %>% ggplot(aes(x = x, y = y, #fill = legend,
#                                      linetype = legend)
#   ) +
#     
#     geom_line(color = c(rep("black",45), "white", "black",rep("black",43), "black", "black")) +
#     
#     scale_linetype_manual(values = c(c(rep("solid", 45), "blank", "dashed"))) +
#     
#     labs(fill = NULL, linetype = NULL) +
#     
#     theme(axis.title = element_blank(),
#           axis.text = element_blank(),
#           axis.ticks = element_blank(),
#           legend.position = c(0.5, 0.5), # move the legend to the center
#           panel.grid = element_blank(),
#           panel.border = element_rect(colour = "white", fill='white', size=0)
#     )
#   
#   legend_plotly<-ggplotly(legend_plot, tooltip = NULL)
#   
#   config(legend_plotly, staticPlot = TRUE) %>%
#     layout(legend = list(orientation = "h",
#                          x = 0.5, y = 0.5, xanchor = "center", yanchor = "right" )
#     )  
# }
# 
# 
# 
# 
