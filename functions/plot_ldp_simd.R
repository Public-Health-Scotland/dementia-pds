plot_ldp_simd <- function(data){
  
  plot <-  data %>% ggplot() +
    
    geom_col(aes(x = simd, y = perc_met, fill = fy,
                 text = paste0("SIMD Quintile: ", simd, "<br>",
                               fy, "<br>",
                               "percentage of LDP standard acheived: ", paste0(perc_met, "%")
                               )),
             position = position_dodge()) +
    
    labs(title = "",
         x = "",
         y = "",
        fill = NULL
    ) +
    
    scale_y_continuous(limits = c(0, NA),
                       labels=function(x) paste0(x,"%")) +
    
    scale_fill_manual(values = phs_colours_32) +
    
    #phsstyles::scale_fill_discrete_phs(palette = "all", name = NULL) +
    
    facet_wrap(vars(geog), ncol = 1) + 
    
    theme(strip.background = element_blank(),
         strip.text.x = element_blank(),
          legend.position = "none",
          #axis.title.x = element_text(size = 11,
                                      #face = "bold",
                                      #margin = margin(t = 7)),
         # title = element_text(size = 14)
         ) +
    
   theme(panel.spacing = unit(1000, "pt")) #this is for 'stacking' facets so only the selected one is visible
  
  # theme(axis.text.x = element_text(angle=45)) 
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                         'zoomOut2d', 'autoScale2d', 
                                         'toggleSpikelines', 
                                         'hoverCompareCartesian', 
                                         'hoverClosestCartesian'), 
           displaylogo = F, editable = F) %>%
     # layout(legend = list(orientation = "h", x = 0.5 , y = 1,
            #  xanchor = "center", yanchor = "top")) %>% 
    layout(margin = list(l = -5, b = 50, t = 40) # to avoid labels getting cut out
          # yaxis = yaxis_nf,
          # xaxis = xaxis_nf,
          # barmode = "grouped"
           ) 
  
}

plot_ldp_simd_legend <- function(data) {
  
  legend <- data %>% ggplot(aes(x = simd, y = 0, fill = fy)) +
    
    geom_col(width = 0) +
      
      labs(fill = NULL) +
      
      phsstyles::scale_fill_discrete_phs(palette = "all", name = NULL) +
    
        theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = c(0.5, 1), # move the legend to the center
          panel.grid = element_blank(),
          panel.border = element_rect(colour = "white", fill='white', size=0)
    )
  
  legend_plotly<-ggplotly(legend, tooltip = NULL)
  
  config(legend_plotly, staticPlot = TRUE) %>%
    layout(legend = list(orientation = "h",
                        x = 0.5, y = 1, xanchor = "center", yanchor = "top" ) 
           )  
}





