plot_ldp_simd <- function(data){
  
  # xaxis_nf <- list(
  #   title = "",
  #   categoryorder = "array",
  #   categoryarray = ldp_all_simd$geog,
  #   showline = TRUE,
  #   linecolor = "rgb(204, 204, 204)",
  #   linewidth = 2,
  #   showgrid = FALSE,
  #   showticklabels = TRUE,
  #   ticks = "inside",
  #   tickcolor = "rgb(204, 204, 204)",
  #   tickwidth = 2,
  #   ticklen = 5,
  #   tickfont = list(
  #     family = "Arial",
  #     size = 10,
  #     color = "rgb(82, 82, 82)"
  #   )
  # )
  
  # yaxis_nf <- list(
  #   title = "",
  #   categoryorder = "array",
  #   categoryarray = ldp_all_simd$geog,
  #   showgrid = TRUE,
  #   zeroline = FALSE,
  #   showline = TRUE,
  #   linecolor = "rgb(204, 204, 204)",
  #   linewidth = 2,
  #   showticklabels = TRUE,
  #   ticks = "inside",
  #   tickcolor = "rgb(204, 204, 204)",
  #   tickwidth = 2,
  #   ticklen = 5,
  #   tickfont = list(
  #     family = "Arial",
  #     size = 12,
  #     color = "rgb(82, 82, 82)"
  #   ),
  #   font = list(size = 12, family = "Arial"),
  #   range = c(0, 108)
  # )
  
  
  plot <-  data %>% ggplot() +
    
    geom_col(aes(x = simd, y = perc_met, fill = fy,
                 text = paste0("SIMD Quintile: ", simd, "<br>",
                               fy, "<br>",
                               "percentage of LDP standard acheived: ", paste0(perc_met, "%")
                               )),
             position = position_dodge()) +
    
    labs(title = "Percentage of LDP standard achieved by SIMD Quintile and Financial Year",
         x = "",
         y = "",
        fill = NULL
    ) +
    
    scale_y_continuous(limits = c(0, NA),
                       labels=function(x) paste0(x,"%")) +
    
    facet_wrap(vars(geog), ncol = 1) + 
    
    theme(strip.background = element_blank(),
         strip.text.x = element_blank(),
          #legend.position = "top",
          #axis.title.x = element_text(size = 11,
                                      #face = "bold",
                                      #margin = margin(t = 7)),
          title = element_text(size = 14)) +
    
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
      layout(legend = list(orientation = "h", x = 0.5 , y = 1,
              xanchor = "center", yanchor = "top")) %>% 
    layout(margin = list(b = 50, t = 40) # to avoid labels getting cut out
          # yaxis = yaxis_nf,
          # xaxis = xaxis_nf,
          # barmode = "grouped"
           ) 
  
}


