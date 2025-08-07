plot_ldp_test <- function(data, measure, measure_text, ncol = 10){
  
  plot <-  ggplot(data) +
    
    geom_col(aes(x = {{measure}}, y = perc_met, group = {{measure}}, fill = {{measure}},
                 text = paste0(geog, "<br>",
                               measure_text, {{measure}}, "<br>",
                               fy, "<br>",
                               "percentage of LDP standard acheived: ",
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
      limits = c(0, NA),
    labels=function(x) paste0(x,"%")
   ) +
    
    scale_fill_manual(values = phs_colours_32) +
    
    facet_wrap(~fy, strip.position = "bottom", ncol = ncol) + 
    
    theme_dementia(xangle = 45) +
    
    theme(strip.background = element_blank(),
          axis.ticks.x = element_blank(),
          strip.text.x = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_blank(),
          legend.position = "none",
         # axis.title.x = element_text(hjust = 0, vjust = 0, size = 10)
    ) 
  
  ggplotly(plot, tooltip = "text") %>%

    config(displayModeBar = TRUE, doubleClick = F,
           modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d',
                                         'zoomOut2d', 'autoScale2d',
                                         'toggleSpikelines',
                                         'hoverCompareCartesian',
                                         'hoverClosestCartesian', 'toImage'),
           displaylogo = F, editable = F) %>%
  layout(xaxis = list(title = paste0(c(rep("&nbsp;", 100),"2016/17     2017/18 2018/19     2019/20    2020/21   2021/22    2022/23    2023/24"),
                                     collapse = ""))) %>%
  layout(margin = list(l = -5, b = 50, t = 40) # to avoid labels getting cut out
  )

}


plot_ldp_test(ldp_all_simd, measure = simd, measure_text = "SIMD Quintile: ")
