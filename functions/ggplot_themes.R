theme_dementia <- function(){
  theme_gray() +
    theme(panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "#d9d9d9"),
          axis.title.y = element_text(size = 10, angle = 0, 
                                      hjust = 0.5, vjust = 0.5),
          axis.title.x = element_text(size = 10, angle = 0, 
                                      hjust = 0.5, vjust = 0.5),
          axis.text = element_text(size = 10),
          axis.line = element_line(),
          legend.position = "none",
          plot.margin = margin(10, 10, 10, 10))
}

phs_colours <- c(
  `purple` = "#3F3685",
  `magenta` = "#9B4393",
  `blue` = "#0078D4",
  `green` = "#83BB26",
  `graphite` = "#948DA3",
  `teal` = "#1E7F84",
  `liberty` = "#6B5C85",
  `rust` = "#C73918"
)


### END OF SCRIPT ###