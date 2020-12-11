
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
          axis.line = element_line())
}