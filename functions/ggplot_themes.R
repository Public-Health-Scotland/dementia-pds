
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

colours <- c(
  `phs-purple` = "#3F3685",
  `phs-magenta` = "#9B4393",
  `phs-blue` = "#0078D4",
  `phs-green` = "#83BB26",
  `phs-graphite` = "#948DA3",
  `phs-teal` = "#1E7F84",
  `phs-liberty` = "#6B5C85",
  `phs-rust` = "#C73918"
)

phs_colours <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (colours)
  
  colours[cols]
}

palettes <- list(
  `main` = phs_colours("phs-purple", "phs-magenta", "phs-blue", "phs-green"),
  `supporting` = phs_colours("phs-graphite", "phs-teal", "phs-liberty", "phs-rust")
)

