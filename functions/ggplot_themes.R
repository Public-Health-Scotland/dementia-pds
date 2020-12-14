
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
          legend.position = "none")
}

phs_colours <- c(
  `phs-purple` = "#3F3685",
  `phs-magenta` = "#9B4393",
  `phs-blue` = "#0078D4",
  `phs-green` = "#83BB26",
  `phs-graphite` = "#948DA3",
  `phs-teal` = "#1E7F84",
  `phs-liberty` = "#6B5C85",
  `phs-rust` = "#C73918"
)

phs_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (phs_colours)
  
  phs_colours[cols]
}

phs_palettes <- list(
  `main` = phs_cols("phs-purple", "phs-magenta", "phs-blue", "phs-green"),
  `supporting` = phs_cols("phs-graphite", "phs-teal", "phs-liberty", "phs-rust")
)

phs_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- phs_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

scale_fill_phs <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- phs_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("phs_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

scale_colour_phs <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- phs_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("phs_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


### END OF SCRIPT ###