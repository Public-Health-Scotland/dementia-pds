phs_colours_extended<-data.frame(c("phs-purple" = "#3F3685",
                                  "phs-rust-80" = "#D26146",
                                  "phs-purple-30" = "#C5C3DA",
                                  "phs-magenta-30" = "#E1C7DF",
                                  "grey-dark" = "#6d6d6d",
                                  "phs-purple-80" = "#655E9D",
                                  "phs-green-dark" = "#618a1c",
                                  "phs-graphite-30" = "#DFDDE3",
                                  "phs-teal-30" = "#BCD9DA",
                                  "phs-liberty-30" = "#D3CEDA",
                                  "phs-rust-30" = "#EEC4BA",
                                  "grey-medium" = "#9e9e9e",
                                  "phs-magenta-80" = "#AF69A9",
                                  "phs-magenta-dark" = "#72316c",
                                  "phs-graphite-dark" = "#6d667e",
                                  "phs-green-30" = "#DAEBBE",
                                  "phs-teal-80" = "#4B999D",
                                  "phs-liberty-dark" = "#584c6e",
                                  "phs-liberty-50" = "#B5AEC2",
                                  "phs-blue-30" = "#B3D7F2",
                                  "phs-rust-50" = "#E39C8C",
                                  "black" = "#000000",
                                  "phs-green-80" = "#9CC951",
                                  "grey-light" = "#BBBBBB",
                                  "phs-blue-dark" = "#0062AD",
                                  "phs-blue-80" = "#3393DD",
                                  "phs-graphite-80" = "#A9A4B5",
                                  "phs-rust-dark" = "#a42f14",
                                  "grey-very-light" = "#d8d8d8",
                                  "phs-teal-dark" = "#176064",
                                  "phs-purple-dark" = "#2b255b",
                                  "grey-very-dark" = "#595959",
                                  "phs-magenta" = "#9B4393",
                                  "phs-blue" = "#0078D4",
                                  "phs-green" = "#83BB26",
                                  "phs-graphite" = "#948DA3",
                                  "phs-teal" = "#1E7F84",
                                  "phs-liberty" = "#6B5C85",
                                  "phs-rust" = "#C73918",
                                  "phs-purple-50" = "#9F9BC2",
                                  "phs-magenta-50" = "#CDA1C9",
                                  "phs-blue-50" = "#80BCEA",
                                  "phs-green-50" = "#C1DD93",
                                  "phs-graphite-80" = "#CAC6D1",
                                  "phs-teal-50" = "#8FBFC2",
                                  "phs-liberty-80" = "#897D9D"
))


phs_colours_extended %<>% rename("hexcode" = 1)


phs_colours_46<-phs_colours_extended$hexcode

phs_colours_32 <- phs_colours_extended %>% slice(1:32)
phs_colours_32<-phs_colours_32$hexcode

phs_colours_31 <- phs_colours_extended %>% slice(2:32)
phs_colours_31<-phs_colours_31$hexcode


phs_colours_15 <- phs_colours_extended %>% slice(1,33:46)
phs_colours_15<-phs_colours_15$hexcode

phs_colours_14 <- phs_colours_extended %>% slice(33:46)
phs_colours_14<-phs_colours_14$hexcode



phs_colours_core <- c(
  "#3F3685",
  "#9B4393",
 "#0078D4",
  "#83BB26",
  "#948DA3",
 "#1E7F84",
  "#6B5C85",
  "#C73918"
)


integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

everyother <- function(x) x[seq_along(x) %% 2 != 0]


theme_dementia <- function(xangle = 0){
  
  theme_set(theme_minimal(base_size = 12)) +
    
    theme(
      axis.ticks = element_line(color = "grey92"),
      axis.line = element_line(colour = "grey70"),
      axis.text.x = element_text(size = 10, angle = xangle, vjust = 0.5, hjust = 0.5),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey92"),
      panel.background = element_blank()
    )
  
}


### END OF SCRIPT ###
