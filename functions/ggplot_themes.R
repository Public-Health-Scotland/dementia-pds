phs_colours_ordered<-data.frame(c("phs-purple" = "#3F3685",
  "phs-magenta" = "#9B4393",
  "phs-blue" = "#0078D4",
  "phs-green" = "#83BB26",
  "phs-graphite" = "#948DA3",
  "phs-teal" = "#1E7F84",
  "phs-liberty" = "#6B5C85",
  "phs-rust" = "#C73918",
  "phs-purple-80" = "#655E9D",
  "phs-magenta-80" = "#AF69A9",
  "phs-blue-80" = "#3393DD",
  "phs-green-80" = "#9CC951",
  "phs-graphite-80" = "#A9A4B5",
  "phs-teal-80" = "#4B999D",
  "phs-liberty-80" = "#897D9D",
  "phs-rust-80" = "#D26146",
  "phs-purple-30" = "#C5C3DA",
  "phs-magenta-30" = "#E1C7DF",
  "phs-green-50" = "#C1DD93",
  "phs-purple-80" = "#655E9D",
  "phs-green" = "#83BB26",
  "phs-graphite-30" = "#DFDDE3",
  "phs-teal-30" = "#BCD9DA",
  "phs-liberty-30" = "#D3CEDA",
  "phs-rust-30" = "#EEC4BA",
  "phs-purple-50" = "#9F9BC2",
  "phs-magenta-50" = "#CDA1C9",
  "phs-blue-50" = "#80BCEA",
  "phs-graphite" = "#948DA3",
  "phs-green-30" = "#DAEBBE",
  "phs-teal-50" = "#8FBFC2",
  "phs-liberty" = "#6B5C85",
  "phs-liberty-50" = "#B5AEC2",
  "phs-blue-30" = "#B3D7F2",
  "phs-rust-50" = "#E39C8C",
  "black" = "#000000",
  "phs-green-80" = "#9CC951",
  "dark-grey" = "#BBBBBB",
  "dark-blue" = "004785",
  "phs-blue" = "#0078D4",
  "phs-graphite-80" = "#A9A4B5",
  "phs-rust" = "#C73918",
  "phs-graphite-50" = "#CAC6D1",
  "phs-teal" = "#1E7F84",
  "phs-magenta" = "#9B4393",
  "phs-liberty-80" = "#897D9D"
))

phs_colours_ordered %<>% rename("hexcode" = 1)


phs_colours_46<-phs_colours_ordered$hexcode


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


phs_colour_names<-data.frame(c(
"phs-purple" = "#3F3685",
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
"phs-graphite-50" = "#CAC6D1",
"phs-teal-50" = "#8FBFC2",
"phs-liberty-50" = "#B5AEC2",
"phs-rust-50" = "#E39C8C",
"phs-purple-30" = "#C5C3DA",
"phs-magenta-30" = "#E1C7DF",
"phs-blue-30" = "#B3D7F2",
"phs-green-30" = "#DAEBBE",
"phs-graphite-30" = "#DFDDE3",
"phs-teal-30" = "#BCD9DA",
"phs-liberty-30" = "#D3CEDA",
"phs-rust-30" = "#EEC4BA",
"phs-purple-80" = "#655E9D",
"phs-magenta-80" = "#AF69A9",
"phs-blue-80" = "#3393DD",
"phs-green-80" = "#9CC951",
"phs-graphite-80" = "#A9A4B5",
"phs-teal-80" = "#4B999D",
"phs-liberty-80" = "#897D9D",
"phs-rust-80" = "#D26146",
"phs-purple-10" = "#ECEBF3",
"phs-magenta-10" = "#F5ECF4",
"phs-blue-10" = "#E6F2FB",
"phs-green-10" = "#F3F8E9",
"phs-graphite-10" = "#F4F4F6",
"phs-teal-10" = "#E9F2F3",
"phs-liberty-10" = "#F0EFF3",
"phs-rust-10" = "#F9EBE8"))


phs_colour_names %<>% rename("hexcode" = 1)

phs_colours_32 <- phs_colour_names %>% slice(1:32)
phs_colours_32<-phs_colours_32$hexcode





                 
                 