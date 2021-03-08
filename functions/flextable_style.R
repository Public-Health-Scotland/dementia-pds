ft_dementia_style <- function(ft) {
  
  cols <- ncol(ft$body[["dataset"]])
  rows <- nrow(ft$body[["dataset"]])
  
  ft %>% 
    
    font(fontname = "Arial", part = "all") %>%
    fontsize(size = 11, part = "all") %>%
    
    # Format header
    bold(part = "header") %>% 
    bg(bg = "#964091", part = "header") %>% 
    color(color = "white", part = "header") %>% 

    # Alignment
    valign(valign = "center", part = "all") %>% 
    align(j = 1, align = "left", part = "all") %>%
    align(j = 2:cols, align = "right", part = "all") %>% 
    align(j = 2:cols, align = "center", part = "header") %>% 

    # Bold Scotland row
    bold(i = rows) %>% 
    
    # Row height
    padding(padding.top = 2, padding.bottom = 2, part = "all") %>%
    height_all(height = 8) %>%

    # Borders
    border_outer(border = fp_border()) %>% 
    border(i = rows - 1, border.bottom = fp_border(), part = "body") %>%
    # border(j = c(1, 4, cols), border.right = fp_border(), part = "all") %>%
    border_inner_v(border = fp_border("black"), part = "all")
  
}