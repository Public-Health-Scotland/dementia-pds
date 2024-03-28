ft_dementia_style <- function(ft) {
  
  cols <- ncol(ft$body[["dataset"]])
  rows <- nrow(ft$body[["dataset"]])
  
   ft %>% 
     
     # Replace NAs (zeros) with dash
     colformat_num(na_str = "-") %>%
     font(fontname = "Arial", part = "all") %>%
     fontsize(size = 12, part = "all") %>%
     
     # Format header
     bold(part = "header") %>% 
     bg(bg = "#43358B", part = "header") %>% 
     color(color = "white", part = "header") %>% 
     fontsize(size = 11, part = "header") %>% 
   
     # Alignment
     align(align = "left", part = "header") %>%
     valign(valign = "center", part = "header") %>%
     valign(valign = "top", part = "body") %>%
   
     # Bold Scotland row
     bold(i = rows) %>% 
     
     # Row height
     padding(padding.top = 2, padding.bottom = 2, part = "all") %>%
     height_all(height = 8) %>%
   
     # Borders
     border(border = fp_border_default(color = "#000000", width = 0.5),
            part = "all")

  
}