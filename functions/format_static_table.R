format_static_table <- function(data, type = c("trend", "hb", "scot", "submission", "error"), is_complete_year = TRUE){
  
  full_width <- ifelse(type %in% c("trend", "submission"), TRUE, FALSE)
  bold_key <- ifelse(type %in% c("trend", "hb", "scot"), TRUE, FALSE)
  bold_row <- ifelse(
    (type %in% c("submission", "error")) | (type == "hb" && is_complete_year == TRUE),
    TRUE, FALSE)
  
  table <- data %>%
    # Align data columns right
    kable(align = c("l", rep("r", ncol(data) - 1))) %>%
    # Width control
    kable_styling(full_width = full_width) %>%
    # Make key column bold
    column_spec(1, bold = bold_key) %>%
    # Make Scotland row bold
    row_spec(nrow(data), bold = bold_row)
  
  if (type == "trend"){
    table %>%
      # Make striped
      kable_styling(c("striped", "hover")) %>%
      # Add scroll box
      scroll_box(height = "685px") %>% 
      # Add header
      add_header_above(c(
        " " = 1, 
        "Financial Year of Diagnosis" = ncol(data) - 1))
    
  } else if (type == "hb"){
    table %>%
      # Add header
      add_header_above(c(
        " " = 1,
        "Health Board" = 1,
        "Integration Authority Area" = ncol(data) - 2))
    
  } else if (type == "scot"){
    table %>%
      # Make % Met column bold
      column_spec(which(names(data) == "% Met Standard/Exempt"), bold = TRUE)
    
  } else if (type == "submission"){
    table %>% 
      # Make health board column span its IAAs
      collapse_rows(columns = 1, valign = "top") %>%
      # Add scroll box
      scroll_box(height = "580px")
    
  } else if (type == "error"){
    table %>% 
      # Add headers
      add_header_above(c(
        " " = 1,
        "Financial Year of Diagnosis" = ncol(data) - 2,
        " " = 1)) %>%
      # Add footnotes
      add_footnote(
        label = paste("A dash (\\-) indicates no records were submitted for diagnoses in this year."),
        notation = "none") 
    
  } else {
    table
  }
  
}
