format_dynamic_table <- function(data, 
                                 type = c("pathway", "outcome", "trend", "completeness"),
                                 measure = "none",
                                 dropdown_grp = "group",
                                 height = 660){
  
  # Set table dimensions
  height <- paste0(height, "px")
  
  if (type == "completeness" | measure == "uptake"){
    margin <- "15px"
    width <- "97%"
  } else {
    margin <- "20px"
    width <- "98%"
  }
  
  # Set dynamic data for all types except completeness which is static
  if (type == "completeness") {
    table_data <- data %>% 
      mutate(key = 1, .before = 1)
  } else {
    table_data <- SharedData$new(
      data,
      key = ~key, 
      group = dropdown_grp)
    data <- table_data$origData()
  }
  
  # If these columns exist, make them bold
  bold_cols <- intersect(c(
    "Integration Authority Area", 
    "Health Board", 
    "Integration Authority Area/Health Board", 
    "Age Group",
    "Gender",
    "Deprivation Quintile",
    " "), 
    names(data))
  
  # Any columns that are not bold should be right-aligned
  right_align <- as.list(setdiff(names(data), c(bold_cols, "geog")))
  
  # Set scroll parameters
  scrollX <- ifelse(type == "pathway", FALSE, TRUE)
  scrollY <- ifelse(type == "trend", FALSE, TRUE)
  
  # Set placement of vertical lines (for not met/ongoing/no contact tables)
  v_lines <- 5
  
  # Set headers for completeness table
  if (type == "completeness"){
    n_groups <- length(str_replace(names(select(data, matches("referrals_"))), "referrals_", ""))
    col_names <- c("Number of referrals", "% Unknown")
    if (length(names(select(data, matches("ytbd")))) > 0) {
      col_names <- c(col_names, "% YTBD")
    }
    
    table_headers <- htmltools::withTags(
      table(class = "display",thead(tr(
        th(rowspan = 2, ""),
        th(rowspan = 2, "Integration Authority Area/Health Board"),
        lapply(all_years, function(year) {th(colspan = length(col_names), year, style = css(textAlign = "center"))}),
        th(colspan = length(col_names), "All", style = css(textAlign = "center"))),
        tr(lapply(rep(col_names, n_groups), th)))))
    
    # Overwrite placement of vertical lines
    v_lines = seq(from = 2, by = length(col_names), length.out = n_groups)
  }
  
  # Set headers for outcomes tables
  if (measure %in% c("not_met", "ongoing")) {
    breakdown_header <- switch(
      measure,
      not_met = "% of Standard Not Met",
      ongoing = "% of PDS Ongoing"
    )
    
    first_headers <- names(data)[1:5]
    second_headers <- names(data)[6:ncol(data)]
    breakdown_cols <- ncol(data) - 5
    
    table_headers <- htmltools::withTags(table(
      class = "display",
      thead(tr(
        lapply(first_headers, function(x) {th(rowspan = 2, x)}),
        th(colspan = breakdown_cols, breakdown_header, style = css(textAlign = "center"))),
        tr(lapply(second_headers, th)))))
  }
  
  # List arguments for DT::datatable
  datatable_args <- list(
    data = table_data,
    rownames = FALSE, 
    extensions = "FixedColumns",
    options = list(
      pageLength = nrow(data),
      scrollX = scrollX,
      fixedColumns = list(leftColumns = 1),
      scrollY = TRUE,
      dom = "t",
      ordering = FALSE,
      columnDefs = list(
        list(visible = FALSE, targets = c("key")),
        list(className = 'dt-right', targets = right_align)
      )
    )
  )
  
  # Headers
  if (measure %in% c("not_met", "ongoing") | type == "completeness"){
    datatable_args$container <- table_headers
  }
  
  # Captions
  if (measure %in% c("wait", "wait_breakdown")){
    datatable_args$caption <- htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: left; color:black;',
      "A dash (-) indicates that due to data quality issues, or lack of data, this measure could not be calculated.")
  }
  
  if (measure == "not_met"){
    datatable_args$caption <- htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: left; color:black;',
      "PDS terminated without contact refers to those with no contact date recorded and a termination date recorded at any time after diagnosis, therefore this may include referrals with a termination date more than 12 months after diagnosis.")
  }
  
  if (measure == "no_contact"){
    datatable_args$caption <- htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: left; color:black;',
      "Referrals not contacted within 12 months of diagnosis includes records with no contact date and those with a contact date more than 12 months after diagnosis, but does not include those with a termination date.", 
      htmltools::tags$br(), 
      "Number of referrals whose PDS ended with no contact refers to those with no contact date recorded and a termination date recorded at any time after diagnosis, therefore this may include referrals with a termination date more than 12 months after diagnosis.")
  }
  
  # Create datatable
  dt <- do.call(DT::datatable, datatable_args) %>%
    DT::formatStyle(columns = bold_cols, fontWeight = "bold")
  
  # Add vertical lines
  if (measure %in% c("not_met", "ongoing", "no_contact") | type == "completeness"){
    dt <- dt %>%
      DT::formatStyle(v_lines, `border-right` = "solid 1px")
  }
  
  # Add background
  if (measure == "wait_breakdown"){
    dt <- dt %>%
      DT::formatStyle(ncol(data) - 1, fontWeight = "bold", backgroundColor = "#E1C7DF")
  } 
  
  # Make Scotland row bold
  if (type == "completeness"){
    dt <- dt %>%
      DT::formatStyle("geog", target = "row", fontWeight = DT::styleEqual("Scotland", "bold"))
  }
  
  bscols(widths = 12, div(style = css(width="97%", height=height, margin=margin), dt))
  
}
