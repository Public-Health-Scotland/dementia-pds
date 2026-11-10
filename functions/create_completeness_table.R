create_completeness_table <- function(data, measure){
  unknown_col <- paste0(measure, "_unknown")
  ytbd_col <- paste0(measure, "_ytbd")
  include_ytbd <- ytbd_col %in% names(data)
  
  comp_data <- data %>%
    bind_rows(
      data %>% 
        group_by(geog) %>% 
        summarise(
          referrals = sum(referrals, na.rm = TRUE),
          !!unknown_col := sum(.data[[unknown_col]], na.rm = TRUE),
          !!!if (include_ytbd) {
            rlang::exprs(!!ytbd_col := sum(.data[[ytbd_col]], na.rm = TRUE))
          } else {
            rlang::exprs()
          },
          .groups = "drop") %>%
        mutate(fy = "All")) %>%
    mutate(name = factor(name, levels = c("ijb", "health_board", "scot"))) %>%
    arrange(name, geog) %>%
    mutate(
      perc_unknown = if_else(referrals > 0, round(.data[[unknown_col]] / referrals * 100, 1), NA_real_),
      !!!if (include_ytbd) {
        rlang::exprs(perc_ytbd = if_else(referrals > 0, round(.data[[ytbd_col]] / referrals * 100, 1), NA_real_))
      } else {
        rlang::exprs()
      }) %>%
    select(geog, fy, referrals, perc_unknown, any_of("perc_ytbd")) %>%
    pivot_wider(
      names_from = fy, 
      values_from = any_of(c("referrals", "perc_unknown", "perc_ytbd")), 
      names_vary = "slowest") %>%
    mutate(
      across(starts_with("referrals"), ~ if_else(is.na(.x), "0", as.character(.x))),
      across(starts_with("perc"), ~ if_else(is.na(.x), "-", paste0(.x, "%")))
    )
  
  n_groups <- length(unique(data$fy)) + 1
  
  col_names <- c("Number of referrals", "% Unknown")
  if (include_ytbd) {
    col_names <- c(col_names, "% YTBD")
  }
  
  headers <- htmltools::withTags(
    table(class = "display",thead(tr(
      th(rowspan = 2, ""),
      th(rowspan = 2, "Integration Authority Area/Health Board"),
      lapply(all_years, function(year) {th(colspan = length(col_names), year, style = css(textAlign = "center"))}),
      th(colspan = length(col_names), "All", style = css(textAlign = "center"))),
      tr(lapply(rep(col_names, n_groups), th)))))
  
  bscols(widths = 12, div(
    style = css(width = "98%",height = "550px",margin = "15px"),
    DT::datatable(
      comp_data,
      container = headers,
      rownames = TRUE,
      extensions = "FixedColumns",
      options = list(
        pageLength = nrow(data),
        scrollX = TRUE,
        scrollY = TRUE,
        fixedColumns = list(leftColumns = 1),
        dom = "t",
        ordering = FALSE,
        columnDefs = list(
          list(
            visible = FALSE,
            targets = 0),
          list(
            className = "dt-right",
            targets = 2:(length(col_names) * n_groups + 1))))) %>%
      DT::formatStyle("geog", target = "row", fontWeight = DT::styleEqual("Scotland", "bold")) %>%
      DT::formatStyle(seq(from = length(col_names) + 1, by = length(col_names), length.out = n_groups), `border-right` = "solid 1px")))
}
