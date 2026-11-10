create_records_summary_table <- function(data){
  data %>%
    filter(str_sub(fy, 1, 4) >= year(start_date)) %>%
    group_by(fy, health_board, ijb) %>% 
    summarise(
      records = sum(records),
      .groups = "drop") %>%
    bind_rows(
      data %>%
        group_by(fy = "All", health_board, ijb) %>% 
        summarise(records = sum(records), .groups = "drop")) %>%
    mutate(records = format(records, big.mark = ",")) %>%
    pivot_wider(
      names_from = fy, 
      values_from = records,
      values_fill = list(records = "0")) %>%
    arrange(health_board, ijb) %>%
    rename(
      `Health Board` = health_board,
      `Integration Authority Area` = ijb) %>%
    kable(align = c("l", "l", rep("r", length(unique(data$fy)) + 1))) %>%
    kable_styling(full_width = TRUE) %>%
    row_spec(length(unique(paste(data$health_board, data$ijb))), bold = TRUE) %>%
    collapse_rows(columns = 1, valign = "top") %>%
    scroll_box(height = "580px")
}
