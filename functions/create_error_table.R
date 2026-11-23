create_error_table <- function(data){
  measure <- data %>%
    select(starts_with("total")) %>%
    names()
  
  if (measure != "total_q_errors"){
    years <- "All (2021/22 onward)"
  } else {
    years <-  "All (2016/17 - 2020/21)"
    data <- data %>% filter(fy %in% c("2016/17", "2017/18", "2018/19", "2019/20", "2020/21"))
  }
  
  data %>% 
    group_by(fy, health_board) %>% 
    summarise(
      rate = round_half_up(sum(.data[[measure]]) / sum(records) * 100, 1),
      .groups = "drop") %>%
    bind_rows(
      data %>% 
        group_by(fy = years, health_board) %>% 
        summarise(
          rate = round_half_up(sum(.data[[measure]]) / sum(records) * 100, 1),
          .groups = "drop")) %>%
    mutate(rate = paste0(rate, "%")) %>%
    pivot_wider(
      names_from = fy,
      values_from = rate,
      values_fill = list(rate = "\\-")) %>%
    rename(`Health Board` = health_board)
}
    