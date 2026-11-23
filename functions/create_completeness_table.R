create_completeness_table <- function(data, measure){
  
  unknown_col <- paste0(measure, "_unknown")
  ytbd_col <- paste0(measure, "_ytbd")
  include_ytbd <- ytbd_col %in% names(data)
  
  data %>%
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
    format_numbers()
  }
