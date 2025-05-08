
ldp_table_dt <- function(data, 
                      scotland = FALSE,
                      ijb = FALSE,
                     # hb = NULL,
                      year,
                      quarter,
                      include_pc = TRUE){
  
  

  
  if(scotland == TRUE){
    data %<>%
      mutate(ijb = "All") %>%
      bind_rows(data %>% 
                  mutate(health_board = "Scotland",
                         ijb          = "All")) %>%
      group_by(health_board, ijb, fy, ldp) %>%
      summarise(referrals = sum(referrals), .groups = "drop")
  }
  
  # 12 Months Complete - IJB level for table
  num_ijb <- data %>% filter(ldp %in% c("complete", "exempt")) %>% group_by(ijb) %>% summarise(num = sum(referrals), .groups = "drop") %>%
    bind_rows(data %>% filter(ldp %in% c("complete", "exempt")) %>% group_by(ijb = health_board) %>% summarise(num = sum(referrals), .groups = "drop"))
  den_ijb <- data %>% filter(ldp %in% c("complete", "exempt", "fail")) %>% group_by(ijb) %>% summarise(den = sum(referrals), .groups = "drop") %>%
    bind_rows(data %>% filter(ldp %in% c("complete", "exempt", "fail")) %>% group_by(ijb = health_board) %>% summarise(den = sum(referrals), .groups = "drop"))
  pds_rate_ijb <- 
    full_join(num_ijb, den_ijb, by = "ijb") %>%
    mutate(rate = (num / den) * 100) %>% 
    select(-num, -den) %>%
    mutate(rate = if_else(!is.na(rate),paste0(format(round_half_up(rate, 1)), "%"), "NA")) %>%
    mutate(ldp = "% Met Standard/Exempt") %>%
    pivot_wider(names_from = ijb, values_from = rate)
  
  table <-
    
    data %>%
    group_by(ijb = health_board, ldp) %>%
    summarise(referrals = sum(referrals), .groups = "drop") %>%
    bind_rows(data %>%
                group_by(ijb = health_board, ldp = "total") %>%
                summarise(referrals = sum(referrals), .groups = "drop"),
              data %>%
                filter(ldp != "Aberdeen") %>% # excludes Aberdeen City 2019/20 and 2020/21 from columns/rows regarding ldp standard
                group_by(ijb, ldp) %>%
                summarise(referrals = sum(referrals), .groups = "drop"),
              data %>%
                group_by(ijb, ldp = "total") %>%
                summarise(referrals = sum(referrals), .groups = "drop")) %>%
    complete(ijb, ldp = c("complete", "fail", "exempt", "ongoing", "total"),
             fill = list(referrals = 0)) %>%
    mutate(ldp = 
             case_when(
               ldp == "complete" ~ "Standard Met",
               ldp == "fail" ~ "Standard Not Met",
               ldp == "ongoing" ~ "PDS Ongoing",
               ldp == "total" ~ "Total Referrals",
               TRUE ~ str_to_title(ldp)
             )) %>%
    mutate(referrals = replace_na(referrals, 0),
           referrals = format(referrals, big.mark = ",")) %>%
    pivot_wider(names_from = ijb, values_from = referrals) %>%
    bind_rows(if(include_pc == TRUE)pds_rate_ijb) %>%
    select(ldp, c(data$health_board, sort(unique(data$ijb)))) %>%
    arrange(match(ldp, c("Total Referrals", "Standard Met", "Standard Not Met", "PDS Ongoing", "Exempt"))) %>% 
    filter(ldp != "Aberdeen") # excludes Aberdeen City 2019/20 and 2020/21 totals column from appearing
  
  if(scotland == TRUE){
    
    table %>%
      select(-All) %>%
      
      pivot_longer(cols = c("Scotland", starts_with("NHS")),
                   names_to = "board",
                   values_to = "value") %>%
      pivot_wider(names_from = ldp,
                  values_from = value) %>%
      rename(" " = "board") %>% 
      
      DT::datatable(rownames = FALSE, extensions = 'Buttons',
                    options = list(
                      buttons =
                        list(list(
                          extend = 'csv',
                          title = paste0("pds_ldp_standard_hb_", year, if (include_pc == FALSE) {paste0("_uptoQ",quarter)}),
                          text = "Download table as CSV"
                    )),
                    pageLength = 15,
                                   scrollX = TRUE,
                                   # fixedColumns = list(leftColumns = 1),
                                   scrollY = FALSE,
                                   dom = "tB",
                                   ordering = FALSE,
                                   columnDefs = list(list(className = 'dt-right', targets = 1:if_else(include_pc, 6, 5))
                                   ))) %>%
      DT::formatStyle(1, fontWeight = "bold") %>% 
      DT::formatStyle(if (include_pc == TRUE) {7}, fontWeight = "bold")
      
      # kable(col.names = c("", names(.)[-1]),
      #       align = c("l", rep("r", length(names(.)) - 1))
      # ) %>%
      # kable_styling(full_width = FALSE) %>%
      # column_spec(1, bold = T) %>%
      # column_spec(if_else(include_pc, 7, 6), bold = include_pc)
    
  }else if(ijb == TRUE){
    
    table %>%
      select(!starts_with("NHS")) %>% 
      pivot_longer(cols = !starts_with("ldp"),
                   names_to = "ijb",
                   values_to = "value") %>%
      pivot_wider(names_from = ldp,
                  values_from = value) %>%
      rename(" " = "ijb") %>% 
      
      DT::datatable(rownames = FALSE, extensions = 'Buttons',
                    options = list(
                      buttons =
                        list(list(
                          extend = 'csv',
                          title = paste0("pds_ldp_standard_iaa_", year, if (include_pc == FALSE) {paste0("_uptoQ",quarter)}),
                          text = "Download table as CSV"
                        )),
                      pageLength = 33,
                      scrollX = TRUE,
                      # fixedColumns = list(leftColumns = 1),
                      scrollY = FALSE,
                      dom = "tB",
                      ordering = FALSE,
                      columnDefs = list(list(className = 'dt-right', targets = 1:if_else(include_pc, 6, 5))
                      ))) %>%
      DT::formatStyle(1, fontWeight = "bold") %>% 
      DT::formatStyle(if (include_pc == TRUE) {7}, fontWeight = "bold")
      
      # kable(col.names = c("", names(.)[-1]),
      #       align = c("l", rep("r", length(names(.)) - 1))
      # ) %>%
      # kable_styling(full_width = FALSE) %>%
      # column_spec(1, bold = T) %>%
      # column_spec(if_else(include_pc, 7, 6), bold = include_pc)
  } else {
    # 
    # headers_ijb <- htmltools::withTags(table(
    #   class = 'display',
    #   thead(
    #     tr(
    #       th(colspan = 1, '    '),
    #       th(rowspan = 2, "I "),
    #       th(colspan = 2, '    2016/17'),
    #       th(colspan = 2, '    2017/18'),
    #       th(colspan = 2, '    2018/19'),
    #       th(colspan = 2, '    2019/20'),
    #       th(colspan = 2, '    2020/21'),
    #       th(colspan = 2, '    2021/22'),
    #       th(colspan = 2, '    2022/23'),
    #       th(colspan = 2, '    2023/24'),
    #       th(colspan = 2, '    2024/25'),
    #       th(colspan = 2, '    All')
       # ),
       # tr(
         # lapply(c(" ",rep(c("Number of referrals", "% Unknown"), 10)), th)
    #     )
    #   )
    # ))
    # 
    # 
    # table %>%
    #   rename(" " = "ldp") %>% 
    #   
    #   DT::datatable(container = headers_ijb, rownames = FALSE, extensions = 'Buttons',
    #                 options = list(
    #                   buttons =
    #                     list(list(
    #                       extend = 'csv',
    #                       title = paste0("pds_ldp_standard_", gsub(" ", "_", hb),"_", year, if (include_pc == FALSE) {paste0("_uptoQ",quarter)}),
    #                       text = "Download table as CSV"
    #                     )),
    #                   pageLength = 6,
    #                   scrollX = TRUE,
    #                   # fixedColumns = list(leftColumns = 1),
    #                   scrollY = FALSE,
    #                   dom = "tB",
    #                   ordering = FALSE,
    #                   columnDefs = list(list(className = 'dt-right', targets = 1:(length(unique(data$ijb))+1))
    #                   ))) %>%
    #   DT::formatStyle(1, fontWeight = "bold") %>% 
    #   DT::formatStyle(0, if (include_pc == TRUE) {target = "row"},
    #                   fontWeight = DT::styleEqual(5,"bold"))
 
      kable(col.names = c("", unique(data$health_board), sort(unique(data$ijb))),
            align = c("l", "r", rep("r", length(unique(data$ijb))))) %>%
      kable_styling(full_width = FALSE) %>%
      column_spec(1, bold = T) %>%
      add_header_above(
        c(" " = 1,
          "Health Board" = 1,
          "Integration Authority Area" = length(unique(data$ijb)))) %>%
      row_spec(if_else(include_pc, 6, 5), bold = include_pc)
    
  }
}
