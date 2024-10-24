####################### Page 7: Data Quality #######################

output$page_7_ui <-  renderUI({

  div(
fluidRow(
      box("This page includes information about data quality. Any queries/errors outstanding following the resubmission deadline will be included in analytical outputs such as these management reports and annual publication. Therefore, the better quality of data submitted, the more accurate these figures will be.",
          background = "blue",
          width = 12
      )),
linebreaks(1),
    fluidRow(column(
      
  radioGroupButtons("quality_tab", label = NULL, choices = quality_list,
    status = "primary",
    direction = "horizontal", 
    justified = T,
    size = "lg"), width = 12)),

    conditionalPanel(
      condition= 'input.quality_tab == "errors"',
      # inputs
      
      
      fluidRow(column(
        p("The table below shows the percentage of records which contain one or more queries/errors. Records with a missing diagnosis date or diagnosis date outwith the reporting period are not included in the below counts. Duplicate records are included."),
        tableOutput("error_table"),
        
        width = 12))
      
       
      ), #cond panel 1
    
      
      conditionalPanel(
        condition= 'input.quality_tab == "records"',
        # inputs
        
        
        fluidRow(column(
          p("The table below shows the number of records submitted by each Health Board. Records with a missing diagnosis date or diagnosis date outwith the reporting period are not included in the below counts. Duplicate records are included."),
          tableOutput("records_table"),
          
        width = 12)
        )
        
        ), #cond panel 2
    
 
  ) # div
}) # renderUI


output$error_table <- renderUI({HTML(
  err %>% 
  filter(fy %in% included_years) %>% 
  group_by(fy, health_board) %>% 
  summarise(err_rate = round_half_up(sum(total_errors) / sum(records) * 100, 1),
            .groups = "drop") %>%
  bind_rows(
    err %>% 
      group_by(fy = "All", health_board) %>% 
      summarise(err_rate = round_half_up(sum(total_errors) / sum(records) * 100, 1),
                .groups = "drop")) %>%
  mutate(err_rate = paste0(err_rate, "%")) %>%
  pivot_wider(names_from = fy,
              values_from = err_rate,
              values_fill = list(err_rate = "\\-")) %>%
  rename(`Health Board` = health_board) %>%
  kable() %>%
  kable_styling(full_width = TRUE) %>%
  row_spec(15, bold = TRUE) %>%
  # Add header above table 
  add_header_above(
    c(" " = 1,
      "Financial Year of Diagnosis" = length(included_years), 
      " " = 1)) %>%
  add_footnote(label = paste("A dash (\\-) indicates no records were submitted",
                             "for diagnoses in this year."),
               notation = "none"))
})


output$records_table <- renderUI({HTML(
err %>%
  filter(fy %in% included_years) %>% 
  filter(str_sub(fy, 1, 4) >= year(start_date)) %>%
  group_by(fy, health_board, ijb) %>% 
  summarise(records = sum(records),
            .groups = "drop") %>%
  bind_rows(
    err %>%
      group_by(fy = "All", health_board, ijb) %>% 
      summarise(records = sum(records),
                .groups = "drop")) %>%
  mutate(records = format(records, big.mark = ",")) %>%
  pivot_wider(names_from = fy, 
              values_from = records,
              values_fill = list(records = "0")) %>%
  arrange(health_board, ijb) %>%
  rename(`Health Board` = health_board,
         `Integration Joint Board (IJB)` = ijb) %>%
  kable(align = c("l", "l", rep("r", length(unique(err$fy)) + 1))) %>%
  kable_styling(full_width = TRUE) %>%
  row_spec(length(unique(paste(err$health_board, err$ijb))), bold = TRUE) %>%
  collapse_rows(columns = 1, valign = "top") %>%
  scroll_box(height = "740px"))
})