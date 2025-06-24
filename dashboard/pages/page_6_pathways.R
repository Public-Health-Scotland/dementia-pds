####################### Page 6: PATHWAYS #######################
#UI ----
  output$pathways_ui <-  renderUI({
    
    div(
      fluidRow(
        column(
          # wait times table by geography
          h3(strong(htmlOutput("table_title_pathways"))),
          fluidRow(column(
            radioButtons("select_hb_ijb_pathways",
                         label = "In the table and chart below show Scotland and: ",
                         choices = c("Health Boards", "Integration Authority Areas"),
                         selected = "Health Boards",
                         inline = TRUE),
            width = 4)), #fluidRow
          DT::dataTableOutput("table_pathways"),
          linebreaks(1),
          plotlyOutput("plot_pathways"),
          width = 12,
          style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available")#column
      ) #fluidRow
    ) # div   
  }) # renderUI

# SERVER----

 #wait times table by geography ----    
 
output$table_title_pathways <- renderUI({HTML(paste0("Average (median) days from diagnosis to first contact by PDS practitioner: Financial Year ", 
                                                   input$select_year_pathways, ", Scotland and ", input$select_hb_ijb_pathways))
})

output$table_pathways <- DT::renderDataTable({
  
  if(input$select_hb_ijb_pathways == "Health Boards"){
    
    median_hb_table_data <- data_wait %>% 
      filter(sex == "All") %>% 
      filter(ijb == "All" | ijb == "Scotland", simd == "All") %>% 
      filter(fy == input$select_year_pathways) %>% 
      select(health_board, fy, total_referrals, perc_contacted, median_diagnosis_to_contact) %>% 
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>%
      mutate(across(starts_with("perc"), ~ paste0(.,"%"))) %>%
      mutate(median_diagnosis_to_contact = if_else(grepl("-",median_diagnosis_to_contact), "   -", median_diagnosis_to_contact)) %>% 
      mutate(median_diagnosis_to_contact = if_else(median_diagnosis_to_contact < "  0", "-", median_diagnosis_to_contact)) %>% 
      mutate(median_diagnosis_to_contact = if_else(health_board == "NHS Grampian" & fy %in% c("2019/20", "2020/21"), "  -", median_diagnosis_to_contact)) %>% 
      mutate(perc_contacted = if_else(health_board == "NHS Grampian" & fy %in% c("2019/20", "2020/21"), "  -", perc_contacted)) %>%
      select(-fy) %>% 
      mutate(median_diagnosis_to_contact = if_else(grepl(".0", median_diagnosis_to_contact), 
                                                   substr(median_diagnosis_to_contact, 1, nchar(median_diagnosis_to_contact) - 2),
                                                   median_diagnosis_to_contact)) %>% 
      rename(`Number of People Referred to PDS` = total_referrals, 
             `% of Referrals contacted by PDS practitioner` = perc_contacted,
             `Average (median) days from diagnosis to first contact` = median_diagnosis_to_contact) %>% 
      rename("Health Board" = "health_board")
    make_table(median_hb_table_data, right_align = 1:3, selected = 1, table_elements = "t")
    
    
  }else{
    
    median_ijb_table_data <- data_wait %>% 
      filter(sex == "All") %>% 
      filter(ijb != "All", simd == "All") %>% 
      filter(fy == input$select_year_pathways) %>% 
      select(ijb, fy, total_referrals, perc_contacted, median_diagnosis_to_contact) %>%
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>%
      mutate(across(starts_with("perc"), ~ paste0(.,"%"))) %>%
      mutate(median_diagnosis_to_contact = if_else(grepl("-",median_diagnosis_to_contact), "   -", median_diagnosis_to_contact)) %>% 
      mutate(ijb = if_else(ijb == "Scotland", "AAA", ijb)) %>%
      arrange(ijb) %>% 
      mutate(ijb = if_else(ijb == "AAA", "Scotland", ijb)) %>% 
      mutate(median_diagnosis_to_contact = if_else(ijb == "Aberdeen City" & fy %in% c("2019/20", "2020/21"), "   -", median_diagnosis_to_contact)) %>% 
      mutate(perc_contacted = if_else(ijb == "Aberdeen City" & fy %in% c("2019/20", "2020/21"), "   -", perc_contacted)) %>%
      select(-fy) %>% 
      mutate(median_diagnosis_to_contact = if_else(grepl(".0", median_diagnosis_to_contact), 
                                                   substr(median_diagnosis_to_contact, 1, nchar(median_diagnosis_to_contact) - 2),
                                                   median_diagnosis_to_contact)) %>% 
      rename(`Number of People Referred to PDS` = total_referrals, 
             `% of Referrals contacted by PDS practitioner` = perc_contacted,
             `Average (median) days from diagnosis to first contact` = median_diagnosis_to_contact) %>% 
      #pivot_longer(!c(ijb,fy), names_to = "measure", values_to = "n") %>% 
      # pivot_wider(names_from = ijb, values_from = n) %>% 
      rename("Integration Authority Area" = "ijb")
    make_table(median_ijb_table_data, right_align = 1:3, selected = 1, rows_to_display = 32, table_elements = "t")
    
  }
  
})


# #1b  plot for wait times ----

output$plot_pathways <- renderPlotly({
  
  if(input$select_hb_ijb_pathways == "Health Boards"){
    
    median_data_hb <- data_wait %>% 
  filter(sex == "All") %>% 
  filter(ijb == "All" | ijb == "Scotland", simd == "All") %>% 
  select(health_board, fy, median_diagnosis_to_contact) %>% 
  mutate(median_diagnosis_to_contact = if_else(is.na(median_diagnosis_to_contact)| median_diagnosis_to_contact < 0, 0, median_diagnosis_to_contact))

  wait_times_hb_chart_data <- left_join(median_data_hb,
                            median_data_hb %>% filter(health_board == "Scotland") %>% select(fy, median_diagnosis_to_contact) %>%  rename(scot_median_diagnosis_to_contact = median_diagnosis_to_contact)) %>% filter(health_board != "Scotland") %>% 
  mutate(median_diagnosis_to_contact = if_else(health_board == "NHS Grampian" & fy %in% c("2019/20", "2020/21"), 0, median_diagnosis_to_contact)) %>% 
  rename(geog = health_board)

plot_bar_median(wait_times_hb_chart_data %>% filter(fy == input$select_year_pathways))
  
}else{
  
  median_data_ijb <- data_wait %>% 
    filter(sex == "All") %>% 
    filter(ijb != "All", simd == "All") %>% 
    select(ijb, fy, median_diagnosis_to_contact) %>% 
    mutate(median_diagnosis_to_contact = if_else(is.na(median_diagnosis_to_contact)| median_diagnosis_to_contact < 0, 0, median_diagnosis_to_contact)) %>% 
    mutate(ijb = if_else(ijb == "Scotland", "AAA", ijb)) %>%
    arrange(ijb) %>% 
    mutate(ijb = if_else(ijb == "AAA", "Scotland", ijb)) %>% 
    mutate(median_diagnosis_to_contact = if_else(ijb == "Aberdeen City" & fy %in% c("2019/20", "2020/21"), 0, median_diagnosis_to_contact))
  
    wait_times_ijb_chart_data <- left_join(median_data_ijb,
                               median_data_ijb %>% filter(ijb == "Scotland") %>% select(fy, median_diagnosis_to_contact) %>%  rename(scot_median_diagnosis_to_contact = median_diagnosis_to_contact)) %>% filter(ijb != "Scotland") %>% 
    rename(geog = ijb)
  
plot_bar_median(wait_times_ijb_chart_data %>% filter(fy == input$select_year_pathways))}  
})


