####################### Page 3: TREND #######################

#output$page_3_ui <-  renderUI({

  #div(
	     #fluidRow(
#             h3("Notes:"),
# 	           p(strong("Please note that both the Dementia Post Diagnostic Support service provision and data submission to PHS have been affected by the COVID-19 pandemic.")),
# 	           p(("LDP Standard performance figures are not provided until data is available for the full financial year.")),
         
	       
	         
	         
# 	       ) #fluidRow
# 	     ) # div
# }) # renderUI
#   


#data table for conditional panel 1 ----    

output$table_pds_trend_title <- renderUI({
  HTML(paste0("Percentage of people referred for dementia PDS who received a minimum of one year’s support; Scotland and ", input$select_pds_trend_table))
})

output$table_hb_ijb_trend <- DT::renderDataTable({
  
  if(input$select_pds_trend_table == "Health Boards"){  
  
  trend_hb_data <- annual_table_data %>% 
    filter(fy %in% included_years) %>% 
  select(health_board, fy, rate) %>%
    mutate(rate = paste0(rate, "%")) %>% 
    distinct(health_board, fy, .keep_all = T) %>% 
    pivot_wider(names_from = fy, values_from = rate) %>% 
    slice(15,1:14) %>% 
    rename(" " = "health_board") 
  make_table(trend_hb_data, right_align = 1:6, selected = 1, table_elements = "t")

}else{

  trend_ijb_data <- annual_table_data %>% 
    filter(fy %in% included_years) %>% 
    filter(!grepl("NHS", ijb)) %>% 
    select(ijb, fy, rate) %>%
    mutate(rate = paste0(rate, "%")) %>% 
    distinct(ijb, fy, .keep_all = T) %>% 
    pivot_wider(names_from = fy, values_from = rate) %>% 
    rename(" " = "ijb") 
  make_table(trend_ijb_data, right_align = 1:6, selected = 1, rows_to_display = 32, table_elements = "t")
}
})


 
#  plot for conditional panel 1 ----

output$chart_title_trend <- renderUI({HTML(paste("Percentage of people referred for dementia PDS who received a minimum of one year’s support; Scotland and ",
                                                 input$select_hb_ijb_trend))
})

trend_chart_data <- reactive({
  annual_table_data %>%
    filter(fy %in% included_years) %>% 
    filter(ijb == input$select_hb_ijb_trend | ijb == "Scotland")})


output$trend_plot <- renderPlotly({
  plot_trend(trend_chart_data(), rate)
})

#data table for conditional panel 2 ----     

output$table_hb_trend_2 <- DT::renderDataTable({
  trend_hb_data_2 <- annual_table_data %>% 
    filter(fy %in% included_years, ldp == "total") %>% 
    select(health_board, fy, exp_perc) %>%
    mutate(exp_perc = paste0(exp_perc, "%")) %>% 
    distinct(health_board, fy, .keep_all = T) %>% 
    pivot_wider(names_from = fy, values_from = exp_perc) %>% 
    slice(15,1:14) %>% 
    rename(" " = "health_board")  
  make_table(trend_hb_data_2, right_align = 1:6, selected = 1, table_elements = "t")
})


#  plot for conditional panel 2 ----

output$chart_title_trend_2 <- renderUI({HTML(paste("Percentage of people estimated to be newly diagnosed with dementia who were referred for post-diagnostic support; Scotland and ",
                                                 input$select_hb_ijb_trend_2))
})

trend_chart_data_2 <- reactive({
  annual_table_data %>%
    filter(fy %in% included_years) %>% 
    filter(ijb == input$select_hb_ijb_trend_2 | ijb == "Scotland")})

output$trend_plot_2 <- renderPlotly({
  plot_trend(trend_chart_data_2(), exp_perc)
})


