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




#data tables       

output$table_hb_trend <- DT::renderDataTable({
  trend_hb_data <- annual_table_data %>% 
    select(health_board, fy, rate) %>%
    distinct(health_board, fy, .keep_all = T) %>% 
    pivot_wider(names_from = fy, values_from = rate) %>% 
    slice(15,1:14) %>% 
    rename(" " = "health_board") 
  make_table(trend_hb_data, right_align = 1:8)
  })

output$table_ijb_trend <- DT::renderDataTable({
  trend_ijb_data <- annual_table_data %>% 
    filter(ijb != "Scotland") %>% filter(!grepl("NHS", ijb)) %>% 
    select(ijb, fy, rate) %>%
    distinct(ijb, fy, .keep_all = T) %>% 
    pivot_wider(names_from = fy, values_from = rate) %>% 
    rename(" " = "ijb") 
  make_table(trend_ijb_data, right_align = 1:8)
})


 
#  plot
output$chart_title_trend <- renderUI({HTML(paste("Percentage of people referred for dementia PDS who received a minimum of one year’s support; Scotland and ",
                                                 input$select_hb_ijb_trend))
})

trend_chart_data <- reactive({
  annual_table_data %>%
    filter(ijb == input$select_hb_ijb_trend | ijb == "Scotland")})


output$trend_plot <- renderPlotly({
  plot_trend(trend_chart_data()
             )
})


