####################### Page 3: TREND #######################
#UI ----
output$page_3_ui <-  renderUI({

  div(
    conditionalPanel(
      condition= 'input.trend_tab == "pds_perc_trend"',
      fluidRow(
        column(
          h2(htmlOutput("table_pds_trend_title")), width = 12)),
      fluidRow(
        column(
          radioButtons("select_pds_trend_table",
                       label = "In the table below show Scotland and: ",
                       choices = c("Health Boards", "Integration Joint Boards"),
                       selected = "Health Boards",
                       inline = TRUE
          ), width = 3)),
      DT::dataTableOutput("table_hb_ijb_trend"),
      linebreaks(1),
      h2(htmlOutput("chart_title_trend")),
      fluidRow(
        column(selectInput("select_hb_ijb_trend",
                           label = "Select Health Board/IJB to show in chart:",
                           choices = c(boards, ijb_list), width = "100%"), width = 3)),
      
      plotlyOutput("trend_plot")
    ), # cond panel 1
    conditionalPanel(
      condition= 'input.trend_tab == "exp_perc_trend"',
      fluidRow(
        column(
          h2("Percentage of people estimated to be newly diagnosed with dementia who were referred for post-diagnostic support; Scotland and Health Boards"),
          DT::dataTableOutput("table_hb_trend_2"),
          linebreaks(1),
          h2(htmlOutput("chart_title_trend_2")), width = 12)),
      fluidRow(
        column(selectInput("select_hb_ijb_trend_2",
                           label = "Select Health Board to show in chart:",
                           choices = boards, width = "100%"), width = 3)),
      
      plotlyOutput("trend_plot_2")
    ) # cond panel 2
 	     ) # div
 }) # renderUI
   

# SERVER----
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


