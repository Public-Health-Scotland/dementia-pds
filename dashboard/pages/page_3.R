####################### Page 3: TRENDS #######################
#UI ----
output$page_3_ui <-  renderUI({

  div(
    conditionalPanel(
      condition= 'input.trend_tab == "referrals_trend"',
      fluidRow(
        column(
          h3(htmlOutput("table_referrals_trend_title")), width = 12)),
      fluidRow(
        column(
          radioButtons("select_referrals_trend_table",
                       label = "In the table below show Scotland and: ",
                       choices = c("Health Boards", "Integration Joint Boards"),
                       selected = "Health Boards",
                       inline = TRUE
          ), width = 12)),
      DT::dataTableOutput("table_referrals_trend"),
      linebreaks(1),
      h3(htmlOutput("chart_title_referrals_trend")),
      fluidRow(
        column(selectInput("select_referrals_trend_plot",
                           label = "Select Health Board/IJB to show in chart:",
                           choices = c("Scotland", boards, ijb_list), width = "100%"), width = 4)),
      
      plotlyOutput("referrals_trend_plot")
    ), # cond panel 1
    conditionalPanel(
      condition= 'input.trend_tab == "pds_perc_trend"',
      fluidRow(
        column(
          h3(htmlOutput("table_pds_trend_title")), width = 12)),
      fluidRow(
        column(
          radioButtons("select_pds_trend_table",
                       label = "In the table below show Scotland and: ",
                       choices = c("Health Boards", "Integration Joint Boards"),
                       selected = "Health Boards",
                       inline = TRUE
          ), width = 12)),
      DT::dataTableOutput("table_hb_ijb_trend"),
      linebreaks(1),
      h3(htmlOutput("chart_title_trend")),
      fluidRow(
        column(selectInput("select_hb_ijb_trend",
                           label = "Select Health Board/IJB to show in chart:",
                           choices = c(boards, ijb_list), width = "100%"), width = 4)),
      
      plotlyOutput("trend_plot")
    ), # cond panel 2
    conditionalPanel(
      condition= 'input.trend_tab == "exp_perc_trend"',
      fluidRow(
        column(
          h3("Percentage of people estimated to be newly diagnosed with dementia who were referred for post-diagnostic support; Scotland and Health Boards"),
          DT::dataTableOutput("table_hb_trend_2"),
          linebreaks(1),
          h3(htmlOutput("chart_title_trend_2")), width = 12)),
      fluidRow(
        column(selectInput("select_hb_ijb_trend_2",
                           label = "Select Health Board to show in chart:",
                           choices = boards, width = "100%"), width = 4)),
      
      plotlyOutput("trend_plot_2")
    ) # cond panel 3
 	     ) # div
 }) # renderUI
   

# SERVER----

#data table for conditional panel 1 (referrals) ----    

output$table_referrals_trend_title <- renderUI({
  HTML(paste0("Number of individuals diagnosed with dementia and referred for post-diagnostic support; Scotland and ", input$select_referrals_trend_table))
})

output$table_referrals_trend <- DT::renderDataTable({
  
  if(input$select_referrals_trend_table == "Health Boards"){  
    
    trend_referrals_hb_data <- annual_table_data %>% 
      filter(fy %in% included_years, ldp == "total") %>% 
      select(health_board, fy, referrals) %>%
      distinct(health_board, fy, .keep_all = T) %>% 
      pivot_wider(names_from = fy, values_from = referrals) %>% 
      rename(" " = "health_board") 
    make_table(trend_referrals_hb_data, right_align = 1:8, selected = 1, table_elements = "t") %>% 
      formatCurrency(c(2:9), currency = "", interval = 3, mark = ",", digits = 0)
    
  }else{
    
    trend_referrals_ijb_data <- annual_table_data %>% 
      filter(fy %in% included_years, ldp == "total") %>% 
      filter(!grepl("NHS", ijb)) %>% 
      select(ijb, fy, referrals) %>%
      distinct(ijb, fy, .keep_all = T) %>% 
      pivot_wider(names_from = fy, values_from = referrals) %>% 
      rename(" " = "ijb") 
    make_table(trend_referrals_ijb_data, right_align = 1:8, selected = 1, rows_to_display = 32, table_elements = "t") %>% 
      formatCurrency(c(2:9), currency = "", interval = 3, mark = ",", digits = 0)
  }
})



#  plot for conditional panel 1 (referrals) ----

output$chart_title_referrals_trend <- renderUI({HTML(paste0("Number of individuals diagnosed with dementia and referred for post-diagnostic support; ",
                                                 input$select_referrals_trend_plot))
})

trend_referrals_chart_data <- reactive({
  annual_table_data %>%
    filter(fy %in% included_years, ldp == "total") %>% 
    filter(ijb == input$select_referrals_trend_plot)})


output$referrals_trend_plot <- renderPlotly({
  plot_trend_referrals(trend_referrals_chart_data(), referrals)
})

#data table for conditional panel 2 (percentage acheived standard) ----    

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
   # slice(15,1:14) %>% 
    rename(" " = "health_board") 
  make_table(trend_hb_data, right_align = 1:8, selected = 1, table_elements = "t")

}else{

  trend_ijb_data <- annual_table_data %>% 
    filter(fy %in% included_years) %>% 
    filter(!grepl("NHS", ijb)) %>% 
    select(ijb, fy, rate) %>%
    mutate(rate = paste0(rate, "%")) %>% 
    distinct(ijb, fy, .keep_all = T) %>% 
    pivot_wider(names_from = fy, values_from = rate) %>% 
    rename(" " = "ijb") 
  make_table(trend_ijb_data, right_align = 1:8, selected = 1, rows_to_display = 32, table_elements = "t")
}
})


 
#  plot for conditional panel 2 (percentage acheived standard) ----

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

#data table for conditional panel 3 (percentage of estimated diagnoses) ----     

output$table_hb_trend_2 <- DT::renderDataTable({
  trend_hb_data_2 <- annual_table_data %>% 
    filter(fy %in% included_years, ldp == "total") %>% 
    select(health_board, fy, exp_perc) %>%
    mutate(exp_perc = paste0(exp_perc, "%")) %>% 
    distinct(health_board, fy, .keep_all = T) %>% 
    pivot_wider(names_from = fy, values_from = exp_perc) %>% 
  #  slice(15,1:14) %>% 
    rename(" " = "health_board")  
  make_table(trend_hb_data_2, right_align = 1:8, selected = 1, table_elements = "t")
})


#  plot for conditional panel 3 (percentage of estimated diagnoses)----

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


