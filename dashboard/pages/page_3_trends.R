####################### Page 3: TRENDS #######################
#UI ----
output$page_3_ui <-  renderUI({

  div(
    # 1 referrals----
    conditionalPanel(condition= 'input.trend_tab == "referrals_trend"',
            fluidRow(
                column(
                  h3(strong(htmlOutput("table_referrals_trend_title"))),
                  radioButtons("select_referrals_trend_table",
                       label = "In the table below show Scotland and: ",
                       choices = c("Health Boards", "Integration Authority Areas"),
                       selected = "Health Boards",
                       inline = TRUE),
                  DT::dataTableOutput("table_referrals_trend"),
  linebreaks(1),
                  h3(strong(htmlOutput("chart_title_referrals_trend"))),
            fluidRow(
                column(
                  selectInput("select_referrals_trend_plot",
                       label = "Select Health Board/Integration Authority Area to show in chart:",
                       choices = c("Scotland", boards, ijb_list), width = "100%"), width = 4)),
                  plotlyOutput("referrals_trend_plot"),
  linebreaks(1),
              width = 12,
              style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available")
    ) # fluidRow
  ), # cond panel 1 (referrals)
    
#2 percentage met standard----
    conditionalPanel(condition= 'input.trend_tab == "pds_perc_trend"',
            fluidRow(
                column(
                   h3(strong(htmlOutput("table_pds_trend_title"))),
                   radioButtons("select_pds_trend_table",
                        label = "In the table below show Scotland and: ",
                        choices = c("Health Boards", "Integration Authority Areas"),
                        selected = "Health Boards",
                        inline = TRUE),
                   DT::dataTableOutput("table_hb_ijb_trend"),
  linebreaks(1),
                   h3(strong(htmlOutput("chart_title_trend"))),
            fluidRow(
                column(
                    selectInput("select_hb_ijb_trend",
                        label = "Select Health Board/Integration Authority Area to show in chart:",
                        choices = c(boards, ijb_list), width = "100%"), width = 4)),
                    plotlyOutput("trend_plot"),
  linebreaks(1),
            width = 12,
            style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available"),
      ) # fluidRow
  ), # cond panel 2 (percent met)
    
 #3 percentage of estimated diagnoses----
       conditionalPanel(condition= 'input.trend_tab == "exp_perc_trend"',
            fluidRow(
                column(
                  h3(strong("Percentage of people estimated to be newly diagnosed with dementia who were referred for post-diagnostic support; Scotland and Health Boards")),
                  DT::dataTableOutput("table_hb_trend_2"),
  linebreaks(1),
                  h3(strong(htmlOutput("chart_title_trend_2"))),
            fluidRow(
                column(
                  selectInput("select_hb_ijb_trend_2",
                      label = "Select Health Board to show in chart:",
                      choices = boards, width = "100%"), width = 4)),
                  plotlyOutput("trend_plot_2"),
  linebreaks(1),
            width = 12,
            style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available"),
         ) # fluidRow
    ) # cond panel 3 (percent of estimated diagnoses)
 	     ) # div
 }) # renderUI
   

# SERVER----

#1a data table for referrals ----    

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
    make_table(trend_referrals_hb_data, right_align = 1:length(included_years), selected = 1, table_elements = "t") %>% 
      formatCurrency(c(2:(length(included_years) + 1)), currency = "", interval = 3, mark = ",", digits = 0)
    
  }else{
    
    trend_referrals_ijb_data <- annual_table_data %>% 
      filter(fy %in% included_years, ldp == "total") %>% 
      filter(!grepl("NHS", ijb)) %>% 
      select(ijb, fy, referrals) %>%
      distinct(ijb, fy, .keep_all = T) %>% 
      pivot_wider(names_from = fy, values_from = referrals) %>% 
      rename(" " = "ijb") 
    make_table(trend_referrals_ijb_data, right_align = 1:length(included_years), selected = 1, rows_to_display = 32, table_elements = "t") %>% 
      formatCurrency(c(2:(length(included_years) + 1)), currency = "", interval = 3, mark = ",", digits = 0)
  }
})



#1b  plot for referrals ----

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

#2a data table for percentage met standard----    

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
  make_table(trend_hb_data, right_align = 1:length(included_years), selected = 1, table_elements = "t")

}else{

  trend_ijb_data <- annual_table_data %>% 
    filter(fy %in% included_years) %>% 
    filter(!grepl("NHS", ijb)) %>% 
    select(ijb, fy, rate) %>%
    mutate(rate = if_else(is.na(rate), "-", paste0(rate, "%"))) %>%
    distinct(ijb, fy, .keep_all = T) %>% 
    pivot_wider(names_from = fy, values_from = rate) %>% 
    rename(" " = "ijb") 
  make_table(trend_ijb_data, right_align = 1:length(included_years), selected = 1, rows_to_display = 32, table_elements = "t")
}
})


 
#2b  plot for percentage met standard ----

output$chart_title_trend <- renderUI({HTML(paste("Percentage of people referred for dementia PDS who received a minimum of one year’s support; Scotland and ",
                                                 input$select_hb_ijb_trend))
})

trend_chart_data <- reactive({
  annual_table_data %>%
    filter(fy %in% included_years, ldp == "total") %>% 
    filter(ijb == input$select_hb_ijb_trend | ijb == "Scotland")})


output$trend_plot <- renderPlotly({
  plot_trend(trend_chart_data(), rate)
})

#3a data table for percentage of estimated diagnoses ----     

output$table_hb_trend_2 <- DT::renderDataTable({
  trend_hb_data_2 <- annual_table_data %>% 
    filter(fy %in% included_years, ldp == "total") %>% 
    select(health_board, fy, exp_perc) %>%
    mutate(exp_perc = paste0(exp_perc, "%")) %>% 
    distinct(health_board, fy, .keep_all = T) %>% 
    pivot_wider(names_from = fy, values_from = exp_perc) %>% 
  #  slice(15,1:14) %>% 
    rename(" " = "health_board")  
  make_table(trend_hb_data_2, right_align = 1:length(included_years), selected = 1, table_elements = "t")
})


#3b plot for percentage of estimated diagnoses----

output$chart_title_trend_2 <- renderUI({HTML(paste("Percentage of people estimated to be newly diagnosed with dementia who were referred for post-diagnostic support; Scotland and ",
                                                 input$select_hb_ijb_trend_2))
})

trend_chart_data_2 <- reactive({
  annual_table_data %>%
    filter(fy %in% included_years, ldp == "total") %>% 
    filter(ijb == input$select_hb_ijb_trend_2 | ijb == "Scotland")
  })

output$trend_plot_2 <- renderPlotly({
  plot_trend(trend_chart_data_2(), exp_perc)
})


