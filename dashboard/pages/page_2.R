####################### Page 2: HEALTH BOARDS LDP#######################
# UI ----
output$page_2_ui <-  renderUI({
  
  div(
# percentage of expected diagnoses ----
    fluidRow(
      shinydashboard::valueBox(
        value = textOutput("hb_exp_perc"),
        subtitle = "of people estimated to be newly diagnosed with dementia were referred for post-diagnostic support.",
        width = 7,
        color = "blue"), #valueBox
      box(htmlOutput("hb_exp_text"),
          title = (p(strong("How is this figure calculated?"))),
          width = 5, background = "black", solidHeader = TRUE), #box background set to black in order to edit ccs styles
    ), #fluidRow
# percentage acheived ldp standard ----
    fluidRow(
      shinydashboard::valueBox(
        value = textOutput("hb_pds_perc"),
        subtitle = "of those referred for post-diagnostic support received a minimum of 12 months of support.",
        width = 7,
        color = "blue"), #valueBox
      box(htmlOutput("hb_pds_text"),
          title = (p(strong("How is this figure calculated?"))),
          width = 5, background = "black"), #box
         ), #fluidRow
    fluidRow(
      linebreaks(1),
      h3(strong(htmlOutput("table_title_p2"))),
      DT::dataTableOutput("table_hb_ijb"),
      linebreaks(1),
      h3(strong(htmlOutput("chart_title_p2"))),
      plotlyOutput("hb_ijb_plot"),
      linebreaks(1)
    )# fluidRow
   ) # div
}) # renderUI


#SERVER ----

# page 2 title ----

output$page_2_title <- renderUI({HTML(paste0("Dementia Post-Diagnostic Support; Local Delivery Plan (LDP) Standard - ", input$select_hb_p2))
})

  #value boxes data ----

  vb_data_hb<- reactive({annual_table_data %>% filter(health_board == input$select_hb_p2, ijb == input$select_hb_p2, fy == input$select_year_p2, ldp == "total")}) 

  # percentage of people estimated to be newly diagnosed with dementia were referred for post-diagnostic support  
  output$hb_exp_perc <- renderText({paste0(vb_data_hb()$exp_perc, "%")})
  
  output$hb_exp_text <- renderUI({
    HTML(paste("A total of", "<b>",  prettyNum(vb_data_hb()$referrals, big.mark = ","), "</b>", "referrals were made to post-diagnostic support. This is divided by",
               "<b>", prettyNum(vb_data_hb()$diagnoses, big.mark = ","), "</b>", "the estimated number of people newly diagnosed with dementia."))})
  
  # percentage of those referred for post-diagnostic support received a minimum of 12 months of support
  output$hb_pds_perc <- renderText({paste0(vb_data_hb()$rate, "%")})
  
  vb_2_data_hb <- reactive({annual_table_data %>% filter(health_board == input$select_hb_p2, ijb == input$select_hb_p2, fy == input$select_year_p2,
                                                     ldp != "fail") %>% select(-diagnoses, -exp_perc) %>% 
                                                  pivot_wider(values_from = referrals, names_from = ldp)})
  
  output$hb_pds_text <- renderUI({
    HTML(paste("<b>", prettyNum(vb_2_data_hb()$complete + vb_2_data_hb()$exempt, big.mark = ","), "</b>", "referrals either met or were exempt from the LDP standard. This is divided by",
               "<b>", prettyNum(vb_2_data_hb()$total -  vb_2_data_hb()$ongoing, big.mark = ","), "</b>", "the total number of referrals (excluding those whose support is ongoing)."))})
  
  
#data table	----
output$table_title_p2 <- renderUI({HTML(paste0("Number of Individuals relating to LDP Standard: Financial Year ", 
                                                input$select_year_p2, ", ", input$select_hb_p2, " andIntegration Authority Area(s)"))
  })

hb_ijb_data_1 <- reactive({annual_table_data %>%
  filter(health_board == input$select_hb_p2 & fy == input$select_year_p2) %>%
  select(ijb,ldp,referrals) %>%
  pivot_wider(names_from = ijb, values_from=referrals)%>%
  mutate(ldp = str_replace(ldp,"complete","Standard Met")) %>%
  mutate(ldp = str_replace(ldp,"exempt","Exempt from Standard")) %>%
  mutate(ldp = str_replace(ldp,"ongoing","PDS Ongoing")) %>%
  mutate(ldp = str_replace(ldp,"fail","Standard Not Met")) %>%
  mutate(ldp = str_replace(ldp,"total","Number of People Referred to PDS")) %>% 
  slice(5,1,2,4,3) %>% 
  rename(" " = "ldp")})

hb_ijb_data_2 <- reactive ({annual_table_data %>%
  filter(health_board == input$select_hb_p2 & fy == input$select_year_p2 & ldp == "total") %>%
  select(ijb,ldp,rate) %>%
  mutate(rate = paste0(rate, "%")) %>%  
  mutate(ldp = str_replace(ldp,"total","Percentage of LDP standard acheived")) %>%
  pivot_wider(names_from = ijb ,values_from = rate) %>% 
  rename(" " = "ldp")})


output$table_hb_ijb <- DT::renderDataTable({
   make_table(rbind(hb_ijb_data_1(),hb_ijb_data_2()), ordering = FALSE, right_align = 2:ncol(hb_ijb_data_2())-1, selected = 6, table_elements = "t") #%>%
   # formatCurrency(2, currency = "", interval = 3, mark = ",", digits = 0)
 })


#referrals monthly plot ----
output$chart_title_p2 <- renderUI({HTML(paste0("Number of Individuals Diagnosed and Referred for PDS: Financial Year ", 
                                              input$select_year_p2, ", ", input$select_hb_p2, " and Integration Authority Area(s)"))
})

hb_ijb_chart_data <- reactive({
  pds_plot_data %>%
    filter(health_board == input$select_hb_p2 & fy == input$select_year_p2)})


output$hb_ijb_plot <- renderPlotly({
  plot_referrals(hb_ijb_chart_data(), scotland = FALSE)
})


