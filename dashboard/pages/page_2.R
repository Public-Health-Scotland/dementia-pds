####################### Page 2 #######################

output$page_2_ui <-  renderUI({

  div(
	     #fluidRow(
#             h3("Notes:"),
# 	           p(strong("Please note that both the Dementia Post Diagnostic Support service provision and data submission to PHS have been affected by the COVID-19 pandemic.")),
# 	           p(("LDP Standard performance figures are not provided until data is available for the full financial year.")),
         
	       fluidRow(
	         column(
	           valueBox(
	             value = textOutput("hb_exp_perc"),
	             subtitle = "of people estimated to be newly diagnosed with dementia were referred for post-diagnostic support.",
	             width = 12,
	             color = "blue"),
	           valueBox(
	             value = textOutput("hb_pds_perc"),
	             subtitle = "of those referred for post-diagnostic support received a minimum of 12 months of support.",
	             width = 12,
	             color = "blue"), width = 7),
	         column(
	           card(card_header(p(strong("How is this figure calculated?"))),
	                htmlOutput("hb_diagnoses")),
	           linebreaks(2),
	           card(card_header(p(strong("How is this figure calculated?"))),
	                htmlOutput("hb_referrals")), width = 5)
	         
	         
	       ) #fluidRow
	     ) # div
}) # renderUI
  
  
  
  #value boxes
  vb_data_hb<- reactive({annual_table_data %>% filter(health_board == input$select_hb_p2, ijb == input$select_hb_p2, fy == input$select_year_p2, ldp == "total")}) 
  
  output$hb_exp_perc <- renderText({paste0(round(100*vb_data_hb()$referrals/vb_data_hb()$diagnoses, 1), "%")})
  
  output$hb_diagnoses <- renderUI({
    HTML(paste("A total of", "<b>",  prettyNum(vb_data_hb()$referrals, big.mark = ","), "</b>", "referrals were made to post-diagnostic support. This is divided by",
               "<b>", prettyNum(vb_data_hb()$diagnoses, big.mark = ","), "</b>", "the estimated number of people newly diagnosed with dementia."))})
  
  vb_2_data_hb <- reactive({annual_table_data %>% filter(health_board == input$select_hb_p2, ijb == input$select_hb_p2, fy == input$select_year_p2,
                                                     ldp == "complete" | ldp == "exempt" | ldp == "total") %>% select(-diagnoses) %>% 
      pivot_wider(values_from = referrals, names_from = ldp)})
  
  output$hb_pds_perc <- renderText({paste0(round(100*(vb_2_data_hb()$complete + vb_2_data_hb()$exempt)/vb_2_data_hb()$total, 1), "%")})
  
  output$hb_referrals <- renderUI({
    HTML(paste("<b>", prettyNum(vb_2_data_hb()$complete + vb_2_data_hb()$exempt, big.mark = ","), "</b>", "referrals either met or were exempt from the LDP standard. This is divided by",
               "<b>", prettyNum(vb_2_data_hb()$total, big.mark = ","), "</b>", "the total number of referrals (excluding those whose support is ongoing)."))})
  
  
#data table	       

hb_ijb_data_1 <- reactive({annual_table_data %>%
  filter(health_board == input$select_hb_p2 & fy == input$select_year_p2) %>%
  select(ijb,ldp,referrals) %>%
  pivot_wider(names_from = ijb, values_from=referrals)%>%
  mutate(ldp = str_replace(ldp,"complete","Standard Met")) %>%
  mutate(ldp = str_replace(ldp,"exempt","Exempt")) %>%
  mutate(ldp = str_replace(ldp,"fail","Standard Not Met")) %>%
  mutate(ldp = str_replace(ldp,"ongoing","PDS Ongoing")) %>%
  mutate(ldp = str_replace(ldp,"total","Total Referrals")) %>% 
  rename(" " = "ldp")})

hb_ijb_data_2 <- reactive ({annual_table_data %>%
  filter(health_board == input$select_hb_p2 & fy == input$select_year_p2 & ldp == "total") %>%
  select(ijb,ldp,rate) %>%
  mutate(ldp = str_replace(ldp,"total","% Met Standard/Exempt"))%>%
  pivot_wider(names_from = ijb ,values_from = rate) %>% 
  rename(" " = "ldp")})


output$table_hb_ijb <- DT::renderDataTable({
   make_table(rbind(hb_ijb_data_1(),hb_ijb_data_2()), ordering = FALSE, right_align = 1:ncol(hb_ijb_data_2())-1)# %>% formatCurrency(c(2:5), currency = "", interval = 3, mark = ",", digits = 0)
 })


#referrals plot
hb_ijb_chart_data <- reactive({
  pds_plot_data %>%
    filter(health_board == input$select_hb_p2 & fy == input$select_year_p2)})


output$hb_ijb_plot <- renderPlotly({
  plot_referrals(hb_ijb_chart_data(), scotland = FALSE)
})


