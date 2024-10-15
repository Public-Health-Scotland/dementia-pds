####################### Page 1 #######################

output$page_1_ui <-  renderUI({

  div(
# 	     fluidRow(
#             h3("Notes:"),
# 	           p(strong("Please note that both the Dementia Post Diagnostic Support service provision and data submission to PHS have been affected by the COVID-19 pandemic.")),
# 	           p(strong(""),
#             em(" "))
#             	      ), #fluidrow
# 	     linebreaks(2),
		    fluidRow(
	       column(
	         valueBox(
	          value = textOutput("scot_exp_perc"),
	          subtitle = "of people estimated to be newly diagnosed with dementia were referred for post-diagnostic support.",
	          width = 12,
	          color = "blue"),
	         valueBox(
	           value = textOutput("scot_pds_perc"),
	           subtitle = "of those referred for post-diagnostic support received a minimum of 12 months of support.",
	           width = 12,
	           color = "blue"), width = 7),
	       column(
	         card(card_header(p(strong("How is this figure calculated?"))),
	                     htmlOutput("scot_diagnoses")),
	         linebreaks(2),
	         card(card_header(p(strong("How is this figure calculated?"))),
	                     htmlOutput("scot_referrals")), width = 5)
	       
		    
	     ) #fluidRow
   ) # div
}) # renderUI



#value boxes
vb_data<- reactive({annual_table_data %>% filter(health_board == "Scotland", ijb == "Scotland", fy == input$select_year_p1, ldp == "total")}) 

output$scot_exp_perc <- renderText({paste0(round(100*vb_data()$referrals/vb_data()$diagnoses, 1), "%")})

output$scot_diagnoses <- renderUI({
  HTML(paste("A total of", "<b>",  prettyNum(vb_data()$referrals, big.mark = ","), "</b>", "referrals were made to post-diagnostic support. This is divided by",
  "<b>", prettyNum(vb_data()$diagnoses, big.mark = ","), "</b>", "the estimated number of people newly diagnosed with dementia."))})

vb_2_data<- reactive({annual_table_data %>% filter(health_board == "Scotland", ijb == "Scotland", fy == input$select_year_p1,
                                         ldp == "complete" | ldp == "exempt" | ldp == "total") %>% select(-diagnoses) %>% 
                                          pivot_wider(values_from = referrals, names_from = ldp)})

output$scot_pds_perc <- renderText({paste0(round(100*(vb_2_data()$complete + vb_2_data()$exempt)/vb_2_data()$total, 1), "%")})

output$scot_referrals <- renderUI({
  HTML(paste("<b>", prettyNum(vb_2_data()$complete + vb_2_data()$exempt, big.mark = ","), "</b>", "referrals either met or were exempt from the LDP standard. This is divided by",
             "<b>", prettyNum(vb_2_data()$total, big.mark = ","), "</b>", "the total number of referrals (excluding those whose support is ongoing)."))})


#referrals plot
scotland_chart_data <- reactive({pds_plot_data %>%
    filter(fy == input$select_year_p1)})


output$ldp_scotland <- renderPlotly({
    plot_referrals(scotland_chart_data(), scotland = TRUE)
})

# Data table hb
output$table_hb <- DT::renderDataTable({
  table_hb_data <- annual_table_data %>%
    filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
    filter(fy == input$select_year_p1) %>%
    group_by(health_board)%>%
    select(health_board,ldp,referrals,rate)%>%
    pivot_wider(names_from=ldp,values_from=referrals) %>% relocate(total, .after = health_board) %>%
    mutate(health_board = if_else(health_board == "Scotland","AAA Scotland", health_board)) %>% 
    arrange(health_board) %>% 
    mutate(health_board = if_else(health_board == "AAA Scotland","Scotland", health_board)) %>% 
    set_colnames(c("Health Board","Total Referrals", "% Met Standard/Exempt","Standard Met","Exempt","Standard Not Met","PDS Ongoing")) 
  make_table(table_hb_data, right_align = 1:6) %>% formatCurrency(c(2,4:7), currency = "", interval = 3, mark = ",", digits = 0)
})

# Data table ijb
table_ijb_data <- reactive({
  annual_table_data %>%
    filter(ijb != "Scotland") %>% filter(!grepl("NHS", ijb)) %>% 
    filter(fy == input$select_year_p1) %>%
    group_by(ijb)%>%
    select(ijb,ldp,referrals,rate)%>%
    pivot_wider(names_from=ldp,values_from=referrals) %>% relocate(total, .after = ijb) %>% 
    arrange(ijb) %>% 
    set_colnames(c("IJB","Total Referrals", "% Met Standard/Exempt", "Standard Met","Exempt","Standard Not Met","PDS Ongoing"))
  
})

output$table_ijb <- DT::renderDataTable({
   make_table(table_ijb_data(), right_align = 1:6) %>% formatCurrency(c(2,4:7), currency = "", interval = 3, mark = ",", digits = 0)
 })