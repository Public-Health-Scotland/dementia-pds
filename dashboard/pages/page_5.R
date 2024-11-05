####################### Page 5 ADDITIONAL ANALYSIS #######################
# UI ----
 output$page_5_ui <-  renderUI({

  div(
 	     conditionalPanel(
    condition= 'input.select_data_add != "waiting_times"',
    # inputs
    
    
    fluidRow(column(selectInput("select_hb_add",
                                label = "Health Board",
                                choices = c("Scotland", boards)),width=6)),
    
    
    #uiOutput("page_5_ui"),
    
    fluidRow(
      # outputs
      h3(htmlOutput("table_title_add")),
      DT::dataTableOutput("table_add"),
      linebreaks(1),
      h3(htmlOutput("chart_title_add_referrals")),
      plotlyOutput("plot_add_referrals", height = "600px"),
      h3(htmlOutput("chart_title_add")),
      plotlyOutput("plot_add", height = "600px"))
  ), #cond panel 1
   
   conditionalPanel(
     condition = 'input.select_data_add == "waiting_times"',
     
     fluidRow(
       # outputs
       h3(htmlOutput("hb_ijb_table_title_wait")),
       column(radioButtons("select_wait_table",
                           label = "In the table below show Scotland and: ",
                           choices = c("Health Boards", "Integration Joint Boards"),
                           selected = "Health Boards",
                           inline = TRUE
       ), width = 6),
       DT::dataTableOutput("table_hb_ijb_wait"),
       linebreaks(2),
       h3(htmlOutput("table_wait_2_title")),
       #linebreaks(1),
       column(selectInput("select_hb_ijb_wait_2",
                          label = "Select Health Board/IJB to show in tables below:",
                          choices = c("Scotland", boards, ijb_list), selected = data_wait_2$ijb == "Scotland", width = "100%"), width = 6)
              ), #fluid Row
       fluidRow(
         column(
       h4(strong("Referrals not exempt from LDP Standard, where post-diagnostic support has ended after being contacted by PDS practitioner:")),
       DT::dataTableOutput("table_wait_2"),width = 6),
        column(
       h4(strong("Referrals exempt from LDP Standard, where post-diagnostic support has ended after being contacted by PDS practitioner:")),
       DT::dataTableOutput("table_wait_2_exempt"), width = 6)
       
       # h3(htmlOutput("")),
       # plotlyOutput("", height = "600px"))
       
     ), # fluidrow
     linebreaks(2)
   ) #cond panel 2
   
  ) #div   
 }) # renderUI
 
 
# SERVER ----

# create table for variables other than waiting times ----
# title 
 
output$table_title_add <- renderUI({HTML(paste0("Number and percentage of people referred for PDS who received a minimum of one year’s support by ", 
                                              
                                              if (input$select_data_add == "data_model"){
                                                "PDS Model of Care"
                                              } else if(input$select_data_add == "data_subtype"){
                                                "Subtype of Dementia"
                                              } else if(input$select_data_add == "data_referral"){
                                                "Source of Referral to PDS"
                                              } else if(input$select_data_add == "data_stage"){
                                                "Clinical Impression of Stage of Dementia at Date of Referral"
                                              },
                                              ": ", input$select_hb_add, ", Financial Year ", input$select_year_add, ", Gender: ", input$select_sex_add
                                              )
                                         )
})




data_selected_add <-reactive({
  if (input$select_data_add == "data_model"){
    data_model
  } else if(input$select_data_add == "data_subtype"){
    data_subtype
  } else if(input$select_data_add == "data_referral"){
    data_referral
  } else if(input$select_data_add == "data_stage"){
    data_stage
  } else if(input$select_data_add == "waiting_times"){
    NULL
  } 
})


output$table_add <- DT::renderDataTable({
    table_data_add <- data_selected_add() %>% 
           filter(health_board == input$select_hb_add, fy == input$select_year_add, sex == input$select_sex_add) %>%  
           select(type, total_referrals, percent_met) %>% 
          mutate(percent_met = paste0(percent_met,"%")) %>% 
           rename(" " = "type","Number of People Referred to PDS" = "total_referrals", "Percentage of LDP standard achieved" = "percent_met")
    make_table(table_data_add, right_align = 1:2, table_elements = "t") %>% 
      formatCurrency(2, currency = "", interval = 3, mark = ",", digits = 0)
  })


#create plots  for variables other than waiting times ----
# proportion plot
output$chart_title_add_referrals <- renderUI({HTML(paste0("Proportion of total referrals for dementia post-diagnostic support by ", 
                                                          
                                                         if (input$select_data_add == "data_model"){
                                                           "PDS Model of Care"
                                                         } else if(input$select_data_add == "data_subtype"){
                                                           "Subtype of Dementia"
                                                         } else if(input$select_data_add == "data_referral"){
                                                           "Source of Referral to PDS"
                                                         } else if(input$select_data_add == "data_stage"){
                                                           "Clinical Impression of Stage of Dementia at Date of Referral"
                                                         },
                                                         ": ", input$select_hb_add, ", Financial Year ", input$select_year_add, ", Gender: ", input$select_sex_add
                                                         ))
})


output$plot_add_referrals <- renderPlotly({
  proportion_bar_chart(data_selected_add() %>% 
                         filter(health_board == input$select_hb_add, fy == input$select_year_add, sex == input$select_sex_add))
})

# ldp plot

output$chart_title_add <- renderUI({HTML(paste0("Percentage of referrals who received a minimum of one 
year’s post-diagnostic support by ", 

if (input$select_data_add == "data_model"){
  "PDS Model of Care"
} else if(input$select_data_add == "data_subtype"){
  "Subtype of Dementia"
} else if(input$select_data_add == "data_referral"){
  "Source of Referral to PDS"
} else if(input$select_data_add == "data_stage"){
  "Clinical Impression of Stage of Dementia at Date of Referral"
},
": ", input$select_hb_add, ", Financial Year ", input$select_year_add, ", Gender: ", input$select_sex_add
))
})

output$plot_add <- renderPlotly({
  percent_met_bar_chart(data_selected_add() %>% 
                          filter(health_board == input$select_hb_add, fy == input$select_year_add, sex == input$select_sex_add))
})
 
#Pathways ----

# Data table diagnosis to contact times
output$hb_ijb_table_title_wait <- renderUI({HTML(paste0("Pathways from diagnosis to contact by PDS practitioner, Financial Year ", 
                                                 input$select_year_add, ", Scotland and ", input$select_wait_table, ", Gender: ", input$select_sex_add))
})

output$table_hb_ijb_wait <- DT::renderDataTable({
  
  if(input$select_wait_table == "Health Boards"){  
    
    table_hb_data_wait <- data_wait %>% 
      filter(fy == input$select_year_add, sex == input$select_sex_add) %>% 
      filter(ijb == "All" | ijb == "Scotland", simd == "All") %>% 
      select(health_board, total_referrals, median_diagnosis_to_referral,
             allocated_referrals, median_referral_to_allocation,
             contacted_referrals, median_allocation_to_contact,
             median_diagnosis_to_contact) %>% 
      set_colnames(c("Health Board","Total Referrals",  "Average (median) days from diagnosis to PDS referral",
                     "Referrals allocated to PDS practitioner", "Average (median) days from referral to allocation of PDS practitioner", 
                     "Referrals contacted by PDS practitioner",  "Average (median) days from allocation to first contact with PDS practitioner",
                     "Average (median) days from diagnosis to first contact"
      )) 
    make_table(table_hb_data_wait, right_align = 1:7, selected = 1, table_elements = "t") %>%
      formatCurrency(c(2,4,6), currency = "", interval = 3, mark = ",", digits = 0)
    
    
  }else{    
    
    
    # # Data table ijb
    
    
    table_hb_data_wait <- data_wait %>% 
      filter(fy == input$select_year_add, sex == input$select_sex_add) %>% 
      filter(ijb != "All", simd == "All") %>% 
      select(ijb, total_referrals, median_diagnosis_to_referral,
             allocated_referrals, median_referral_to_allocation,
             contacted_referrals, median_allocation_to_contact,
             median_diagnosis_to_contact) %>% 
      mutate(ijb = if_else(ijb == "Scotland", "AAA", ijb)) %>%
      arrange(ijb) %>% 
      mutate(ijb = if_else(ijb == "AAA", "Scotland", ijb)) %>% 
      set_colnames(c("IJB","Total Referrals",  "Average (median) days from diagnosis to PDS referral",
                     "Referrals allocated to PDS practitioner", "Average (median) days from referral to allocation of PDS practitioner", 
                     "Referrals contacted by PDS practitioner",  "Average (median) days from allocation to first contact with PDS practitioner",
                     "Average (median) days from diagnosis to first contact"
      )) 
    make_table(table_hb_data_wait, right_align = 1:7, selected = 1, table_elements = "t", rows_to_display = 32) %>% 
      formatCurrency(c(2,4,6), currency = "", interval = 3, mark = ",", digits = 0)
  }
})


# table 2 (contact to termination)
output$table_wait_2_title<- renderUI({HTML(paste0("Pathways from contact by PDS practitioner to termination of PDS, Financial Year ", 
                                                  input$select_year_add, ", ", input$select_hb_ijb_wait_2, ", Gender: ", input$select_sex_add))
})

output$table_wait_2 <- DT::renderDataTable({
  wait_table_2_data <- data_wait_2 %>% 
    filter(ijb == input$select_hb_ijb_wait_2,
           fy == input$select_year_add,
           sex == input$select_sex_add,
           !grepl("exempt", termination_or_transition_reason)) %>% 
    select(termination_or_transition_reason, referrals, median_contact_to_termination) %>% 
    set_colnames(c("Reason for termination of post-diagnostic support", "Number of referrals", "Average (median) days from contact to termination/transition"))
  make_table(wait_table_2_data, table_elements = "t") %>% 
    formatCurrency(c(2,3), currency = "", interval = 3, mark = ",", digits = 0)
})

output$table_wait_2_exempt <- DT::renderDataTable({
  wait_table_2_data_exempt <- data_wait_2 %>%
    filter(ijb == input$select_hb_ijb_wait_2,
           fy == input$select_year_add,
           sex == input$select_sex_add,
           grepl("exempt", termination_or_transition_reason)) %>%
    select(termination_or_transition_reason, referrals, median_contact_to_termination) %>%
    set_colnames(c("Reason for termination of post-diagnostic support", "Number of referrals", "Average (median) days from contact to termination/transition"))
  make_table(wait_table_2_data_exempt, table_elements = "t") %>%
    formatCurrency(c(2,3), currency = "", interval = 3, mark = ",", digits = 0)
})

  