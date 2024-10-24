####################### Page 5 ADDITIONAL ANALYSIS #######################

# output$page_5_ui <-  renderUI({

  # div(
# 	     fluidRow(
#             h3("Notes:"),
# 	           p(strong("Please note that both the Dementia Post Diagnostic Support service provision and data submission to PHS have been affected by the COVID-19 pandemic.")),
# 	           p(("")),
#
# 	      ) #fluidrow

   
      
 #}) # renderUI

# create table for variables other than waiting times
# title 
 
output$table_title_add <- renderUI({HTML(paste("Number of Individuals relating to LDP Standard by ", 
                                              
                                              if (input$select_data_add == "data_model"){
                                                "Model of Care"
                                              } else if(input$select_data_add == "data_subtype"){
                                                "Subtype of Dementia"
                                              } else if(input$select_data_add == "data_referral"){
                                                "Source of Referral to PDS"
                                              } else if(input$select_data_add == "data_stage"){
                                                "Clinical Impression of Stage of Dementia at Date of Referral"
                                              }))
})



# chart
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
           rename(" " = "type","Referrals" = "total_referrals", "% Met Standard/Exempt" = "percent_met")
    make_table(table_data_add, right_align = 1:2)
  })


#create plots  for variables other than waiting times
# proportion plot
output$chart_title_add_referrals <- renderUI({HTML(paste("Proportion of total referrals for dementia post-diagnostic support by", 
                                                          
                                                         if (input$select_data_add == "data_model"){
                                                           "Model of Care"
                                                         } else if(input$select_data_add == "data_subtype"){
                                                           "Subtype of Dementia"
                                                         } else if(input$select_data_add == "data_referral"){
                                                           "Source of Referral to PDS"
                                                         } else if(input$select_data_add == "data_stage"){
                                                           "Clinical Impression of Stage of Dementia at Date of Referral"
                                                         }))
})


output$plot_add_referrals <- renderPlotly({
  proportion_bar_chart(data_selected_add() %>% 
                         filter(health_board == input$select_hb_add, fy == input$select_year_add, sex == input$select_sex_add))
})

# ldp plot

output$chart_title_add <- renderUI({HTML(paste("Percentage of referrals who received a minimum of one 
year’s post-diagnostic support by", 

if (input$select_data_add == "data_model"){
  "Model of Care"
} else if(input$select_data_add == "data_subtype"){
  "Subtype of Dementia"
} else if(input$select_data_add == "data_referral"){
  "Source of Referral to PDS"
} else if(input$select_data_add == "data_stage"){
  "Clinical Impression of Stage of Dementia at Date of Referral"
}))
})

output$plot_add <- renderPlotly({
  percent_met_bar_chart(data_selected_add() %>% 
                          filter(health_board == input$select_hb_add, fy == input$select_year_add, sex == input$select_sex_add))
})
 
#WAITING TIMES

# Data table hb
output$hb_table_title_wait <- renderUI({HTML(paste("Scotland/Health Board - Waiting Times for Individuals Referred to PDS, Financial Year ", 
                                                 input$select_year_add))
})

output$table_hb_wait <- DT::renderDataTable({
  table_hb_data_wait <- data_wait %>% 
    filter(fy == input$select_year_add, sex == input$select_sex_add) %>% 
    filter(ijb == "All", simd == "All") %>% 
    select(health_board, total_referrals, median_diagnosis_to_referral, median_referral_to_contact, median_diagnosis_to_contact) %>% 
    set_colnames(c("Health Board","Total Referrals", "Average (median) days from diagnois to PDS referral", 
                   "Average (median) days from referral to PDS first contact", "Average (median) days from diagnois to PDS first contact")) 
  make_table(table_hb_data_wait, right_align = 1:4) %>% formatCurrency(2, currency = "", interval = 3, mark = ",", digits = 0)
})

# # Data table ijb

output$ijb_table_title_wait <- renderUI({HTML(paste("IJB - Waiting Times for Individuals Referred to PDS, Financial Year ", 
                                                 input$select_year_add))
})

output$table_ijb_wait <- DT::renderDataTable({
  table_hb_data_wait <- data_wait %>% 
    filter(fy == input$select_year_add, sex == input$select_sex_add) %>% 
    filter(ijb != "All", simd == "All") %>% 
    select(ijb, total_referrals, median_diagnosis_to_referral, median_referral_to_contact, median_diagnosis_to_contact) %>% 
    arrange(ijb) %>% 
    set_colnames(c("IJB","Total Referrals", "Average (median) days from diagnois to PDS referral", 
                   "Average (median) days from referral to PDS first contact", "Average (median) days from diagnois to PDS first contact")) 
  make_table(table_hb_data_wait, right_align = 1:4) %>% formatCurrency(2, currency = "", interval = 3, mark = ",", digits = 0)
})
# 
# output$table_ijb <- DT::renderDataTable({
#   make_table(table_ijb_data(), right_align = 1:6) %>% formatCurrency(c(2,4:7), currency = "", interval = 3, mark = ",", digits = 0)
# })
