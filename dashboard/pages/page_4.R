####################### Page 4 #######################

# output$page_4_ui <-  renderUI({

#   div(
# 	     fluidRow(
#             h3("Notes:"),
# 	           p(strong("Please note that both the Dementia Post Diagnostic Support service provision and data submission to PHS have been affected by the COVID-19 pandemic.")),
# 	           p(("")),
#
# 	      ) #fluidrow
#    ) # div
# }) # renderUI

data_selected_p4 <- reactive({get(input$select_data_p4)})

output$table_title_p4 <- renderUI({HTML(paste("Number of Individuals relating to LDP Standard by ", 
                                              
                                              if (input$select_data_p4 == "data_model"){
                                                "Model of Care"
                                              } else if(input$select_data_p4 == "data_subtype"){
                                                "Subtype of Dementia"
                                              } else if(input$select_data_p4 == "data_referral"){
                                                "Source of Referral to PDS"
                                              } else if(input$select_data_p4 == "data_stage"){
                                                "Clinical Impression of Stage of Dementia at Date of Referral"
                                              }))
})

output$chart_title_p4 <- renderUI({HTML(paste("Percentage of referrals who received a minimum of one 
year’s post-diagnostic support by", 

if (input$select_data_p4 == "data_model"){
  "Model of Care"
} else if(input$select_data_p4 == "data_subtype"){
  "Subtype of Dementia"
} else if(input$select_data_p4 == "data_referral"){
  "Source of Referral to PDS"
} else if(input$select_data_p4 == "data_stage"){
  "Clinical Impression of Stage of Dementia at Date of Referral"
      }))
})


#filter data
data_p4 <- reactive({data_selected_p4() %>% filter(health_board == input$select_hb_p4, fy == input$select_year_p4, sex == input$select_sex_p4)
                })

# create table
output$table_p4 <- DT::renderDataTable({
    table_data_p4 <- data_p4() %>% 
           select(type, total_referrals, percent_met) %>% 
          mutate(percent_met = paste0(percent_met,"%")) %>% 
           rename(" " = "type","Referrals" = "total_referrals", "% Met Standard/Exempt" = "percent_met")
    make_table(table_data_p4, right_align = 1:2)
  })

 
#create plot

output$plot_p4 <- renderPlotly({
  percent_met_bar_chart(data_p4())
})


