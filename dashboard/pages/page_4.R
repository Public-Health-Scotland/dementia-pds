####################### Page 4 DEMOGRAPHICS #######################

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



#filter data
data_selected <- reactive({get(input$select_data_demo)})

data_demo <- reactive({data_selected() %>% filter(health_board == input$select_hb_demo, fy == input$select_year_demo, sex == input$select_sex_demo)
                })

# create table
output$table_title_demo <- renderUI({HTML(paste("Number of Individuals relating to LDP Standard by ", 
                                                
                                                if (input$select_data_demo == "data_age"){
                                                  "Age Group"
                                                } else if(input$select_data_demo == "data_simd"){
                                                  "Scottish Index of Multiple Deprivation (SIMD)"
                                                } else if(input$select_data_demo == "data_accom"){
                                                  "Accommodation Type"
                                                }))
})


output$table_demo <- DT::renderDataTable({
    table_data_demo <- data_demo() %>% 
           select(type, total_referrals, percent_met) %>% 
          mutate(percent_met = paste0(percent_met,"%")) %>% 
           rename(" " = "type","Referrals" = "total_referrals", "% Met Standard/Exempt" = "percent_met")
    make_table(table_data_demo, right_align = 1:2)
  })

 
#create plots

output$chart_title_demo_referrals <- renderUI({HTML(paste("Proportion of total referrals for dementia post-diagnostic support by", 

if (input$select_data_demo == "data_age"){
  "Age Group"
} else if(input$select_data_demo == "data_simd"){
  "Scottish Index of Multiple Deprivation (SIMD)"
} else if(input$select_data_demo == "data_accom"){
  "Accommodation Type"
}))
})

output$plot_demo_referrals <- renderPlotly({
  proportion_bar_chart(data_demo())
})


output$chart_title_demo_ldp <- renderUI({HTML(paste("Percentage of referrals who received a minimum of one 
year’s post-diagnostic support by", 

if (input$select_data_demo == "data_age"){
  "Age Group"
} else if(input$select_data_demo == "data_simd"){
  "Scottish Index of Multiple Deprivation (SIMD)"
} else if(input$select_data_demo == "data_accom"){
  "Accommodation Type"
}))
})

output$plot_demo_ldp <- renderPlotly({
  percent_met_bar_chart(data_demo())
})


