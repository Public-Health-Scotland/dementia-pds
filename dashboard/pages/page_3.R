####################### Page 3 #######################

# output$page_3_ui <-  renderUI({

#   div(
# 	     fluidRow(
#             h3("Notes:"),
# 	           p(strong("Please note that both the Dementia Post Diagnostic Support service provision and data submission to PHS have been affected by the COVID-19 pandemic.")),
# 	           p(("")),
#
# 	      ) #fluidrow
#    ) # div
# }) # renderUI

data_selected <- reactive({get(input$select_data_p3)})

output$table_title_p3 <- renderUI({HTML(paste("Number of Individuals relating to LDP Standard by ", 
                                              
                                              if (input$select_data_p3 == "data_age"){
                                                "Age"
                                              } else if(input$select_data_p3 == "data_simd"){
                                                "Scottish Index of Multiple Deprivation (SIMD)"
                                              } else if(input$select_data_p3 == "data_accom"){
                                                "Accommodation Type"
                                              }))
})

output$chart_title_p3 <- renderUI({HTML(paste("Percentage of referrals who received a minimum of one 
year’s post-diagnostic support by", 

if (input$select_data_p3 == "data_age"){
  "Age"
} else if(input$select_data_p3 == "data_simd"){
  "Scottish Index of Multiple Deprivation (SIMD)"
} else if(input$select_data_p3 == "data_accom"){
  "Accommodation Type"
}))
})


#filter data
data_p3 <- reactive({data_selected() %>% filter(health_board == input$select_hb_p3, fy == input$select_year_p3, sex == input$select_sex_p3)
                })

# create table
output$table_p3 <- DT::renderDataTable({
    table_data_p3 <- data_p3() %>% 
           select(type, total_referrals, percent_met) %>% 
          mutate(percent_met = paste0(percent_met,"%")) %>% 
           rename(" " = "type","Referrals" = "total_referrals", "% Met Standard/Exempt" = "percent_met")
    make_table(table_data_p3, right_align = 1:2)
  })

 
#create plot

output$plot_p3 <- renderPlotly({
  percent_met_bar_chart(data_p3())
})


