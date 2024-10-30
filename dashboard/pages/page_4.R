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

data_demo <- reactive({data_selected() %>% filter(health_board == input$select_hb_demo, fy == input$select_year_demo)
                })

# create table
output$table_title_demo <- renderUI({HTML(paste0("Number and percentage of people referred for PDS who received a minimum of one year’s support by ", 
                                                 
                                                 if (input$select_data_demo == "data_age"){
                                                   "Age Group"
                                                 } else if(input$select_data_demo == "data_simd"){
                                                   "Scottish Index of Multiple Deprivation (SIMD)"
                                                 } else if(input$select_data_demo == "data_accom"){
                                                   "Accommodation Type"
                                                 },
                                                 ": ", input$select_hb_demo, ", Financial Year ", input$select_year_demo, ", Gender: ", input$select_sex_demo
)
)
})


output$table_demo <- DT::renderDataTable({
    table_data_demo <- data_demo() %>% filter(sex == input$select_sex_demo) %>% 
      select(type, total_referrals, complete, exempt, ongoing, not_met, percent_met) %>% 
      mutate(percent_met = paste0(percent_met,"%")) %>% 
      set_colnames(c(" ","Number of People Referred to PDS", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved"))
    make_table(table_data_demo, right_align = 1:6, table_elements = "t") %>% 
      formatCurrency(c(2:5), currency = "", interval = 3, mark = ",", digits = 0)
  })

 
#create plots ----
#chart 1 title
output$chart_title_demo_referrals <- renderUI({HTML(paste0("Proportion of total referrals for PDS by ", 

if (input$select_data_demo == "data_age"){
  "Age Group"
} else if(input$select_data_demo == "data_simd"){
  "Scottish Index of Multiple Deprivation (SIMD)"
} else if(input$select_data_demo == "data_accom"){
  "Accommodation Type"
},
": ", input$select_hb_demo, ", Financial Year ", input$select_year_demo, ", Gender: ", input$select_sex_demo
)
)
})

#plot 1 if all genders are selected
output$plot_demo_referrals_all <- renderPlotly({
  proportion_bar_chart(if(input$select_sex_chart_1 == "All"){
                        data_demo() %>% filter(sex == "All")
                  }else if(input$select_sex_chart_1 == "Male/Female"){
                        data_demo() %>% filter(sex == "Male" | sex == "Female")
                  },
                       x_text_angle = if_else(input$select_data_demo == "data_accom", 45, 0),
                       legend = if_else(input$select_sex_chart_1 == "All", "none", "right")
                  )
})

#plot 1 if specific gender is selected
output$plot_demo_referrals <- renderPlotly({
  proportion_bar_chart(data_demo() %>% filter(sex == input$select_sex_demo),
                                       x_text_angle = if_else(input$select_data_demo == "data_accom", 45, 0))
})

# chart 2 title
output$chart_title_demo_ldp <- renderUI({HTML(paste0("Percentage of referrals who received a minimum of one 
year’s post-diagnostic support by ", 

if (input$select_data_demo == "data_age"){
  "Age Group"
} else if(input$select_data_demo == "data_simd"){
  "Scottish Index of Multiple Deprivation (SIMD)"
} else if(input$select_data_demo == "data_accom"){
  "Accommodation Type"
},
": ", input$select_hb_demo, ", Financial Year ", input$select_year_demo, ", Gender: ", input$select_sex_demo
)
)
})

output$chart_title_demo_ldp_all <- renderUI({HTML(paste0("Percentage of referrals who received a minimum of one 
year’s post-diagnostic support by ", 

if (input$select_data_demo == "data_age"){
  "Age Group"
} else if(input$select_data_demo == "data_simd"){
  "Scottish Index of Multiple Deprivation (SIMD)"
} else if(input$select_data_demo == "data_accom"){
  "Accommodation Type"
},
": ", input$select_hb_demo, ", Financial Year ", input$select_year_demo, ", Gender: ", input$select_sex_demo
)
)
})
#plot 2 if all genders are selected
output$plot_demo_ldp_all <- renderPlotly({
  percent_met_bar_chart(if(input$select_sex_chart_2 == "All"){
    data_demo() %>% filter(sex == "All")
  }else if(input$select_sex_chart_2 == "Male/Female"){
    data_demo() %>% filter(sex == "Male" | sex == "Female")
  },
  x_text_angle = if_else(input$select_data_demo == "data_accom", 45, 0),
  legend = if_else(input$select_sex_chart_2 == "All", "none", "right")
  )
})

#plot 2 if specific gender is selected
output$plot_demo_ldp <- renderPlotly({
  percent_met_bar_chart(data_demo() %>% filter(sex == input$select_sex_demo),
                                         x_text_angle = if_else(input$select_data_demo == "data_accom", 45, 0))
})


