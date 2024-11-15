####################### Page 4 DEMOGRAPHICS #######################
#UI----
output$page_4_ui <-  renderUI({
  #gender ----
  div(
    conditionalPanel(condition = 'input.select_data_demo == "data_sex"',
                     
                     column(selectInput("select_simd_demo",
                                         label = "Select SIMD Quintile:",
                                         choices = simd_list), width=6),
                      
                           #outputs
                              fluidRow(
                                 column(
                                 h3(htmlOutput("table_title_gender")),
                                 DT::dataTableOutput("table_gender"),
                                 linebreaks(1),
                                 column(
                                 h3(htmlOutput("chart_title_gender_referrals")),
                                 linebreaks(1),
                                 plotlyOutput("plot_gender_referrals"), width = 6),
                                 column(
                                 h3(htmlOutput("chart_title_gender_ldp")),
                                 plotlyOutput("plot_gender_ldp"), width = 6),
                                 width = 12) 
                               ) # fluid row
                    
    ), #cond panel 1 
    
    # age, simd, accommodation ----
    conditionalPanel(condition = 'input.select_data_demo != "data_sex"',
                           # inputs
                              column(selectInput("select_sex_demo",
                                                    label="Select Gender:",
                                                    choices=c("All", "Female", "Male")),width=6),
                               
                         #outputs
                               fluidRow(
                                 column(
                                 h3(htmlOutput("table_title_demo")),
                                 DT::dataTableOutput("table_demo"),
                                 linebreaks(1),
                                 h3(htmlOutput("chart_title_demo_referrals")),
                                 
                                 conditionalPanel(condition= 'input.select_sex_demo == "All"',
                                                  radioButtons("select_sex_chart_1",
                                                               label="Choose how genders are displayed in chart:",
                                                               choices=c("show all genders combined" = "All",
                                                                         "show female/male comparison" = "Male/Female"),
                                                               inline =TRUE),
                                                  plotlyOutput("plot_demo_referrals_all", height = "500px"),
                                                  h3(htmlOutput("chart_title_demo_ldp_all")),
                                                  radioButtons("select_sex_chart_2",
                                                               label="Choose how genders are displayed in chart:",
                                                               choices=c("show all genders combined" = "All",
                                                                         "show female/male comparison" = "Male/Female"),
                                                               inline =TRUE),
                                                  plotlyOutput("plot_demo_ldp_all", height = "500px")),
                                 
                                 conditionalPanel(condition= 'input.select_sex_demo != "All"',
                                                  plotlyOutput("plot_demo_referrals", height = "500px"),
                                                  h3(htmlOutput("chart_title_demo_ldp")),
                                                  plotlyOutput("plot_demo_ldp", height = "500px")), width = 12)
                               ) # fluid row
                     ) # cond panel 2 
  ) # div
}) # renderUI

# SERVER ----

# create table for gender ----
# table title
output$table_title_gender <- renderUI({HTML(paste0("Number and percentage of people referred for PDS who received a minimum of one year’s support by Gender: ",
                                                   input$select_hb_ijb_demo, ", Financial Year ", input$select_year_demo))
  
})

output$table_gender <- DT::renderDataTable({
  table_data_gender <- data_sex %>% 
    mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>%
    filter(ijb == input$select_hb_ijb_demo,
           fy == input$select_year_demo,
           simd == "All") %>% 
    select(type, total_referrals, complete, exempt, ongoing, not_met, percent_met) %>% 
    mutate(percent_met = paste0(percent_met,"%")) %>% 
    set_colnames(c(" ","Number of People Referred to PDS", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved"))
  make_table(table_data_gender, right_align = 1:6, table_elements = "t") %>% 
    formatCurrency(c(2:5), currency = "", interval = 3, mark = ",", digits = 0)
})


#create plots for gender----
#plot 1 title
output$chart_title_gender_referrals <- renderUI({HTML(paste0("Proportion of total referrals for PDS by Gender: ", 
                                                     input$select_hb_ijb_demo, ", Financial Year ", input$select_year_demo))
})

#plot 1 
output$plot_gender_referrals <- renderPlotly({
  proportion_bar_chart(data_sex %>%
                         mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>%
                         filter(ijb == input$select_hb_ijb_demo, fy == input$select_year_demo, simd == "All"),
  x_text_angle = 0, fill = type
  )
})


# plot 2 title
output$chart_title_gender_ldp <- renderUI({HTML(paste0("Percentage of referrals who received a minimum of one 
year’s post-diagnostic support by Gender: ", 
input$select_hb_ijb_demo, ", Financial Year ", input$select_year_demo))
})

#plot 2
output$plot_gender_ldp <- renderPlotly({
  percent_met_bar_chart(data_sex %>%
                          mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>%
                          filter(ijb == input$select_hb_ijb_demo, fy == input$select_year_demo, simd == "All"),
  x_text_angle = 0, fill = type
  )
})





# create table for age, simd, accommodation----

#filter data
data_selected <-reactive({
  if (input$select_data_demo != "data_sex"){
    get(input$select_data_demo)
  } else {
    NULL
  }
})

data_demo <- reactive({data_selected() %>%
    mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>%
    filter(ijb == input$select_hb_ijb_demo, fy == input$select_year_demo)
})
# table title
output$table_title_demo <- renderUI({HTML(paste0("Number and percentage of people referred for PDS who received a minimum of one year’s support by ", 
                                                 
                                                 if (input$select_data_demo == "data_age"){
                                                   "Age Group"
                                                 } else if(input$select_data_demo == "data_simd"){
                                                   "Scottish Index of Multiple Deprivation (SIMD)"
                                                 } else if(input$select_data_demo == "data_accom"){
                                                   "Accommodation Type"
                                                 },
                                                 ": ", input$select_hb_ijb_demo, ", Financial Year ", input$select_year_demo, ", Gender: ", input$select_sex_demo
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


#create plots for age, simd, accommodation----
#chart 1 title
output$chart_title_demo_referrals <- renderUI({HTML(paste0("Proportion of total referrals for PDS by ", 
                                                           
                                                           if (input$select_data_demo == "data_age"){
                                                             "Age Group"
                                                           } else if(input$select_data_demo == "data_simd"){
                                                             "Scottish Index of Multiple Deprivation (SIMD)"
                                                           } else if(input$select_data_demo == "data_accom"){
                                                             "Accommodation Type"
                                                           },
                                                           ": ", input$select_hb_ijb_demo, ", Financial Year ", input$select_year_demo, ", Gender: ", input$select_sex_demo
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
": ", input$select_hb_ijb_demo, ", Financial Year ", input$select_year_demo, ", Gender: ", input$select_sex_demo
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
": ", input$select_hb_ijb_demo, ", Financial Year ", input$select_year_demo, ", Gender: ", input$select_sex_demo
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


