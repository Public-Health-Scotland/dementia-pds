####################### Page 4 DEMOGRAPHICS #######################
#UI----
output$page_4_ui <-  renderUI({
  #1 gender ----
  div(
    conditionalPanel(condition = 'input.select_data_demo == "data_sex"',
                     
            fluidRow(
                column(
                         h3(strong(htmlOutput("table_title_gender"))),
                         DT::dataTableOutput("table_gender"),
    linebreaks(1),
                  fluidRow(
                      column(
                           h3(strong(htmlOutput("chart_title_gender_referrals"))),
                           width = 6),
                      column(
                           h3(strong(htmlOutput("chart_title_gender_ldp"))),
                           width = 6)
                      ), #fluidRow
                  fluidRow(                      
                       column(
                           plotlyOutput("plot_gender_referrals"), width = 6),
                       column(
                           plotlyOutput("plot_gender_ldp"), width = 6),
    linebreaks(1)), #fluidRow
                             
    #                      h3(strong(htmlOutput("chart_title_gender_referrals_trend"))),
    #                      #DT::dataTableOutput("gender_trend_table"),
    #                      plotlyOutput("plot_gender_referrals_trend"),
    # linebreaks(1),
    #                      h3(strong(htmlOutput("chart_title_gender_ldp_trend"))),
    #                      plotlyOutput("plot_gender_ldp_trend"),
    # linebreaks(1),
                         width = 12,
                         style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; margin-left: 1px; height:-webkit-fill-available; background-color: white"
                      ) # column
                  ), # fluid row
    ), #cond panel 1 
    
    #2 age, simd, accommodation ----
    conditionalPanel(condition = 'input.select_data_demo != "data_sex"',
                          
                fluidRow(
                    column(
                          h3(strong(htmlOutput("table_title_demo"))),
                          DT::dataTableOutput("table_demo"),
      linebreaks(1),
                          h3(strong(htmlOutput("chart_title_demo_referrals"))),
                                 
                     conditionalPanel(condition= 'input.select_sex_demo == "All"',
                          radioButtons("select_sex_chart_1",
                                        label="Choose how genders are displayed in chart:",
                                        choices=c("show all genders combined" = "All",
                                                  "show female/male comparison" = "Male/Female"),
                                                   inline =TRUE),
                          plotlyOutput("plot_demo_referrals_all", height = "500px"),
                          h3(strong(htmlOutput("chart_title_demo_ldp_all"))),
                          radioButtons("select_sex_chart_2",
                                        label="Choose how genders are displayed in chart:",
                                        choices=c("show all genders combined" = "All",
                                                   "show female/male comparison" = "Male/Female"),
                                                    inline =TRUE),
                          plotlyOutput("plot_demo_ldp_all", height = "500px")),
                                 
                     conditionalPanel(condition= 'input.select_sex_demo != "All"',
                          plotlyOutput("plot_demo_referrals", height = "500px"),
                          h3(strong(htmlOutput("chart_title_demo_ldp"))),
                          plotlyOutput("plot_demo_ldp", height = "500px")),
                        #  h3(strong(htmlOutput("chart_title_demo_referrals_trend"))),
                          #DT::dataTableOutput("gender_trend_table"),
                          #plotlyOutput("plot_demo_referrals_trend"),
                         # h3(strong(htmlOutput("chart_title_demo_ldp_trend"))),
                          #plotlyOutput("plot_demo_ldp_trend"),
      linebreaks(1),
                          width = 12,
                          style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; margin-left: 1px; height:-webkit-fill-available; background-color: white") 
                     ), # fluid row
               ) # cond panel 2 
  ) # div
}) # renderUI

# SERVER ----

#1a table for gender ----
# table title
output$table_title_gender <- renderUI({HTML(paste0("Number and percentage of people referred for PDS who received a minimum of one year’s support by Gender: ",
                                                   input$select_hb_ijb_demo, ", Financial Year ", input$select_year_demo,
                                                   ", SIMD Quintile: ", input$select_simd_demo))
  
})

output$table_gender <- DT::renderDataTable({
  table_data_gender <- data_sex %>% 
    mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>%
    filter(ijb == input$select_hb_ijb_demo,
           fy == input$select_year_demo,
           simd == input$select_simd_demo) %>% 
    select(type, total_referrals, complete, exempt, ongoing, not_met, percent_met) %>% 
    mutate(percent_met = paste0(percent_met,"%")) %>% 
    set_colnames(c(" ","Number of People Referred to PDS", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved"))
  make_table(table_data_gender, right_align = 1:6, table_elements = "t") %>% 
    formatCurrency(c(2:5), currency = "", interval = 3, mark = ",", digits = 0)
})


#1b plots for gender----
#proportion plot title
output$chart_title_gender_referrals <- renderUI({HTML(paste0("Proportion of total referrals for PDS by Gender: ", 
                                      input$select_hb_ijb_demo, ", Financial Year ", input$select_year_demo,
                                      ", SIMD Quintile: ", input$select_simd_demo))
})

#proportion plot 
output$plot_gender_referrals <- renderPlotly({
  proportion_bar_chart(data_sex %>%
                         mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>%
                         filter(ijb == input$select_hb_ijb_demo, fy == input$select_year_demo, simd == input$select_simd_demo),
  x_text_angle = 0, fill = type
  )
})


# percent met title
output$chart_title_gender_ldp <- renderUI({HTML(paste0("Percentage of referrals who received a minimum of one year’s post-diagnostic support by Gender: ", 
                                input$select_hb_ijb_demo, ", Financial Year ", input$select_year_demo, ", SIMD Quintile: "
                                , input$select_simd_demo))
})

#percent met plot
output$plot_gender_ldp <- renderPlotly({
  percent_met_bar_chart(data_sex %>%
                          mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>%
                          filter(ijb == input$select_hb_ijb_demo, fy == input$select_year_demo, simd == input$select_simd_demo),
  x_text_angle = 0, fill = type)
})

#referrals trend plot title
# output$chart_title_gender_referrals_trend <- renderUI({HTML(paste0("Number of individuals diagnosed with dementia and referred for PDS by Gender - Trend; ", 
#                                                     input$select_hb_ijb_demo, ", SIMD Quintile: ", input$select_simd_demo))
# })

# #referrals trend plot
# output$plot_gender_referrals_trend <- renderPlotly({
# plot_trend_referrals(data_sex %>% 
# mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>%
#   filter(ijb == input$select_hb_ijb_demo, simd == input$select_simd_demo, type != "Unknown", type != "Not Specified"),
#   measure = total_referrals, group = type)
# })


# #percent met trend plot title
# output$chart_title_gender_ldp_trend <- renderUI({HTML(paste0("Percentage of referrals who received a minimum of one year’s PDS by Gender - Trend; ", 
#                                                                    input$select_hb_ijb_demo, ", SIMD Quintile: ", input$select_simd_demo))
# })

#percent met trend plot
# output$plot_gender_ldp_trend <- renderPlotly({
#   plot_trend(data_sex %>% 
#                          mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>%
#                          filter(ijb == input$select_hb_ijb_demo, simd == input$select_simd_demo, type != "Unknown", type != "Not Specified"),
#                        measure = percent_met, group = type)
# })



#2a  create table for age, simd, accommodation----

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


#2b create plots for age, simd, accommodation----
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

#referrals trend plot title
# output$chart_title_demo_referrals_trend <- renderUI({HTML(paste0("Number of individuals diagnosed with dementia and referred for PDS by ",
#                                                                    
#                                                                    
#                                                                    
#                                                                    if (input$select_data_demo == "data_age"){
#                                                              "Age Group"
#                                                            } else if(input$select_data_demo == "data_simd"){
#                                                              "Scottish Index of Multiple Deprivation (SIMD)"
#                                                            } else if(input$select_data_demo == "data_accom"){
#                                                              "Accommodation Type"
#                                                            },
#                                                                    
#                                                                    
#                                                                    
#                                                                    
#                                                                    " - Trend; ", 
#                                                                    input$select_hb_ijb_demo, ", Gender: ", input$select_sex_demo))
# })
# 
# #referrals trend plot
# output$plot_demo_referrals_trend <- renderPlotly({
#   plot_trend_referrals(data_selected() %>% 
#                          mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>%
#                          filter(ijb == input$select_hb_ijb_demo, sex == input$select_sex_demo, type != "Unknown"),# type != "Not Specified"),
#                        measure = total_referrals, group = type)
# })
# 

# #percent met trend plot title
# output$chart_title_demo_ldp_trend <- renderUI({HTML(paste0("Percentage of referrals who received a minimum of one year’s PDS by ",
#                                                              
#                                                              
#                                                              if (input$select_data_demo == "data_age"){
#                                                                "Age Group"
#                                                              } else if(input$select_data_demo == "data_simd"){
#                                                                "Scottish Index of Multiple Deprivation (SIMD)"
#                                                              } else if(input$select_data_demo == "data_accom"){
#                                                                "Accommodation Type"
#                                                              },
#                                                              
#                                                              
#                                                              " - Trend; ", 
#                                                              input$select_hb_ijb_demo, ", Gender: ", input$select_sex_demo))
# })
# 
# #percent met trend plot
# output$plot_demo_ldp_trend <- renderPlotly({
#   plot_trend(data_selected()%>% 
#                mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>%
#                filter(ijb == input$select_hb_ijb_demo, sex == input$select_sex_demo, type != "Unknown"),# type != "Not Specified"),
#              measure = percent_met, group = type)
# })


