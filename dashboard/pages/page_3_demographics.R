####################### Page 3 DEMOGRAPHICS #######################
#UI----
output$demo_ui <-  renderUI({
  div(
    # age, simd, gender ----
                fluidRow(
                    column(
                      #plots----
                      fluidRow(
                        column(
                          h3(strong(htmlOutput("chart_title_demo_referrals"))),
                          width = 6),
                        column(
                          h3(strong(htmlOutput("chart_title_demo_ldp"))),
                          width = 6)
                      ), #fluidRow
                      fluidRow(                      
                        column(
                          plotlyOutput("plot_demo_referrals"), width = 6),
                        column(
                          plotlyOutput("plot_demo_ldp"), width = 6),
                        linebreaks(1)
                      ), #fluidRow
                      # table ----
                          h3(strong(htmlOutput("table_title_demo"))),
                          DT::dataTableOutput("table_demo"),
      linebreaks(1),
                          
                          width = 12,
                          style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; margin-left: 1px; height:-webkit-fill-available; background-color: white") 
                     ), # fluid row
           ) # div
}) # renderUI

# SERVER ----

# create plots for age, simd, gender----
#chart 1 title
output$chart_title_demo_referrals <- renderUI({HTML(paste0("Proportion of total referrals for PDS by ", 
                                                           
                                                           if (input$select_data_demo == "data_age"){
                                                             "Age Group"
                                                           } else if(input$select_data_demo == "data_simd"){
                                                             "Deprivation Quintile"
                                                           } else if(input$select_data_demo == "data_sex"){
                                                             "Gender"
                                                           },
                                                           ": ", "Scotland", ", Financial Year ", input$select_year_demo
)
)
})

#plot proportion
output$plot_demo_referrals <- renderPlotly({
  percent_bar_chart(data_demo(), category = type, measure = total_referrals/sum(total_referrals)*100,
                       x_text_angle = if_else(input$select_data_demo == "data_age", 45, 0), fill = type, limit = NA
                      )
})

# chart 2 title
output$chart_title_demo_ldp <- renderUI({HTML(paste0("Percentage of referrals who received a minimum of one 
year’s post-diagnostic support by ", 

if (input$select_data_demo == "data_age"){
  "Age Group"
} else if(input$select_data_demo == "data_simd"){
  "Deprivation Quintile"
} else if(input$select_data_demo == "data_sex"){
  "Gender"
},
": ", "Scotland", ", Financial Year ", input$select_year_demo
)
)
})


#plot outcomes
output$plot_demo_ldp <- renderPlotly({
  percent_bar_chart(data_demo(), category = type, measure = percent_met,
                        x_text_angle = if_else(input$select_data_demo == "data_age", 45, 0),
                        fill = type)
})

#create table for age, simd, gender----

#filter data
data_selected <-reactive({
  get(input$select_data_demo)
})

data_demo <- reactive({data_selected() %>%
    mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>%
    filter(ijb == "Scotland", fy == input$select_year_demo)
})
# table title
output$table_title_demo <- renderUI({HTML(paste0("Number and percentage of people referred for PDS who received a minimum of one year’s support by ", 
                                                 
                                                 if (input$select_data_demo == "data_age"){
                                                   "Age Group"
                                                 } else if(input$select_data_demo == "data_simd"){
                                                   "Deprivation Quintile"
                                                 } else if(input$select_data_demo == "data_sex"){
                                                   "Gender"
                                                 },
                                                 ": ", "Scotland", ", Financial Year ", input$select_year_demo
)
)
})


output$table_demo <- DT::renderDataTable({
  
  table_data_demo <- bind_rows(
    
    data_demo() %>% 
      select(type, total_referrals, complete, exempt, ongoing, not_met, percent_met) %>% 
      arrange(type),
    
    data_demo() %>% 
      filter(ijb == "Scotland",
             fy == input$select_year_demo
      ) %>% 
      summarise(type = "Total",
                total_referrals = sum(total_referrals),
                complete = sum(complete),
                exempt = sum(exempt),
                ongoing = sum(ongoing),
                not_met = sum(not_met)) %>%
      mutate(percent_met = round(((complete + exempt)/(complete + exempt + not_met))*100, 1)) 
  ) %>% 
    mutate(perc_prop = round(100*total_referrals/max(total_referrals),1), .after = total_referrals) %>% 
    mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>%
    mutate(across(starts_with("perc"), ~ paste0(.,"%"))) %>% 
    set_colnames(c(" ","Number of People Referred to PDS", "Proportion of Total Referrals", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved"))
  make_table(table_data_demo, right_align = 1:7, table_elements = "t", ordering = FALSE, selected = nrow(table_data_demo)) 
  
})


