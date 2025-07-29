####################### Page 3 DEMOGRAPHICS #######################
#UI----
output$demo_ui <-  renderUI({
  div(
    ## age, simd, gender ----
                fluidRow(
                    column(
                      ##plots----
                      fluidRow(
                        column(
                          h4(strong(htmlOutput("chart_title_demo_referrals"))),
                          width = 6),
                        column(
                          h4(strong(htmlOutput("chart_title_demo_ldp"))),
                          width = 6)
                      ), #fluidRow
                      fluidRow(                      
                        column(
                          plotlyOutput("plot_demo_referrals", height = "300px"), width = 6),
                        column(
                          plotlyOutput("plot_demo_ldp", height = "300px"), width = 6),
                        linebreaks(1)
                      ), #fluidRow
                          ## table ----
                          h4(strong(htmlOutput("table_title_demo"))),
                          ###download button----
                          downloadButton("downloadData_demo", 
                                     "Download table data"),
                          DT::dataTableOutput("table_demo"),
      linebreaks(1),
                          
                          width = 12,
                          style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; margin-left: 1px; height:-webkit-fill-available; background-color: white") 
                     ), # fluid row
           ) # div
}) # renderUI

# SERVER ----

#select data from sidebar
data_selected <-reactive({
  get(input$select_data_demo)
})

data_demo <- reactive({
  data_selected() %>%
    mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>%
    filter(ijb == "Scotland", fy == input$select_year_demo)
})

## create plots for age, simd, gender----
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

##create table for age, simd, gender----


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

#filter data
table_data_demo <- reactive({
  
  #table_data_demo <-
    bind_rows(
    
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
    mutate(across(where(is.numeric), ~if_else(is.na(.), "-", format(., big.mark = ",")))) %>%
    mutate(across(starts_with("perc"), ~ if_else(grepl("-", .), ., paste0(.,"%")))) %>% 
    set_colnames(c(if(input$select_data_demo == "data_sex"){
                                                     "Gender"
                                     }else if(input$select_data_demo == "data_age"){
                                             "Age Group"
                                       }else{
                                      "Deprivation Quintile_"},
    
    "Number of People Referred to PDS", "Proportion of Total Referrals", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved"))
})


output$table_demo <- DT::renderDataTable({
  
  make_table(table_data_demo(), right_align = 1:7, ordering = FALSE, 
             selected = nrow(table_data_demo()))                                                                                                  
  
  
})


### download button----
output$downloadData_demo <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(table_data_demo() %>% mutate(financial_year = input$select_year_demo, 
                                           geography = "Scotland",
                                           .before = everything()) %>% 
                mutate(financial_year = case_when(
                  financial_year == provisional_year_sup ~paste0(provisional_year,"P"),
                  financial_year == revised_year_sup ~paste0(revised_year,"R"),
                  TRUE ~financial_year)),
              file, row.names = FALSE)
  }
)

### END OF SCRIPT ###
