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
      if(input$select_data_demo == "data_simd"){
         p(paste0("Sources: Public Health Scotland quarterly dementia post-diagnostic support dataset: Data submissions from NHS Boards as at ",
               format(end_date, "%d %B %Y"),"; Scottish Government Scottish Index of Multiple Deprivation (SIMD) 2020."))
        }else{
         p(paste0("Source: Public Health Scotland quarterly dementia post-diagnostic support dataset: Data submissions from NHS Boards as at ",
                  format(end_date, "%d %B %Y")))
        },
      ## notes----
      h4(strong("Notes:")),
      p(paste0("ᴾ Figures for ", provisional_year," are provisional subject to all service users completing their support.")),
      if(input$select_data_demo != "data_sex"){
      p(paste0("ᴿ Figures for ", revised_year," have been revised and are now final."))
        },
      p("For detailed information on how the % LDP standard achieved is calculated, and how 'standard met', 'exempt from standard', 'PDS ongoing' and 'standard not met' are defined, please see the",
        a(
          href = "#",
          "Methodology",
          onclick = "Shiny.setInputValue('method_link', Math.random()); return false;"),
        "page."),	
      if (input$select_data_demo == "data_age"){
        p("Age is calculated as at the dementia diagnosis date. There are a small number of records with an incomplete date of birth and therefore the age group is unknown.")
      } else if(input$select_data_demo == "data_simd"){
        p("Deprivation is calculated by matching postcode to the Scottish Index of Multiple Deprivation (SIMD) quintiles. Each quintile consists of approximately 20% of the general population living in Scotland, 
          with deprivation quintile 1 indicating the 20% of the population living in the most deprived areas and deprivation quintile 5 indicating the 20% of the population living in the least deprived areas.
          There are a small number of records where it was not possible to assign a deprivation category. Possible reasons for not being able to assign a deprivation category are that no postcode was provided 
          or the postcode provided is invalid, not in Scotland, or is a newly added postcode.")
      } else if(input$select_data_demo == "data_sex"){
        p("Gender is based on the sex recorded for each referral. There are a small number of records where sex is either not specified (includes refused/not provided) or not known (i.e. indeterminate sex, includes ‘Intersex’).")
      },
      p("Figures for 2018/19, 2019/20 and 2020/21 for NHS Grampian and Scotland are affected by the change in service provision of PDS within Aberdeen City during 2019. See Note 5 on the",
        a(
          href = "#",
          "Home",
          onclick = "Shiny.setInputValue('home_link', Math.random()); return false;"),
        "page for further information."),
                          
                          width = 12,
                          #fix panel so sidebar and navigation bar do not scroll with content
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
   filter(fy == input$select_year_demo)
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
  
    bind_rows(
    # breakdown of selected demographic
    data_demo() %>% 
      select(type, total_referrals, complete, exempt, ongoing, not_met, percent_met) %>% 
      arrange(type),
    #totals for final row
    data_demo() %>% 
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
  
  make_table(table_data_demo(), right_align = 1:7, ordering = FALSE, scrollY = FALSE, 
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

# the following dynamically updates the selection list to reflect that gender data from 2021/22
# has not been revised as it was not included in 2024 publication
# REMOVE this section from 2026 onwards----

observe({
  if(input$select_data_demo != "data_sex" & input$select_year_demo == "2021/22")
   {
    updateSelectInput(session,"select_year_demo",
                      label = "Select Financial Year of Diagnosis:",
                      choices = included_years,
                      selected = "2021/22ᴿ")
  }else if(input$select_data_demo != "data_sex" & input$select_year_demo != "2021/22"){
    updateSelectInput(session,"select_year_demo",
                      label = "Select Financial Year of Diagnosis:",
                      choices = included_years,
                      selected = input$select_year_demo)
  }else if(input$select_data_demo == "data_sex" & input$select_year_demo == "2021/22ᴿ"){
    updateSelectInput(session,"select_year_demo",
                      label = "Select Financial Year of Diagnosis:",
                      choices = included_years_sup,
                      selected = "2021/22")
  }else if(input$select_data_demo == "data_sex" & input$select_year_demo != "2021/22ᴿ"){
    updateSelectInput(session,"select_year_demo",
                      label = "Select Financial Year of Diagnosis:",
                      choices = included_years_sup,
                      selected = input$select_year_demo)
  }
})



### END OF SCRIPT ###
