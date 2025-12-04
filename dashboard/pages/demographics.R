####################### Page 4 DEMOGRAPHICS #######################
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
      p(paste0("ᴿ Figures for ", revised_year," have been revised and are now final. "),
               if(input$select_data_demo == "data_age"){    
                 ("Figures for 2021/22 have been revised to include the 11 additional 
                  records for Aberdeen City in the Total figure. Figures for the 
                  percentage of LDP standard achieved by age group for 2019/20 and 
                  2020/21 have also been revised to correct for the fact that 
                  previously published figures for these years erroneously included
                  the Aberdeen City referrals in the denominator when they should 
                  have been excluded. The corrected figures for each age group have 
                  been revised upwards (between 1 and 4 percentage points, except 
                  the 59 and under age group which hasn’t changed) for these years.")
               }else{
                 (" Figures for 2021/22 have been revised to include the 11 
                  additional records for Aberdeen City in the Total figure. 
                  Figures for the percentage of LDP standard achieved by SIMD 
                  quintile for 2019/20 and 2020/21 have also been revised to 
                  correct for the fact that previously published figures for 
                  these years erroneously included the Aberdeen City referrals 
                  in the denominator when they should have been excluded.  The 
                  corrected figures for each SIMD quintile have been revised 
                  upwards (between 1 and 4 percentage points, except for deprivation 
                  quintile 5 which has increased by 6.9 percentage points in 2019/20) 
                  for these years.")
               }
               )
        },
      p("For detailed information on how the Percentage LDP Standard Achieved is calculated, and how 'Standard Met', 'Exempt from Standard', 'PDS Ongoing' and 'Standard Not Met' are defined, please see the",
        a(
          href = "#",
          "Methodology",
          onclick = "Shiny.setInputValue('method_link', Math.random()); return false;"),
        "page."),	
      if(input$select_data_demo == "data_age"){
        p("Age is calculated as at the dementia diagnosis date. There are a small number of records with an incomplete date of birth and therefore the age group is unknown.")
      } else if(input$select_data_demo == "data_simd"){
        p("Deprivation is calculated by matching postcode to the Scottish Index of Multiple Deprivation (SIMD) quintiles. Each quintile consists of approximately 20% of the general population living in Scotland, 
          with deprivation quintile 1 indicating the 20% of the population living in the most deprived areas and deprivation quintile 5 indicating the 20% of the population living in the least deprived areas.
           The quintiles used in this report are from SIMD 2020v2. Further information on SIMD can be found on the",
          a("Scottish Government,", href ="https://www.alzscot.org/about-us/care/post-diagnostic-support/", target="_blank"),
          "website.",
        br(),
        "There are a small number of records where it was not possible to assign a deprivation category. Possible reasons for not being able to assign a deprivation category are that no postcode was provided 
          or the postcode provided is invalid, not in Scotland, or is a newly added postcode.")
      } else if(input$select_data_demo == "data_sex"){
        p("Gender is based on the sex recorded for each referral. There are a small number of records where sex is either not specified (includes refused/not provided) or not known (i.e. indeterminate sex, includes ‘Intersex’).   These records are not reported separately in the tables but are included in the Totals shown.")
      },
      p("Figures for 2018/19, 2019/20 and 2020/21 are affected by the 
        change in service provision of PDS within Aberdeen City during 
        2019. See Note 5 on the",
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
 
    plot_bar_perc_no_line(data_demo(), category = type, measure = referrals/sum(referrals)*100,
                          x_text_angle = if_else(input$select_data_demo == "data_age", 45, 0), 
                          fill = type, ylimit = (max(data_demo()$referrals)/sum(data_demo()$referrals))*100+1
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
  plot_bar_perc_no_line(data_demo(), category = type, measure = percent_met,
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
  
  df <- bind_rows(
    # breakdown of selected demographic
    data_demo() %>% 
      select(type, referrals, complete, exempt, ongoing, not_met, percent_met) %>% 
      arrange(type),
    #totals for final row
    data_demo() %>% 
      summarise(type = "Total",
                referrals = sum(referrals),
                complete = sum(complete),
                exempt = sum(exempt),
                ongoing = sum(ongoing),
                not_met = sum(not_met)) %>%
      mutate(percent_met = round(((complete + exempt)/(complete + exempt + not_met))*100, 1)) 
  ) %>% 
    mutate(perc_prop = round(100*referrals/max(referrals),1), .after = referrals) %>% 
    mutate(across(where(is.numeric), ~if_else(is.na(.), "-", format(., big.mark = ",")))) %>%
    mutate(across(starts_with("perc"), ~ if_else(grepl("-", .), ., paste0(.,"%")))) %>%
    # adds superscript R for NHS Grampian and incorrect formula revisions. 
    #From 2026 onward REMOVE the first if statement and keep the column names that are currently set as else----
    set_colnames(if((input$select_year_demo == "2019/20" | input$select_year_demo == "2020/21") & input$select_data_demo != "data_sex"){
    c(if(input$select_data_demo == "data_sex"){
                                                     "Gender"
                                     }else if(input$select_data_demo == "data_age"){
                                             "Age Group"
                                       }else{
                                      "Deprivation Quintile_"},
    
    "Number of People Referred to PDSᴿ", "Proportion of Total Referralsᴿ", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP Standard Achievedᴿ")
    }else{
      c(if(input$select_data_demo == "data_sex"){
        "Gender"
      }else if(input$select_data_demo == "data_age"){
        "Age Group"
      }else{
        "Deprivation Quintile_"},
      
      "Number of People Referred to PDS", "Proportion of Total Referrals", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP Standard Achieved")
    }
    )
  if (input$select_data_demo=='data_sex'){
    df <- df %>% filter(!Gender %in% c("Not Specified", "Unknown"))
  }
  return(df)
})


output$table_demo <- DT::renderDataTable({
  
  make_table(table_data_demo(),
             right_align = 1:7, ordering = FALSE, scrollY = FALSE, 
             selected = nrow(table_data_demo()))                                                                                                  
  
  
})


### download button data----
output$downloadData_demo <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(table_data_demo() %>%
                mutate(across(where(is.factor), ~as.character(.))) %>% 
                mutate(`Financial Year` = input$select_year_demo, 
                                           Geography = "Scotland",
                                           .before = everything()) %>% 
                mutate(`Financial Year` = case_when(
                  `Financial Year` == provisional_year_sup ~paste0(provisional_year,"P"),
                  `Financial Year` == revised_year_sup ~paste0(revised_year,"R"),
                  TRUE ~`Financial Year`)) %>%
                #### #changes superscript R to in line R for downloaded csv since superscript is not supported for NHS Grampian revision
                #REMOVE the next 7 lines from 2026 onwards----
                rename_with(
                if((input$select_year_demo == "2019/20" | input$select_year_demo == "2020/21") & input$select_data_demo != "data_sex"){
                 ~ "Number of People Referred to PDS(R)"
                }else{
                  ~ "Number of People Referred to PDS"
                }, .cols = 4
                ) %>% 
                #####changes superscript R to in line R for downloaded csv since superscript is not supported for NHS Grampian revision
                #REMOVE the next 7 lines from 2026 onwards----
              rename_with(
                if((input$select_year_demo == "2019/20" | input$select_year_demo == "2020/21") & input$select_data_demo != "data_sex"){
                  ~ "Proportion of Total Referrals(R)"
                }else{
                  ~ "Proportion of Total Referrals"
                }, .cols = 5
              ) %>% 
                #####changes superscript R to in line R for downloaded csv since superscript is not supported for NHS Grampian revision
                #REMOVE the next 7 lines from 2026 onwards----
              rename_with(
                if((input$select_year_demo == "2019/20" | input$select_year_demo == "2020/21") & input$select_data_demo != "data_sex"){
                  ~ "Percentage of LDP Standard Achieved(R)"
                }else{
                  ~ "Percentage of LDP Standard Achieved"
                }, .cols = last_col()
              ) %>% 
                #### adds revision and provisional note
                rbind(
                  if(input$select_year_demo == revised_year_sup){
                    c("Note: R indicates data has been revised. Please see dashboard for further information.",rep("",9))
                  }else if(input$select_year_demo == provisional_year_sup){
                    c("Note: P indicates data is provisional. Please see dashboard for further information.",rep("",9))
                    #REMOVE the following two lines from 2026 onward----
                  }else if((input$select_year_demo == "2019/20" | input$select_year_demo == "2020/21") & input$select_data_demo != "data_sex"){
                    c("Note: R indicates data has been revised. Please see dashboard for further information.",rep("",9))
                  }else{
                    rep("",10)
                  }
                ),
              file, row.names = FALSE)
  }
)

# the following dynamically updates the selection list to reflect that gender data from 2021/22
# has not been revised as it was not included in 2024 publication
# REMOVE this section from 2026 onwards----

observe({
  if(input$select_data_demo != "data_sex" & input$select_year_demo == "2021/22"){
    updateSelectInput(session,"select_year_demo",
                      label = "Select Financial Year of Diagnosis:",
                      choices = included_years_demographics,
                      selected = "2021/22ᴿ")
  }else if(input$select_data_demo != "data_sex" & input$select_year_demo != "2021/22"){
    updateSelectInput(session,"select_year_demo",
                      label = "Select Financial Year of Diagnosis:",
                      choices = included_years_demographics,
                      selected = input$select_year_demo)
  }else if(input$select_data_demo == "data_sex" & input$select_year_demo == "2021/22ᴿ"){
    updateSelectInput(session,"select_year_demo",
                      label = "Select Financial Year of Diagnosis:",
                      choices = included_years_2025_gender_wait,
                      selected = "2021/22")
  }else if(input$select_data_demo == "data_sex" & input$select_year_demo != "2021/22ᴿ"){
    updateSelectInput(session,"select_year_demo",
                      label = "Select Financial Year of Diagnosis:",
                      choices = included_years_2025_gender_wait,
                      selected = input$select_year_demo)
  }
})



### END OF SCRIPT ###
