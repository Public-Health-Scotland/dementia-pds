####################### Page 1: LDP Standard #######################
# UI ----
output$ldp_ui <-  renderUI({
  
  div(
    fluidRow(column(
      linebreaks(1),
      radioGroupButtons("ldp_tab", label = NULL, choices = ldp_tab_list,
                        status = "tab",
                        direction = "horizontal",
                        justified = T,
                        size = "normal"),
      width = 12)
    ), #fluidRow
    ##LDP PART 1----
        conditionalPanel(condition = 'input.ldp_tab == "ldp_part_1"',
                    # ## OUTCOMES BY YEAR----
                     conditionalPanel(condition = 'input.ldp_sidebar == "outcomes"',
                                      fluidRow(
                                        column(
                                          h3(strong(htmlOutput("title_part_1"))),
                                          linebreaks(1),          
                                          ### value box ----
                                          fluidRow(
                                            column(
                                            shinydashboard::valueBox(
                                              value = textOutput("scot_exp_perc"),
                                              subtitle = "of people estimated to be newly diagnosed with dementia were referred for post-diagnostic support.",
                                              width = 7,
                                              color = "blue"), #valueBox
                                            box(htmlOutput("scot_exp_text"),
                                                status = "primary", 
                                                title = (p(strong("How is this figure calculated?"))),
                                                width = 5), # box
                                            width = 12)#column
                                          ), #fluidRow
                                          fluidRow(column(
                                            linebreaks(1),
                                            ####plot ----
                                            h4(strong(htmlOutput("hb_exp_plot_title"))),
                                            plotlyOutput("hb_exp_plot"),
                                            ####  table ----
                                            h4(strong(htmlOutput("hb_exp_table_title"))),
                                            #####download button ldp1----
                                            downloadButton("downloadData_ldp1", 
                                                           "Download table data"),
                                            DT::dataTableOutput("table_hb_exp"),
                                            linebreaks(1),
                                            width = 12)
                                          ), # fluid Row
                                          p(paste0("Sources: Public Health Scotland quarterly dementia post-diagnostic support dataset: Data submissions from NHS Boards as at ",
                                                   format(end_date, "%d %B %Y"), "; Estimated and Projected Diagnosis Rates for Dementia in Scotland paper: 2014-2020; National Records of Scotland (NRS) mid-2021 population estimates.")),
                                          h4(strong("Notes:")),
                                          p(paste0("ᴾ Figures for ", provisional_year," are provisional subject to all service users completing their support.")),
                                          p(paste0("ᴿ Figures for ", revised_year," have been revised and are now final.")),
                                          p("The estimated number of people newly diagnosed with dementia is subject to the limitations detailed within the paper below published by the Scottish Government in 2016: ", 
                                            br(),
                                            a('Estimated and Projected Diagnosis Rates for Dementia in Scotland: 2014-2020', href = 'https://www.gov.scot/publications/estimated-projected-diagnosis-rates-dementia-scotland-2014-2020/'),
                                            br(),
                                             "Estimates are used as follows: calendar year 2016 estimates for 2016/17, calendar year 2017 estimates for 2017/18, calendar year 2018 estimates for 2018/19, calendar year 2019 estimates for 2019/20 and calendar year 2020 estimates for 2020/21.",
                                            br(),
                                            "For 2021/22 and 2022/23, the estimated number of people newly diagnosed with dementia has been calculated using the rates referenced in the paper above and the National Records of Scotland (NRS) mid-2021 population estimates. See Notes on Home page for further information."),					
                                          p("Figures for 2018/19, 2019/20 and 2020/21 for NHS Grampian and Scotland are affected by the change in service provision of PDS within Aberdeen City during 2019. See Notes on Home page for further information."),
                                          width = 12,
                                          style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available")
                                      ) #fluidRow
                     ), #cond panel outcomes
                     ##TRENDS ----
                     conditionalPanel(condition = 'input.ldp_sidebar == "trends"',
                                     fluidRow(
                                        column(
                                          ### plot ----
                                          h4(strong(htmlOutput("chart_title_trend_part_1"))),
                                          fluidRow(column(
                                            column(
                                              selectInput("select_hb_trend_part_1",
                                                          label = "Select Health Board to show in chart:",
                                                          choices = c("", boards), width = "100%"), width = 5),width = 12),
                                            ),
                                          plotlyOutput("trend_plot_part_1", height = "310px"),
                                          #linebreaks(1),
                                          ### table ----
                                          h4(strong(htmlOutput("table_title_hb_trend_part_1"))),
                                          #####download button ldp1 trend----
                                          downloadButton("downloadData_ldp1_trend", 
                                                         "Download table data"),
                                          DT::dataTableOutput("table_hb_trend_part_1"),
                                          linebreaks(1),
                                          width = 12,
                                          style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available"
                                          ), # column
                                      ) # fluidRow
                           ) #cond panel trends
    ), #cond panel part 1
    ## LDP PART 2 ----
    conditionalPanel(condition = 'input.ldp_tab == "ldp_part_2"',
                  #OUTCOMES BY YEAR----
                     conditionalPanel(condition = 'input.ldp_sidebar == "outcomes"',
                                      fluidRow(
                                        column(
                                          h3(strong(htmlOutput("title_part_2"))),
                                          linebreaks(1),
                                          ### value box ----
                                          fluidRow(column(
                                            shinydashboard::valueBox(
                                              value = textOutput("scot_pds_perc"),
                                              subtitle = "of those referred for post-diagnostic support received a minimum of 12 months of support.",
                                              width = 7,
                                              color = "blue"), #valueBox
                                            box(htmlOutput("scot_pds_text"),
                                                status = "primary",
                                                title = (p(strong("How is this figure calculated?"))),
                                                width = 5), #box
                                            width = 12)
                                          ), #fluidRow
                                          fluidRow(column(
                                            linebreaks(1),
                                            ###plot ----
                                            h4(strong(htmlOutput("perc_met_plot_title"))),
                                            plotlyOutput("perc_met_plot"),
                                            ### table ----
                                            h4(strong(htmlOutput("perc_met_table_title"))),
                                            #####download button ldp2----
                                            downloadButton("downloadData_ldp2", 
                                                           "Download table data"),
                                            DT::dataTableOutput("perc_met_table"),
                                            linebreaks(1),
                                            width = 12)
                                          ), # fluid Row
                                          width = 12,
                                          style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available"),
                                      ) #fluidRow
                     ), # cond panel outcomes
              ## TRENDS----
              conditionalPanel(condition = 'input.ldp_sidebar == "trends"', 
                               fluidRow(
                                 column(
                                   ###plot----
                                   h4(strong(htmlOutput("chart_title_trend_part_2"))),
                                   fluidRow(column(
                                              column(
                                       selectInput("select_hb_ijb_trend_part_2",
                                                   label = "Select Health Board/Integration Authority to show in chart:",
                                                   choices = c("", boards, ijb_list), width = "100%"), width = 5), width = 12)),
                                   plotlyOutput("trend_plot_part_2", height = "310px"),
                                   #linebreaks(1),
                                   ###table----
                                   h4(strong(htmlOutput("table_trend_part_2_title"))),
                                   radioButtons("select_table_trend_part_2",
                                                label = "In the table below show:",
                                                choices = c("Health Boards", "Integration Authority Areas"),
                                                selected = "Health Boards",
                                                inline = TRUE),
                                   #####download button ldp2 trend----
                                   downloadButton("downloadData_ldp2_trend", 
                                                  "Download table data"),
                                   DT::dataTableOutput("table_hb_ijb_trend_part_2"),
                                   linebreaks(1),
                                   width = 12,
                                   style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available"
                                   ), # column
                               ) # fluidRow
              ) #cond panel trends
    ) # cond panel part 2
               )# div
}) # renderUI

#SERVER ----

##part 1 title ----
output$title_part_1 <- renderUI({HTML(paste("Percentage of estimated diagnoses referred for PDS; Scotland, ", 
                                            input$select_year_p1))
})

##part 2 title ----
output$title_part_2 <- renderUI({HTML(paste("Percentage of referrals for PDS who received one year's support; Scotland, ", 
                                            input$select_year_p1))
})

##value boxes data ----
vb_data<- reactive({annual_table_data %>% filter(health_board == "Scotland", ijb == "Scotland", fy == input$select_year_p1, ldp == "total")}) 

## percentage of people estimated to be newly diagnosed with dementia were referred for post-diagnostic support
output$scot_exp_perc <- renderText({paste0(vb_data()$exp_perc, "%")})

output$scot_exp_text <- renderUI({
  HTML(paste("A total of", "<b>",  prettyNum(vb_data()$referrals, big.mark = ","), "</b>", "referrals were made to post-diagnostic support. This is divided by",
  "<b>", prettyNum(vb_data()$diagnoses, big.mark = ","), "</b>", "the estimated number of people newly diagnosed with dementia."))})

## percentage of those referred for post-diagnostic support received a minimum of 12 months of support
output$scot_pds_perc <- renderText({paste0(vb_data()$percent_met, "%")})

vb_2_data<- reactive({annual_table_data %>% filter(health_board == "Scotland", ijb == "Scotland", fy == input$select_year_p1,
                                                   ldp != "fail") %>% select(-diagnoses, -exp_perc) %>% 
                                            pivot_wider(values_from = referrals, names_from = ldp)})

output$scot_pds_text <- renderUI({
  HTML(paste("<b>", prettyNum(vb_2_data()$complete + vb_2_data()$exempt, big.mark = ","), "</b>", "referrals either met or were exempt from the LDP standard. This is divided by",
             "<b>", prettyNum(vb_2_data()$total - vb_2_data()$ongoing, big.mark = ","), "</b>", "the total number of referrals (excluding those whose support is ongoing)."))})


## plot ldp part 1 ----
output$hb_exp_plot_title <- renderUI({HTML(paste0("Percentage of people estimated to be newly diagnosed with dementia who were referred for PDS; ", 
                                                      input$select_year_p1, ", Scotland and Health Boards"))
})

output$hb_exp_plot <- renderPlotly({
  percent_bar_chart(annual_table_data %>% filter(grepl("NHS", ijb) | ijb == "Scotland", fy == input$select_year_p1, ldp == "total") %>% 
                          mutate(colour = if_else(ijb == "Scotland", "B", "A")), 
                        category = ijb, 
                        measure = exp_perc,
                       fill = colour)
})



## data table ldp part 1 ----
output$hb_exp_table_title <- renderUI({HTML(paste0("Number and percentage of people estimated to be newly diagnosed with dementia who were referred for PDS; ", 
                                                      input$select_year_p1, ", Scotland and Health Boards"))
})

table_hb_exp_data <- reactive({
  annual_table_data %>% 
    filter(fy == input$select_year_p1) %>%
    filter(grepl("NHS", ijb) | ijb == "Scotland", !is.na(diagnoses)) %>% 
    group_by(health_board)%>%
    select(health_board, diagnoses, referrals)%>%
    mutate(exp_perc = paste0(round(referrals/diagnoses*100, 1), "%")) %>%  
    arrange(health_board) %>% 
    set_colnames(c("Health Board","Estimated Number of People Newly Diagnosed with Dementia", "Number of People Referred to PDS","Percentage of Estimated Number of People Diagnosed with Dementia Referred to PDS"))
})

output$table_hb_exp <- DT::renderDataTable({
  make_table(table_hb_exp_data(), right_align = 1:3, selected = 1, filename = paste0("pds_perc_of_expected_diagnoses_", input$select_year_p1)) %>%
               formatCurrency(c(2,3), currency = "", interval = 3, mark = ",", digits = 0)
})

### download button ldp1----
output$downloadData_ldp1 <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(table_hb_exp_data() %>% mutate(financial_year = input$select_year_p1, 
                                           .before = everything()) %>% 
                mutate(financial_year = case_when(
                  financial_year == provisional_year_sup ~paste0(provisional_year,"P"),
                  financial_year == revised_year_sup ~paste0(revised_year,"R"),
                  TRUE ~financial_year)),
              file, row.names = FALSE)
  }
)

##plot trends part 1----

output$chart_title_trend_part_1 <- renderUI({HTML(paste0("Percentage of people estimated to be newly diagnosed with dementia who were referred for PDS; Trend, Scotland "),
                                                  if(input$select_hb_trend_part_1 == ""){""
                                                  }else{
                                                    paste0("and ", input$select_hb_trend_part_1)})
})

trend_chart_data_part_1 <- reactive({
  annual_table_data %>%
    filter(fy %in% included_years_sup, ldp == "total") %>% 
    filter(ijb == input$select_hb_trend_part_1 | ijb == "Scotland")
})

output$trend_plot_part_1 <- renderPlotly({
  plot_trend_perc(trend_chart_data_part_1(), exp_perc)
})

## data table trends part 1----     

output$table_title_hb_trend_part_1 <- renderUI({HTML(paste0("Percentage of people estimated to be newly diagnosed with dementia who were referred for PDS; Trend, Scotland and Health Boards"))})

table_hb_trend_part_1_data <- reactive({
  annual_table_data %>% 
    filter(fy %in% included_years_sup, ldp == "total") %>% 
    select(health_board, fy, exp_perc) %>%
    mutate(exp_perc = paste0(exp_perc, "%")) %>% 
    distinct(health_board, fy, .keep_all = T) %>% 
    rename("Health Board" = "health_board")  
})

output$table_hb_trend_part_1 <- DT::renderDataTable({
  make_table(table_hb_trend_part_1_data() %>% 
               pivot_wider(names_from = fy, values_from = exp_perc),
             right_align = 1:length(included_years_sup), selected = 1, filename = paste0("pds_perc_of_expected_diagnoses_trend"))
})


### download button ldp1 trend----
output$downloadData_ldp1_trend <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(table_hb_trend_part_1_data() %>% 
                mutate(fy = case_when(
                  fy == provisional_year_sup ~paste0(provisional_year,"P"),
                  fy == revised_year_sup ~paste0(revised_year,"R"),
                  TRUE ~fy))  %>% 
                pivot_wider(names_from = fy, values_from = exp_perc) %>%
                mutate(measure = "Percentage of Estimated Number of People Diagnosed with Dementia Referred to PDS", 
                                             .before = everything()), 
              file, row.names = FALSE)
  }
)



##plot ldp part 2 ----
output$perc_met_plot_title <- renderUI({HTML(paste0("Percentage of people referred for PDS who received a minimum of one year’s support within 12 month's of diagnosis; ", 
                                                    input$select_year_p1, ", Scotland and ", input$select_hb_ijb))
})

output$perc_met_plot <- renderPlotly({
  
  if(input$select_hb_ijb == "Health Boards"){
    
    percent_bar_chart(annual_table_data %>% filter(grepl("NHS", ijb) | ijb == "Scotland", fy == input$select_year_p1, ldp == "total") %>% 
                        mutate(colour = if_else(ijb == "Scotland", "B", "A")), 
                      category = ijb, 
                      measure = percent_met,
                      fill = colour)
    
  }else{
    
    percent_bar_chart(annual_table_data %>% filter(!grepl("NHS", ijb), fy == input$select_year_p1, ldp == "total") %>% 
                        mutate(colour = if_else(ijb == "Scotland", "B", "A")), 
                      category = ijb, 
                      measure = percent_met,
                      fill = colour)
  }
    
  })
    
    ## data table lpd part 2 ----
    output$perc_met_table_title <- renderUI({HTML(paste0("Number and percentage of people referred for PDS who received a minimum of one year’s support within 12 month's of diagnosis; ", 
                                                       input$select_year_p1, ", Scotland and ", input$select_hb_ijb))
    })


table_ldp2_data <- reactive({
  
  if(input$select_hb_ijb == "Health Boards"){
    
   annual_table_data %>% 
      filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
      filter(fy == input$select_year_p1) %>%
      select(health_board,ldp,referrals,percent_met)%>%
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
      mutate(percent_met = if_else(percent_met == "   NA", "-", paste0(percent_met, "%"))) %>% 
      pivot_wider(names_from=ldp,values_from=referrals) %>% 
      select(health_board, total, complete, exempt, ongoing, fail, percent_met) %>% 
      arrange(health_board) %>% 
      set_colnames(c("Health Board","Number of People Referred to PDS", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved")) 
    
  }else{
    
  annual_table_data %>%
      filter(!grepl("NHS", ijb)) %>% 
      filter(fy == input$select_year_p1) %>%
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
      mutate(percent_met = if_else(percent_met == "   NA", "-", paste0(percent_met, "%"))) %>% 
      mutate(referrals = if_else(fy %in% c("2019/20", "2020/21") & ijb == "Aberdeen City" & ldp != "total", "-", as.character(referrals))) %>% 
      select(ijb,ldp,referrals,percent_met) %>%
      pivot_wider(names_from=ldp,values_from=referrals) %>% 
      select(ijb, total, complete, exempt, ongoing, fail, percent_met) %>% 
      arrange(ijb) %>% 
      set_colnames(c("Integration Authority Area","Number of People Referred to PDS", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved"))
  
  }
  
})
    
    output$perc_met_table <- DT::renderDataTable({
      
          make_table(table_ldp2_data(), right_align = 1:6, selected = 1, rows_to_display = 32, filename = paste0("pds_perc_standard_met_exempt_iaa_", input$select_year_p1))
        
          })
    
    
    ### download button ldp2----
    output$downloadData_ldp2 <- downloadHandler(
      filename = paste0("pds_data_as_at_", end_date, ".csv"),
      content = function(file) {
        write.csv(table_ldp2_data() %>% mutate(financial_year = input$select_year_p1, 
                                                 .before = everything()) %>% 
                    mutate(financial_year = case_when(
                      financial_year == provisional_year_sup ~paste0(provisional_year,"P"),
                      financial_year == revised_year_sup ~paste0(revised_year,"R"),
                      TRUE ~financial_year)),
                  file, row.names = FALSE)
      }
    )
    


##plot trends part 2 ----

output$chart_title_trend_part_2 <- renderUI({HTML(paste("Percentage of people referred for PDS who received a minimum of one year’s support within 12 months of diagnosis; Trend, Scotland "),
                                                        if(input$select_hb_ijb_trend_part_2 == ""){""
                                                        }else{
                                                          paste0("and ", input$select_hb_ijb_trend_part_2)})
})

trend_chart_data <- reactive({
  annual_table_data %>%
    filter(fy %in% included_years_sup, ldp == "total") %>% 
    filter(ijb == input$select_hb_ijb_trend_part_2 | ijb == "Scotland")})


output$trend_plot_part_2 <- renderPlotly({
  plot_trend_perc(trend_chart_data(), percent_met)
})



##data table trends part 2----    

output$table_trend_part_2_title <- renderUI({
  HTML(paste0("Percentage of people referred for PDS who received a minimum of one year’s support within 12 months of diagnosis; Trend, Scotland and ", input$select_table_trend_part_2))
})

table_trend_part_2_data <- reactive({
  
  
  if(input$select_table_trend_part_2 == "Health Boards"){  
    
    trend_hb_data <- annual_table_data %>% 
      filter(fy %in% included_years_sup) %>% 
      select(health_board, fy, percent_met) %>%
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
      mutate(percent_met = if_else(percent_met == "   NA", "-", paste0(percent_met, "%"))) %>% 
      distinct(health_board, fy, .keep_all = T) %>% 
      rename("Health Board" = "health_board") 
  
  }else{
    
    trend_ijb_data <- annual_table_data %>% 
      filter(fy %in% included_years_sup) %>% 
      filter(!grepl("NHS", ijb)) %>% 
      select(ijb, fy, percent_met) %>%
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
      mutate(percent_met = if_else(percent_met == "   NA", "-", paste0(percent_met, "%"))) %>% 
      distinct(ijb, fy, .keep_all = T) %>% 
      rename("Integration Authority Area" = "ijb") 
   
  }
  
})

output$table_hb_ijb_trend_part_2 <- DT::renderDataTable({
 
    make_table(table_trend_part_2_data() %>% 
                 pivot_wider(names_from = fy, values_from = percent_met),
               right_align = 1:length(included_years_sup), selected = 1, rows_to_display = 32, filename = paste0("pds_perc_met_standard_exempt_iaa_trend"))
  
})

### download button ldp2 trend----
output$downloadData_ldp2_trend <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(table_trend_part_2_data() %>% 
                mutate(fy = case_when(
                  fy == provisional_year_sup ~paste0(provisional_year,"P"),
                  fy == revised_year_sup ~paste0(revised_year,"R"),
                  TRUE ~fy)) %>% 
                pivot_wider(names_from = fy, values_from = percent_met) %>% 
                mutate(measure = "Percentage of LDP standard achieved", 
                         .before = everything()), 
              file, row.names = FALSE)
  }
)

### END OF SCRIPT ###