####################### Page 1 REFERRALS & RATES #######################
#UI ----
output$rates_ui <-  renderUI({
  
  div(
    fluidRow(column(
    linebreaks(1),
    radioGroupButtons("RandR_tab", label = NULL, choices = RandR_tab_list,
                      status = "tab",
                      direction = "horizontal",
                      justified = T,
                      size = "normal"),
    width = 12)
  ), #fluidRow
  ##TOTAL Referrals----
  conditionalPanel(condition = 'input.RandR_tab == "RandR_totals"',
                   column(
                     ### REFERRALS BY YEAR----
                     conditionalPanel(condition = 'input.RandR_sidebar == "referrals"',
                                      fluidRow(
                                          h3(strong(htmlOutput("title_totals_randr"))),
                                          linebreaks(1),          
                                          #### value box ----
                                          fluidRow(
                                              shinydashboard::valueBox(
                                              value = textOutput("scot_randr"),
                                              subtitle = "people were diagnosed with dementia and referred for post-diagnostic support.",
                                              width = 12,
                                              color = "fuchsia"), #valueBox
                                            # box(htmlOutput("scot_randr_text"),
                                            #     status = "primary", 
                                            #     title = (p(strong("How is this figure calculated?"))),
                                            #     width = 5), # box
                                      
                                          ), #fluidRow
                                          fluidRow(
                                            column(
                                                   linebreaks(1),
                                                   ####plot ----
                                                   h4(strong(htmlOutput("totals_RandR_plot_title"))),
                                                   plotlyOutput("totals_RandR_plot"),
                                                   ####  table ----
                                                   h4(strong(htmlOutput("totals_randr_table_title"))),
                                                   #####download button totals----
                                                   downloadButton("downloadData_totals", 
                                                                  "Download table data"),
                                                   DT::dataTableOutput("table_totals_randr"),
                                                   linebreaks(1),
                                                   width = 12) #column
                                          ), # fluid Row
                                      ) #fluidRow
                     ), #cond panel referrals
                     ###TRENDS ----
                     conditionalPanel(condition = 'input.RandR_sidebar == "trends"',
                                      fluidRow(
                                          #### plot ----
                                          h4(strong(htmlOutput("randr_chart_title_trend_totals"))),
                                          plotlyOutput("hb_RandR_trend_plot"),
                                          linebreaks(1),
                                          #### table ----
                                          h4(strong(htmlOutput("randr_table_title_trend_totals"))),
                                          #####download button totals trend----
                                          downloadButton("downloadData_totals_trend", 
                                                         "Download table data"),
                                          DT::dataTableOutput("randr_table_trend_totals"),
                                          linebreaks(1),
                                        ) # fluidRow
                        ), #cond panel trends
                    p("Source: Public Health Scotland quarterly dementia post-diagnostic support dataset: Data submissions from NHS Boards as at ",
                     format(end_date, "%d %B %Y")),
                   ### Notes----
                   h4(strong("Notes:")),
                  p(paste0("ᴾ Figures for ", provisional_year, " and ", extra_referrals_year, " are provisional subject to all service users completing their support.")),
                   p(paste0("ᴿ Figures for ", revised_year," have been revised and are now final."),
                    em("Due to the discovery of previously unpublished data submitted by NHS Grampian, revisions have been 
                    made in this publication for diagnoses in financial year 2020/21. The impact of this is that the Number of 
                    People Referred to PDS has increased by 11 for Aberdeen City, NHS Grampian and Scotland.")),
                      
              p("Figures for 2018/19, 2019/20 and 2020/21 for Aberdeen City, NHS Grampian and Scotland are affected by the change in service provision of PDS within Aberdeen City during 2019. See Note 5 on the",
                 a(
                        href = "#",
                    "Home",
                      onclick = "Shiny.setInputValue('home_link', Math.random()); return false;"),
                   "page for further information."),
                        width = 12,
                        #fix panel so sidebar and navigation bar do not scroll with content
                        style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available"
                )# column
      ), #cond panel totals
                   ## RATES ----
                   conditionalPanel(condition = 'input.RandR_tab == "RandR_rates"',
                                    column(
                                    ### REFERRALS BY YEAR----
                                    conditionalPanel(condition = 'input.RandR_sidebar == "referrals"',
                                                     fluidRow(
                                                         h3(strong(htmlOutput("randr_title_rates"))),
                                                         linebreaks(1),
                                                         #### value box ----
                                                         fluidRow(
                                                           shinydashboard::valueBox(
                                                             value = textOutput("scot_rate"),
                                                             subtitle = "people per 10,000 population (aged 65 and over) were diagnosed with dementia and referred for post-diagnostic support.",
                                                             width = 12,
                                                             color = "fuchsia"), #valueBox
                                                           # box(htmlOutput("scot_referrals_text"),
                                                           #     status = "primary",
                                                           #     title = (p(strong("How is this figure calculated?"))),
                                                           #     width = 5), #box
                                                         ), #fluidRow
                                                         fluidRow(
                                                           column(
                                                           linebreaks(1),
                                                           ####plot ----
                                                           h4(strong(htmlOutput("rates_plot_title"))),
                                                           plotlyOutput("rates_plot"),
                                                           #### table ----
                                                           h4(strong(htmlOutput("rates_table_title"))),
                                                           DT::dataTableOutput("rates_table"),
                                                           linebreaks(1),
                                                         width = 12)#column
                                                         ), # fluid Row
                                                     ) #fluidRow
                                    ), # cond panel referrals
                                    ### TRENDS----
                                    conditionalPanel(condition = 'input.RandR_sidebar == "trends"', 
                                                     fluidRow(
                                                         ####plot----
                                                         h4(strong(htmlOutput("randr_chart_title_trend_rates"))),
                                                         plotlyOutput("randr_trend_plot_rates"),
                                                         linebreaks(1),
                                                         ####table----
                                                         h4(strong(htmlOutput("randr_table_trend_rates_title"))),
                                                         DT::dataTableOutput("randr_table_trend_rates"),
                                                         linebreaks(1)
                                                     ) # fluidRow
                                    ),#cond panel trends
                                    p(paste0("Sources: Public Health Scotland quarterly dementia post-diagnostic support dataset: Data submissions from NHS Boards as at ",
                                      format(end_date, "%d %B %Y"), "; National Records of Scotland (NRS) mid-2021, mid-2022, and mid-2023 population estimates.")),
                                    ### Notes----
                                    h4(strong("Notes:")),
                                    p(paste0("ᴾ Figures for ", provisional_year, " and ", extra_referrals_year, " are provisional subject to all service users completing their support.")),
                                    #p(paste0("ᴿ Figures for ", revised_year," have been revised and are now final."), UNCOMMENT for 2026 publication
                                    p("Rates are calculated using the NRS mid-year population estimates of the 65 and over age group for each geographical area. See Note 8 on the",
                                      a(
                                        href = "#",
                                        "Home",
                                        onclick = "Shiny.setInputValue('home_link', Math.random()); return false;"),
                                      "page for further information."),
                                      p("Figures for 2018/19, 2019/20 and 2020/21 for Aberdeen City, NHS Grampian and Scotland are affected by the change in service provision of PDS within Aberdeen City during 2019. See Note 5 on the",
                                      a(
                                        href = "#",
                                        "Home",
                                        onclick = "Shiny.setInputValue('home_link', Math.random()); return false;"),
                                      "page for further information."),
                                    width = 12,
                                    style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available"
                                    ), # column
                   ) # cond panel rates
  )# div
  }) # renderUI
  
  #SERVER ----

  ## TOTAL Referrals----
  ## REFERRALS BY YEAR----
  ### total referrals title ----
  output$title_totals_randr <- renderUI({HTML(paste("Number of People Referred for PDS; Scotland, ", 
                                                    input$select_year_randr))
  })
  
  
  ### value boxes data ----
  vb_data_totals<- reactive({annual_table_data %>% filter(health_board == "Scotland", ijb == "Scotland", fy == input$select_year_randr, ldp == "total")}) 
  
  # total number of people diagnosed and referred for post-diagnostic support
  output$scot_randr <- renderText({prettyNum(vb_data_totals()$referrals, big.mark = ",")})
  
  # output$scot_randr_text <- renderUI({
  #   HTML(paste("A total of", "<b>",  prettyNum(vb_data()$referrals, big.mark = ","), "</b>", "referrals were made to post-diagnostic support. This is divided by",
  #              "<b>", prettyNum(vb_data()$diagnoses, big.mark = ","), "</b>", "the estimated number of people newly diagnosed with dementia."))})
  
  
  ### plot total referrals bar chart ----
  output$totals_RandR_plot_title <- renderUI({HTML(paste0("Number of people diagnosed with dementia who were referred for PDS; ",
                                                          input$select_year_randr, ", " ,input$select_hb_ijb_randr))
  })
  
  output$totals_RandR_plot <- renderPlotly({
    if(input$select_hb_ijb_randr == "Health Boards"){
      
      plot_bar_no_line(annual_table_data %>% filter(grepl("NHS", ijb), fy == input$select_year_randr, ldp == "total") %>% 
                         rename(geog = health_board),
                       ytitle = "Referrals",
                       measure = referrals, measure_text = "Number of referrals to PDS: ")
      
    }else{
      plot_bar_no_line(annual_table_data %>% filter((!grepl(("NHS|Scotland"), annual_table_data$ijb)),fy == input$select_year_randr, ldp == "total") %>% 
                         rename(geog = ijb),
                       ytitle = "Referrals",
                       measure = referrals, measure_text = "Number of referrals to PDS: ")
    } 
  })
  
  ### data table total referrals ----
  output$totals_randr_table_title <- renderUI({HTML(paste0("Number of people diagnosed with dementia who were referred for PDS; ", 
                                                           input$select_year_randr, ", Scotland and ", input$select_hb_ijb_randr))
  })
  
  table_totals_data <- reactive({
    
    if(input$select_hb_ijb_randr == "Health Boards"){
      
      annual_table_data %>%
        filter(fy == input$select_year_randr, ldp == "total") %>%
        select(ijb,referrals)%>%
        mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
        filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
        arrange(ijb) %>% 
        set_colnames(
          # adds superscript R for NHS Grampian revisions.
          #From 2026 onward REMOVE the if statement and keep the column names that are currently set as else
          if(input$select_year_randr == "2020/21"){
            c("Health Board","Number of People Referred to PDSᴿ")
          }else{
            c("Health Board","Number of People Referred to PDS")
          }
        )
      
    }else{
      
      annual_table_data %>%
        filter(fy == input$select_year_randr, ldp == "total") %>%
        select(ijb,referrals)%>%
        mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
        filter(!grepl("NHS", ijb)) %>% 
        arrange(ijb) %>% 
        set_colnames(
          #adds superscript R for NHS Grampian revisions.
          #From 2026 onward REMOVE the if statement and keep the column names that are currently set as else
          if(input$select_year_randr == "2020/21"){
            c("Integration Authority Area","Number of People Referred to PDSᴿ")
          }else{
            c("Integration Authority Area","Number of People Referred to PDS")
          }
        )
    }
  })
  
  output$table_totals_randr <- DT::renderDataTable({
    
    make_table(table_totals_data(),
               right_align = 1, selected = 1, rows_to_display = 32)
    
  })
  
  #### download button totals----
  output$downloadData_totals <- downloadHandler(
    filename = paste0("pds_data_as_at_", end_date, ".csv"),
    content = function(file) {
      write.csv(table_totals_data() %>% mutate(`Financial Year` = input$select_year_randr, 
                                               .before = everything()) %>% 
                  #changes superscript R to in line R for downloaded csv since superscript is not supported 
                  mutate(`Financial Year`  = case_when(
                    `Financial Year`  == provisional_year_sup ~paste0(provisional_year,"P"),
                    `Financial Year`  == revised_year_sup ~paste0(revised_year,"R"),
                    `Financial Year`  == extra_referrals_year_sup ~paste0(extra_referrals_year,"P"),
                    TRUE ~`Financial Year` )) %>% 
                  #From 2026 onward REMOVE the following 6 lines which only apply to Grampian revisions made in 2025
                  set_colnames(
                    if(input$select_hb_ijb_randr == "Health Boards" & input$select_year_randr == "2020/21"){
                      c("Financial Year", "Health Board","Number of People Referred to PDS(R)")
                    }else if(input$select_hb_ijb_randr == "Health Boards" & input$select_year_randr != "2020/21"){
                      c("Financial Year", "Health Board","Number of People Referred to PDS")  
                    }else if(input$select_hb_ijb_randr != "Health Boards" & input$select_year_randr == "2020/21"){
                      c("Financial Year", "Integration Authority Area","Number of People Referred to PDS(R)")
                    }else if(input$select_hb_ijb_randr != "Health Boards" & input$select_year_randr != "2020/21"){
                      c("Financial Year", "Integration Authority Area","Number of People Referred to PDS")  
                    }
                  ),
                file, row.names = FALSE)
    }
  )
  
  
  
  
  ## TRENDS ----
  ###plot trends total referrals----
  
  output$randr_chart_title_trend_totals <- renderUI({HTML(paste0("Number of people diagnosed with dementia who were referred for PDS; Trend, ",
                                                                 input$select_randr_trend_totals))
  })
  
  output$hb_RandR_trend_plot <- renderPlotly({
    plot_trend(annual_table_data %>% filter(ijb == input$select_randr_trend_totals, fy %in% included_years_extra_referrals, ldp == "total"),
               measure = referrals, ytitle = "Referrals",
               colours = if(input$select_randr_trend_totals == "Scotland"){"#9B4393"
               }else{"#0078D4"}
    )
  })
  
  ### data table trends total referrals----     
  
  output$randr_table_title_trend_totals <- renderUI({HTML(paste0("Number of people diagnosed with dementia who were referred for PDS; Trend, Scotland and ", input$select_hb_ijb_randr))})
  
  
  table_trend_totals_data <- reactive({
    
    if(input$select_hb_ijb_randr == "Health Boards"){
      annual_table_data %>% 
        filter(fy %in% included_years_extra_referrals, ldp == "total") %>% 
        filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
        select(health_board, fy, referrals) %>%
        #adds superscript R for revised NHS Grampian data
        mutate(fy = if_else(fy == "2020/21", paste0("2020/21", "ᴿ"),fy)) %>% #REMOVE from 2026 onward
        rename("Health Board" = "health_board")  
    }else{
      annual_table_data %>% 
        filter(fy %in% included_years_extra_referrals, ldp == "total") %>% 
        filter(!grepl("NHS", ijb)) %>% 
        select(ijb, fy, referrals) %>%
        #adds superscript R for revised NHS Grampian data
        mutate(fy = if_else(fy == "2020/21", paste0("2020/21", "ᴿ"),fy)) %>% #REMOVE from 2026 onward
        rename("Integration Authority Area" = "ijb")  
      
    }
    
  })
  
  
  output$randr_table_trend_totals <- DT::renderDataTable({
    make_table(table_trend_totals_data() %>% 
                 pivot_wider(names_from = fy, values_from = referrals) %>% 
                 mutate(across(where(is.numeric), ~prettyNum(., big.mark = ","))), right_align = 1:length(included_years_extra_referrals), selected = 1, 
               table_elements = "t", rows_to_display = 32)
  })
  
  ### download button totals trend----
  output$downloadData_totals_trend <- downloadHandler(
    filename = paste0("pds_data_as_at_", end_date, ".csv"),
    content = function(file) {
      write.csv(table_trend_totals_data() %>% 
                  #changes superscript R to in line R for downloaded csv since superscript is not supported 
                  mutate(fy = case_when(
                    fy == provisional_year_sup ~paste0(provisional_year,"P"),
                    fy == revised_year_sup ~paste0(revised_year,"R"),
                    fy == extra_referrals_year_sup ~paste0(extra_referrals_year,"P"),
                    # remove the following line from 2026 onwards
                    fy == "2020/21ᴿ" ~ "2020/21R",
                    TRUE ~fy)) %>% 
                  pivot_wider(names_from = fy, values_from = referrals) %>% 
                  mutate(across(where(is.numeric), ~prettyNum(., big.mark = ","))) %>% 
                  mutate(Measure = "Number of people diagnosed with dementia who were referred for PDS", 
                         .before = everything()), 
                file, row.names = FALSE)
    }
  )
  
  ## RATES ----
  ## RATES BY YEAR----
  
  ### rates title ----
  output$randr_title_rates <- renderUI({HTML(paste("Number of People Referred for PDS per 10,000 Population (65+); Scotland, ", 
                                                   input$select_year_randr))
  })
  
 
  ### value boxes data ----
  vb_data_rates <- reactive({data_rates %>% filter(health_board == "Scotland", ijb == "Scotland", fy == input$select_year_randr)}) 
  
  # percentage of those referred for post-diagnostic support received a minimum of 12 months of support
  output$scot_rate <- renderText({paste0(vb_data_rates()$pop_rate_10000)})
  
  # vb_2_data<- reactive({annual_table_data %>% filter(health_board == "Scotland", ijb == "Scotland", fy == input$select_year_randr,
  #                                                    ldp != "fail") %>% select(-diagnoses, -exp_perc) %>% 
  #     pivot_wider(values_from = referrals, names_from = ldp)})
  # 
  # output$scot_referrals_text <- renderUI({
  #   HTML(paste("<b>", prettyNum(vb_2_data()$complete + vb_2_data()$exempt, big.mark = ","), "</b>", "referrals either met or were exempt from the LDP standard. This is divided by",
  #              "<b>", prettyNum(vb_2_data()$total - vb_2_data()$ongoing, big.mark = ","), "</b>", "the total number of referrals (excluding those whose support is ongoing)."))})
  
  
  
  ### plot rates ----
  output$rates_plot_title <- renderUI({HTML(paste0("Number of people per 10,000 population (65+) who were referred for PDS; ", 
                                                      input$select_year_randr, ", Scotland and ", input$select_hb_ijb_randr))
  })
  
  
  rates_chart_data <- reactive({
    
    filtered_rates_data <- data_rates %>% filter(fy == input$select_year_randr)
    
    left_join(
      if(input$select_hb_ijb_randr == "Health Boards"){
        filtered_rates_data %>% filter(grepl("NHS", ijb))
      }else{
        filtered_rates_data %>% filter(!grepl("NHS", ijb), ijb != "Scotland")   
      },
      filtered_rates_data %>% filter(ijb == "Scotland") %>% select(fy, pop_rate_10000)%>%
        rename(scot_pop_rate_10000 = pop_rate_10000)) %>% 
      rename(geog = ijb)
  })
  
  output$rates_plot <- renderPlotly({
    
    plot_bar(rates_chart_data(),
                       measure = pop_rate_10000, scot_measure = scot_pop_rate_10000)
    
  })
  
  
  ## data table rates ----
  output$rates_table_title <- renderUI({HTML(paste0("Number of people per 10,000 population (65+) who were referred for PDS; ", 
                                                       input$select_year_randr, ", Scotland and ", input$select_hb_ijb_randr))
  })
  
  
  table_rates_data <- reactive({
    
    if(input$select_hb_ijb_randr == "Health Boards"){
      
      data_rates %>% 
        filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
        filter(fy == input$select_year_randr) %>%
        select(health_board,pop_rate_10000)%>%
        arrange(health_board) %>% 
        set_colnames(
            c("Health Board","Number of People Referred to PDS per 10,000 population (65+)"))
      
    }else{
      
      data_rates %>%
        filter(!grepl("NHS", ijb)) %>% 
        filter(fy == input$select_year_randr) %>%
        select(ijb,pop_rate_10000)%>%
        set_colnames(
          c("Integration Authority Area","Number of People Referred to PDS per 10,000 population (65+)"))
       }
  })
  
  output$rates_table <- DT::renderDataTable({
    
    make_table(table_rates_data(),
               right_align = 1, selected = 1, rows_to_display = 32)
    
  })
  
  
  # ### download button ldp2----
  # output$downloadData_ldp2 <- downloadHandler(
  #   filename = paste0("pds_data_as_at_", end_date, ".csv"),
  #   content = function(file) {
  #     write.csv(table_ldp2_data() %>% mutate(`Financial Year` = input$select_year_ldp, 
  #                                            .before = everything()) %>% 
  #                 #changes superscript R to in line R for downloaded csv since superscript is not supported 
  #                 mutate(`Financial Year`  = case_when(
  #                   `Financial Year`  == provisional_year_sup ~paste0(provisional_year,"P"),
  #                   `Financial Year`  == revised_year_sup ~paste0(revised_year,"R"),
  #                   TRUE ~`Financial Year` )) %>% 
  #                 #From 2026 onward REMOVE the following 11 lines which only apply to Grampian revisions made in 2025
  #                 set_colnames(
  #                   if(input$select_hb_ijb == "Health Boards" & input$select_year_ldp == "2020/21"){
  #                     c("Financial Year", "Health Board","Number of People Referred to PDS(R)", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved")
  #                   }else if(input$select_hb_ijb == "Health Boards" & input$select_year_ldp != "2020/21"){
  #                     c("Financial Year", "Health Board","Number of People Referred to PDS", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved")  
  #                   }else if(input$select_hb_ijb != "Health Boards" & input$select_year_ldp == "2020/21"){
  #                     c("Financial Year", "Integration Authority Area","Number of People Referred to PDS(R)", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved")
  #                   }else if(input$select_hb_ijb != "Health Boards" & input$select_year_ldp != "2020/21"){
  #                     c("Financial Year", "Integration Authority Area","Number of People Referred to PDS", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved")  
  #                   }
  #                 ),
  #               file, row.names = FALSE)
  #   }
  # )
  # 
  
  
  
  ###plot rates bar chart ----
  # output$referrals_plot_title <- renderUI({HTML(paste0("Percentage of people referred for PDS who received a minimum of one year’s support within 12 month's of diagnosis; ", 
  #                                                      input$select_year_randr, ", Scotland and ", input$select_hb_ijb))
  # })
  # 
  # output$referrals_plot <- renderPlotly({
  #   
  #   if(input$select_hb_ijb == "Health Boards"){
  #     
  #     plot_bar(annual_table_data %>% filter(grepl("NHS", ijb) | ijb == "Scotland", fy == input$select_year_randr, ldp == "total") %>% 
  #                mutate(colour = if_else(ijb == "Scotland", "B", "A")), 
  #              category = ijb, 
  #              measure = percent_met,
  #              fill = colour)
  #     
  #   }else{
  #     
  #     plot_bar(annual_table_data %>% filter(!grepl("NHS", ijb), fy == input$select_year_randr, ldp == "total") %>% 
  #                mutate(colour = if_else(ijb == "Scotland", "B", "A")), 
  #              category = ijb, 
  #              measure = percent_met,
  #              fill = colour)
  #   }
  #   
  # })
  
  ### data table rates bar chart ----
  # output$referrals_table_title <- renderUI({HTML(paste0("Number and percentage of people referred for PDS who received a minimum of one year’s support within 12 month's of diagnosis; ", 
  #                                                       input$select_year_randr, ", Scotland and ", input$select_hb_ijb))
  # })
  # 
  # output$referrals_table <- DT::renderDataTable({
  #   
  #   if(input$select_hb_ijb == "Health Boards"){
  #     
  #     table_hb_data <- annual_table_data %>% 
  #       filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
  #       filter(fy == input$select_year_randr) %>%
  #       select(health_board,ldp,referrals,percent_met)%>%
  #       mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
  #       mutate(percent_met = if_else(percent_met == "   NA", "-", paste0(percent_met, "%"))) %>% 
  #       pivot_wider(names_from=ldp,values_from=referrals) %>% 
  #       select(health_board, total, complete, exempt, ongoing, fail, percent_met) %>% 
  #       arrange(health_board) %>% 
  #       set_colnames(c("NHS Board","Number of People Referred to PDS", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved")) 
  #     make_table(table_hb_data, right_align = 1:6, selected = 1, table_elements = "t") 
  #     
  #     
  #   }else{
  #     
  #     
  #     table_ijb_data <- annual_table_data %>%
  #       filter(!grepl("NHS", ijb)) %>% 
  #       filter(fy == input$select_year_randr) %>%
  #       mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
  #       mutate(percent_met = if_else(percent_met == "   NA", "-", paste0(percent_met, "%"))) %>% 
  #       mutate(referrals = if_else(fy %in% c("2019/20", "2020/21") & ijb == "Aberdeen City" & ldp != "total", "-", as.character(referrals))) %>% 
  #       select(ijb,ldp,referrals,percent_met) %>%
  #       pivot_wider(names_from=ldp,values_from=referrals) %>% 
  #       select(ijb, total, complete, exempt, ongoing, fail, percent_met) %>% 
  #       arrange(ijb) %>% 
  #       set_colnames(c("Integration Authority Area","Number of People Referred to PDS", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved")),
  #     make_table(table_ijb_data, right_align = 1:6, selected = 1, rows_to_display = 32, table_elements = "t")
  #     
  #   }
  # })
  
  ## TRENDS----
  #plot rates trends ----
  # output$randr_chart_title_trend_rates <- renderUI({HTML(paste("Percentage of people referred for PDS who received a minimum of one year’s support within 12 months of diagnosis; Trend, Scotland "),
  #                                                         if(input$randr_select_trend_rates == "Scotland"){""
  #                                                         }else{
  #                                                           paste0("and ", input$randr_select_trend_rates)})
  # })
  # 
  # trend_chart_data <- reactive({
  #   annual_table_data %>%
  #     filter(fy %in% included_years, ldp == "total") %>% 
  #     filter(ijb == input$randr_select_trend_rates | ijb == "Scotland")})
  # 
  # 
  # output$randr_trend_plot_rates <- renderPlotly({
  #   plot_trend(trend_chart_data(), percent_met)
  # })
  
  
  #data table rates trends----    
  
  # output$randr_table_trend_rates_title <- renderUI({
  #   HTML(paste0("Percentage of people referred for PDS who received a minimum of one year’s support within 12 months of diagnosis; Trend, Scotland and ", input$select_hb_ijb_randr))
  # })
  # 
  # output$randr_table_trend_rates <- DT::renderDataTable({
  #   
  #   if(input$select_hb_ijb_randr == "Health Boards"){  
  #     
  #     trend_hb_data <- annual_table_data %>% 
  #       filter(fy %in% included_years) %>% 
  #       select(health_board, fy, percent_met) %>%
  #       mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
  #       mutate(percent_met = if_else(percent_met == "   NA", "-", paste0(percent_met, "%"))) %>% 
  #       distinct(health_board, fy, .keep_all = T) %>% 
  #       pivot_wider(names_from = fy, values_from = percent_met) %>% 
  #       rename(" " = "health_board") 
  #     make_table(trend_hb_data, right_align = 1:length(included_years), selected = 1, table_elements = "t")
  #     
  #   }else{
  #     
  #     trend_ijb_data <- annual_table_data %>% 
  #       filter(fy %in% included_years) %>% 
  #       filter(!grepl("NHS", ijb)) %>% 
  #       select(ijb, fy, percent_met) %>%
  #       mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
  #       mutate(percent_met = if_else(percent_met == "   NA", "-", paste0(percent_met, "%"))) %>% 
  #       distinct(ijb, fy, .keep_all = T) %>% 
  #       pivot_wider(names_from = fy, values_from = percent_met) %>% 
  #       rename(" " = "ijb") 
  #     make_table(trend_ijb_data, right_align = 1:length(included_years), selected = 1, rows_to_display = 32, table_elements = "t")
  #   }
  # })
  
  
  
  # updates radio buttons label depending on selection
  observe(if(input$RandR_sidebar == "trends"){
    updateRadioButtons(session, "select_hb_ijb_randr",
                       label = "In the table show:"
    )
  }
  )
  observe(if(input$RandR_sidebar == "referrals"){
    updateRadioButtons(session, "select_hb_ijb_randr",
                       label = "In the chart and table show:"
    )
  }
  )
  
  ### END OF SCRIPT ###
  
  
  