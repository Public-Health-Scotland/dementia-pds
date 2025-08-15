####################### Page 1 REFERRALS & RATES #######################
#UI ----
output$rates_ui <-  renderUI({
  
  div(
  #   fluidRow(column(
  #   linebreaks(1),
  #   radioGroupButtons("RandR_tab", label = NULL, choices = RandR_tab_list,
  #                     status = "tab",
  #                     direction = "horizontal",
  #                     justified = T,
  #                     size = "normal"),
  #   width = 12)
  # ), #fluidRow
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
                                          plotlyOutput("totals_RandR_trend_plot"),
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
                                                           #####download button rates----
                                                           downloadButton("downloadData_rates", 
                                                                          "Download table data"),
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
                                                         #####download button rates trend----
                                                         downloadButton("downloadData_rates_trend", 
                                                                        "Download table data"),
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
  # filter data to years included in publication and to total referrals----
  referrals_data_sel_yrs <- annual_table_data %>% filter(fy %in% included_years_extra_referrals, ldp == "total")

  ## REFERRALS BY YEAR----
  ### total referrals title ----
  output$title_totals_randr <- renderUI({HTML(paste("Number of People Referred for PDS; Scotland, ", 
                                                    input$select_year_randr))
  })
  
  
  ### value boxes data ----
  vb_data_totals<- reactive({referrals_data_sel_yrs %>% filter(health_board == "Scotland", ijb == "Scotland", fy == input$select_year_randr)}) 
  
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
      
      plot_bar_no_line(referrals_data_sel_yrs %>% filter(grepl("NHS", ijb), fy == input$select_year_randr) %>% 
                         rename(geog = health_board),
                       ytitle = "Referrals",
                       measure = referrals, measure_text = "Number of referrals to PDS: ")
      
    }else{
      plot_bar_no_line(referrals_data_sel_yrs %>% filter((!grepl(("NHS|Scotland"), referrals_data_sel_yrs$ijb)),fy == input$select_year_randr) %>% 
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
      
      referrals_data_sel_yrs %>%
        filter(fy == input$select_year_randr) %>%
        filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
        select(ijb,referrals)%>%
        mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
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
      
      referrals_data_sel_yrs %>%
        filter(fy == input$select_year_randr) %>%
        filter(!grepl("NHS", ijb)) %>% 
        select(ijb,referrals)%>%
        mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
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
  
  #### download button data totals----
  output$downloadData_totals <- downloadHandler(
    filename = paste0("pds_data_as_at_", end_date, ".csv"),
    content = function(file) {
      write.csv(table_totals_data() %>%
                  mutate(across(where(is.factor), ~as.character(.))) %>% 
                  mutate(`Financial Year` = input$select_year_randr, 
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
                  ) %>% 
                  rbind(
                    if(input$select_year_randr == revised_year_sup){
                      c("Note: R indicates data has been revised. Please see dashboard for further information.",rep("",2))
                    }else if(input$select_year_randr == provisional_year_sup | input$select_year_randr == extra_referrals_year_sup){
                      c("Note: P indicates data is provisional. Please see dashboard for further information.",rep("",2))
                      #REMOVE the following two lines from 2026 onward----
                      }else if(input$select_year_randr == "2020/21"){
                        c(,"Note: R indicates data has been revised. Please see dashboard for further information.",rep("",2))
                      }else{
                      rep("",3)
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
  
  output$totals_RandR_trend_plot <- renderPlotly({
    plot_trend(referrals_data_sel_yrs %>% filter(ijb == input$select_randr_trend_totals),
               measure = referrals, ytitle = "Referrals",
               colours = if(input$select_randr_trend_totals == "Scotland"){"#9B4393"
               }else{"#0078D4"}
    )
  })
  
  ### data table trends total referrals----     
  
  output$randr_table_title_trend_totals <- renderUI({HTML(paste0("Number of people diagnosed with dementia who were referred for PDS; Trend, Scotland and ", input$select_hb_ijb_randr))})
  
  
  table_trend_totals_data <- reactive({
    
    if(input$select_hb_ijb_randr == "Health Boards"){
      referrals_data_sel_yrs %>% 
        filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
        select(health_board, fy, referrals) %>%
        #adds superscript R for revised NHS Grampian data
        mutate(fy = if_else(fy == "2020/21", paste0("2020/21", "ᴿ"),fy)) %>% #REMOVE from 2026 onward
        rename("Health Board" = "health_board")  
    }else{
      referrals_data_sel_yrs %>% 
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
  
  ### download button data totals trend----
  output$downloadData_totals_trend <- downloadHandler(
    filename = paste0("pds_data_as_at_", end_date, ".csv"),
    content = function(file) {
      write.csv(table_trend_totals_data() %>% 
                  mutate(across(where(is.factor), ~as.character(.))) %>% 
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
                         .before = everything()) %>% 
                  rbind(c("Note: P indicates data is provisional. Please see dashboard for further information.",rep("",length(included_years_extra_referrals)+1),)
                        
                        ) %>% 
                  rbind(c("Note: R indicates data has been revised. Please see dashboard for further information.",rep("",length(included_years_extra_referrals)+1))
                        ),
                file, row.names = FALSE)
    }
  )
  
  ## RATES ----
  
  # filter rates data to years included in publication----
  #REMOVE the two lines below from 2026 onwards ----
  data_rates_sel_yrs <- data_rates %>% 
    filter(fy %in% included_years_extra_referrals_2025_rates)#
  #UNCOMMENT the two lines below from 2026 onwards ----
  # data_rates_sel_yrs <- data_rates %>% 
  #   filter(fy %in% included_years_extra_referrals)
  
  ## RATES BY YEAR----
  
  ### rates title ----
  output$randr_title_rates <- renderUI({HTML(paste("Number of People Referred for PDS per 10,000 Population (65+); Scotland, ", 
                                                   input$select_year_randr))
  })
  
 
  ### value boxes data ----
  vb_data_rates <- reactive({data_rates_sel_yrs %>% filter(health_board == "Scotland", ijb == "Scotland", fy == input$select_year_randr)}) 
  
  # percentage of those referred for post-diagnostic support received a minimum of 12 months of support
  output$scot_rate <- renderText({paste0(vb_data_rates()$pop_rate_10000)})
  
  # vb_2_data<- reactive({referrals_data_sel_yrs %>% filter(health_board == "Scotland", ijb == "Scotland", fy == input$select_year_randr,
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
    
    filtered_rates_data <- data_rates_sel_yrs %>% filter(fy == input$select_year_randr)
    
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
                       measure = pop_rate_10000, scot_measure = scot_pop_rate_10000, ytitle = "rate per 10,000 population")
    
  })
  
  
  ## data table rates ----
  output$rates_table_title <- renderUI({HTML(paste0("Number of people per 10,000 population (65+) who were referred for PDS; ", 
                                                       input$select_year_randr, ", Scotland and ", input$select_hb_ijb_randr))
  })
  
  
  table_rates_data <- reactive({
    
    if(input$select_hb_ijb_randr == "Health Boards"){
      
      data_rates_sel_yrs %>% 
        filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
        filter(fy == input$select_year_randr) %>%
        select(health_board,pop_rate_10000)%>%
        arrange(health_board) %>% 
        set_colnames(
            c("Health Board","Number of People per 10,000 population (65+) Referred to PDS "))
      
    }else{
      
      data_rates_sel_yrs %>%
        filter(!grepl("NHS", ijb)) %>% 
        filter(fy == input$select_year_randr) %>%
        select(ijb,pop_rate_10000)%>%
        set_colnames(
          c("Integration Authority Area","Number of People per 10,000 population (65+) Referred to PDS"))
       }
  })
  
  output$rates_table <- DT::renderDataTable({
    
    make_table(table_rates_data(),
               right_align = 1, selected = 1, rows_to_display = 32)
    
  })
  
  
  ### download button data rates----
  output$downloadData_rates <- downloadHandler(
    filename = paste0("pds_data_as_at_", end_date, ".csv"),
    content = function(file) {
      write.csv(table_rates_data() %>% 
                  mutate(across(where(is.factor), ~as.character(.))) %>% 
                  mutate(`Financial Year` = input$select_year_randr,
                                             .before = everything()) %>%
                  #changes superscript P to in line P for downloaded csv since superscript is not supported
                  mutate(`Financial Year`  = case_when(
                    `Financial Year`  == provisional_year_sup ~paste0(provisional_year,"P"),
                    `Financial Year`  == revised_year_sup ~paste0(revised_year,"R"),
                    `Financial Year`  == extra_referrals_year_sup ~paste0(extra_referrals_year,"P"),
                    TRUE ~`Financial Year` ) 
                                   ) %>% 
                  rbind(
                    if(input$select_year_randr == provisional_year_sup | input$select_year_randr == extra_referrals_year_sup){
                      c("Note: P indicates data is provisional. Please see dashboard for further information.",rep("",2))
                    }else if(input$select_year_randr == revised_year_sup){
                      c("Note: R indicates data has been revised. Please see dashboard for further information.",rep("",2))
                    }else{
                      rep("",3)
                    }
                  ),
                file, row.names = FALSE)
    }
  )

  
  ## TRENDS----
  
  ##plot trends rates ----
  
  output$randr_chart_title_trend_rates <- renderUI({HTML(paste("Number of people per 10,000 population (65+) who were referred for PDS; Trend, Scotland "),
                                                    if(input$randr_select_trend_rates == "Scotland"){""
                                                    }else{
                                                      paste0("and ", input$randr_select_trend_rates)})
  })
  
  rates_trend_chart_data <- reactive({
    data_rates_sel_yrs %>%
     # filter(fy %in% included_years_extra_referrals) %>% 
      filter(ijb == input$randr_select_trend_rates | ijb == "Scotland")})
  
  
  output$randr_trend_plot_rates <- renderPlotly({
    plot_trend(rates_trend_chart_data(), pop_rate_10000, ytitle = "rate per 10,000 population")
  })
  
  
  
  ##data table trends rates----    
  
  output$randr_table_trend_rates_title <- renderUI({
    HTML(paste0("Number of people per 10,000 population (65+) who were referred for PDS; Trend, Scotland and ",
                input$select_hb_ijb_randr))
  })
  
  table_trend_rates_data <- reactive({
    
    
    
    if(input$select_hb_ijb_randr == "Health Boards"){  
      
      data_rates_sel_yrs %>% 
       # filter(fy %in% included_years_extra_referrals) %>% 
        filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
        select(health_board, fy, pop_rate_10000) %>%
        #distinct(health_board, fy, .keep_all = T) %>% 
        rename("Health Board" = "health_board") 
      
    }else{
      
      data_rates_sel_yrs %>% 
       # filter(fy %in% included_years_extra_referrals) %>% 
        filter(!grepl("NHS", ijb)) %>% 
        select(ijb, fy, pop_rate_10000) %>%
        #distinct(ijb, fy, .keep_all = T) %>% 
        rename("Integration Authority Area" = "ijb") 
      
    }
    
  })
  
  output$randr_table_trend_rates <- DT::renderDataTable({
    
    make_table(table_trend_rates_data() %>% 
                 pivot_wider(names_from = fy, values_from = pop_rate_10000),
               right_align = 1:length(included_years_extra_referrals), selected = 1, rows_to_display = 32)
    
  })
  
  ### download button data rates trend----
  output$downloadData_rates_trend <- downloadHandler(
    filename = paste0("pds_data_as_at_", end_date, ".csv"),
    content = function(file) {
      write.csv(table_trend_rates_data() %>% 
                  mutate(across(where(is.factor), ~as.character(.))) %>% 
                  mutate(fy = case_when(
                    fy == provisional_year_sup ~paste0(provisional_year,"P"),
                    fy  == extra_referrals_year_sup ~paste0(extra_referrals_year,"P"),
                    fy == revised_year_sup ~paste0(revised_year,"R"),
                    TRUE ~fy)) %>% 
                  pivot_wider(names_from = fy, values_from = pop_rate_10000) %>% 
                  mutate(Measure = "Number of people per 10,000 population (65+) who were referred for PDS", 
                         .before = everything()) %>% 
                  rbind(c("Note: P indicates data is provisional. Please see dashboard for further information.",rep("",length(included_years_extra_referrals)+1))
                    ),# %>% 
                # UNCOMMENT the line below from 2026 onward----
                #rbind(c("Note: R indicates data has been revised. Please see dashboard for further information.",rep("",length(included_years_extra_referrals)+1))),
                file, row.names = FALSE)
    }
  )
  
  
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
  
  
  # the following dynamically updates the selection list to reflect that rates data from 2021/22
  # has not been revised as it was not included in 2024 publication
  # REMOVE this section from 2026 onwards----
  
  observe({
    if(input$RandR_tab != "RandR_rates" & input$select_year_randr != "2021/22"){
      updateSelectInput(session,"select_year_randr",
                        label = "Select Financial Year of Diagnosis:",
                        choices = included_years_extra_referrals,
                        selected =  input$select_year_randr)
    }else if(input$RandR_tab != "RandR_rates" & input$select_year_randr == "2021/22"){
      updateSelectInput(session,"select_year_randr",
                        label = "Select Financial Year of Diagnosis:",
                        choices = included_years_extra_referrals,
                        selected = "2021/22ᴿ")
    }else if(input$RandR_tab == "RandR_rates" & input$select_year_randr == "2021/22ᴿ"){
      updateSelectInput(session,"select_year_randr",
                        label = "Select Financial Year of Diagnosis:",
                        choices = included_years_extra_referrals_2025_rates,
                        selected = "2021/22")
    }else if(input$RandR_tab == "RandR_rates" & input$select_year_randr != "2021/22ᴿ"){
      updateSelectInput(session,"select_year_randr",
                        label = "Select Financial Year of Diagnosis:",
                        choices = included_years_extra_referrals_2025_rates,
                        selected = input$select_year_randr)
    }
  })
  
  ### END OF SCRIPT ###
  
  
  