####################### Page 2: LDP Standard #######################
# UI ----
output$ldp_ui <-  renderUI({
  
  div(
    ##LDP PART 1----
    conditionalPanel(condition = 'input.ldp_tab == "ldp_part_1"',
                     column(
                       ### OUTCOMES BY YEAR----
                       conditionalPanel(condition = 'input.ldp_sidebar == "outcomes"',
                                        fluidRow(
                                          h3(strong(htmlOutput("title_part_1"))),
                                          linebreaks(1),          
                                          #### value boxes ----
                                          fluidRow(
                                            column(
                                              shinydashboard::valueBox(
                                                value = textOutput("scot_exp_perc"),
                                                subtitle = "of people estimated to be newly diagnosed with dementia were referred for post-diagnostic support.",
                                                width = 7,
                                                color = "fuchsia"), #valueBox
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
                                        ) #fluidRow
                       ), #cond panel outcomes
                       ##TRENDS ----
                       conditionalPanel(condition = 'input.ldp_sidebar == "trends"',
                                        fluidRow(
                                          ### plot ----
                                          h4(strong(htmlOutput("chart_title_trend_part_1"))),
                                          plotlyOutput("trend_plot_part_1", height = "310px"),
                                          #linebreaks(1),
                                          ### table ----
                                          h4(strong(htmlOutput("table_title_hb_trend_part_1"))),
                                          #####download button ldp1 trend----
                                          downloadButton("downloadData_ldp1_trend", 
                                                         "Download table data"),
                                          DT::dataTableOutput("table_hb_trend_part_1"),
                                          linebreaks(1),
                                        ) # fluidRow
                       ), #cond panel trends
                       p(paste0("Sources: Public Health Scotland quarterly dementia post-diagnostic support dataset: Data submissions from NHS Boards as at ",
                                format(end_date, "%d %B %Y"), "; Estimated and Projected Diagnosis Rates for Dementia in Scotland paper: 2014-2020; National Records of Scotland (NRS) mid-2021 and mid-2022 population estimates.")),
                       ### Notes----
                       h4(strong("Notes:")),
                       p(paste0("ᴾ Figures for ", provisional_year," are provisional subject to all service users completing their support.")),
                       p(paste0("ᴿ Figures for ", revised_year," have been revised and are now final."),
                         em("Figures for Aberdeen City NHS Grampian and Scotland for 2020/21 have been revised due to the receipt of additional
                            data. The 2020/21 figures include an additional 11 referrals from those previously published. The Percentage of 
                            Estimated Number of People Diagnosed with Dementia Referred to PDS for NHS Grampian is now 19.4% (previously 18.8%) 
                            and 33.8% for Scotland (previously 33.7%).")),
                       p("The estimated number of people newly diagnosed with dementia is subject to the limitations detailed within the paper below published by the Scottish Government in 2016: ", 
                         br(),
                         a('Estimated and Projected Diagnosis Rates for Dementia in Scotland: 2014-2020', href = 'https://www.gov.scot/publications/estimated-projected-diagnosis-rates-dementia-scotland-2014-2020/', target="_blank"),
                         br(),
                         "Estimates are used as follows: calendar year 2016 estimates for 2016/17, calendar year 2017 estimates for 2017/18, calendar year 2018 estimates for 2018/19, calendar year 2019 estimates for 2019/20 and calendar year 2020 estimates for 2020/21.",
                         br(),
                         "For 2021/22 and 2022/23, the estimated number of people newly diagnosed with dementia has been calculated using the rates referenced in the paper above and the National Records of Scotland (NRS) mid-2021 and mid-2022 population estimates. See Note 2 on the",
                         a(
                           href = "#",
                           "Home",
                           onclick = "Shiny.setInputValue('home_link', Math.random()); return false;"),
                         "page for further information."),					
                       p("Figures for 2018/19, 2019/20 and 2020/21 for NHS Grampian and Scotland are affected by the change in service provision of PDS within Aberdeen City during 2019. See Note 5 on the",
                         a(
                           href = "#",
                           "Home",
                           onclick = "Shiny.setInputValue('home_link', Math.random()); return false;"),
                         "page for further information."),
                       width = 12,
                       #fix panel so sidebar and navigation bar do not scroll with content
                       style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available"
                     )# column
    ), #cond panel part 1
    ## LDP PART 2 ----
    conditionalPanel(condition = 'input.ldp_tab == "ldp_part_2"',
                     column(
                       ###OUTCOMES BY YEAR----
                       conditionalPanel(condition = 'input.ldp_sidebar == "outcomes"',
                                        fluidRow(
                                          h3(strong(htmlOutput("title_part_2"))),
                                          linebreaks(1),
                                          ####value boxes ----
                                          fluidRow(column(
                                            shinydashboard::valueBox(
                                              value = textOutput("scot_pds_perc"),
                                              subtitle = "of those referred for post-diagnostic support received a minimum of 12 months of support.",
                                              width = 7,
                                              color = "fuchsia"), #valueBox
                                            box(htmlOutput("scot_pds_text"),
                                                status = "primary",
                                                title = (p(strong("How is this figure calculated?"))),
                                                width = 5), #box
                                            width = 12)
                                          ), #fluidRow
                                          fluidRow(column(
                                            linebreaks(1),
                                            ####plot ----
                                            h4(strong(htmlOutput("perc_met_plot_title"))),
                                            plotlyOutput("perc_met_plot"),
                                            #### table ----
                                            h4(strong(htmlOutput("perc_met_table_title"))),
                                            #####download button ldp2----
                                            downloadButton("downloadData_ldp2", 
                                                           "Download table data"),
                                            DT::dataTableOutput("perc_met_table"),
                                            linebreaks(1),
                                            width = 12)
                                          ), # fluid Row
                                        ) #fluidRow
                       ), # cond panel outcomes
                       ### TRENDS----
                       conditionalPanel(condition = 'input.ldp_sidebar == "trends"', 
                                        fluidRow(
                                          ####plot----
                                          h4(strong(htmlOutput("chart_title_trend_part_2"))),
                                          plotlyOutput("trend_plot_part_2", height = "310px"),
                                          #linebreaks(1),
                                          ####table----
                                          h4(strong(htmlOutput("table_trend_part_2_title"))),
                                          #####download button ldp2 trend----
                                          downloadButton("downloadData_ldp2_trend", 
                                                         "Download table data"),
                                          DT::dataTableOutput("table_hb_ijb_trend_part_2"),
                                          linebreaks(1),
                                        ) # fluidRow
                       ), #cond panel trends
                       p(paste0("Source: Public Health Scotland quarterly dementia post-diagnostic support dataset: Data submissions from NHS Boards as at ",
                                format(end_date, "%d %B %Y"))),
                       #### Notes----
                       h4(strong("Notes:")),
                       p(paste0("ᴾ Figures for ", provisional_year," are provisional subject to all service users completing their support.")),
                       p(paste0("ᴿ Figures for ", revised_year," have been revised and are now final."),
                         em("")),
                       p("For detailed information on how the Percentage LDP Standard Achieved is calculated, and how 'Standard Met', 'Exempt from Standard', 'PDS Ongoing' and 'Standard Not Met' are defined, please see the",
                         a(
                           href = "#",
                           "Methodology",
                           onclick = "Shiny.setInputValue('method_link', Math.random()); return false;"),
                         "page."),					
                       p("Figures for 2018/19, 2019/20 and 2020/21 for Aberdeen City, NHS Grampian and Scotland are affected by the change in service provision of PDS within Aberdeen City during 2019. See Note 5 on the",
                         a(
                           href = "#",
                           "Home",
                           onclick = "Shiny.setInputValue('home_link', Math.random()); return false;"),
                         "page for further information."),
                       p("NHS Shetland / Shetland Islands did not have a PDS worker in post from 2022/23 Q1 through 2023/24 Q3. This will affect the figures for NHS Shetland / Shetland Islands for 2022/23. See Note 7 on the",
                         a(
                           href = "#",
                           "Home",
                           onclick = "Shiny.setInputValue('home_link', Math.random()); return false;"),
                         "page for further information."),
                       width = 12,
                       #fix panel so sidebar and navigation bar do not scroll with content
                       style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available"
                     ), # column
    ) # cond panel part 2
  )# div
}) # renderUI

#SERVER ----

# filter to years included in publication----
ldp_data <- annual_table_data %>% filter(fy %in% included_years)

##LDP PART 1----
## OUTCOMES BY YEAR----
##part 1 title ----
output$title_part_1 <- renderUI({HTML(paste("Percentage of estimated diagnoses referred for PDS; Scotland, ", 
                                            input$select_year_ldp))
})


##value boxes----
vb_data<- reactive({ldp_data %>% filter(health_board == "Scotland", ijb == "Scotland", fy == input$select_year_ldp, ldp == "total")}) 

## percentage of people estimated to be newly diagnosed with dementia were referred for post-diagnostic support
output$scot_exp_perc <- renderText({paste0(vb_data()$exp_perc, "%")})

output$scot_exp_text <- renderUI({
  HTML(paste("A total of", "<b>",  prettyNum(vb_data()$referrals, big.mark = ","), "</b>", "referrals were made to post-diagnostic support. This is divided by",
             "<b>", prettyNum(vb_data()$diagnoses, big.mark = ","), "</b>", "the estimated number of people newly diagnosed with dementia."))})


## plot ldp part 1 ----
output$hb_exp_plot_title <- renderUI({HTML(paste0("Percentage of people estimated to be newly diagnosed with dementia who were referred for PDS; ", 
                                                  input$select_year_ldp, ", Scotland and Health Boards"))
})

hb_exp_chart_data <- reactive({
  left_join(
    ldp_data %>% filter(grepl("NHS", ijb), fy == input$select_year_ldp, ldp == "total"),
    ldp_data %>% filter(ijb == "Scotland", ldp == "total") %>% select(fy, exp_perc) %>%
      rename(scot_exp_perc = exp_perc))
})

output$hb_exp_plot <- renderPlotly({
  plot_bar_perc(hb_exp_chart_data(),
                     measure = exp_perc, scot_measure = scot_exp_perc, legend = "bottom")
})


## data table ldp part 1 ----
output$hb_exp_table_title <- renderUI({HTML(paste0("Number and percentage of people estimated to be newly diagnosed with dementia who were referred for PDS; ", 
                                                   input$select_year_ldp, ", Scotland and Health Boards"))
})

table_hb_exp_data <- reactive({
  
  ldp_data %>% 
    filter(fy == input$select_year_ldp) %>%
    filter(grepl("NHS", ijb) | ijb == "Scotland", !is.na(diagnoses)) %>% 
    select(health_board, diagnoses, referrals)%>%
    mutate(exp_perc = paste0(round(referrals/diagnoses*100, 1), "%")) %>%  
    mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
    arrange(health_board) %>% 
    # adds superscript R for NHS Grampian revisions. 
    #From 2026 onward REMOVE the if statement and keep the column names that are currently set as else
    set_colnames(if(input$select_year_ldp == "2020/21"){
      c("Health Board","Estimated Number of People Newly Diagnosed with Dementia",
        "Number of People Referred to PDSᴿ","Percentage of Estimated Number of People Diagnosed with Dementia Referred to PDSᴿ")
    }else{c("Health Board","Estimated Number of People Newly Diagnosed with Dementia",
            "Number of People Referred to PDS","Percentage of Estimated Number of People Diagnosed with Dementia Referred to PDS")
    }
    )
})

output$table_hb_exp <- DT::renderDataTable({
  make_table(table_hb_exp_data(), right_align = 1:3, selected = 1)
})

### download button data ldp1----
output$downloadData_ldp1 <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(table_hb_exp_data() %>%
               mutate(across(where(is.factor), ~as.character(.))) %>% 
                mutate(`Financial Year` = input$select_year_ldp, 
                                             .before = everything()) %>% 
                #changes superscript R to in line R for downloaded csv since superscript is not supported 
                mutate(`Financial Year`  = case_when(
                  `Financial Year`  == provisional_year_sup ~paste0(provisional_year,"P"),
                  `Financial Year`  == revised_year_sup ~paste0(revised_year,"R"),
                  TRUE ~`Financial Year` )) %>% 
                #From 2026 onward REMOVE the following 6 lines which only apply to Grampian revisions made in 2025
                set_colnames(if(input$select_year_ldp == "2020/21"){
                  c("Financial Year", "Health Board","Estimated Number of People Newly Diagnosed with Dementia",
                    "Number of People Referred to PDS(R)","Percentage of Estimated Number of People Diagnosed with Dementia Referred to PDS(R)")
                }else{c("Financial Year", "Health Board","Estimated Number of People Newly Diagnosed with Dementia",
                        "Number of People Referred to PDS","Percentage of Estimated Number of People Diagnosed with Dementia Referred to PDS")
                }
                ) %>% 
                ##### adds revision and provisional note
                rbind(
                  if(input$select_year_ldp == revised_year_sup){
                    c("Note: R indicates data has been revised. Please see dashboard for further information.",rep("",4))
                  }else if(input$select_year_ldp == provisional_year_sup){
                    c("Note: P indicates data is provisional. Please see dashboard for further information.",rep("",4))
                    #REMOVE the following two lines from 2026 onward----
                  }else if(input$select_year_ldp == "2020/21"){
                    c("Note: R indicates data has been revised. Please see dashboard for further information.",rep("",4))
                  }else{
                    rep("",5)
                  }
                ),
              file, row.names = FALSE)
  }
)
##TRENDS ----
##plot trends part 1----

output$chart_title_trend_part_1 <- renderUI({HTML(paste0("Percentage of people estimated to be newly diagnosed with dementia who were referred for PDS; Trend, Scotland "),
                                                  if(input$select_hb_trend_part_1 == "Scotland"){""
                                                  }else{
                                                    paste0("and ", input$select_hb_trend_part_1)})
})

trend_chart_data_part_1 <- reactive({
  ldp_data %>%
    filter(ijb == input$select_hb_trend_part_1 | ijb == "Scotland", ldp == "total")
})

output$trend_plot_part_1 <- renderPlotly({
  plot_trend_perc(trend_chart_data_part_1(), exp_perc)
})

## data table trends part 1----     

output$table_title_hb_trend_part_1 <- renderUI({HTML(paste0("Percentage of people estimated to be newly diagnosed with dementia who were referred for PDS; Trend, Scotland and Health Boards"))})

table_hb_trend_part_1_data <- reactive({
  ldp_data %>% 
    filter(grepl("NHS", ijb) | ijb == "Scotland", ldp == "total") %>% 
    select(health_board, fy, exp_perc) %>%
    mutate(exp_perc = paste0(exp_perc, "%")) %>% 
    #adds superscript R for revised NHS Grampian data
    mutate(fy = if_else(fy == "2020/21", paste0("2020/21", "ᴿ"),fy)) %>% #REMOVE from 2026 onward
    rename("Health Board" = "health_board")  
})

output$table_hb_trend_part_1 <- DT::renderDataTable({
  make_table(table_hb_trend_part_1_data() %>% 
               pivot_wider(names_from = fy, values_from = exp_perc),
             right_align = 1:length(included_years), selected = 1)
})


### download button data ldp1 trend----
output$downloadData_ldp1_trend <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(table_hb_trend_part_1_data() %>% 
                mutate(across(where(is.factor), ~as.character(.))) %>% 
                #changes superscript R to in line R for downloaded csv since superscript is not supported 
                mutate(fy = case_when(
                  fy == provisional_year_sup ~paste0(provisional_year,"P"),
                  fy == revised_year_sup ~paste0(revised_year,"R"),
                  # remove the following line from 2026 onwards
                  fy == "2020/21ᴿ" ~ "2020/21R",
                  TRUE ~fy))  %>% 
                pivot_wider(names_from = fy, values_from = exp_perc) %>%
                mutate(Measure = "Percentage of Estimated Number of People Diagnosed with Dementia Referred to PDS", 
                       .before = everything()) %>% 
                rbind(c("Note: P indicates data is provisional. Please see dashboard for further information.",rep("",length(included_years)+1))
                      
                ) %>% 
                rbind(c("Note: R indicates data has been revised. Please see dashboard for further information.",rep("",length(included_years)+1))
                ), 
              file, row.names = FALSE)
  }
)

## LDP PART 2 ----
##OUTCOMES BY YEAR----
##part 2 title ----
output$title_part_2 <- renderUI({HTML(paste("Percentage of referrals for PDS who received one year's support; Scotland, ", 
                                            input$select_year_ldp))
})

##value boxes----
## percentage of those referred for post-diagnostic support received a minimum of 12 months of support
output$scot_pds_perc <- renderText({paste0(vb_data()$percent_met, "%")})

vb_2_data<- reactive({ldp_data %>% filter(health_board == "Scotland", ijb == "Scotland", fy == input$select_year_ldp,
                                                   ldp != "fail") %>% select(-diagnoses, -exp_perc) %>% 
    pivot_wider(values_from = referrals, names_from = ldp)})

output$scot_pds_text <- renderUI({
  HTML(paste("<b>", prettyNum(vb_2_data()$complete + vb_2_data()$exempt, big.mark = ","), "</b>", "referrals either met or were exempt from the LDP standard. This is divided by",
             "<b>", prettyNum(vb_2_data()$total - vb_2_data()$ongoing, big.mark = ","), "</b>", "the total number of referrals (excluding those whose support is ongoing)."))})

##plot ldp part 2 ----
output$perc_met_plot_title <- renderUI({HTML(paste0("Percentage of people referred for PDS who received a minimum of one year’s support within 12 month's of diagnosis; ", 
                                                    input$select_year_ldp, ", Scotland and ", input$select_hb_ijb))
})


perc_met_chart_data <- reactive({
  
  filtered_annual_data <- ldp_data %>% filter(fy == input$select_year_ldp, ldp == "total")
  
  left_join(
    if(input$select_hb_ijb == "Health Boards"){
      filtered_annual_data %>% filter(grepl("NHS", ijb))
    }else{
      filtered_annual_data %>% filter(!grepl("NHS", ijb), ijb != "Scotland")   
    },
    filtered_annual_data %>% filter(ijb == "Scotland") %>% select(fy, percent_met)%>%
      rename(scot_percent_met = percent_met))
})

output$perc_met_plot <- renderPlotly({
  
  plot_bar_perc(perc_met_chart_data(),
                     measure = percent_met, scot_measure = scot_percent_met, legend = "bottom")
  
})

## data table lpd part 2 ----
output$perc_met_table_title <- renderUI({HTML(paste0("Number and percentage of people referred for PDS who received a minimum of one year’s support within 12 month's of diagnosis; ", 
                                                     input$select_year_ldp, ", Scotland and ", input$select_hb_ijb))
})


table_ldp2_data <- reactive({
  
  if(input$select_hb_ijb == "Health Boards"){
    
    ldp_data %>% 
      filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
      filter(fy == input$select_year_ldp) %>%
      select(health_board,ldp,referrals,percent_met)%>%
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
      mutate(percent_met = if_else(percent_met == "   NA", "-", paste0(percent_met, "%"))) %>% 
      pivot_wider(names_from=ldp,values_from=referrals) %>% 
      select(health_board, total, complete, exempt, ongoing, fail, percent_met) %>% 
      arrange(health_board) %>% 
      set_colnames(
        # adds superscript R for NHS Grampian revisions. 
        #From 2026 onward REMOVE the if statement and keep the column names that are currently set as else
        if(input$select_year_ldp == "2020/21"){
          c("Health Board","Number of People Referred to PDSᴿ", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved")
        }else{
          c("Health Board","Number of People Referred to PDS", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved")  
        }
      )
    
  }else{
    
    ldp_data %>%
      filter(!grepl("NHS", ijb)) %>% 
      filter(fy == input$select_year_ldp) %>%
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
      mutate(percent_met = if_else(percent_met == "   NA", "-", paste0(percent_met, "%"))) %>% 
      mutate(referrals = if_else(fy %in% c("2019/20", "2020/21") & ijb == "Aberdeen City" & ldp != "total", "-", as.character(referrals))) %>% 
      select(ijb,ldp,referrals,percent_met) %>%
      pivot_wider(names_from=ldp,values_from=referrals) %>% 
      select(ijb, total, complete, exempt, ongoing, fail, percent_met) %>% 
      arrange(ijb) %>% 
      set_colnames(
        # adds superscript R for NHS Grampian revisions. 
        #From 2026 onward REMOVE the if statement and keep the column names that are currently set as else
        if(input$select_year_ldp == "2020/21"){
          c("Integration Authority Area","Number of People Referred to PDSᴿ", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved")
        }else{
          c("Integration Authority Area","Number of People Referred to PDS", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved")  
        }
      )
  }
})

output$perc_met_table <- DT::renderDataTable({
  
  make_table(table_ldp2_data(),
             right_align = 1:6, selected = 1, rows_to_display = 32)
  
})


### download button data ldp2----
output$downloadData_ldp2 <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(table_ldp2_data() %>% 
                mutate(across(where(is.factor), ~as.character(.))) %>% 
                mutate(`Financial Year` = input$select_year_ldp, 
                                           .before = everything()) %>% 
                #changes superscript R to in line R for downloaded csv since superscript is not supported 
                mutate(`Financial Year`  = case_when(
                  `Financial Year`  == provisional_year_sup ~paste0(provisional_year,"P"),
                  `Financial Year`  == revised_year_sup ~paste0(revised_year,"R"),
                  TRUE ~`Financial Year` )) %>% 
                #From 2026 onward REMOVE the following 11 lines which only apply to Grampian revisions made in 2025
                set_colnames(
                  if(input$select_hb_ijb == "Health Boards" & input$select_year_ldp == "2020/21"){
                    c("Financial Year", "Health Board","Number of People Referred to PDS(R)", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved")
                  }else if(input$select_hb_ijb == "Health Boards" & input$select_year_ldp != "2020/21"){
                    c("Financial Year", "Health Board","Number of People Referred to PDS", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved")  
                  }else if(input$select_hb_ijb != "Health Boards" & input$select_year_ldp == "2020/21"){
                    c("Financial Year", "Integration Authority Area","Number of People Referred to PDS(R)", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved")
                  }else if(input$select_hb_ijb != "Health Boards" & input$select_year_ldp != "2020/21"){
                    c("Financial Year", "Integration Authority Area","Number of People Referred to PDS", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved")  
                  }
                )%>% 
                rbind(
                  if(input$select_year_ldp == revised_year_sup){
                    c(rep("",4),"Note: R indicates data has been revised. Please see dashboard for further information.")
                  }else if(input$select_year_ldp == provisional_year_sup){
                    c(rep("",4),"Note: P indicates data is provisional. Please see dashboard for further information.")
                    #REMOVE the following two lines from 2026 onward----
                  }else if(input$select_year_ldp == "2020/21"){
                    c(rep("",4),"Note: R indicates data has been revised. Please see dashboard for further information.")
                  }else{
                    rep("",5)
                  }
                ),
              file, row.names = FALSE)
  }
)


##TRENDS ----
##plot trends part 2 ----

output$chart_title_trend_part_2 <- renderUI({HTML(paste("Percentage of people referred for PDS who received a minimum of one year’s support within 12 months of diagnosis; Trend, Scotland "),
                                                  if(input$select_hb_ijb_trend_part_2 == "Scotland"){""
                                                  }else{
                                                    paste0("and ", input$select_hb_ijb_trend_part_2)})
})

trend_chart_data <- reactive({
  ldp_data %>%
    filter(ijb == input$select_hb_ijb_trend_part_2 | ijb == "Scotland", ldp == "total")})


output$trend_plot_part_2 <- renderPlotly({
  plot_trend_perc(trend_chart_data(), percent_met)
})



##data table trends part 2----    

output$table_trend_part_2_title <- renderUI({
  HTML(paste0("Percentage of people referred for PDS who received a minimum of one year’s support within 12 months of diagnosis; Trend, Scotland and ", input$select_hb_ijb))
})

table_trend_part_2_data <- reactive({
  
  
  if(input$select_hb_ijb == "Health Boards"){  
    
    ldp_data %>% 
      filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
      select(health_board, fy, percent_met) %>%
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
      mutate(percent_met = if_else(percent_met == "   NA", "-", paste0(percent_met, "%"))) %>% 
      distinct(health_board, fy, .keep_all = T) %>% 
      rename("Health Board" = "health_board") 
    
  }else{
    
    ldp_data %>% 
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
             right_align = 1:length(included_years), selected = 1, rows_to_display = 32)
  
})

### download button data ldp2 trend----
output$downloadData_ldp2_trend <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(table_trend_part_2_data() %>% 
                mutate(across(where(is.factor), ~as.character(.))) %>% 
                mutate(fy = case_when(
                  fy == provisional_year_sup ~paste0(provisional_year,"P"),
                  fy == revised_year_sup ~paste0(revised_year,"R"),
                  TRUE ~fy)) %>% 
                pivot_wider(names_from = fy, values_from = percent_met) %>% 
                mutate(Measure = "Percentage of LDP standard achieved", 
                       .before = everything()) %>% 
                rbind(c("Note: P indicates data is provisional. Please see dashboard for further information.",rep("",length(included_years)+1))
                      
                ) %>% 
                rbind(c("Note: R indicates data has been revised. Please see dashboard for further information.",rep("",length(included_years)+1))
                ), 
              file, row.names = FALSE)
  }
)

# updates radio buttons label depending on selection
observe(if(input$ldp_sidebar == "trends"){
    updateRadioButtons(session, "select_hb_ijb",
                       label = "In the table show:"
    )
  }
)
observe(if(input$ldp_sidebar == "outcomes"){
  updateRadioButtons(session, "select_hb_ijb",
                     label = "In the chart and table show:"
  )
}
)


### END OF SCRIPT ###