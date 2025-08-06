
####################### Page 2 REFERRALS & RATES #######################
#UI ----
output$rates_ui <-  renderUI({
  
  div(    fluidRow(column(
    linebreaks(1),
    radioGroupButtons("RandR_tab", label = NULL, choices = RandR_tab_list,
                      status = "tab",
                      direction = "horizontal",
                      justified = T,
                      size = "normal"),
    width = 12)
  ), #fluidRow
  #RandR Total Referrals----
  conditionalPanel(condition = 'input.RandR_tab == "RandR_part_1"',
                   # Referral BY YEAR----
                   conditionalPanel(condition = 'input.RandR_sidebar == "RandRreferrals"',
                                    fluidRow(
                                      column(
                                        h3(strong(htmlOutput("title_part_1_randr"))),
                                        linebreaks(1),          
                                        # value box ----
                                        fluidRow(
                                          shinydashboard::valueBox(
                                            value = textOutput("scot_randr"),
                                            subtitle = "of people estimated to be newly diagnosed with dementia were referred for post-diagnostic support.",
                                            width = 7,
                                            color = "fuchsia"), #valueBox
                                          box(htmlOutput("scot_randr_text"),
                                              status = "primary", 
                                              title = (p(strong("How is this figure calculated?"))),
                                              width = 5), # box
                                        ), #fluidRow
                                        fluidRow(
                                          linebreaks(1),
                                          #plot ----
                                          h4(strong(htmlOutput("hb_RandR_plot_title"))),
                                          plotlyOutput("hb_RandR_plot"),
                                          #plot ijb ----
                                          h3(strong(htmlOutput("ijb_RandR_plot_title"))),
                                          plotlyOutput("ijb_RandR_plot"),
                                          #  table ----
                                          h4(strong(htmlOutput("hb_randr_table_title"))),
                                          DT::dataTableOutput("table_hb_randr"),
                                          linebreaks(1),
                                          #  table ----
                                          h3(strong(htmlOutput("ijb_randr_table_title"))),
                                          DT::dataTableOutput("table_ijb_randr"),
                                          linebreaks(1)
                                        ), # fluid Row
                                        width = 12,
                                        style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available"),
                                    ) #fluidRow
                   ), #cond panel outcomes
                   #TRENDS ----
                   conditionalPanel(condition = 'input.RandR_sidebar == "RandRtrends"',
                                    fluidRow(
                                      column(
                                        # plot ----
                                        h4(strong(htmlOutput("randr_chart_title_trend_part_1"))),
                                        fluidRow(
                                          column(
                                            selectInput("select_randr_hb_trend_part_1",
                                                        label = "Select Health Board/Integration Authority to show in chart:",
                                                        choices = c("Scotland", boards, ijb_list), width = "100%"), width = 5)),
                                        plotlyOutput("hb_RandR_trend_plot"),
                                        #linebreaks(1),
                                        # table ----
                                        h4(strong(htmlOutput("randr_table_title_hb_trend_part_1"))),
                                        DT::dataTableOutput("randr_table_hb_trend_part_1"),
                                        linebreaks(1),
                                        width = 12,
                                        style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available"
                                      ), # column
                                    ) # fluidRow
                   ) #cond panel trends
  ), #cond panel part 1
  # Referral PART 2 ----
  conditionalPanel(condition = 'input.RandR_tab == "RandR_part_2"',
                   #Referrals BY YEAR----
                   conditionalPanel(condition = 'input.RandR_sidebar == "RandRreferrals"',
                                    fluidRow(
                                      column(
                                        h3(strong(htmlOutput("randr_title_part_2"))),
                                        linebreaks(1),
                                        # value box ----
                                        fluidRow(
                                          shinydashboard::valueBox(
                                            value = textOutput("scot_pds_referrals"),
                                            subtitle = "of those referred for post-diagnostic support received a minimum of 12 months of support.",
                                            width = 7,
                                            color = "fuchsia"), #valueBox
                                          box(htmlOutput("scot_referrals_text"),
                                              status = "primary",
                                              title = (p(strong("How is this figure calculated?"))),
                                              width = 5), #box
                                        ), #fluidRow
                                        fluidRow(
                                          linebreaks(1),
                                          #plot ----
                                          h4(strong(htmlOutput("referrals_plot_title"))),
                                          plotlyOutput("referrals_plot"),
                                          # table ----
                                          h4(strong(htmlOutput("referrals_table_title"))),
                                          DT::dataTableOutput("referrals_table"),
                                          linebreaks(1),
                                        ), # fluid Row
                                        width = 12,
                                        style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available"),
                                    ) #fluidRow
                   ), # cond panel outcomes
                   # TRENDS----
                   conditionalPanel(condition = 'input.RandR_sidebar == "RandRtrends"', 
                                    fluidRow(
                                      column(
                                        #plot----
                                        h4(strong(htmlOutput("randr_chart_title_trend_part_2"))),
                                        fluidRow(
                                          column(
                                            selectInput("randr_select_hb_ijb_trend_part_2",
                                                        label = "Select Health Board/Integration Authority to show in chart:",
                                                        choices = c("Scotland", boards, ijb_list), width = "100%"), width = 5)),
                                        plotlyOutput("randr_trend_plot_part_2"),
                                        #linebreaks(1),
                                        #table----
                                        h4(strong(htmlOutput("randr_table_trend_part_2_title"))),
                                        radioButtons("randr_select_table_trend_part_2",
                                                     label = "In the table below show:",
                                                     choices = c("Health Boards", "Integration Authority Areas"),
                                                     selected = "Health Boards",
                                                     inline = TRUE),
                                        DT::dataTableOutput("randr_table_hb_ijb_trend_part_2"),
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

#part 1 title ----
output$title_part_1_randr <- renderUI({HTML(paste("Percentage of estimated diagnoses referred for PDS; Scotland, ", 
                                                  input$select_year_p1_randr))
})

#part 2 title ----
output$randr_title_part_2 <- renderUI({HTML(paste("Percentage of referrals for PDS who received one year's support; Scotland, ", 
                                                  input$select_year_p1_randr))
})

#value boxes data ----
vb_data<- reactive({annual_table_data %>% filter(health_board == "Scotland", ijb == "Scotland", fy == input$select_year_p1_randr, ldp == "total")}) 

# total number of people estimated to be newly diagnosed with dementia were referred for post-diagnostic support
output$scot_randr <- renderText({paste0(vb_data()$referrals)})

output$scot_randr_text <- renderUI({
  HTML(paste("A total of", "<b>",  prettyNum(vb_data()$referrals, big.mark = ","), "</b>", "referrals were made to post-diagnostic support. This is divided by",
             "<b>", prettyNum(vb_data()$diagnoses, big.mark = ","), "</b>", "the estimated number of people newly diagnosed with dementia."))})

# percentage of those referred for post-diagnostic support received a minimum of 12 months of support
output$scot_pds_referrals <- renderText({paste0(vb_data()$percent_met, "%")})

vb_2_data<- reactive({annual_table_data %>% filter(health_board == "Scotland", ijb == "Scotland", fy == input$select_year_p1_randr,
                                                   ldp != "fail") %>% select(-diagnoses, -exp_perc) %>% 
    pivot_wider(values_from = referrals, names_from = ldp)})

output$scot_referrals_text <- renderUI({
  HTML(paste("<b>", prettyNum(vb_2_data()$complete + vb_2_data()$exempt, big.mark = ","), "</b>", "referrals either met or were exempt from the LDP standard. This is divided by",
             "<b>", prettyNum(vb_2_data()$total - vb_2_data()$ongoing, big.mark = ","), "</b>", "the total number of referrals (excluding those whose support is ongoing)."))})


# plot RandR part 1 ----
output$hb_RandR_plot_title <- renderUI({HTML(paste0("Number of Individuals Diagnosed and Referred for PDS; ", 
                                                    input$select_year_p1_randr, ", Health Boards"))
})

output$hb_RandR_plot <- renderPlotly({
  plot_bar_no_line(annual_table_data %>% filter(grepl("NHS", ijb), fy == input$select_year_p1_randr, ldp == "total") %>% 
                     rename(geog = health_board),
                   ytitle = "Number Referred",
                   measure = referrals, measure_text = "Number of referrals to PDS: "
  )
})

# plot RandR IJB part 1 ----
output$ijb_RandR_plot_title <- renderUI({HTML(paste0("Total number of people estimated to be newly diagnosed with dementia who were referred for PDS; ", 
                                                    input$select_year_p1_randr, ", Scotland and IJB"))
})

output$ijb_RandR_plot <- renderPlotly({
  plot_bar_no_line(annual_table_data %>% filter((!grepl(("NHS|Scotland"), annual_table_data$ijb)),fy == input$select_year_p1_randr, ldp == "total") %>% 
                     rename(geog = ijb),
                   ytitle = "Referrals",
                   measure = referrals, measure_text = "Number of referrals to PDS"
  )
})

# data table referrals part 1 ----
output$hb_randr_table_title <- renderUI({HTML(paste0("Number and percentage of people estimated to be newly diagnosed with dementia who were referred for PDS; ", 
                                                     input$select_year_p1_randr, ", Scotland and Health Boards"))
})

output$table_hb_randr <- DT::renderDataTable({
  table_hb_randr_data <- annual_table_data %>% 
    filter(fy == input$select_year_p1_randr) %>%
    filter(grepl("NHS", ijb) | ijb == "Scotland", !is.na(diagnoses)) %>% 
    group_by(health_board)%>%
    select(health_board, diagnoses, referrals)%>%
    mutate(exp_perc = paste0(round(referrals/diagnoses*100, 1), "%")) %>%  
    arrange(health_board) %>% 
    set_colnames(c("Health Board","Estimated Number of People Newly Diagnosed with Dementia", "Number of People Referred to PDS","Percentage of Estimated Number of People Diagnosed with Dementia Referred to PDS"))
  make_table(table_hb_randr_data, right_align = 1:3, selected = 1, table_elements = "t") %>% formatCurrency(c(2,3), currency = "", interval = 3, mark = ",", digits = 0)
})

# data table ijb referrals part 1 ----
output$ijb_randr_table_title <- renderUI({HTML(paste0("Number and percentage of people estimated to be newly diagnosed with dementia who were referred for PDS; ", 
                                                     input$select_year_p1_randr, ", Scotland and Health Boards"))
})

output$table_ijb_randr <- DT::renderDataTable({
  table_ijb_randr_data <- annual_table_data %>% 
    filter(fy == input$select_year_p1_randr) %>%
    filter(!grepl(("NHS|Scotland"), annual_table_data$ijb), !is.na(diagnoses)) %>% 
    group_by(ijb)%>%
    select(ijb, diagnoses, referrals)%>%
    mutate(exp_perc = paste0(round(referrals/diagnoses*100, 1), "%")) %>%  
    arrange(ijb) %>% 
    set_colnames(c("IJB","Estimated Number of People Newly Diagnosed with Dementia", "Number of People Referred to PDS","Percentage of Estimated Number of People Diagnosed with Dementia Referred to PDS"))
  make_table(table_ijb_randr_data, right_align = 1:3, selected = 1, table_elements = "t") %>% formatCurrency(c(2,3), currency = "", interval = 3, mark = ",", digits = 0)
})


#plot referrals part 2 ----
output$referrals_plot_title <- renderUI({HTML(paste0("Percentage of people referred for PDS who received a minimum of one year’s support within 12 month's of diagnosis; ", 
                                                     input$select_year_p1_randr, ", Scotland and ", input$select_hb_ijb))
})

output$referrals_plot <- renderPlotly({
  
  if(input$select_hb_ijb == "Health Boards"){
    
    plot_bar(annual_table_data %>% filter(grepl("NHS", ijb) | ijb == "Scotland", fy == input$select_year_p1_randr, ldp == "total") %>% 
               mutate(colour = if_else(ijb == "Scotland", "B", "A")), 
             category = ijb, 
             measure = percent_met,
             fill = colour)
    
  }else{
    
    plot_bar(annual_table_data %>% filter(!grepl("NHS", ijb), fy == input$select_year_p1_randr, ldp == "total") %>% 
               mutate(colour = if_else(ijb == "Scotland", "B", "A")), 
             category = ijb, 
             measure = percent_met,
             fill = colour)
  }
  
})

# data table referrals part 2 ----
output$referrals_table_title <- renderUI({HTML(paste0("Number and percentage of people referred for PDS who received a minimum of one year’s support within 12 month's of diagnosis; ", 
                                                      input$select_year_p1_randr, ", Scotland and ", input$select_hb_ijb))
})

output$referrals_table <- DT::renderDataTable({
  
  if(input$select_hb_ijb == "Health Boards"){
    
    table_hb_data <- annual_table_data %>% 
      filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
      filter(fy == input$select_year_p1_randr) %>%
      select(health_board,ldp,referrals,percent_met)%>%
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
      mutate(percent_met = if_else(percent_met == "   NA", "-", paste0(percent_met, "%"))) %>% 
      pivot_wider(names_from=ldp,values_from=referrals) %>% 
      select(health_board, total, complete, exempt, ongoing, fail, percent_met) %>% 
      arrange(health_board) %>% 
      set_colnames(c("NHS Board","Number of People Referred to PDS", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved")) 
    make_table(table_hb_data, right_align = 1:6, selected = 1, table_elements = "t") 
    
    
  }else{
    
    
    table_ijb_data <- annual_table_data %>%
      filter(!grepl("NHS", ijb)) %>% 
      filter(fy == input$select_year_p1_randr) %>%
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
      mutate(percent_met = if_else(percent_met == "   NA", "-", paste0(percent_met, "%"))) %>% 
      mutate(referrals = if_else(fy %in% c("2019/20", "2020/21") & ijb == "Aberdeen City" & ldp != "total", "-", as.character(referrals))) %>% 
      select(ijb,ldp,referrals,percent_met) %>%
      pivot_wider(names_from=ldp,values_from=referrals) %>% 
      select(ijb, total, complete, exempt, ongoing, fail, percent_met) %>% 
      arrange(ijb) %>% 
      set_colnames(c("Integration Authority Area","Number of People Referred to PDS", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved"))
    make_table(table_ijb_data, right_align = 1:6, selected = 1, rows_to_display = 32, table_elements = "t")
    
  }
})

#plot trends part 1----

output$randr_chart_title_trend_part_1 <- renderUI({HTML(paste0("Number of Individuals Diagnosed and Referred for PDS; Trend, ",
                                                        input$select_randr_hb_trend_part_1))
})

output$hb_RandR_trend_plot <- renderPlotly({
  plot_trend(annual_table_data %>% filter(ijb == input$select_randr_hb_trend_part_1, fy %in% included_years_extra_referrals, ldp == "total"),
             measure = referrals, ytitle = "Number Referred",
             colours = if(input$select_randr_hb_trend_part_1 == "Scotland"){"#9B4393"
                                                                                    }else{"#0078D4"}
  )
})

# data table trends part 1----     

output$randr_table_title_hb_trend_part_1 <- renderUI({HTML(paste0("Number of Individuals Diagnosed and Referred for PDS; Trend, Scotland and Health Boards"))})

output$randr_table_hb_trend_part_1 <- DT::renderDataTable({
  trend_hb_data_2 <- annual_table_data %>% 
    filter(fy %in% included_years_extra_referrals, ldp == "total") %>% 
    select(health_board, fy, referrals) %>%
    distinct(health_board, fy, .keep_all = T) %>% 
    pivot_wider(names_from = fy, values_from = referrals) %>% 
    mutate(across(where(is.numeric), ~prettyNum(., big.mark = ","))) %>% 
    rename("Health Board" = "health_board")  
  make_table(trend_hb_data_2, right_align = 1:length(included_years_extra_referrals), selected = 1, table_elements = "t")
})




#plot trends part 2 ----

output$randr_chart_title_trend_part_2 <- renderUI({HTML(paste("Percentage of people referred for PDS who received a minimum of one year’s support within 12 months of diagnosis; Trend, Scotland "),
                                                        if(input$randr_select_hb_ijb_trend_part_2 == "Scotland"){""
                                                        }else{
                                                          paste0("and ", input$randr_select_hb_ijb_trend_part_2)})
})

trend_chart_data <- reactive({
  annual_table_data %>%
    filter(fy %in% included_years, ldp == "total") %>% 
    filter(ijb == input$select_hb_ijb_trend_part_2 | ijb == "Scotland")})


output$randr_trend_plot_part_2 <- renderPlotly({
  plot_trend(trend_chart_data(), percent_met)
})



#data table trends part 2----    

output$randr_table_trend_part_2_title <- renderUI({
  HTML(paste0("Percentage of people referred for PDS who received a minimum of one year’s support within 12 months of diagnosis; Trend, Scotland and ", input$randr_select_table_trend_part_2))
})

output$randr_table_hb_ijb_trend_part_2 <- DT::renderDataTable({
  
  if(input$randr_select_table_trend_part_2 == "Health Boards"){  
    
    trend_hb_data <- annual_table_data %>% 
      filter(fy %in% included_years) %>% 
      select(health_board, fy, percent_met) %>%
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
      mutate(percent_met = if_else(percent_met == "   NA", "-", paste0(percent_met, "%"))) %>% 
      distinct(health_board, fy, .keep_all = T) %>% 
      pivot_wider(names_from = fy, values_from = percent_met) %>% 
      rename(" " = "health_board") 
    make_table(trend_hb_data, right_align = 1:length(included_years), selected = 1, table_elements = "t")
    
  }else{
    
    trend_ijb_data <- annual_table_data %>% 
      filter(fy %in% included_years) %>% 
      filter(!grepl("NHS", ijb)) %>% 
      select(ijb, fy, percent_met) %>%
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
      mutate(percent_met = if_else(percent_met == "   NA", "-", paste0(percent_met, "%"))) %>% 
      distinct(ijb, fy, .keep_all = T) %>% 
      pivot_wider(names_from = fy, values_from = percent_met) %>% 
      rename(" " = "ijb") 
    make_table(trend_ijb_data, right_align = 1:length(included_years), selected = 1, rows_to_display = 32, table_elements = "t")
  }
})


