####################### Page 4 PATHWAYS #######################
#UI ----
  output$pathways_ui <-  renderUI({
    
    div(
      ## Waiting times by year----
     conditionalPanel(condition = 'input.pathways_sidebar == "wait"',
                fluidRow(
                    column(
                    h4(strong(htmlOutput("plot_title_pathways"))),
                    ###plot----
                    plotlyOutput("plot_pathways"),
                    ### table----
                    h4(strong(htmlOutput("table_title_pathways"))),
                    #####download button----
                    downloadButton("downloadData_pathways", 
                                   "Download table data"),
                    DT::dataTableOutput("table_pathways"), 
                    width = 12,
                    #fix panel so sidebar and navigation bar do not scroll with content
                    style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available",
   linebreaks(1))#column
             ) #fluidRow
          ), # conditionalPanel waiting times by year
     ## Trends ----
   conditionalPanel(condition = 'input.pathways_sidebar == "trends"',
                    fluidRow(
                      column(
                        ###plot----
                        h4(strong(htmlOutput("plot_title_pathways_trend"))),
                        plotlyOutput("plot_pathways_trend", height = "310px"),
                       # linebreaks(1),
                        ###table----
                        h4(strong(htmlOutput("table_title_pathways_trend"))),
                        #####download button trend----
                        downloadButton("downloadData_pathways_trend", 
                                      "Download table data"),
                        DT::dataTableOutput("table_pathways_trend"),
                        linebreaks(1),
                        width = 12,
                       #fix panel so sidebar and navigation bar do not scroll with content
                        style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available"
                      ), # column
                    ) # fluidRow
   ) #cond panel trends
   
          ) # div   
  }) # renderUI

# SERVER----
  
  ## bar plot for wait times ----
  output$plot_title_pathways <- renderUI({HTML(paste0("Average (median) days from diagnosis to first contact by PDS practitioner: Financial Year ", 
                                                      input$select_year_pathways, ", Scotland and ", input$select_hb_ijb_pathways))
                                               })

  
  
  output$plot_pathways <- renderPlotly({
    
    if(input$select_hb_ijb_pathways == "Health Boards"){
      
      median_data_hb <- data_wait %>% 
        filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
        select(health_board, fy, median_diagnosis_to_contact) %>% 
        mutate(median_diagnosis_to_contact = if_else(is.na(median_diagnosis_to_contact)| median_diagnosis_to_contact < 0, 0, median_diagnosis_to_contact))
      
      wait_times_hb_chart_data <- left_join(median_data_hb,
                                            median_data_hb %>% filter(health_board == "Scotland") %>% select(fy, median_diagnosis_to_contact) %>%
        rename(scot_median_diagnosis_to_contact = median_diagnosis_to_contact)) %>%
        filter(health_board != "Scotland") %>% 
        mutate(median_diagnosis_to_contact = if_else(health_board == "NHS Grampian" & fy %in% c("2019/20", "2020/21"), 0, median_diagnosis_to_contact)) %>% 
        rename(geog = health_board)
      
      plot_bar(wait_times_hb_chart_data %>% filter(fy == input$select_year_pathways))
      
    }else{
      
      median_data_ijb <- data_wait %>% 
        filter(!grepl("NHS", ijb)) %>% 
        select(ijb, fy, median_diagnosis_to_contact) %>% 
        mutate(median_diagnosis_to_contact = if_else(is.na(median_diagnosis_to_contact)| median_diagnosis_to_contact < 0, 0, median_diagnosis_to_contact)) %>% 
        arrange(ijb) %>% 
        mutate(median_diagnosis_to_contact = if_else(ijb == "Aberdeen City" & fy %in% c("2019/20", "2020/21"), 0, median_diagnosis_to_contact))
      
      wait_times_ijb_chart_data <- left_join(median_data_ijb,
                                             median_data_ijb %>% filter(ijb == "Scotland") %>% select(fy, median_diagnosis_to_contact) %>% 
        rename(scot_median_diagnosis_to_contact = median_diagnosis_to_contact)) %>% 
        filter(ijb != "Scotland") %>% 
        rename(geog = ijb)
      
      plot_bar(wait_times_ijb_chart_data %>% filter(fy == input$select_year_pathways))}  
  })

 ##wait times table by geography ----    
 
output$table_title_pathways <- renderUI({HTML(paste0("Number of referrals and average (median) days from diagnosis to first contact by PDS practitioner: Financial Year ", 
                                                   input$select_year_pathways, ", Scotland and ", input$select_hb_ijb_pathways))
})
  
  
median_table_data <- reactive({
    
    if(input$select_hb_ijb_pathways == "Health Boards"){
      
      data_wait %>% 
        filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
        filter(fy == input$select_year_pathways) %>% 
        select(health_board, fy, total_referrals, perc_contacted, median_diagnosis_to_contact) %>% 
        mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>%
        mutate(across(starts_with("perc"), ~ paste0(.,"%"))) %>%
        mutate(median_diagnosis_to_contact = if_else(grepl("-",median_diagnosis_to_contact), "   -", median_diagnosis_to_contact)) %>% 
        mutate(median_diagnosis_to_contact = if_else(median_diagnosis_to_contact < "  0", "-", median_diagnosis_to_contact)) %>% 
        mutate(median_diagnosis_to_contact = if_else(health_board == "NHS Grampian" & fy %in% c("2019/20", "2020/21"), "  -", median_diagnosis_to_contact)) %>% 
        mutate(perc_contacted = if_else(health_board == "NHS Grampian" & fy %in% c("2019/20", "2020/21"), "  -", perc_contacted)) %>%
        select(-fy) %>% 
        mutate(median_diagnosis_to_contact = if_else(grepl(".0", median_diagnosis_to_contact), 
                                                     substr(median_diagnosis_to_contact, 1, nchar(median_diagnosis_to_contact) - 2),
                                                     median_diagnosis_to_contact)) %>% 
        rename(`Number of People Referred to PDS` = total_referrals, 
               `% of Referrals contacted by PDS practitioner` = perc_contacted,
               `Average (median) days from diagnosis to first contact` = median_diagnosis_to_contact) %>% 
        rename("Health Board" = "health_board")
      
    }else{
      
      data_wait %>% 
        filter(!grepl("NHS", ijb)) %>% 
        filter(fy == input$select_year_pathways) %>% 
        select(ijb, fy, total_referrals, perc_contacted, median_diagnosis_to_contact) %>%
        mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>%
        mutate(across(starts_with("perc"), ~ paste0(.,"%"))) %>%
        mutate(median_diagnosis_to_contact = if_else(grepl("-",median_diagnosis_to_contact), "   -", median_diagnosis_to_contact)) %>% 
        arrange(ijb) %>% 
        mutate(median_diagnosis_to_contact = if_else(ijb == "Aberdeen City" & fy %in% c("2019/20", "2020/21"), "   -", median_diagnosis_to_contact)) %>% 
        mutate(perc_contacted = if_else(ijb == "Aberdeen City" & fy %in% c("2019/20", "2020/21"), "   -", perc_contacted)) %>%
        select(-fy) %>% 
        mutate(median_diagnosis_to_contact = if_else(grepl(".0", median_diagnosis_to_contact), 
                                                     substr(median_diagnosis_to_contact, 1, nchar(median_diagnosis_to_contact) - 2),
                                                     median_diagnosis_to_contact)) %>% 
        rename(`Number of People Referred to PDS` = total_referrals, 
               `% of Referrals contacted by PDS practitioner` = perc_contacted,
               `Average (median) days from diagnosis to first contact` = median_diagnosis_to_contact) %>% 
        rename("Integration Authority Area" = "ijb")
      
    }
    
  })

output$table_pathways <- DT::renderDataTable({
  
    make_table(median_table_data(), right_align = 1:3, selected = 1, rows_to_display = 32, filename = paste0("pds_wait_times_iaa_", input$select_year_pathways))
    
})

### download button ldp1----
output$downloadData_pathways <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(median_table_data() %>% mutate(financial_year = input$select_year_pathways, 
                                             .before = everything()) %>% 
                mutate(financial_year = case_when(
                  financial_year == provisional_year_sup ~paste0(provisional_year,"P"),
                  financial_year == revised_year_sup ~paste0(revised_year,"R"),
                  TRUE ~financial_year)),
              file, row.names = FALSE)
  }
)


## trend plot for wait times ----
output$plot_title_pathways_trend <- renderUI({HTML(paste0("Average (median) days from diagnosis to first contact by PDS practitioner; Trend, Scotland "),
                                                   if(input$select_hb_ijb_pathways_trend == "Scotland"){""
                                                   }else{
                                                     paste0("and ", input$select_hb_ijb_pathways_trend)})
})

trend_pathways_chart_data <- reactive({
  data_wait %>%
    filter(fy %in% included_years_sup) %>% # CHANGE to included_years from 2026 onwards
    #coding Aberdeen City and NHS Grampian medians as -999 so they do not appear on chart
    mutate(median_diagnosis_to_contact = 
             if_else(ijb == "Aberdeen City" & fy %in% c("2019/20", "2020/21"), -999, median_diagnosis_to_contact)) %>% 
    mutate(median_diagnosis_to_contact = 
             if_else(ijb == "NHS Grampian" & fy %in% c("2019/20", "2020/21"), -999, median_diagnosis_to_contact)) %>% 
    # recoding all negative wait times as NA
    mutate(median_diagnosis_to_contact = if_else(median_diagnosis_to_contact < 0, NA, median_diagnosis_to_contact)) %>% 
    filter(ijb == input$select_hb_ijb_pathways_trend | ijb == "Scotland")})

output$plot_pathways_trend <- renderPlotly({
  plot_trend(trend_pathways_chart_data(), measure = median_diagnosis_to_contact, ytitle = "Median Wait (days)")
  
})

##wait times table trend ----    

output$table_title_pathways_trend <- renderUI({HTML(paste0("Average (median) days from diagnosis to first contact by PDS practitioner; Trend, Scotland and ", input$select_pathways_trend_table))
})

median_table_trend_data <- reactive({
  
  if(input$select_pathways_trend_table == "Health Boards"){
    
    median_hb_trend_table_data <- data_wait %>%
      filter(fy %in% included_years_sup) %>% # CHANGE to included_years from 2026 onwards
      filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
      #coding NHS Grampian medians as -999 so they do not appear on chart
      mutate(median_diagnosis_to_contact = 
               if_else(ijb == "NHS Grampian" & fy %in% c("2019/20", "2020/21"), -999, median_diagnosis_to_contact)) %>%
      select(health_board, fy, median_diagnosis_to_contact) %>%
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>%
      # recoding all negative wait times as "-"
      mutate(median_diagnosis_to_contact = if_else(grepl("-",median_diagnosis_to_contact), "   -", median_diagnosis_to_contact)) %>% 
      # displays whole numbers without ".0"
      mutate(median_diagnosis_to_contact = if_else(grepl(".0", median_diagnosis_to_contact), 
                                                   substr(median_diagnosis_to_contact, 1, nchar(median_diagnosis_to_contact) - 2),
                                                   median_diagnosis_to_contact)) %>% 
      rename("Health Board" = "health_board")
    
  }else{
    
    median_ijb_trend_table_data <- data_wait %>%
      filter(fy %in% included_years_sup) %>% # CHANGE to included_years from 2026 onwards 
      filter(!grepl("NHS", ijb)) %>% 
      arrange(ijb) %>% 
      #coding Aberdeen City medians as -999 so they do not appear on chart
      mutate(median_diagnosis_to_contact = 
               if_else(ijb == "Aberdeen City" & fy %in% c("2019/20", "2020/21"), -999, median_diagnosis_to_contact)) %>% 
      select(ijb, fy, median_diagnosis_to_contact) %>%
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>%
      # recoding all negative wait times as "-"
      mutate(median_diagnosis_to_contact = if_else(grepl("-",median_diagnosis_to_contact), "   -", median_diagnosis_to_contact)) %>% 
      # displays whole numbers without ".0"
      mutate(median_diagnosis_to_contact = if_else(grepl(".0", median_diagnosis_to_contact), 
                                                   substr(median_diagnosis_to_contact, 1, nchar(median_diagnosis_to_contact) - 2),
                                                   median_diagnosis_to_contact)) %>% 
      rename("Integration Authority" = "ijb")
    
  }
  
})


output$table_pathways_trend <- DT::renderDataTable({
  
    make_table(median_table_trend_data() %>% 
                 pivot_wider(names_from = fy, values_from = median_diagnosis_to_contact),
               right_align = 1:length(included_years), rows_to_display = 32, selected = 1, filename = paste0("pds_wait_times_iaa_trend"))
  
})


### download button trend----
output$downloadData_pathways_trend <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(median_table_trend_data() %>% 
                mutate(fy = case_when(
                  fy == provisional_year_sup ~paste0(provisional_year,"P"),
                  fy == revised_year_sup ~paste0(revised_year,"R"),
                  TRUE ~fy)) %>% 
                pivot_wider(names_from = fy, values_from = median_diagnosis_to_contact) %>% 
                mutate(measure = "Average (median) days from diagnosis to first contact", 
                       .before = everything()), 
              file, row.names = FALSE)
  }
)

### END OF SCRIPT ###
