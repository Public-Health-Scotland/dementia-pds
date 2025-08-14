####################### Page 3 PATHWAYS #######################
#UI ----
output$pathways_ui <-  renderUI({
  
  div(column(
    ## Waiting times by year----
    conditionalPanel(condition = 'input.pathways_sidebar == "wait"',
                     fluidRow(
                       #  column(
                       h4(strong(htmlOutput("plot_title_pathways"))),
                       ###plot----
                       plotlyOutput("plot_pathways"),
                       ### table----
                       h4(strong(htmlOutput("table_title_pathways"))),
                       #####download button----
                       downloadButton("downloadData_pathways", 
                                      "Download table data"),
                       DT::dataTableOutput("table_pathways"), 
                       
                       linebreaks(1)
                     ) #fluidRow
    ), # conditionalPanel waiting times by year
    ## Trends ----
    conditionalPanel(condition = 'input.pathways_sidebar == "trends"',
                     fluidRow(
                       # column(
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
                       #), # column
                     ) # fluidRow
    ), #cond panel trends
  p(paste0("Source: Public Health Scotland quarterly dementia post-diagnostic support dataset: Data submissions from NHS Boards as at ",
           format(end_date, "%d %B %Y"))),
    ## Notes----
  h4(strong("Notes:")),
  p(paste0("ᴾ Figures for ", provisional_year," are provisional subject to all service users completing their support.")),
  p("Date of diagnosis is the date recorded for when the service user received a confirmed diagnosis of dementia.
                       This diagnosis must be confirmed by a doctor or clinical practitioner with sufficient training and experience in the diagnosis of dementia.", 
    br(), 
    "First contact is the date on which an appropriate face to face direct contact took place with the service user by the PDS Practitioner or PDS Team 
                        with the knowledge and skills to introduce each model of care. Direct contact can be done in person or by video link and is not restricted to both 
                        parties being in the same room."),					
  p("Figures for 2018/19, 2019/20 and 2020/21 for Aberdeen City, NHS Grampian and Scotland are affected by the change in service provision of PDS within Aberdeen City during 2019. See Note 5 on the",
    a(
      href = "#",
      "Home",
      onclick = "Shiny.setInputValue('home_link', Math.random()); return false;"),
    "page for further information."),
  p("NHS Shetland / Shetland Islands did not have a PDS worker in post from 2022/23 Q1 through 2023/24 Q3. This will affect the pathway waiting times for NHS Shetland / Shetland Islands for 2022/23. See Note 7 on the",
    a(
      href = "#",
      "Home",
      onclick = "Shiny.setInputValue('home_link', Math.random()); return false;"),
    "page for further information."),
  width = 12,
  #fix panel so sidebar and navigation bar do not scroll with content
  style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available"
  ), # column
  ) # div   
}) # renderUI

# SERVER----
#REMOVE the two lines below from 2026 onwards ----
data_wait_sel_yrs <- data_wait %>% 
  filter(fy %in% included_years_2025_gender_wait)
#UNCOMMENT the two lines below from 2026 onwards ----
# data_wait_sel_yrs <- data_wait %>% 
#   filter(fy %in% included_years)

## bar plot for wait times ----
output$plot_title_pathways <- renderUI({HTML(paste0("Average (median) days from diagnosis to first contact by PDS practitioner: Financial Year ", 
                                                    input$select_year_pathways, ", Scotland and ", input$select_hb_ijb_pathways))
})

wait_times_chart_data <- reactive({
  
  median_data <- data_wait_sel_yrs %>% filter(fy == input$select_year_pathways) %>% 
    mutate(median_diagnosis_to_contact = if_else(is.na(median_diagnosis_to_contact)| median_diagnosis_to_contact < 0, 0, median_diagnosis_to_contact))
  
  if(input$select_hb_ijb_pathways == "Health Boards"){
  
left_join(median_data %>% filter(grepl("NHS", ijb)) %>% 
            select(health_board, fy, median_diagnosis_to_contact),
          median_data %>% filter(health_board == "Scotland") %>% select(fy, median_diagnosis_to_contact) %>%
                      rename(scot_median_diagnosis_to_contact = median_diagnosis_to_contact)) %>%
    mutate(median_diagnosis_to_contact = if_else(health_board == "NHS Grampian" & fy %in% c("2019/20", "2020/21"), 0, median_diagnosis_to_contact)) %>% 
    rename(geog = health_board)
  
}else{
  
left_join(median_data %>% 
            filter(!grepl("NHS", ijb), ijb != "Scotland") %>% 
            select(ijb, fy, median_diagnosis_to_contact),
          median_data %>% filter(ijb == "Scotland") %>% select(fy, median_diagnosis_to_contact) %>% 
                rename(scot_median_diagnosis_to_contact = median_diagnosis_to_contact)) %>% 
    mutate(median_diagnosis_to_contact = if_else(ijb == "Aberdeen City" & fy %in% c("2019/20", "2020/21"), 0, median_diagnosis_to_contact)) %>% 
    rename(geog = ijb)
}
  
})

output$plot_pathways <- renderPlotly({
      plot_bar(wait_times_chart_data())
})

##wait times table by geography ----    

output$table_title_pathways <- renderUI({HTML(paste0("Number of referrals and average (median) days from diagnosis to first contact by PDS practitioner: Financial Year ", 
                                                     input$select_year_pathways, ", Scotland and ", input$select_hb_ijb_pathways))
})


median_table_data <- reactive({
  
  if(input$select_hb_ijb_pathways == "Health Boards"){
    
    data_wait_sel_yrs %>% 
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
    
    data_wait_sel_yrs %>% 
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

### download button data ----
output$downloadData_pathways <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(median_table_data() %>%
                mutate(across(where(is.factor), ~as.character(.))) %>% 
                mutate(`Financial Year` = input$select_year_pathways, 
                                             .before = everything()) %>% 
                mutate(`Financial Year` = case_when(
                  `Financial Year` == provisional_year_sup ~paste0(provisional_year,"P"),
                  `Financial Year` == revised_year_sup ~paste0(revised_year,"R"),
                  TRUE ~`Financial Year`)) %>% 
                #### adds revision and provisional note
                rbind(
                  if(input$select_year_pathways == provisional_year_sup){
                    c(rep("",4),"Note: P indicates data is provisional. Please see dashboard for further information.")
                    }else if(input$select_year_randr == revised_year_sup){
                      c("","","Note: R indicates data has been revised. Please see dashboard for further information.")
                  }else{
                    rep("",5)
                  }
                ),
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
  data_wait_sel_yrs %>%
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

output$table_title_pathways_trend <- renderUI({HTML(paste0("Average (median) days from diagnosis to first contact by PDS practitioner; Trend, Scotland and ", input$select_hb_ijb_pathways))
})

median_table_trend_data <- reactive({
  
  if(input$select_hb_ijb_pathways == "Health Boards"){
    
    median_hb_trend_table_data <- data_wait_sel_yrs %>%
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
    
    median_ijb_trend_table_data <- data_wait_sel_yrs %>%
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


### download button data trend----
output$downloadData_pathways_trend <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(median_table_trend_data() %>% 
                mutate(across(where(is.factor), ~as.character(.))) %>% 
                mutate(fy = case_when(
                  fy == provisional_year_sup ~paste0(provisional_year,"P"),
                  fy == revised_year_sup ~paste0(revised_year,"R"),
                  TRUE ~fy)) %>% 
                pivot_wider(names_from = fy, values_from = median_diagnosis_to_contact) %>% 
                mutate(Measure = "Average (median) days from diagnosis to first contact", 
                       .before = everything()) %>% 
                #### adds revision and provisional note
                rbind(c(rep("",length(included_years)+1),"Note: P indicates data is provisional. Please see dashboard for further information.")
                ),# %>% 
              # UNCOMMENT the line below from 2026 onward----
              #rbind(c(rep("",length(included_years)+1),"Note: R indicates data has been revised. Please see dashboard for further information.")), 
              file, row.names = FALSE)
  }
)

# updates radio buttons label depending on selection
observe(if(input$pathways_sidebar == "trends"){
  updateRadioButtons(session, "select_hb_ijb_pathways",
                     label = "In the table show:"
  )
}
)
observe(if(input$pathways_sidebar == "wait"){
  updateRadioButtons(session, "select_hb_ijb_pathways",
                     label = "In the chart and table show:"
  )
}
)


### END OF SCRIPT ###
