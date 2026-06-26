############################### PAGE 3: Pathways ###############################.

################################################################################.
### UI ----
################################################################################.

output$pathways_ui <-  renderUI({
  div(column(
    
    ##############################################.
    ## Time to first contact by Financial Year ----
    ##############################################.
    
    conditionalPanel(
      condition = 'input.pathways_sidebar == "wait"',
      fluidRow(
        h4(strong(htmlOutput("plot_title_pathways"))), # Plot title
        plotlyOutput("plot_pathways"), # Plot
        h4(strong(htmlOutput("table_title_pathways"))), # Table title
        downloadButton("downloadData_pathways", "Download table data"), # Download button
        DT::dataTableOutput("table_pathways"), # Table
        linebreaks(1)
      ) # fluidRow
    ), # conditionalPanel
    
    ##############################################.
    ## Trends ----
    ##############################################.
    
    conditionalPanel(
      condition = 'input.pathways_sidebar == "trends"',
      fluidRow(
        h4(strong(htmlOutput("plot_title_pathways_trend"))), # Plot title
        plotlyOutput("plot_pathways_trend", height = "310px"), # Plot
        h4(strong(htmlOutput("table_title_pathways_trend"))), # Table title
        downloadButton("downloadData_pathways_trend", "Download table data"), # Download button
        DT::dataTableOutput("table_pathways_trend"), # Table
        linebreaks(1),
      ) # fluidRow
    ), # conditionalPanel
    
    ##############################################.
    ## Notes ----
    ##############################################.
    
    p(paste0("Source: Public Health Scotland quarterly dementia post-diagnostic support dataset: Data submissions from NHS Boards as at ",
             format(end_date, "%d %B %Y")
    )),

    h4(strong("Notes:")),
    p(paste0("ᴾ Figures for ", provisional_year, " and ", extra_referrals_year, " are provisional subject to all service users completing their support.")),
    p(paste0("ᴿ Figures for ", revised_year, " have been revised and are now final.")),
    p("Date of diagnosis is the date recorded for when the service user received a confirmed diagnosis of dementia.
      This diagnosis must be confirmed by a doctor or clinical practitioner with sufficient training and experience in the diagnosis of dementia.", 
      br(), 
      "First contact is the date on which an appropriate face to face direct contact took place with the service user by the PDS Practitioner or PDS Team 
      with the knowledge and skills to introduce each model of care. Direct contact can be done in person or by video link and is not restricted to both 
      parties being in the same room."),					
    p("Figures for 2018/19, 2019/20 and 2020/21 for Aberdeen City, NHS Grampian and Scotland are affected by the change in service provision of PDS within Aberdeen City during 2019. See Note 5 on the",
      a(href = "#", "Home", onclick = "Shiny.setInputValue('home_link', Math.random()); return false;"),
      "page for further information."),
    p("NHS Shetland / Shetland Islands did not have a PDS worker in post from 2022/23 Q1 through 2023/24 Q3. This will affect the pathway waiting times for NHS Shetland / Shetland Islands for 2022/23 and 2023/24. See Note 7 on the",
      a(href = "#", "Home", onclick = "Shiny.setInputValue('home_link', Math.random()); return false;"),
      "page for further information."),
    p("NHS Orkney/Orkney Islands had no referrals in 2022/23 (Q3 and Q4) and 2023/24 (Q1 and Q2) as they were unable to access a consultant psychiatrist. This will affect the pathway waiting times for NHS Orkney Islands / Orkney Islands for 2022/23 and 2023/24."),
    width = 12,
    style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available" # Fix panel so sidebar and navigation bar do not scroll with content
    
  ),) # column, div
}) # renderUI

################################################################################.
### SERVER ----
################################################################################.

# Filter data ----
data_wait_sel_yrs <- data_wait %>% filter(fy %in% included_years_extra_referrals)

##############################################.
## Time to first contact by Financial Year ----
##############################################.

# Plot ---------------------------------------

# Plot title (Time to first contact by Financial Year)
output$plot_title_pathways <- renderUI({
  HTML(paste0("Average (median) days from diagnosis to first contact by PDS practitioner: Financial Year ", 
              input$select_year_pathways, ", Scotland and ", input$select_hb_ijb_pathways))
})

# Plot data (Time to first contact by Financial Year)
wait_times_chart_data <- reactive({
  median_data <- data_wait_sel_yrs %>% 
    filter(fy == input$select_year_pathways) %>% 
    mutate(median_diagnosis_to_contact = if_else(is.na(median_diagnosis_to_contact) | median_diagnosis_to_contact < 0, 0, median_diagnosis_to_contact))
  if (input$select_hb_ijb_pathways == "Health Boards") {
    # Health Boards
    left_join(median_data %>% 
                filter(grepl("NHS", ijb)) %>% 
                select(health_board, fy, median_diagnosis_to_contact),
              median_data %>% 
                filter(health_board == "Scotland") %>% 
                select(fy, median_diagnosis_to_contact) %>%
                rename(scot_median_diagnosis_to_contact = median_diagnosis_to_contact)) %>%
      mutate(median_diagnosis_to_contact = if_else(health_board == "NHS Grampian" & fy %in% c("2019/20", "2020/21"), 0, median_diagnosis_to_contact)) %>% 
      rename(geog = health_board)
  } else {
    # IAA
    left_join(median_data %>% 
                filter(!grepl("NHS", ijb), ijb != "Scotland") %>% 
                select(ijb, fy, median_diagnosis_to_contact),
              median_data %>% 
                filter(ijb == "Scotland") %>% 
                select(fy, median_diagnosis_to_contact) %>% 
                rename(scot_median_diagnosis_to_contact = median_diagnosis_to_contact)) %>% 
      mutate(median_diagnosis_to_contact = if_else(ijb == "Aberdeen City" & fy %in% c("2019/20", "2020/21"), 0, median_diagnosis_to_contact)) %>% 
      rename(geog = ijb)
  }
})

# Plot (Time to first contact by Financial Year)
output$plot_pathways <- renderPlotly({
  plot_bar(wait_times_chart_data())
})

# Table --------------------------------------

# Table title (Time to first contact by Financial Year)
output$table_title_pathways <- renderUI({
  HTML(paste0("Number of referrals and average (median) days from diagnosis to first contact by PDS practitioner: Financial Year ", 
              input$select_year_pathways, ", Scotland and ", input$select_hb_ijb_pathways))
})

# Table data (Time to first contact by Financial Year)
median_table_data <- reactive({
  if (input$select_hb_ijb_pathways == "Health Boards") {
    # Health Boards
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
      mutate(median_diagnosis_to_contact = if_else(
        grepl(".0", median_diagnosis_to_contact), 
        substr(median_diagnosis_to_contact, 1, nchar(median_diagnosis_to_contact) - 2),
        median_diagnosis_to_contact)) %>% 
      rename(`Number of People Referred to PDS` = total_referrals, 
             `% of Referrals contacted by PDS practitioner` = perc_contacted,
             `Average (median) days from diagnosis to first contact` = median_diagnosis_to_contact) %>% 
      rename("Health Board" = "health_board")
  } else {
    # IAA
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
      mutate(median_diagnosis_to_contact = if_else(
        grepl(".0", median_diagnosis_to_contact), 
        substr(median_diagnosis_to_contact, 1, nchar(median_diagnosis_to_contact) - 2),
        median_diagnosis_to_contact)) %>% 
      rename(`Number of People Referred to PDS` = total_referrals, 
             `% of Referrals contacted by PDS practitioner` = perc_contacted,
             `Average (median) days from diagnosis to first contact` = median_diagnosis_to_contact) %>% 
      rename("Integration Authority Area" = "ijb")
  }
})

# Table (Time to first contact by Financial Year)
output$table_pathways <- DT::renderDataTable({
  make_table(
    median_table_data(), 
    right_align = 1:3, 
    selected = 1, 
    rows_to_display = 32, 
    filename = paste0("pds_wait_times_iaa_", input$select_year_pathways))
})

# Download data ------------------------------

# Download data (Time to first contact by Financial Year)
output$downloadData_pathways <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(
      median_table_data() %>%
        mutate(across(where(is.factor), ~as.character(.))) %>% 
        mutate(`Financial Year` = input$select_year_pathways, .before = everything()) %>% 
        mutate(`Financial Year` = case_when(
          `Financial Year` == provisional_year_sup ~paste0(provisional_year,"P"),
          `Financial Year` == revised_year_sup ~paste0(revised_year,"R"),
          TRUE ~`Financial Year`)) %>% 
        rbind( # Add revision and provisional notes
          if(input$select_year_pathways == provisional_year_sup){
            c("Note: P indicates data is provisional. Please see dashboard for further information.",rep("",4))
          } else if (input$select_year_randr == revised_year_sup){
            c("Note: R indicates data has been revised. Please see dashboard for further information.",rep("",4))
          } else {
            rep("",5)
          }
        ),
      file, 
      row.names = FALSE
    )
  }
)

##############################################.
## Trends ----
##############################################.

# Plot ---------------------------------------

# Plot title (Trends)
output$plot_title_pathways_trend <- renderUI({
  HTML(paste0("Average (median) days from diagnosis to first contact by PDS practitioner; Trend, Scotland "),
       if(input$select_hb_ijb_pathways_trend == "Scotland") {
         ""
       } else {
         paste0("and ", input$select_hb_ijb_pathways_trend)
       }
  )
})

# Plot data (Trends)
trend_pathways_chart_data <- reactive({
  data_wait_sel_yrs %>%
    #coding Aberdeen City and NHS Grampian medians as -999 so they do not appear on chart
    mutate(median_diagnosis_to_contact = if_else(
      ijb == "Aberdeen City" & fy %in% c("2019/20", "2020/21"), -999, median_diagnosis_to_contact)) %>% 
    mutate(median_diagnosis_to_contact = if_else(
      ijb == "NHS Grampian" & fy %in% c("2019/20", "2020/21"), -999, median_diagnosis_to_contact)) %>% 
    # recoding all negative wait times as NA
    mutate(median_diagnosis_to_contact = if_else(median_diagnosis_to_contact < 0, NA, median_diagnosis_to_contact)) %>% 
    filter(ijb == input$select_hb_ijb_pathways_trend | ijb == "Scotland")
})

# Plot (Trends)
output$plot_pathways_trend <- renderPlotly({
  plot_trend(
    trend_pathways_chart_data(), 
    measure = median_diagnosis_to_contact, ytitle = "Median time (days)"
  )
})

# Table --------------------------------------  

# Table title (Trends)
output$table_title_pathways_trend <- renderUI({
  HTML(paste0("Average (median) days from diagnosis to first contact by PDS practitioner; Trend, Scotland and ", 
              input$select_hb_ijb_pathways))
})

# Table data (Trends)
median_table_trend_data <- reactive({
  if (input$select_hb_ijb_pathways == "Health Boards") {
    # Health Boards
    median_hb_trend_table_data <- data_wait_sel_yrs %>%
      filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
      mutate(median_diagnosis_to_contact = if_else( # Coding NHS Grampian medians as -999 so they do not appear on chart
        ijb == "NHS Grampian" & fy %in% c("2019/20", "2020/21"), -999, median_diagnosis_to_contact)) %>%
      select(health_board, fy, median_diagnosis_to_contact) %>%
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>%
      mutate(median_diagnosis_to_contact = if_else( # Recoding all negative wait times as "-"
        grepl("-",median_diagnosis_to_contact), 
        "   -", 
        median_diagnosis_to_contact)) %>% 
      mutate(median_diagnosis_to_contact = if_else( # Displays whole numbers without ".0"
        grepl(".0", median_diagnosis_to_contact), 
        substr(median_diagnosis_to_contact, 1, nchar(median_diagnosis_to_contact) - 2),
        median_diagnosis_to_contact)) %>% 
      rename("Health Board" = "health_board")
  } else {
    # IAA
    median_ijb_trend_table_data <- data_wait_sel_yrs %>%
      filter(!grepl("NHS", ijb)) %>% 
      arrange(ijb) %>% 
      mutate(median_diagnosis_to_contact = if_else( # Coding Aberdeen City medians as -999 so they do not appear on chart
        ijb == "Aberdeen City" & fy %in% c("2019/20", "2020/21"), 
        -999, 
        median_diagnosis_to_contact)) %>% 
      select(ijb, fy, median_diagnosis_to_contact) %>%
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>%
      mutate(median_diagnosis_to_contact = if_else( # Recoding all negative wait times as "-"
        grepl("-",median_diagnosis_to_contact), 
        "   -", 
        median_diagnosis_to_contact)) %>% 
      mutate(median_diagnosis_to_contact = if_else( # Displays whole numbers without ".0"
        grepl(".0", median_diagnosis_to_contact), 
        substr(median_diagnosis_to_contact, 1, nchar(median_diagnosis_to_contact) - 2),
        median_diagnosis_to_contact)) %>% 
      rename("Integration Authority" = "ijb")
  }
})

# Table (Trends)
output$table_pathways_trend <- DT::renderDataTable({
  make_table(
    median_table_trend_data() %>% 
      pivot_wider(names_from = fy, values_from = median_diagnosis_to_contact),
    right_align = 1:length(included_years), 
    rows_to_display = 32, 
    selected = 1, 
    filename = paste0("pds_wait_times_iaa_trend"))
})

# Download data ------------------------------

# Download data (Trends)
output$downloadData_pathways_trend <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(
      median_table_trend_data() %>% 
        mutate(across(where(is.factor), ~as.character(.))) %>% 
        mutate(fy = case_when(
          fy == provisional_year_sup ~paste0(provisional_year,"P"),
          fy == revised_year_sup ~paste0(revised_year,"R"),
          TRUE ~fy)) %>% 
        pivot_wider(names_from = fy, values_from = median_diagnosis_to_contact) %>% 
        mutate(Measure = "Average (median) days from diagnosis to first contact", .before = everything()) %>% 
        rbind(c("Note: P indicates data is provisional. Please see dashboard for further information.", rep("", length(included_years) + 1))) %>% # Add provisional note
        rbind(c("Note: R indicates data has been revised. Please see dashboard for further information.", rep("", length(included_years) + 1))), # Add revision note
      file, 
      row.names = FALSE
    )
  }
)

################################# END OF SCRIPT ################################.