########################### PAGE 1: Referrals & Rates ##########################.

################################################################################.
### UI ----
################################################################################.

output$rates_ui <-  renderUI({
  div(
    
    ##############################################.
    ## Total Referrals ----
    ##############################################.
    
    conditionalPanel(
      condition = 'input.RandR_tab == "RandR_totals"',
      column(
        
        ##############################################.
        ## Referrals by Financial Year ----
        ##############################################.
        
        conditionalPanel(
          condition = 'input.RandR_sidebar == "referrals"',
          fluidRow(
            h3(strong(htmlOutput("title_totals_randr"))),
            linebreaks(1),          
            fluidRow( 
              shinydashboard::valueBox( # Value box
                value = textOutput("scot_randr"),
                subtitle = "people were diagnosed with dementia and referred for post-diagnostic support.",
                width = 12,
                color = "fuchsia"
              ), # valueBox
            ), # fluidRow
            fluidRow(column(
              linebreaks(1),
              h4(strong(htmlOutput("totals_RandR_plot_title"))), # Plot title
              plotlyOutput("totals_RandR_plot"), # Plot
              h4(strong(htmlOutput("totals_randr_table_title"))), # Table title
              downloadButton("downloadData_totals", "Download table data"), # Download button
              DT::dataTableOutput("table_totals_randr"), # Table
              linebreaks(1),
              width = 12
            )), # column, fluidRow
          ) # fluidRow
        ), # conditionalPanel
        
        ##############################################.
        ## Trends ----
        ##############################################.
        conditionalPanel(
          condition = 'input.RandR_sidebar == "trends"',
          fluidRow(
            h4(strong(htmlOutput("randr_chart_title_trend_totals"))), # Plot title
            plotlyOutput("totals_RandR_trend_plot"), # Plot
            linebreaks(1),
            h4(strong(htmlOutput("randr_table_title_trend_totals"))), # Table title
            downloadButton("downloadData_totals_trend", "Download table data"), # Download button
            DT::dataTableOutput("randr_table_trend_totals"), # Table
            linebreaks(1),
          ) # fluidRow
        ), # conditionalPanel
        
        ##############################################.
        ## Notes (Total Referrals) ----
        ##############################################.
        # Sources
        p("Source: Public Health Scotland quarterly dementia post-diagnostic support dataset: Data submissions from NHS Boards as at ",
          format(end_date, "%d %B %Y")),
        # Notes
        h4(strong("Notes:")),
        # Provisional years
        p(paste0("ᴾ Figures for ", provisional_year, " and ", extra_referrals_year, " are provisional subject to all service users completing their support.")),
        # Revised years
        p(paste0("ᴿ Figures for ", revised_year," have been revised and are now final."),),
        # NHS Grampian / Aberdeen City
        p("Figures for 2018/19, 2019/20 and 2020/21 for Aberdeen City, NHS Grampian and Scotland are affected by the change in service provision of PDS within Aberdeen City during 2019. See Note 5 on the",
          a(href = "#", "Home", onclick = "Shiny.setInputValue('home_link', Math.random()); return false;"), "page for further information."),
        # NHS Orkney / Orkney Islands
        p("NHS Orkney/Orkney Islands had no referrals in 2022/23 (Q3 and Q4) and 2023/24 (Q1 and Q2) as they were unable to access a consultant psychiatrist. This will affect the figures for NHS Orkney Islands / Orkney Islands for 2022/23 and 2023/24. See Note 8 on the",
          a(href = "#", "Home", onclick = "Shiny.setInputValue('home_link', Math.random()); return false;"), "page for further information."),
        # Formatting
        width = 12,
        style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available" # Fix panel so sidebar and navigation bar do not scroll with content
        
      ) # column
    ), # conditionalPanel
    
    ##############################################.
    ## Rates per 10,000 Population ----
    ##############################################.
    
    conditionalPanel(
      condition = 'input.RandR_tab == "RandR_rates"',
      column(
        
        ##############################################.
        ## Referrals by Financial Year ----
        ##############################################.
        
        conditionalPanel(
          condition = 'input.RandR_sidebar == "referrals"',
          fluidRow(
            h3(strong(htmlOutput("randr_title_rates"))),
            linebreaks(1),
            fluidRow(
              shinydashboard::valueBox( # Value box
                value = textOutput("scot_rate"),
                subtitle = "people per 10,000 population (aged 65 and over) were diagnosed with dementia and referred for post-diagnostic support.",
                width = 12,
                color = "fuchsia"
              ), # valueBox
            ), # fluidRow
            fluidRow(column(
              linebreaks(1),
              h4(strong(htmlOutput("rates_plot_title"))), # Plot title
              plotlyOutput("rates_plot"), # Plot
              h4(strong(htmlOutput("rates_table_title"))), # Table title
              downloadButton("downloadData_rates", "Download table data"), # Download button
              DT::dataTableOutput("rates_table"), # Table
              linebreaks(1),
              width = 12
            )), # column, fluidRow
          ) # fluidRow 
        ), # conditionalPanel
        
        ##############################################.
        ## Trends ----
        ##############################################.
        
        conditionalPanel(
          condition = 'input.RandR_sidebar == "trends"', 
          fluidRow(
            h4(strong(htmlOutput("randr_chart_title_trend_rates"))),
            plotlyOutput("randr_trend_plot_rates"),
            linebreaks(1),
            h4(strong(htmlOutput("randr_table_trend_rates_title"))),
            downloadButton("downloadData_rates_trend", "Download table data"),
            DT::dataTableOutput("randr_table_trend_rates"),
            linebreaks(1)
          ) # fluidRow
        ), # conditionalPanel
        
        ##############################################.
        ## Notes (Rates per 10,000 Population) ----
        ##############################################.
        # Sources
        p(paste0("Sources: Public Health Scotland quarterly dementia post-diagnostic support dataset: Data submissions from NHS Boards as at ",
                 format(end_date, "%d %B %Y"), "; National Records of Scotland (NRS) mid-year population estimates.")),
        # Notes
        h4(strong("Notes:")),
        # Provisional years
        p(paste0("ᴾ Figures for ", provisional_year, " and ", extra_referrals_year, " are provisional subject to all service users completing their support.")),
        # Revised years
        p(paste0("ᴿ Figures for ", revised_year," have been revised and are now final.")),
        # Population estimates
        p("Rates are calculated using the NRS mid-year population estimates of the 65 and over age group for each geographical area. See Note 9 on the",
          a(href = "#", "Home", onclick = "Shiny.setInputValue('home_link', Math.random()); return false;"), "page for further information."),
        # NHS Grampian / Aberdeen City
        p("Figures for 2018/19, 2019/20 and 2020/21 for Aberdeen City, NHS Grampian and Scotland are affected by the change in service provision of PDS within Aberdeen City during 2019. See Note 5 on the",
          a(href = "#", "Home", onclick = "Shiny.setInputValue('home_link', Math.random()); return false;"), "page for further information."),
        # NHS Orkney/Orkney Islands
        p("NHS Orkney/Orkney Islands had no referrals in 2022/23 (Q3 and Q4) and 2023/24 (Q1 and Q2) as they were unable to access a consultant psychiatrist. This will affect the figures for NHS Orkney Islands / Orkney Islands for 2022/23 and 2023/24. See Note 8 on the",
          a(href = "#", "Home", onclick = "Shiny.setInputValue('home_link', Math.random()); return false;"), "page for further information."),
        # Formatting
        width = 12,
        style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available"
      ), # column
    ) # conditionalPanel
    
  ) # div
}) # renderUI

################################################################################.
### SERVER ----
################################################################################.

##############################################.
## Total Referrals ----
##############################################.

# Filter data (Total Referrals) ----
referrals_data_sel_yrs <- annual_table_data %>% filter(fy %in% included_years_extra_referrals, ldp == "total")

##############################################.
## Referrals by Financial Year ----
##############################################.

# Value box ----------------------------------

# Value box title (Total Referrals; Referrals by Financial Year)
output$title_totals_randr <- renderUI({
  HTML(paste("Number of People Referred for PDS; Scotland, ", input$select_year_randr))
})

# Value box data (Total Referrals; Referrals by Financial Year)
vb_data_totals <- reactive({
  referrals_data_sel_yrs %>% 
    filter(health_board == "Scotland", ijb == "Scotland", fy == input$select_year_randr)
}) 

# Value box text (Total Referrals; Referrals by Financial Year)
output$scot_randr <- renderText({
  prettyNum(vb_data_totals()$referrals, big.mark = ",")
})

# Plot ---------------------------------------

# Plot title (Total Referrals; Referrals by Financial Year)
output$totals_RandR_plot_title <- renderUI({
  HTML(paste0("Number of people diagnosed with dementia who were referred for PDS; ",
              input$select_year_randr, 
              ", " ,
              input$select_hb_ijb_randr))
})

# Plot data (Total Referrals; Referrals by Financial Year)
totals_bar_plot_data <- reactive({
  if(input$select_hb_ijb_randr == "Health Boards"){
    referrals_data_sel_yrs %>% 
      filter(grepl("NHS", ijb), fy == input$select_year_randr) %>% 
      rename(geog = health_board)
  }else{
    referrals_data_sel_yrs %>% 
      filter((!grepl(("NHS|Scotland"), referrals_data_sel_yrs$ijb)), fy == input$select_year_randr) %>% 
      rename(geog = ijb)
  }
})

# Plot (Total Referrals; Referrals by Financial Year)
output$totals_RandR_plot <- renderPlotly({
  plot_bar_no_line(
    totals_bar_plot_data(),
    ytitle = "Number Referred",
    measure = referrals, 
    measure_text = "Number of people referred for PDS: ")
})

# Table --------------------------------------

# Table title (Total Referrals; Referrals by Financial Year)
output$totals_randr_table_title <- renderUI({
  HTML(paste0("Number of people diagnosed with dementia who were referred for PDS; ", 
              input$select_year_randr, ", Scotland and ", input$select_hb_ijb_randr))
})

# Table data (Total Referrals; Referrals by Financial Year)
table_totals_data <- reactive({
  if(input$select_hb_ijb_randr == "Health Boards"){
    referrals_data_sel_yrs %>%
      filter(fy == input$select_year_randr) %>%
      filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
      select(ijb,referrals) %>%
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
      arrange(ijb) %>% 
      set_colnames(c("Health Board","Number of People Referred to PDS"))
  }else{
    referrals_data_sel_yrs %>%
      filter(fy == input$select_year_randr) %>%
      filter(!grepl("NHS", ijb)) %>% 
      select(ijb,referrals)%>%
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
      arrange(ijb) %>% 
      set_colnames(c("Integration Authority Area","Number of People Referred to PDS"))
  }
})

# Table (Total Referrals; Referrals by Financial Year)
output$table_totals_randr <- DT::renderDataTable({
  make_table(
    table_totals_data(),
    right_align = 1, 
    selected = 1, 
    rows_to_display = 32)
})

# Download data ------------------------------

# Download data (Total Referrals; Referrals by Financial Year)
output$downloadData_totals <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(
      table_totals_data() %>%
        mutate(across(where(is.factor), ~as.character(.))) %>% 
        mutate(`Financial Year` = input$select_year_randr, .before = everything()) %>% 
        mutate(`Financial Year`  = case_when( # Changes superscript R to in line R for downloaded csv since superscript is not supported 
          `Financial Year`  == provisional_year_sup ~paste0(provisional_year,"P"),
          `Financial Year`  == revised_year_sup ~paste0(revised_year,"R"),
          `Financial Year`  == extra_referrals_year_sup ~paste0(extra_referrals_year,"P"),
          TRUE ~`Financial Year` )) %>% 
        rbind(
          if(input$select_year_randr == revised_year_sup){
            c("Note: R indicates data has been revised. Please see dashboard for further information.",rep("",2))
          }else if(input$select_year_randr == provisional_year_sup | input$select_year_randr == extra_referrals_year_sup){
            c("Note: P indicates data is provisional. Please see dashboard for further information.",rep("",2))
          }else{
            rep("",3)
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

# Plot title (Total Referrals; Trends)
output$randr_chart_title_trend_totals <- renderUI({
  HTML(paste0("Number of people diagnosed with dementia who were referred for PDS; Trend, ",
              input$select_randr_trend_totals))
})

# Plot (Total Referrals; Trends)
output$totals_RandR_trend_plot <- renderPlotly({
  plot_trend(
    referrals_data_sel_yrs %>% filter(ijb == input$select_randr_trend_totals),
    measure = referrals, ytitle = "Number Referred",
    colours = if (input$select_randr_trend_totals == "Scotland") {"#9B4393"} else {"#0078D4"}
  )
})

# Table --------------------------------------

# Table title (Total Referrals; Trends)
output$randr_table_title_trend_totals <- renderUI({
  HTML(paste0("Number of people diagnosed with dementia who were referred for PDS; Trend, Scotland and ", 
              input$select_hb_ijb_randr))
})

# Table data (Total Referrals; Trends)
table_trend_totals_data <- reactive({
  if(input$select_hb_ijb_randr == "Health Boards"){
    referrals_data_sel_yrs %>% 
      filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
      select(health_board, fy, referrals) %>%
      rename("Health Board" = "health_board")  
  }else{
    referrals_data_sel_yrs %>% 
      filter(!grepl("NHS", ijb)) %>% 
      select(ijb, fy, referrals) %>%
      rename("Integration Authority Area" = "ijb")  
  }
})

# Table (Total Referrals; Trends)
output$randr_table_trend_totals <- DT::renderDataTable({
  make_table(
    table_trend_totals_data() %>% 
      pivot_wider(names_from = fy, values_from = referrals) %>% 
      mutate(across(where(is.numeric), ~prettyNum(., big.mark = ","))), right_align = 1:length(included_years_extra_referrals), selected = 1, 
    table_elements = "t", rows_to_display = 32)
})

# Download data ------------------------------

# Download data (Total Referrals; Trends)
output$downloadData_totals_trend <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(
      table_trend_totals_data() %>% 
        mutate(across(where(is.factor), ~as.character(.))) %>% 
        mutate(fy = case_when( # Changes superscript R to in line R for downloaded csv since superscript is not supported 
          fy == provisional_year_sup ~paste0(provisional_year,"P"),
          fy == revised_year_sup ~paste0(revised_year,"R"),
          fy == extra_referrals_year_sup ~paste0(extra_referrals_year,"P"),
          TRUE ~fy)) %>% 
        pivot_wider(names_from = fy, values_from = referrals) %>% 
        mutate(across(where(is.numeric), ~prettyNum(., big.mark = ","))) %>% 
        mutate(Measure = "Number of people diagnosed with dementia who were referred for PDS", .before = everything()) %>% 
        rbind(c("Note: P indicates data is provisional. Please see dashboard for further information.", rep("", length(included_years_extra_referrals)+ 1))) %>% 
        rbind(c("Note: R indicates data has been revised. Please see dashboard for further information.", rep("", length(included_years_extra_referrals) + 1))),
      file, 
      row.names = FALSE
    )
  }
)

##############################################.
## Rates per 10,000 Population ----
##############################################.

# Filter data (Rates per 10,000 Population) ----
data_rates_sel_yrs <- data_rates %>% filter(fy %in% included_years_extra_referrals)

##############################################.
## Referrals by Financial Year ----
##############################################.

# Value box ----------------------------------

# Value box title (Rates per 10,000 Population; Referrals by Financial Year)
output$randr_title_rates <- renderUI({
  HTML(paste("Number of People Referred for PDS per 10,000 Population (65+); Scotland, ", 
             input$select_year_randr))
})

# Value box value (Rates per 10,000 Population; Referrals by Financial Year)
vb_data_rates <- reactive({
  data_rates_sel_yrs %>% 
    filter(health_board == "Scotland", ijb == "Scotland", fy == input$select_year_randr)
}) 

# Value box text (Rates per 10,000 Population; Referrals by Financial Year)
output$scot_rate <- renderText({
  paste0(vb_data_rates()$pop_rate_10000)
})

# Plot ---------------------------------------

# Plot title (Rates per 10,000 Population; Referrals by Financial Year)
output$rates_plot_title <- renderUI({
  HTML(paste0("Number of people per 10,000 population (65+) who were referred for PDS; ", 
              input$select_year_randr, 
              ", Scotland and ", 
              input$select_hb_ijb_randr))
})

# Plot data (Rates per 10,000 Population; Referrals by Financial Year)
rates_chart_data <- reactive({
  filtered_rates_data <- data_rates_sel_yrs %>% 
    filter(fy == input$select_year_randr)
  left_join(
    if(input$select_hb_ijb_randr == "Health Boards"){
      filtered_rates_data %>% filter(grepl("NHS", ijb))
    }else{
      filtered_rates_data %>% filter(!grepl("NHS", ijb), ijb != "Scotland")   
    },
    filtered_rates_data %>% 
      filter(ijb == "Scotland") %>% 
      select(fy, pop_rate_10000)%>%
      rename(scot_pop_rate_10000 = pop_rate_10000)) %>% 
    rename(geog = ijb)
})

# Plot (Rates per 10,000 Population; Referrals by Financial Year)
output$rates_plot <- renderPlotly({
  plot_bar(
    data = rates_chart_data(),
    ytitle = "Number per 10,000 population",
    measure_text = "Number per 10,000 population: ",
    measure = pop_rate_10000, 
    scot_measure = scot_pop_rate_10000, 
    scot_measure_text = "Number per 10,000 population: ")
})

# Table --------------------------------------

# Table title (Rates per 10,000 Population; Referrals by Financial Year)
output$rates_table_title <- renderUI({
  HTML(paste0("Number of people per 10,000 population (65+) who were referred for PDS; ", 
              input$select_year_randr, 
              ", Scotland and ", 
              input$select_hb_ijb_randr))
})

# Table data (Rates per 10,000 Population; Referrals by Financial Year)
table_rates_data <- reactive({
  if(input$select_hb_ijb_randr == "Health Boards"){
    data_rates_sel_yrs %>% 
      filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
      filter(fy == input$select_year_randr) %>%
      select(health_board,pop_rate_10000)%>%
      arrange(health_board) %>% 
      set_colnames(c("Health Board","Number of People per 10,000 population (65+) Referred to PDS "))
  }else{
    data_rates_sel_yrs %>%
      filter(!grepl("NHS", ijb)) %>% 
      filter(fy == input$select_year_randr) %>%
      select(ijb,pop_rate_10000)%>%
      set_colnames(c("Integration Authority Area","Number of People per 10,000 population (65+) Referred to PDS"))
  }
})

# Table (Rates per 10,000 Population; Referrals by Financial Year)
output$rates_table <- DT::renderDataTable({
  make_table(
    table_rates_data(),
    right_align = 1, 
    selected = 1, 
    rows_to_display = 32)
})

# Download data ------------------------------

# Download data (Rates per 10,000 Population; Referrals by Financial Year)
output$downloadData_rates <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(
      table_rates_data() %>% 
        mutate(across(where(is.factor), ~as.character(.))) %>% 
        mutate(`Financial Year` = input$select_year_randr,.before = everything()) %>%
        mutate(`Financial Year`  = case_when( # Changes superscript P to in line P for downloaded csv since superscript is not supported
          `Financial Year`  == provisional_year_sup ~paste0(provisional_year,"P"),
          `Financial Year`  == revised_year_sup ~paste0(revised_year,"R"),
          `Financial Year`  == extra_referrals_year_sup ~paste0(extra_referrals_year,"P"),
          TRUE ~`Financial Year`)) %>% 
        rbind(
          if(input$select_year_randr == provisional_year_sup | input$select_year_randr == extra_referrals_year_sup){
            c("Note: P indicates data is provisional. Please see dashboard for further information.",rep("",2))
          }else if(input$select_year_randr == revised_year_sup){
            c("Note: R indicates data has been revised. Please see dashboard for further information.",rep("",2))
          }else{
            rep("",3)
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

# Plot title (Rates per 10,000 Population; Trends)
output$randr_chart_title_trend_rates <- renderUI({
  HTML(paste("Number of people per 10,000 population (65+) who were referred for PDS; Trend, Scotland "),
       if(input$randr_select_trend_rates == "Scotland"){
         ""
       }else{
         paste0("and ", input$randr_select_trend_rates)
       })
})

# Plot data (Rates per 10,000 Population; Trends)
rates_trend_chart_data <- reactive({
  data_rates_sel_yrs %>% filter(ijb == input$randr_select_trend_rates | ijb == "Scotland")
})

# Plot (Rates per 10,000 Population; Trends)
output$randr_trend_plot_rates <- renderPlotly({
  plot_trend(rates_trend_chart_data(), pop_rate_10000, ytitle = "rate per 10,000 population")
})

# Table --------------------------------------

# Table title (Rates per 10,000 Population; Trends)
output$randr_table_trend_rates_title <- renderUI({
  HTML(paste0("Number of people per 10,000 population (65+) who were referred for PDS; Trend, Scotland and ",
              input$select_hb_ijb_randr))
})

# Table data (Rates per 10,000 Population; Trends)
table_trend_rates_data <- reactive({
  if(input$select_hb_ijb_randr == "Health Boards"){  
    data_rates_sel_yrs %>% 
      filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
      select(health_board, fy, pop_rate_10000) %>%
      rename("Health Board" = "health_board") 
  }else{
    data_rates_sel_yrs %>% 
      filter(!grepl("NHS", ijb)) %>% 
      select(ijb, fy, pop_rate_10000) %>%
      rename("Integration Authority Area" = "ijb")
  }
})

# Table (Rates per 10,000 Population; Trends)
output$randr_table_trend_rates <- DT::renderDataTable({
  make_table(
    table_trend_rates_data() %>% 
      pivot_wider(names_from = fy, values_from = pop_rate_10000),
    right_align = 1:length(included_years_extra_referrals), 
    selected = 1, 
    rows_to_display = 32)
})

# Download data ------------------------------

# Download data (Rates per 10,000 Population; Trends)
output$downloadData_rates_trend <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(
      table_trend_rates_data() %>% 
        mutate(across(where(is.factor), ~as.character(.))) %>% 
        mutate(fy = case_when(
          fy == provisional_year_sup ~paste0(provisional_year,"P"),
          fy  == extra_referrals_year_sup ~paste0(extra_referrals_year,"P"),
          fy == revised_year_sup ~paste0(revised_year,"R"),
          TRUE ~fy)) %>% 
        pivot_wider(names_from = fy, values_from = pop_rate_10000) %>% 
        mutate(Measure = "Number of people per 10,000 population (65+) who were referred for PDS", .before = everything()) %>% 
        rbind(c("Note: P indicates data is provisional. Please see dashboard for further information.",
                rep("",length(included_years_extra_referrals)+1))) %>% 
        rbind(c("Note: R indicates data has been revised. Please see dashboard for further information.",
                rep("",length(included_years_extra_referrals)+1))),
      file, 
      row.names = FALSE
    )
  }
)

################################# END OF SCRIPT ################################.