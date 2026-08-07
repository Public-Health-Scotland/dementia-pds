############################# PAGE 4: Demographics #############################.

################################################################################.
### UI ----
################################################################################.

output$demo_ui <-  renderUI({
  div(
    
    ##############################################.
    ## Age / Sex / SIMD ----
    ##############################################.
    
    fluidRow(column(
      fluidRow(
        column(h4(strong(htmlOutput("chart_title_demo_referrals"))), width = 6), # Plot title (referrals)
        column(h4(strong(htmlOutput("chart_title_demo_ldp"))), width = 6) # Plot title (LDP)
      ),
      fluidRow(
        column(plotlyOutput("plot_demo_referrals", height = "300px"), width = 6), # Plot (referrals)
        column(plotlyOutput("plot_demo_ldp", height = "300px"), width = 6), # Plot (LDP)
        linebreaks(1)
      ),
      h4(strong(htmlOutput("table_title_demo"))), # Table title
      downloadButton("downloadData_demo", "Download table data"), # Download button
      DT::dataTableOutput("table_demo"), # Table
      linebreaks(1),
      
      ##############################################.
      ## Notes ----
      ##############################################.
      # Sources
      if(input$select_data_demo == "data_simd"){ # SIMD
        p(paste0("Sources: Public Health Scotland quarterly dementia post-diagnostic support dataset: Data submissions from NHS Boards as at ",
                 format(end_date, "%d %B %Y"),"; Scottish Government Scottish Index of Multiple Deprivation (SIMD) 2020."))
      }else{ # Age / Sex
        p(paste0("Source: Public Health Scotland quarterly dementia post-diagnostic support dataset: Data submissions from NHS Boards as at ",
                 format(end_date, "%d %B %Y")))
      },
      # Notes
      h4(strong("Notes:")),
      # Provisional years
      p(paste0("ᴾ Figures for ", provisional_year," are provisional subject to all service users completing their support.")),
      # Revised years
      p(paste0("ᴿ Figures for ", revised_year," have been revised and are now final. ")),
      # LDP Standard Calculation
      p("For detailed information on how the Percentage LDP Standard Achieved is calculated, and how 'Standard Met', 'Exempt from Standard', 'PDS Ongoing' and 'Standard Not Met' are defined, please see the",
        a(href = "#", "Methodology", onclick = "Shiny.setInputValue('method_link', Math.random()); return false;"), "page."),	
      # Age
      if (input$select_data_demo == "data_age") {
        p("Age is calculated as at the dementia diagnosis date. There are a small number of records with an incomplete date of birth and therefore the age group is unknown.")
      # SIMD
      } else if (input$select_data_demo == "data_simd") {
        p("Deprivation is calculated by matching postcode to the Scottish Index of Multiple Deprivation (SIMD) quintiles. Each quintile consists of approximately 20% of the general population living in Scotland, 
          with deprivation quintile 1 indicating the 20% of the population living in the most deprived areas and deprivation quintile 5 indicating the 20% of the population living in the least deprived areas.
          The quintiles used in this report are from SIMD 2020v2. Further information on SIMD can be found on the",
          a("Scottish Government,", href ="https://www.alzscot.org/about-us/care/post-diagnostic-support/", target="_blank"),
            "website.",
          br(),
          "There are a small number of records where it was not possible to assign a deprivation category. Possible reasons for not being able to assign a deprivation category are that no postcode was provided 
          or the postcode provided is invalid, not in Scotland, or is a newly added postcode.")
      # Sex
      } else if (input$select_data_demo == "data_sex") {
        p("There are a small number of records where sex is either not specified (includes refused/not provided) or not known (i.e. indeterminate sex, includes ‘Intersex’). These records are not reported separately in the tables but are included in the Totals shown.")
      },
      # NHS Grampian / Aberdeen City
      p("Figures for 2018/19, 2019/20 and 2020/21 are affected by the change in service provision of PDS within Aberdeen City during 2019. See Note 5 on the",
        a(href = "#", "Home", onclick = "Shiny.setInputValue('home_link', Math.random()); return false;"), "page for further information."),
      # Formatting
      width = 12,
      style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; margin-left: 1px; height:-webkit-fill-available; background-color: white" # Fix panel so sidebar and navigation bar do not scroll with content
      
    )), # column, fluuidRow
  ) # div
}) # renderUI

################################################################################.
### SERVER ----
################################################################################.

# Filter data ----
data_selected <- reactive({get(input$select_data_demo)})
data_demo <- reactive({data_selected() %>% filter(fy == input$select_year_demo) %>% mutate(percent_referrals = referrals/sum(referrals)*100)})

##############################################.
## Age / Sex / SIMD ----
##############################################.

# Plots --------------------------------------

# Plot title (referrals)
output$chart_title_demo_referrals <- renderUI({
  HTML(paste0("Proportion of total referrals for PDS by ", 
              if (input$select_data_demo == "data_age"){
                "Age Group"
              } else if(input$select_data_demo == "data_simd"){
                "Deprivation Quintile"
              } else if(input$select_data_demo == "data_sex"){
                "Sex"
              },
              ": ", "Scotland", ", Financial Year ", input$select_year_demo))
})

# Plot (referrals)
output$plot_demo_referrals <- renderPlotly({
    plot_bar_perc_no_line(
      data_demo(), 
      category = type, 
      measure = percent_referrals,
      x_text_angle = if_else(input$select_data_demo == "data_age", 45, 0), 
      fill = type, 
      ylimit = (max(data_demo()$referrals)/sum(data_demo()$referrals))*100+1)
})

# Plot title (LDP)
output$chart_title_demo_ldp <- renderUI({
  HTML(paste0("Percentage of referrals who received a minimum of one year’s post-diagnostic support by ", 
              if (input$select_data_demo == "data_age"){
                "Age Group"
              } else if(input$select_data_demo == "data_simd"){
                "Deprivation Quintile"
              } else if(input$select_data_demo == "data_sex"){
                "Sex"
              },
              ": ", "Scotland", ", Financial Year ", input$select_year_demo))
})

# Plot (LDP)
output$plot_demo_ldp <- renderPlotly({
  plot_bar_perc_no_line(
    data_demo(), 
    category = type, 
    measure = percent_met,
    x_text_angle = if_else(input$select_data_demo == "data_age", 45, 0),
    fill = type)
})

# Table --------------------------------------

# Table title
output$table_title_demo <- renderUI({
  HTML(paste0("Number and percentage of people referred for PDS who received a minimum of one year’s support by ", 
              if (input$select_data_demo == "data_age"){
                "Age Group"
              } else if(input$select_data_demo == "data_simd"){
                "Deprivation Quintile"
              } else if(input$select_data_demo == "data_sex"){
                "Sex"
              },
              ": ", "Scotland", ", Financial Year ", input$select_year_demo))
})

# Table data
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
      mutate(percent_met = round(((complete + exempt)/(complete + exempt + not_met))*100, 1))) %>% 
    mutate(perc_prop = round(100*referrals/max(referrals),1), .after = referrals) %>% 
    mutate(across(where(is.numeric), ~if_else(is.na(.), "-", format(., big.mark = ",")))) %>%
    mutate(across(starts_with("perc"), ~ if_else(grepl("-", .), ., paste0(.,"%")))) %>%
    set_colnames(
      c(if(input$select_data_demo == "data_sex"){
        "Sex"
      }else if(input$select_data_demo == "data_age"){
        "Age Group"
      }else{
        "Deprivation Quintile_"},
      "Number of People Referred to PDS", "Proportion of Total Referrals", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP Standard Achieved")
      ) 
  if (input$select_data_demo=='data_sex'){
    df <- df %>% filter(!Sex %in% c("Not Specified", "Unknown"))
  }
  return(df)
})

# Table
output$table_demo <- DT::renderDataTable({
  make_table(
    table_data_demo(),
    right_align = 1:7, 
    ordering = FALSE, 
    scrollY = FALSE, 
    selected = nrow(table_data_demo()))                                                                                                  
})

# Download data ------------------------------
output$downloadData_demo <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(
      table_data_demo() %>%
        mutate(across(where(is.factor), ~as.character(.))) %>% 
        mutate(`Financial Year` = input$select_year_demo, 
               Geography = "Scotland", 
               .before = everything()) %>% 
        mutate(`Financial Year` = case_when(
          `Financial Year` == provisional_year_sup ~paste0(provisional_year,"P"),
          `Financial Year` == revised_year_sup ~paste0(revised_year,"R"),
          TRUE ~`Financial Year`)) %>%
        rbind( # Add revision and provisional notes
          if(input$select_year_demo == revised_year_sup){
            c("Note: R indicates data has been revised. Please see dashboard for further information.",rep("",9))
          }else if(input$select_year_demo == provisional_year_sup){
            c("Note: P indicates data is provisional. Please see dashboard for further information.",rep("",9))
          }else{
            rep("",10)
          }),
      file,
      row.names = FALSE)
  }
)

################################# END OF SCRIPT ################################.