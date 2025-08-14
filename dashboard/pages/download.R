####################### Page 6: download #######################
# UI ----
output$download_ui <-  renderUI({
  
  div(
    fluidRow(column(
      p("This page allows you to download data from the report."),
      p("Use the dropdown menu to select data for Scotland, Health Boards or Integration Authority Areas. 
        You can use the additional selection options to further refine the data. 
        Note that gender, age group, and deprivation quintile are only available at Scotland level. 
        LDP standard part 2 is only available at Scotland and Health Board level. "),
      p("Use the download button to download the selected data as a .csv file.
        A preview of the data (first 10 rows) is shown at the bottom of this page."),width = 12),
      column(selectInput("download", "Show data for:", choices = download_list), width = 3)
    ),#fluidRow
    # checkboxes for financial year ----
    fluidRow(box(width = 3, height = "335px",
                 background = "blue",
                 p(style = "color: #3F3685", strong("Select financial years:")),
                 actionButton(style = "width: 49%; margin-right: -2.5px", "selectall", label = "Select All"),
                 actionButton(style = "width: 49%; margin-left: -2px", "deselectall", label = "Deselect All"),
                 checkboxGroupInput("select_year_dl", NULL,
                                    choices = included_years_extra_referrals, selected = included_years_extra_referrals),
               ),
             # dropdown filters for measure, gender, age group and simd if Scotland data is chosen----
             conditionalPanel(condition = 'input.download == "download_data_scotland"',
                              box(width = 9, height = "335px",
                                  background = "blue",
                                  pickerInput("select_gender_dl", "Select genders:", width = "100%",
                                              choices = unique(download_data_scotland$gender), selected = "All",
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 10),
                                              multiple = TRUE),
                                  
                                  pickerInput("select_age_dl", "Select age groups:", width = "100%",
                                              choices = unique(download_data_scotland$age_group), selected = "All",
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 10),
                                              multiple = TRUE
                                  ),
                                  
                                  pickerInput("select_simd_dl", "Select deprivation quintiles:", width = "100%",
                                              choices = unique(download_data_scotland$deprivation_quintile), selected = "All",
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 10),
                                              multiple = TRUE),
                                  
                                  pickerInput("select_measure_dl_scot", "Select measures:", width = "100%",
                                              choices = unique(download_data_scotland$measure), selected = unique(download_data_scotland$measure),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 10,
                                                selectedTextFormat = "count > 3"),
                                              multiple = TRUE)
                              )#box
             ),#condPanel
             # healthboard dropdown menu----
             conditionalPanel(condition = 'input.download == "download_data_hb"',
                              box(width = 9, height = "335px",
                                  background = "blue",
                                  pickerInput("select_hb_dl", "Select Health Boards:", width = "100%",
                                              choices = unique(download_data_hb$geography), selected = unique(download_data_hb$geography),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 10,
                                                selectedTextFormat = "count > 6"),
                                              multiple = TRUE),
                                  
                                  pickerInput("select_measure_dl_hb", "Select measures:", width = "100%",
                                              choices = unique(download_data_hb$measure), selected = unique(download_data_hb$measure),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 10,
                                                selectedTextFormat = "count > 3"),
                                              multiple = TRUE)
                              )#box
             ),# cond panel hb
             #Integration Authority dropdown menu ----             
             conditionalPanel(condition = 'input.download == "download_data_ijb"',
                              box(width = 9, height = "335px", 
                                  background = "blue",
                                  pickerInput("select_ijb_dl", "Select Integration Authority Areas:", width = "100%",
                                              choices = unique(download_data_ijb$geography), selected = unique(download_data_ijb$geography),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 10,
                                                selectedTextFormat = "count > 6"),
                                              multiple = TRUE),
                                  
                                  pickerInput("select_measure_dl_ijb", "Select measures:", width = "100%",
                                              choices = unique(download_data_ijb$measure), selected = unique(download_data_ijb$measure),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 10,
                                                selectedTextFormat = "count > 3"),
                                              multiple = TRUE)
                              )#box
             )# cond panel ijb
    ), # fluidRow
    #download button----
    fluidRow(column(linebreaks(1),
                    downloadButton("downloadData", 
                                   "Download data"),
                    #data table----
                    h4(strong("Preview of data to download (first 10 rows):")),
                    DT::dataTableOutput("table_download"), 
                    linebreaks(1), 
                    p(paste0("Sources: Public Health Scotland quarterly dementia post-diagnostic support dataset: Data submissions from NHS Boards as at ",
                             format(end_date, "%d %B %Y"), "; Estimated and Projected Diagnosis Rates for Dementia in Scotland paper: 2014-2020; National Records of Scotland (NRS) mid-2021, mid-2022 and mid-2023 population estimates.")),
                    ### Notes----
                    h4(strong("Notes:")),
                    p(paste0("ᴾ Figures for ", provisional_year, " and ", extra_referrals_year, " are provisional subject to all service users completing their support.")),
                    p(paste0("ᴿ Figures for ", revised_year," have been revised and are now final.")),
                     width = 12) #column
    )#fluidRow
  ) # div
})

# SERVER----

#data selection ----
# selects data (scotland, hbs, or ijbs)
download_data <- reactive({
  get(input$download)
})

#filters selected data depending on dropdown selections
download_data_filtered <- reactive({

  
  if(input$download == "download_data_scotland"){
    download_data() %>% filter((financial_year == extra_referrals_year_sup &
                                  (measure == "number of people referred to PDS"| measure == "number of people referred to PDS per 10,000 population (65+)") &
                                  gender == "All" &
                                  age_group == "All" &
                                  deprivation_quintile == "All") |
                                 financial_year %in% included_years) %>%
      filter(financial_year %in% input$select_year_dl, 
                               gender %in% input$select_gender_dl,
                               age_group %in% input$select_age_dl,
                               deprivation_quintile %in% input$select_simd_dl,
                               measure %in% input$select_measure_dl_scot)
  }else if(input$download == "download_data_hb"){
    download_data() %>% filter((financial_year == extra_referrals_year_sup &
                                  (measure == "number of people referred to PDS"| measure == "number of people referred to PDS per 10,000 population (65+)"))|
                 financial_year %in% included_years) %>%
      filter(financial_year %in% input$select_year_dl,
                               geography %in% input$select_hb_dl,
                               measure %in% input$select_measure_dl_hb)
  }else{
      download_data() %>% filter((financial_year == extra_referrals_year_sup &
                                    (measure == "number of people referred to PDS"| measure == "number of people referred to PDS per 10,000 population (65+)"))|
                                   financial_year %in% included_years) %>% 
      filter(financial_year %in% input$select_year_dl,
                                 geography %in% input$select_ijb_dl,
                                 measure %in% input$select_measure_dl_ijb)
  }
})


# adds select all and deselect all buttons for financial year checkboxes
observe({
  if(is.null(input$selectall))  return(NULL) 
  else if (input$selectall > 0)
  {
    updateCheckboxGroupInput(session,"select_year_dl", NULL,
                             choices = included_years_extra_referrals, selected = included_years_extra_referrals)
  }
})

observe({
  if(is.null(input$deselectall))  return(NULL) 
  else if (input$deselectall > 0)
  {
    updateCheckboxGroupInput(session,"select_year_dl", NULL,
                             choices = included_years_extra_referrals)
  }
})


#data table ----
#displays preview of selected data
output$table_download <- DT::renderDataTable({
  make_table(download_data_filtered(), rows_to_display = 10,
             table_elements = "ti", scrollY = FALSE)
  
}
)

# download button----
output$downloadData <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(download_data_filtered() %>%
                mutate(financial_year = case_when(
                  financial_year == provisional_year_sup ~paste0(provisional_year,"P"),
                  financial_year == revised_year_sup ~paste0(revised_year,"R"),
                  financial_year == extra_referrals_year_sup ~paste0(extra_referrals_year,"P"),
                                                    TRUE ~financial_year)),
               file, row.names = FALSE)
  }
)



