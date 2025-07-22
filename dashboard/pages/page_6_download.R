####################### Page 6: download #######################
# UI ----
output$download_ui <-  renderUI({
  
  div(
    fluidRow(column(
      p("This page allows you to download data from the report."),
      p("Use the dropdown menu to select data for Scotland, Health Boards or Integration Authority Areas. 
        You can use the additional selection options to further refine the data."),
      p("Use the download button to download the selected data as a csv file.
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
                                    choices = included_years_no_sup, selected = included_years_no_sup),
               ),
             # dropdown filters for measure, gender, age group and simd if Scotland data is chosen----
             conditionalPanel(condition = 'input.download == "download_data_scotland"',
                              box(width = 9, height = "335px",
                                  background = "blue",
                                  pickerInput("select_gender_dl", "Select genders:", width = "100%",
                                              choices = unique(download_data_scotland$gender), selected = unique(download_data_scotland$gender),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 10),
                                              multiple = TRUE),
                                  
                                  pickerInput("select_age_dl", "Select age groups:", width = "100%",
                                              choices = unique(download_data_scotland$age_group), selected = unique(download_data_scotland$age_group),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 10),
                                              multiple = TRUE
                                  ),
                                  
                                  pickerInput("select_simd_dl", "Select deprivation quintiles:", width = "100%",
                                              choices = unique(download_data_scotland$deprivation_quintile), selected = unique(download_data_scotland$deprivation_quintile),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 10),
                                              multiple = TRUE),
                                  
                                  pickerInput("select_measure_dl_scot", "Select measures:", width = "100%",
                                              choices = unique(download_data_scotland$measure), selected = unique(download_data_scotland$measure),
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                size = 10,
                                                selectedTextFormat = "count > 5"),
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
                                                selectedTextFormat = "count > 5"),
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
                                                selectedTextFormat = "count > 5"),
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
                    
                    linebreaks(1), width = 12)
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
    download_data() %>% filter(financial_year %in% input$select_year_dl, 
                               gender %in% input$select_gender_dl,
                               age_group %in% input$select_age_dl,
                               deprivation_quintile %in% input$select_simd_dl)
  }else if(input$download == "download_data_hb"){
    download_data() %>% filter(financial_year %in% input$select_year_dl,
                               geography %in% input$select_hb_dl)
    
  }else{
      download_data() %>% filter(financial_year %in% input$select_year_dl,
                                 geography %in% input$select_ijb_dl)
  }
})


# adds select all and deselect all buttons for financial year checkboxes
observe({
  if(is.null(input$selectall))  return(NULL) 
  else if (input$selectall > 0)
  {
    updateCheckboxGroupInput(session,"select_year_dl", NULL,
                             choices = included_years_no_sup, selected = included_years_no_sup)
  }
})

observe({
  if(is.null(input$deselectall))  return(NULL) 
  else if (input$deselectall > 0)
  {
    updateCheckboxGroupInput(session,"select_year_dl", NULL,
                             choices = included_years_no_sup)
  }
})


#data table ----
#displays preview of selected data
output$table_download <- DT::renderDataTable({
  make_table(download_data_filtered(), rows_to_display = 10,
             table_elements = "ti", scrollX = TRUE)
  
}
)

# download button----
output$downloadData <- downloadHandler(
  filename = paste0("pds_data_as_at_", end_date, ".csv"),
  content = function(file) {
    write.csv(download_data_filtered(), file, row.names = FALSE)
  }
)



