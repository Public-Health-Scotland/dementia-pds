####################### Core functions #######################

# Add n linebreaks
linebreaks <- function(n){HTML(strrep(br(), n))}

# Remove warnings from icons 
icon_no_warning_fn = function(icon_name) {
  icon(icon_name, verify_fa=FALSE)
}

# Generic data table
make_table <- function(input_data_table,
                       rows_to_display = 20,
                       filter = "none",
                       ordering = TRUE,
                       scrollX = FALSE,
                       right_align = NULL,
                       selected = NULL,
                       table_elements = "Bt",
                       filename = NULL,
                       button_name = " Download table data as..."
){

  # Take out underscores in column names for display purposes
  table_colnames  <-  gsub("_", " ", colnames(input_data_table))

  dt <- DT::datatable(input_data_table, style = 'bootstrap',
                      class = 'table-bordered table-condensed',
                      rownames = FALSE,
                      filter=filter,
                      colnames = table_colnames,
                      selection = list(selected = selected),
                      extensions = 'Buttons',
                      options = list(
                          buttons = list(
                            list(
                              extend = "collection", 
                               buttons = list(
                                      list(
                                        extend = 'csv', 
                                        filename = filename,
                                        text = ".csv" ),
                                      list(
                                        extend = 'excel', 
                                        filename = filename,
                                        text = ".xlsx")
                                  ), text = paste0(icon_no_warning_fn("download"), button_name))),
                        pageLength = rows_to_display,
                                     scrollX = scrollX,
                                     scrollY = FALSE,
                                     dom = table_elements,
                                     ordering = ordering,
                                     autoWidth = FALSE,
                                     columnDefs = list(list(className = 'dt-right', targets = right_align)),
                                     # style header
                                     initComplete = htmlwidgets::JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C5C3DA', 'color': '#3F3685'});",
                                       "$(this.api().table().row().index()).css({'background-color': '#C5C3DA', 'color': '#3F3685'});",
                                       "}")))
  

  return(dt)
}








