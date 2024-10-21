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
                       right_align = NULL
){

  # Take out underscores in column names for display purposes
  table_colnames  <-  gsub("_", " ", colnames(input_data_table))

  dt <- DT::datatable(input_data_table, style = 'bootstrap',
                      class = 'table-bordered table-condensed',
                      rownames = FALSE,
                      filter=filter,
                      colnames = table_colnames,
                      options = list(pageLength = rows_to_display,
                                     scrollX = FALSE,
                                     scrollY = FALSE,
                                     dom = 'tip',
                                     ordering = ordering,
                                     autoWidth = TRUE,
                                     columnDefs = list(list(className = 'dt-right', targets = right_align)),
                                     # style header
                                     initComplete = htmlwidgets::JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C5C3DA', 'color': '#3F3685'});",
                                       "$(this.api().table().row().index()).css({'background-color': '#C5C3DA', 'color': '#3F3685'});",
                                       "}")))
  

  return(dt)
}

#plot referrals function

plot_referrals <- function(data, 
                           scotland = FALSE){ 
  
  yaxis_plots[["title"]] <- "Number"
  xaxis_plots[["title"]] <- "Month of Diagnosis"
  
  # Determine whether IJBs breakdown is included
  ijb_group <- ifelse(scotland == TRUE | 
                        n_distinct(data$ijb) == 2, 
                      FALSE, 
                      TRUE)
  
  if(ijb_group == TRUE){
    
    data <- data 
    
  }else{
    
    data %<>% 
      mutate(health_board = ifelse(scotland == TRUE, "Scotland", health_board)) %>%
      group_by(fy, month, health_board) %>%
      summarise(referrals = sum(referrals), .groups = "drop")
    
  }
  
  data %<>%
    
    mutate(year = if_else(month %in% 1:3,
                          paste0(substr(fy, 1, 2), substr(fy, 6, 7)),
                          substr(fy, 1, 4)),
           month_full = month(month, label = TRUE, abbr = FALSE),
           month_abbr = forcats::fct_relevel(
             month(month, label = TRUE),
             "Jan", "Feb", "Mar", after = Inf))
  
  plot <- data %>%
    
    ggplot(aes(x = month_abbr,
               y = referrals,
               group = if(ijb_group == TRUE){ijb}else{health_board},
               colour = if(ijb_group == TRUE){ijb}else{health_board},
               text = paste0(if(ijb_group == TRUE){ijb}else{health_board}, "<br>",
                             month_full, " ", year, "<br>",
                             "Referrals: ", format(referrals, big.mark = ",")))) +
    geom_line() +
    
    geom_point() +

    scale_y_continuous(limits = c(0, NA)) +
    
    phsstyles::scale_colour_discrete_phs(palette = "all", name = NULL) +
    
    # Custom labels on x tick marks
    scale_x_discrete(labels = paste(levels(data$month_abbr),
                                    c(rep(min(data$year), 9), rep(max(data$year), 3)))) +
    
    theme(legend.title = element_blank(),
          legend.position = ifelse(scotland == TRUE, "none", "bottom"),
          axis.text.x = element_text(angle=45)) 
    

  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove, 
           displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.5 , y = -0.5,
                         xanchor = "center", yanchor = "bottom")) %>% 
    layout(margin = list(b = 30, t = 10), # to avoid labels getting cut out
          yaxis = yaxis_plots, xaxis = xaxis_plots)

  
}

# trend plot function
plot_trend <- function(data){
  
  yaxis_plots[["title"]] <- ""
  xaxis_plots[["title"]] <- ""
  
  data %<>%
    
    select(ijb, fy, rate) %>%
    distinct(ijb, fy, .keep_all = T) %>% 
    mutate(rate = as.numeric(substr(rate,1,5)))
  
  
  plot <- data %>%
    
    ggplot(aes(x = fy,
               y = rate,
               group = ijb,
               colour = ijb,
               text = paste0(ijb, "<br>",
                             fy, "<br>",
                             rate, "%"))) +
    
    geom_point() +
    
    geom_line() + 
    
    scale_y_continuous(limits = c(0, 100),
                       labels=function(x) paste0(x,"%")) + 
    
    phsstyles::scale_colour_discrete_phs(palette = "all", name = NULL) +
    
    theme(legend.title = element_blank(),
    )
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove, 
           displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.5 , y = -0.2,
                         xanchor = "center", yanchor = "bottom")) %>% 
    layout(margin = list(b = 30, t = 10), # to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots)
  
  
}

# bar chart for proportion of referrals

proportion_bar_chart <- function(data){
  
  yaxis_plots[["title"]] <- ""
  xaxis_plots[["title"]] <- ""
  
  data %<>% filter(type != "Unknown") 
  
  plot <-  data %>% ggplot(aes(x = type, y = total_referrals/sum(total_referrals)*100, fill = type,
                                text = paste0(type, "<br>",
                                              "Proportion of total referrals: ", round(total_referrals/sum(total_referrals)*100,1), "%"))) +
    geom_col() +
    
    scale_y_continuous(limits = c(0, NA),
                       labels=function(x) paste0(x,"%")) +
    
    phsstyles::scale_fill_discrete_phs(palette = "all", name = NULL) +
    
    theme(legend.title = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(angle=45))
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove, 
           displaylogo = F, editable = F) %>%
    layout(margin = list(b = 30, t = 30), # to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots)
  
}

# bar chart for ldp

percent_met_bar_chart <- function(data){
  
  yaxis_plots[["title"]] <- ""
  xaxis_plots[["title"]] <- ""
  
  data %<>% filter(type != "Unknown") 
  
  plot <-  data %>% ggplot(aes(type, percent_met, fill = type,
                               text = paste0(type, "<br>",
                               "Percentage of Referrals Achieved LDP Standard: ", percent_met, "%"))) +
    geom_col() +
    
    scale_y_continuous(limits = c(0, 100),
                       labels=function(x) paste0(x,"%")) +
    
    phsstyles::scale_fill_discrete_phs(palette = "all", name = NULL) +
    
    theme(legend.title = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(angle=45))
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove, 
           displaylogo = F, editable = F) %>%
    layout(margin = list(b = 30, t = 30), # to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots)
  
}






