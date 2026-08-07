####################### ADDITIONAL ANALYSIS #######################
# UI ----
output$add_ui <-  renderUI({
  
  div(
    
    #subtype, stage, referral source, and model of care----
    conditionalPanel(
      condition= 'input.select_data_add == "data_subtype" || input.select_data_add == "data_stage" || input.select_data_add == "data_referral" || input.select_data_add == "data_model"',
      # inputs
      
      fluidRow(column(
        # outputs
        h3(strong(htmlOutput("table_title_add"))),
        DT::dataTableOutput("table_add"),
        linebreaks(1),
        h3(strong(htmlOutput("chart_title_add_referrals"))),
        
        
        conditionalPanel(condition= 'input.select_sex_add == "All"',
                         radioButtons("select_sex_chart_1_add",
                                      label="Choose how genders are displayed in chart:",
                                      choices=c("show all genders combined" = "All",
                                                "show female/male comparison" = "Male/Female"),
                                      inline =TRUE),
                         plotlyOutput("plot_add_referrals_all", height = "550px"),
                         h3(strong(htmlOutput("chart_title_add_ldp_all"))),
                         radioButtons("select_sex_chart_2_add",
                                      label="Choose how genders are displayed in chart:",
                                      choices=c("show all genders combined" = "All",
                                                "show female/male comparison" = "Male/Female"),
                                      inline =TRUE),
                         plotlyOutput("plot_add_ldp_all", height = "550px")),
        
        conditionalPanel(condition= 'input.select_sex_add != "All"',
                         plotlyOutput("plot_add_referrals", height = "550px"),
                         h3(strong(htmlOutput("chart_title_add_ldp"))),
                         plotlyOutput("plot_add_ldp", height = "550px")), 
        linebreaks(1),
         width = 12,
        style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; margin-left: 1px; height:-webkit-fill-available; background-color: white")
      ) # fluid Row
    ), #cond panel 1
    
    #uptake----
    conditionalPanel(
      condition = 'input.select_data_add == "uptake"',
      
      fluidRow(column(
        # outputs
        
        h3(strong(htmlOutput("hb_ijb_table_title_uptake"))),
        column(radioButtons("select_uptake_table",
                            label = "In the table below show Scotland and: ",
                            choices = c("Health Boards", "Integration Authority Areas"),
                            selected = "Health Boards",
                            inline = TRUE
        ), width = 6),
        DT::dataTableOutput("table_hb_ijb_uptake"),
        linebreaks(2),
        h3(strong(htmlOutput("table_uptake_2_title"))),
        fluidRow(
        column(selectInput("select_hb_ijb_uptake_2",
                           label = "Select Health Board/Integration Authority Area to show in table and chart below:",
                           choices = c("Scotland", boards, ijb_list), selected = data_wait_2$ijb == "Scotland", width = "100%"),
               width = 6)),
       
      fluidRow(
          column(
            linebreaks(1),
            DT::dataTableOutput("table_uptake_2"), width = 6),
          # h3(strong(htmlOutput("simd_uptake_plot_title"))),
          column(plotlyOutput("plot_simd_uptake"), width = 6)
        ), # fluidrow
      linebreaks(1),
           width = 12,
           style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; margin-left: 1px; height:-webkit-fill-available; background-color: white"))
    ), #cond panel 2
    
    
    # pathways----
    conditionalPanel(
      condition = 'input.select_data_add == "waiting_times"',
      
      fluidRow(column(
        
        # outputs
        h3(strong(htmlOutput("hb_ijb_table_title_wait"))),
        column(radioButtons("select_wait_table",
                            label = "In the table below show Scotland and: ",
                            choices = c("Health Boards", "Integration Authority Areas"),
                            selected = "Health Boards",
                            inline = TRUE
        ), width = 6),
        DT::dataTableOutput("table_hb_ijb_wait"),
        linebreaks(2),
        h3(strong(htmlOutput("table_wait_2_title"))),
        #linebreaks(1),
        column(selectInput("select_hb_ijb_wait_2",
                           label = "Select Health Board/Integration Authority Area to show in table below:",
                           choices = c("Scotland", boards, ijb_list), selected = data_wait_2$ijb == "Scotland", width = "100%"), width = 5),
        
        #h4(strong("Referrals where post-diagnostic support has ended after being contacted by PDS practitioner:")),
        DT::dataTableOutput("table_wait_2"),
        linebreaks(1),
        #  column(
        # h4(strong("Referrals exempt from LDP Standard, where post-diagnostic support has ended after being contacted by PDS practitioner:")),
        # DT::dataTableOutput("table_wait_2_exempt"), width = 6)
        
        # h3(htmlOutput("")),
        # plotlyOutput("", height = "600px"))
        
      width = 12,
      style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; margin-left: 1px; height:-webkit-fill-available")
      ), # fluidrow
    ), #cond panel 3
  
  ) #div   
}) # renderUI
 
 
# SERVER ----

# create table for subtype, stage, referral source, and model of care ----
# title 
 
output$table_title_add <- renderUI({HTML(paste0("Number and percentage of people referred for PDS who received a minimum of one year’s support by ", 
                                              
                                              if (input$select_data_add == "data_model"){
                                                "PDS Model of Care"
                                              } else if(input$select_data_add == "data_subtype"){
                                                "Subtype of Dementia"
                                              } else if(input$select_data_add == "data_referral"){
                                                "Source of Referral to PDS"
                                              } else if(input$select_data_add == "data_stage"){
                                                "Clinical Impression of Stage of Dementia at Date of Referral"
                                              },
                                              ": ", input$select_hb_ijb_add, ", Financial Year ", input$select_year_add, ", Gender: ", input$select_sex_add
                                              )
                                         )
})




data_selected_add <-reactive({
  if (input$select_data_add == "data_model"){
    data_model
  } else if(input$select_data_add == "data_subtype"){
    data_subtype
  } else if(input$select_data_add == "data_referral"){
    data_referral
  } else if(input$select_data_add == "data_stage"){
    data_stage
  } else {
    NULL
  } 
})


output$table_add <- DT::renderDataTable({
    table_data_add <- data_selected_add() %>% 
           mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>%
           filter(ijb == input$select_hb_ijb_add, fy == input$select_year_add, sex == input$select_sex_add) %>%  
      select(type, total_referrals, complete, exempt, ongoing, not_met, percent_met) %>% 
      mutate(percent_met = paste0(percent_met,"%")) %>% 
      set_colnames(c(" ","Number of People Referred to PDS", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved"))
    make_table(table_data_add, right_align = 1:6, table_elements = "t") %>% 
      formatCurrency(2:5, currency = "", interval = 3, mark = ",", digits = 0)
  })


#create plots for subtype, stage, referral source, and model of care ----

# proportion plot
output$chart_title_add_referrals <- renderUI({HTML(paste0("Proportion of total referrals for dementia post-diagnostic support by ", 
                                                          
                                                         if (input$select_data_add == "data_model"){
                                                           "PDS Model of Care"
                                                         } else if(input$select_data_add == "data_subtype"){
                                                           "Subtype of Dementia"
                                                         } else if(input$select_data_add == "data_referral"){
                                                           "Source of Referral to PDS"
                                                         } else if(input$select_data_add == "data_stage"){
                                                           "Clinical Impression of Stage of Dementia at Date of Referral"
                                                         },
                                                         ": ", input$select_hb_ijb_add, ", Financial Year ", input$select_year_add, ", Gender: ", input$select_sex_add
                                                         ))
})



#plot 1 if all genders are selected
output$plot_add_referrals_all <- renderPlotly({
  proportion_bar_chart(if(input$select_sex_chart_1_add == "All"){
    data_selected_add() %>% filter(sex == "All") %>% mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>%
      filter(ijb == input$select_hb_ijb_add, fy == input$select_year_add)
  }else if(input$select_sex_chart_1_add == "Male/Female"){
    data_selected_add() %>% filter(sex == "Male" | sex == "Female") %>% mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>%
      filter(ijb == input$select_hb_ijb_add, fy == input$select_year_add)
  },
  legend = if_else(input$select_sex_chart_1_add == "All", "none", "right")
  )
})

# plot 1 if specific gender is selected
output$plot_add_referrals <- renderPlotly({
  proportion_bar_chart(data_selected_add() %>% mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>%
                         filter(ijb == input$select_hb_ijb_add, fy == input$select_year_add, sex == input$select_sex_add))
})

# ldp plot

output$chart_title_add_ldp_all <- renderUI({HTML(paste0("Percentage of referrals who received a minimum of one 
year’s post-diagnostic support by ", 

if (input$select_data_add == "data_model"){
  "PDS Model of Care"
} else if(input$select_data_add == "data_subtype"){
  "Subtype of Dementia"
} else if(input$select_data_add == "data_referral"){
  "Source of Referral to PDS"
} else if(input$select_data_add == "data_stage"){
  "Clinical Impression of Stage of Dementia at Date of Referral"
},
": ", input$select_hb_ijb_add, ", Financial Year ", input$select_year_add, ", Gender: ", input$select_sex_add
))
})


output$chart_title_add_ldp <- renderUI({HTML(paste0("Percentage of referrals who received a minimum of one 
year’s post-diagnostic support by ", 

if (input$select_data_add == "data_model"){
  "PDS Model of Care"
} else if(input$select_data_add == "data_subtype"){
  "Subtype of Dementia"
} else if(input$select_data_add == "data_referral"){
  "Source of Referral to PDS"
} else if(input$select_data_add == "data_stage"){
  "Clinical Impression of Stage of Dementia at Date of Referral"
},
": ", input$select_hb_ijb_add, ", Financial Year ", input$select_year_add, ", Gender: ", input$select_sex_add
))
})

#plot 2 if all genders are selected
output$plot_add_ldp_all <- renderPlotly({
  percent_met_bar_chart(if(input$select_sex_chart_2_add == "All"){
    data_selected_add() %>% filter(sex == "All") %>% mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>%
      filter(ijb == input$select_hb_ijb_add, fy == input$select_year_add)
  }else if(input$select_sex_chart_2_add == "Male/Female"){
    data_selected_add() %>% filter(sex == "Male" | sex == "Female") %>% mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>%
      filter(ijb == input$select_hb_ijb_add, fy == input$select_year_add)
  },
  legend = if_else(input$select_sex_chart_2_add == "All", "none", "right")
  )
})

#plot 2 if specific gender is selected
output$plot_add_ldp <- renderPlotly({
  percent_met_bar_chart(data_selected_add() %>% mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>%
                          filter(ijb == input$select_hb_ijb_add, fy == input$select_year_add, sex == input$select_sex_add))
})
 

#Uptake ----

# Data table 
output$hb_ijb_table_title_uptake<- renderUI({HTML(paste0("Number of Referrals by PDS Uptake Decision, Financial Year ", 
                                                        input$select_year_add, ", Scotland and ", input$select_uptake_table, ", Gender: ", input$select_sex_add))
})

output$table_hb_ijb_uptake <- DT::renderDataTable({
  
  if(input$select_uptake_table == "Health Boards"){  
    
    table_hb_data_uptake <- data_uptake %>% 
      filter(fy == input$select_year_add, sex == input$select_sex_add) %>% 
      filter(ijb == "All" | ijb == "Scotland", simd == "All") %>% 
      pivot_wider(names_from = pds_uptake_decision, values_from = referrals) %>% 
      mutate(across(where(is.numeric), ~ replace(., is.na(.), 0))) %>% 
      adorn_totals("col") 
    
    last_column <- ncol(table_hb_data_uptake)
    
    table_hb_data_uptake_2 <- table_hb_data_uptake %>%  select(1,last_column,6:(last_column - 1)) %>% 
      rowwise() %>% mutate(perc_accepted = paste0(round(sum(c_across(c(3:4)))/sum(c_across(c(2)))*100,1), "%")) %>% 
      rename("Number of People Referred to PDS" = "Total", 
             "Health Board" = "health_board",
             "Percentage Accepted" = "perc_accepted")
    
    last_column_2 <- ncol(table_hb_data_uptake_2)
    
    make_table(table_hb_data_uptake_2, right_align = 2:(last_column_2 - 1), selected = 1, table_elements = "t") %>%
      formatCurrency(c(3:last_column_2 - 1), currency = "", interval = 3, mark = ",", digits = 0)
    
    
  }else{    
    
# Data table ijb
    
    table_hb_data_uptake <- data_uptake %>% 
      filter(fy == input$select_year_add, sex == input$select_sex_add, simd == "All") %>% 
      mutate(ijb = if_else(health_board == "Scotland", "AAA", ijb)) %>%
      arrange(ijb) %>% 
      filter(ijb != "All") %>% 
      mutate(ijb = if_else(ijb == "AAA", "Scotland", ijb)) %>% 
      pivot_wider(names_from = pds_uptake_decision, values_from = referrals) %>% 
      mutate(across(where(is.numeric), ~ replace(., is.na(.), 0))) %>% 
      adorn_totals("col") 
    
      last_column <- ncol(table_hb_data_uptake)
      
      table_hb_data_uptake_2 <- table_hb_data_uptake %>% select(2,last_column,6:(last_column - 1)) %>% 
      rowwise() %>% mutate(perc_accepted = paste0(round(sum(c_across(c(3:4)))/sum(c_across(c(2)))*100,1), "%")) %>%
      rename("Number of People Referred to PDS" = "Total",
             "Integration Authority Area" = "ijb",
             "Percentage Accepted" = "perc_accepted")
      
      last_column_2 <- ncol(table_hb_data_uptake_2)
      
    make_table(table_hb_data_uptake_2, right_align = 2:(last_column_2 - 1), selected = 1, table_elements = "t", rows_to_display = 32) %>%
      formatCurrency(c(3:last_column_2 - 1), currency = "", interval = 3, mark = ",", digits = 0)
  }
})


#table 2 (simd)
output$table_uptake_2_title<- renderUI({HTML(paste0("Number of Referrals and Percentage of People that Accepted PDS by Scottish Index of Multiple Deprivation (SIMD), Financial Year ",
                                                  input$select_year_add, ", ", input$select_hb_ijb_uptake_2, ", Gender: ", input$select_sex_add))
})

output$table_uptake_2 <- DT::renderDataTable({
  uptake_table_2_data <- data_uptake %>%
    pivot_wider(names_from = pds_uptake_decision, values_from = referrals) %>%
    mutate(across(where(is.numeric), ~ replace(., is.na(.), 0))) %>%
    mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>% 
    filter(ijb == input$select_hb_ijb_uptake_2,
           fy == input$select_year_add,
           sex == input$select_sex_add,
           simd != "All",
           simd != "Unknown") %>%
    select(5:9) %>% 
   # rowwise() %>% mutate(perc_accepted = paste0(round(sum(c_across(c(2,3)))/sum(c_across(c(2:5)))*100,1), "%")) %>% 
    rename("SIMD" = "simd")
        #   "Percentage Accepted" = "perc_accepted")
  make_table(uptake_table_2_data, right_align = 1:4, table_elements = "t") %>%
    formatCurrency(c(2:4), currency = "", interval = 3, mark = ",", digits = 0)
})


# output$simd_uptake_plot_title<- renderUI({HTML(paste0("Percentage of People that Accepted PDS by Scottish Index of Multiple Deprivation (SIMD), Financial Year ",
#                                                     input$select_year_add, ", ", input$select_hb_ijb_uptake_2, ", Gender: ", input$select_sex_add))
# })


# simd chart
output$plot_simd_uptake <- renderPlotly({
  percent_uptake_bar_chart(data_uptake %>% 
                             pivot_wider(names_from = pds_uptake_decision, values_from = referrals) %>%
                             mutate(across(where(is.numeric), ~ replace(., is.na(.), 0))) %>%
                             mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>% 
                             filter(ijb == input$select_hb_ijb_uptake_2,
                                    fy == input$select_year_add,
                                    sex == input$select_sex_add,
                                    simd != "All",
                                    simd != "Unknown") %>% 
                             rowwise() %>% mutate(perc_accepted = round(sum(c_across(c(6,7)))/sum(c_across(c(6:9)))*100,1))
                           #x_text_angle = 0

  )
})




#Pathways ----

# Data table diagnosis to contact times
output$hb_ijb_table_title_wait <- renderUI({HTML(paste0("Pathways from diagnosis to contact by PDS practitioner, Financial Year ", 
                                                 input$select_year_add, ", Scotland and ", input$select_wait_table, ", Gender: ", input$select_sex_add))
})

output$table_hb_ijb_wait <- DT::renderDataTable({
  
  if(input$select_wait_table == "Health Boards"){  
    
    table_hb_data_wait <- data_wait %>% 
      mutate(across(starts_with("median"), ~ replace(., is.na(.), "-"))) %>% 
      # mutate(across(starts_with("median"), ~ replace(., .<0, "-"))) %>%
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>%
      mutate(across(starts_with("referrals"),
                    ~if_else(health_board == "NHS Grampian" & fy %in% c("2019/20", "2020/21"), "-", .))) %>%
      mutate(across(starts_with("median"),
                    ~if_else(health_board == "NHS Grampian" & fy %in% c("2019/20", "2020/21"), "-", .))) %>% 
      filter(fy == input$select_year_add, sex == input$select_sex_add) %>% 
      filter(ijb == "All" | ijb == "Scotland", simd == "All") %>% 
      select(health_board, total_referrals, median_diagnosis_to_referral,
             referrals_allocated, median_referral_to_allocation,
             referrals_contacted, median_allocation_to_contact,
             median_diagnosis_to_contact) %>% 
      set_colnames(c("Health Board","Number of People Referred to PDS",  "Average (median) days from diagnosis to PDS referral",
                     "Referrals allocated to PDS practitioner", "Average (median) days from referral to allocation of PDS practitioner", 
                     "Referrals contacted by PDS practitioner",  "Average (median) days from allocation to first contact with PDS practitioner",
                     "Average (median) days from diagnosis to first contact"
      )) 
    make_table(table_hb_data_wait, right_align = 1:7, selected = 1, table_elements = "t") #%>%
      #formatCurrency(c(2,4,6), currency = "", interval = 3, mark = ",", digits = 0)
    
    
  }else{    
    
    
    # # Data table ijb
    
    
    table_hb_data_wait <- data_wait %>% 
      mutate(across(starts_with("median"), ~ replace(., is.na(.), "-"))) %>% 
      # mutate(across(starts_with("median"), ~ replace(., .<0, "-"))) %>%
      mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>%
      mutate(across(starts_with("referrals"),
                    ~if_else(ijb == "Aberdeen City" & fy %in% c("2019/20", "2020/21"), "-", .))) %>%
      mutate(across(starts_with("median"),
                    ~if_else(ijb == "Aberdeen City" & fy %in% c("2019/20", "2020/21"), "-", .))) %>% 
      filter(fy == input$select_year_add, sex == input$select_sex_add) %>% 
      filter(ijb != "All", simd == "All") %>% 
      select(ijb, total_referrals, median_diagnosis_to_referral,
             referrals_allocated, median_referral_to_allocation,
             referrals_contacted, median_allocation_to_contact,
             median_diagnosis_to_contact) %>% 
      mutate(ijb = if_else(ijb == "Scotland", "AAA", ijb)) %>%
      arrange(ijb) %>% 
      mutate(ijb = if_else(ijb == "AAA", "Scotland", ijb)) %>% 
      set_colnames(c("Integration Authority Area","Number of People Referred to PDS",  "Average (median) days from diagnosis to PDS referral",
                     "Referrals allocated to PDS practitioner", "Average (median) days from referral to allocation of PDS practitioner", 
                     "Referrals contacted by PDS practitioner",  "Average (median) days from allocation to first contact with PDS practitioner",
                     "Average (median) days from diagnosis to first contact"
      )) 
    
    make_table(table_hb_data_wait, right_align = 1:7, selected = 1, table_elements = "t", rows_to_display = 32) #%>% 
    #  formatCurrency(c(2,4,6), currency = "", interval = 3, mark = ",", digits = 0)
  }
})


# table 2 (contact to termination)
output$table_wait_2_title<- renderUI({HTML(paste0("Pathways from contact by PDS practitioner to end of post-diagnostic support, Financial Year ", 
                                                  input$select_year_add, ", ", input$select_hb_ijb_wait_2, ", Gender: ", input$select_sex_add))
})

output$table_wait_2 <- DT::renderDataTable({
  wait_table_2_data <- data_wait_2 %>% 
    pivot_wider(names_from = ldp, values_from = c(referrals, median_contact_to_termination)) %>%
    filter(!(ijb == "Aberdeen City" & fy == "2019/20")) %>% 
    filter(ijb == input$select_hb_ijb_wait_2,
           fy == input$select_year_add,
           sex == input$select_sex_add) %>% 
    mutate(across(starts_with("referrals"), ~ replace(., is.na(.), 0))) %>% 
   # mutate(across(starts_with("median"), ~ replace(., is.na(.), "-"))) %>% 
    select(termination_or_transition_reason,
           referrals_All,
           referrals_complete,
           referrals_exempt,
           referrals_fail, median_contact_to_termination_All) %>% 
    set_colnames(c("Reason for end of post-diagnostic support",
                   "Number of people whose PDS has ended",
                   "Standard Met",
                   "Exempt from Standard",
                   "Standard Not Met", "Average (median) days from first contact with PDS practitioner to end of PDS"))
  
  make_table(wait_table_2_data, right_align = 1:4, table_elements = "t") %>% 
    formatCurrency(c(2:5), currency = "", interval = 3, mark = ",", digits = 0)
})

# output$table_wait_2 <- DT::renderDataTable({
#   wait_table_2_data <- data_wait_2 %>% 
#     filter(ijb == input$select_hb_ijb_wait_2,
#            fy == input$select_year_add,
#            sex == input$select_sex_add,
#            !grepl("exempt", termination_or_transition_reason)) %>% 
#     select(termination_or_transition_reason, referrals, median_contact_to_termination) %>% 
#     set_colnames(c("Reason for termination of post-diagnostic support", "Number of referrals", "Average (median) days from contact to termination"))
#   make_table(wait_table_2_data, table_elements = "t") %>% 
#     formatCurrency(c(2,3), currency = "", interval = 3, mark = ",", digits = 0)
# })

# output$table_wait_2_exempt <- DT::renderDataTable({
#   wait_table_2_data_exempt <- data_wait_2 %>%
#     filter(ijb == input$select_hb_ijb_wait_2,
#            fy == input$select_year_add,
#            sex == input$select_sex_add,
#            grepl("exempt", termination_or_transition_reason)) %>%
#     select(termination_or_transition_reason, referrals, median_contact_to_termination) %>%
#     set_colnames(c("Reason for termination of post-diagnostic support", "Number of referrals", "Average (median) days from contact to termination"))
#   make_table(wait_table_2_data_exempt, table_elements = "t") %>%
#     formatCurrency(c(2,3), currency = "", interval = 3, mark = ",", digits = 0)
# })

  