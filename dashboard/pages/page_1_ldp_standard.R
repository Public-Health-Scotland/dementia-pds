####################### Page 1: LDP Standard #######################
# UI ----
output$ldp_ui <-  renderUI({

  div(
#LDP PART 1----
    conditionalPanel(condition = 'input.select_ldp == "ldp_part_1"',
    fluidRow(
       column(
         h2(strong(htmlOutput("title_part_1"))),
linebreaks(1),          
 # value box ----
         fluidRow(
             shinydashboard::valueBox(
                 value = textOutput("scot_exp_perc"),
                 subtitle = "of people estimated to be newly diagnosed with dementia were referred for post-diagnostic support.",
                 width = 7,
                 color = "blue"), #valueBox
             box(htmlOutput("scot_exp_text"),
                    status = "primary", 
                    title = (p(strong("How is this figure calculated?"))),
                    width = 5), # box
         ), #fluidRow
        fluidRow(
linebreaks(1),

#  table ----
              h3(strong(htmlOutput("hb_exp_table_title_p1"))),
              DT::dataTableOutput("table_hb_exp"),
linebreaks(1)
           ), # fluid Row
            width = 12,
            style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available"),
         ) #fluidRow
      ), #cond panel 1

#LDP PART 2 ----
conditionalPanel(condition = 'input.select_ldp == "ldp_part_2"',
                 fluidRow(
                   column(
                     h2(strong(htmlOutput("title_part_2"))),
                     linebreaks(1),          
                    
                     # value box ----
                     fluidRow(
                       shinydashboard::valueBox(
                         value = textOutput("scot_pds_perc"),
                         subtitle = "of those referred for post-diagnostic support received a minimum of 12 months of support.",
                         width = 7,
                         color = "blue"), #valueBox
                       box(htmlOutput("scot_pds_text"),
                           status = "primary", 
                           title = (p(strong("How is this figure calculated?"))),
                           width = 5), #box
                     ), #fluidRow
                     fluidRow(
                       linebreaks(1),
                       # table ----
                       h3(strong(htmlOutput("pds_table_title_p1"))),
                       fluidRow(column(
                         radioButtons("select_hb_ijb",
                                      label = "In the table below show Scotland and: ",
                                      choices = c("Health Boards", "Integration Authority Areas"),
                                      selected = "Health Boards",
                                      inline = TRUE),
                         width = 12)),
                       DT::dataTableOutput("table_pds"),
                       linebreaks(1),
                     ), # fluid Row
                     width = 12,
                     style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; padding-right: 45px; height:-webkit-fill-available"),
                 ) #fluidRow
) #cond panel 2

)# div
}) # renderUI

#SERVER ----

#value boxes data ----
vb_data<- reactive({annual_table_data %>% filter(health_board == "Scotland", ijb == "Scotland", fy == input$select_year_p1, ldp == "total")}) 

# percentage of people estimated to be newly diagnosed with dementia were referred for post-diagnostic support
output$scot_exp_perc <- renderText({paste0(vb_data()$exp_perc, "%")})

output$scot_exp_text <- renderUI({
  HTML(paste("A total of", "<b>",  prettyNum(vb_data()$referrals, big.mark = ","), "</b>", "referrals were made to post-diagnostic support. This is divided by",
  "<b>", prettyNum(vb_data()$diagnoses, big.mark = ","), "</b>", "the estimated number of people newly diagnosed with dementia."))})

# percentage of those referred for post-diagnostic support received a minimum of 12 months of support
output$scot_pds_perc <- renderText({paste0(vb_data()$rate, "%")})

vb_2_data<- reactive({annual_table_data %>% filter(health_board == "Scotland", ijb == "Scotland", fy == input$select_year_p1,
                                                   ldp != "fail") %>% select(-diagnoses, -exp_perc) %>% 
                                            pivot_wider(values_from = referrals, names_from = ldp)})

output$scot_pds_text <- renderUI({
  HTML(paste("<b>", prettyNum(vb_2_data()$complete + vb_2_data()$exempt, big.mark = ","), "</b>", "referrals either met or were exempt from the LDP standard. This is divided by",
             "<b>", prettyNum(vb_2_data()$total - vb_2_data()$ongoing, big.mark = ","), "</b>", "the total number of referrals (excluding those whose support is ongoing)."))})


#part 1 title ----
 output$title_part_1 <- renderUI({HTML(paste("Percentage of estimated diagnoses referred to PDS: Scotland, Financial Year ", 
                                              input$select_year_p1))
 })

#part 2 title ----
output$title_part_2 <- renderUI({HTML(paste("Percentage receving one year's PDS support: Scotland, Financial Year ", 
                                            input$select_year_p1))
})



# Data table hb pds percentage ----
#output$hb_table_title_p1 <- renderUI({HTML(paste("Number and percentage of people referred for dementia post-diagnostic support who received a minimum of one year’s support: Financial Year ", 
                                               #  input$select_year_p1))
#})

output$pds_table_title_p1 <- renderUI({HTML(paste0("Number and percentage of people referred for dementia post-diagnostic support who received a minimum of one year’s support: Financial Year ", 
  input$select_year_p1, ", Scotland and ", input$select_hb_ijb))
})

output$table_pds <- DT::renderDataTable({
  
  if(input$select_hb_ijb == "Health Boards"){
  
  table_hb_data <- annual_table_data %>% 
    filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
    filter(fy == input$select_year_p1) %>%
    select(health_board,ldp,referrals,rate)%>%
    mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>%
    mutate(across(starts_with("perc"), ~ paste0(.,"%"))) %>%
    pivot_wider(names_from=ldp,values_from=referrals) %>% 
    select(health_board, total, complete, exempt, ongoing, fail, rate) %>% 
    mutate(health_board = if_else(health_board == "Scotland","AAA Scotland", health_board)) %>% 
    arrange(health_board) %>% 
    mutate(health_board = if_else(health_board == "AAA Scotland","Scotland", health_board)) %>% 
    mutate(rate = paste0(rate, "%")) %>% 
    set_colnames(c("NHS Board","Number of People Referred to PDS", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved")) 
   make_table(table_hb_data, right_align = 1:6, selected = 1, table_elements = "t") 

  
}else{

# Data table ijb pds percentage ----
#output$ijb_table_title_p1 <- renderUI({HTML(paste("IJB - Number and percentage of people referred for dementia post-diagnostic support who received a minimum of one year’s support: Financial Year ", 
                                                  #input$select_year_p1))
#})

 table_ijb_data <- annual_table_data %>%
    filter(!grepl("NHS", ijb)) %>% 
    filter(fy == input$select_year_p1) %>%
   mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
   mutate(rate = if_else(is.na(rate), "-", paste0(rate, "%"))) %>% 
   mutate(referrals = if_else(fy %in% c("2019/20", "2020/21") & ijb == "Aberdeen City" & ldp != "total", "-", as.character(referrals))) %>% 
   select(ijb,ldp,referrals,rate) %>%
   pivot_wider(names_from=ldp,values_from=referrals) %>% 
   select(ijb, total, complete, exempt, ongoing, fail, rate) %>% 
   arrange(ijb) %>% 
   set_colnames(c("Integration Authority Area","Number of People Referred to PDS", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved"))
 make_table(table_ijb_data, right_align = 1:6, selected = 1, rows_to_display = 32, table_elements = "t")
  
}
})

# Data table hb expected percentage ----
output$hb_exp_table_title_p1 <- renderUI({HTML(paste0("Number and percentage of people estimated to be newly diagnosed with dementia who were referred for post-diagnostic support: Financial Year ", 
                                                 input$select_year_p1, ", Scotland and Health Boards"))
})

output$table_hb_exp <- DT::renderDataTable({
  table_hb_exp_data <- annual_table_data %>% 
    filter(fy == input$select_year_p1) %>%
    filter(grepl("NHS", ijb) | ijb == "Scotland", !is.na(diagnoses)) %>% 
    group_by(health_board)%>%
    select(health_board, diagnoses, referrals)%>%
    mutate(exp_perc = paste0(round(referrals/diagnoses*100, 1), "%")) %>%  
    mutate(health_board = if_else(health_board == "Scotland","AAA Scotland", health_board)) %>% 
    arrange(health_board) %>% 
    mutate(health_board = if_else(health_board == "AAA Scotland","Scotland", health_board)) %>% 
    set_colnames(c("Health Board","Estimated Number of People Newly Diagnosed with Dementia", "Number of People Referred to PDS","Percentage of Estimated Number of People Diagnosed with Dementia Referred to PDS"))
  make_table(table_hb_exp_data, right_align = 1:3, selected = 1, table_elements = "t") %>% formatCurrency(c(2,3), currency = "", interval = 3, mark = ",", digits = 0)
})




