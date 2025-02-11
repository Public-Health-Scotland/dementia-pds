####################### Page 6: methodology #######################
# UI ----
output$page_6_ui <-  renderUI({

  div(
    conditionalPanel(
      condition= 'input.method_tab == "ldp_class"',
   
      fluidRow(column(
        p("The following steps are taken to ensure the data is of sufficient quality for analysis:"),
        
        p(tags$ul(tags$li("Remove records with diagnosis date outwith the reporting period."),
                  tags$li("Remove records with missing diagnosis date.")
        )
        ),
        h3("LDP Standard Met"),
        
        p(tags$ul(tags$li("Started PDS within 12 months of diagnosis and support ongoing after 12 months."),
                  tags$li("Started PDS within 12 months of diagnosis and PDS ended after at least 11 months.")
        )
        ),
        h3("LDP Standard Not Met"),
        
        p(tags$ul(tags$li("PDS started more than 12 months after diagnosis."),
                  tags$li("PDS not started and more than 12 months has passed since diagnosis."),
                  tags$li("PDS terminated (for non-exempt reason) less than 11 months after first contact date."),
                  tags$li("PDS terminated (for non-exempt reason) before first contact made.")
        )
        ),
        h3("Exempt from LDP Standard"),
        
        p(tags$ul(tags$li("No first contact date or less than 12 months between diagnosis and first contact date and one of the following termination reasons:"),
          tags$ul(tags$li("03 Service user has died."),
                  tags$li("04 Service user has moved to a different Health Board area."),
                  tags$li("05 Service user has terminated PDS early/refused."),
                  tags$li("06 Service user no longer able to engage in PDS.")
        )
        )
        ),
        h3("PDS Ongoing"),
        
        p(tags$ul(tags$li("Less than 12 months since diagnosis and PDS not yet started."),
                  tags$li("PDS started within 12 months and not yet ended.")
        )
        ),
        
        width = 12)),
      
      
      
    ), #cond panel 1
    
conditionalPanel(
  condition = 'input.method_tab == "exp_diag"',
  
  fluidRow(column(
    p("In December 2016, the Scottish Government published a report:",
             a("Estimated and Projected Diagnosis Rates for Dementia in Scotland: 2014-2020.", href="https://www.gov.scot/publications/estimated-projected-diagnosis-rates-dementia-scotland-2014-2020/"),
             "Estimations in this report are available per calendar year and health board therefore analysis of this part of the LDP standard is unavailable by Integration Authority Area or any other breakdowns. Information on the methodology used to calculate these figures and the limitations of this are available in the report."),
           p("For financial years ",
             em("2021/22 and 2022/23"),
             " the rates referenced in the report above were used to create national, age specific rates of dementia incidence per 1,000 population which were then applied to the National Records of Scotland (NRS) Mid-2021 Population Estimates to obtain the incidence estimates."),
           p("Please note that as estimations are available by calendar year and figures in this report are by financial year, the estimation for the calendar year with the majority of months in the selected financial year is used. For example, analysis for financial year 2018/19 uses estimations for the calendar year 2018."),
    width = 12)) #fluid Row
), #cond panel 2
    
    conditionalPanel(
      condition = 'input.method_tab == "duplicates"',
      
      fluidRow(column(
        p("For a relatively small number of individuals, multiple records have been submitted. To avoid counting these service users more than once, the following rules have been applied to select only one record per CHI Number:"),
               p(tags$ol(tags$li("Keep record with earliest diagnosis date. If these are the same, then;"),
                         tags$li("Keep record with termination reason 04 Service user has moved to a different Health Board area. If no record was terminated for this reason, then;"),
                         tags$li("Keep record with earliest first contact date.")
               )
               ), 
               p("There also exists a Service Level Agreement between NHS Highland and NHS Greater Glasgow & Clyde health boards, where some PDS is provided to Argyll & Bute residents by West Dunbartonshire Integration Authority Area. The support provided to these service users has been apportioned to NHS Highland in this report, as part of the LDP Standard is a population based measure, and so by not including these Argyll & Bute residents this figure may be skewed."),
               p("If you have any queries regarding the above, please contact ", a("phs.dementiapds@phs.scot", href="mailto:phs.dementiapds@phs.scot")),
               
      width = 12)) #fluid Row
    ) #cond panel 3
 
  ) # div
}) # renderUI

