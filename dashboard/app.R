################################################################################.
# dementia pds
# Original author(s): Abram McCormick
# Original date: 2024-09-24
# Written/run on RStudio R 4.4.2
# Description of content: Dementia PDS dashboard for annual publication
################################################################################.

################################################################################.
# SETUP ----
################################################################################.

# Get packages, data and define lists
source("setup.R")

# Get whether password protect is on or off
load("data/AUTH_ENABLED.RData")

# Get password protect credentials
credentials <- readRDS("data/credentials.rds")

################################################################################.
# UI ----
################################################################################.

ui <- 
    fluidPage(
  tagList(
# Specify most recent fontawesome library - change version as needed
tags$style("@import url(https://use.fontawesome.com/releases/v6.1.2/css/all.css);"),

# set up navigation bar
navbarPage(
    id = "intabset", # id used for jumping between tabs
    title = div(
        tags$a(img(src = "phs-logo.png", height = 40),
               href = "https://www.publichealthscotland.scot/",
               target = "_blank"), # PHS logo links to PHS website
    style = "position: relative; top: -5px;"),
    windowTitle = "Dementia PDS",# Title for browser tab
    header = tags$head(includeCSS("www/styles.css"), # CSS stylesheet
                       includeScript("www/app.js"),
    tags$link(rel = "shortcut icon", href = "favicon_phs.ico") # Icon for browser tab
    ), 
    
##############################################.
# INTRO PAGE: Home ----
##############################################.
tabPanel(title = "Home",
    icon = icon_no_warning_fn("circle-info"),
    value = "intro",
    
    box(class = "header", h2("Dementia Post-Diagnostic Support"),
        width = 12,
        collapsible = FALSE, collapsed = FALSE),
        # linebreaks(1),
    sidebarLayout(
      #sidebar buttons
      sidebarPanel(radioGroupButtons("home_select", label = NULL, choices = home_list,
                                     status = "primary",
                                     direction = "vertical", 
                                     justified = T,
                                     size = "lg"), width = 3),
             mainPanel(
                uiOutput("intro_page_ui") #pages/intro_page.R
      )
    )

), # tabpanel

##############################################.
# PAGE 1: Referrals and Rates ----
##############################################.
tabPanel(title = "Referrals & Rates",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("braille"),
         value = "rates",
         
         box(class = "header", h2("Dementia Post-Diagnostic Support; Referrals to PDS"),
             width = 12,
             collapsible = TRUE, collapsed = FALSE),
         
         sidebarLayout(
           
           sidebarPanel(
             #sidebar buttons
             radioGroupButtons("RandR_sidebar", label = NULL, choices = RandR_sidebar_list,
                               status = "secondary",
                               direction = "vertical",
                               justified = T,
                               size = "lg"
             ),
             conditionalPanel(condition = 'input.RandR_sidebar == "referrals"',
                              linebreaks(1),
                              # dropdown menu to select financial year
                              selectInput("select_year_randr",
                                          label = "Select Financial Year of Diagnosis:",
                                          choices = included_years_extra_referrals,
                                          selected= extra_referrals_year_sup),
             ), #cond panel total referrals
             conditionalPanel(condition = 'input.RandR_tab == "RandR_totals"',
                              conditionalPanel(condition = 'input.RandR_sidebar == "trends"',
                                               linebreaks(1),  
                                               #dropdown menu to select geography for totals trend plot
                                               selectInput("select_randr_trend_totals",
                                                           label = "Select Health Board/Integration Authority to show in chart:",
                                                           choices = c("Scotland", boards, ijb_list))
                              ), #cond panel trends
             ),#cond panel part 1
             conditionalPanel(condition = 'input.RandR_tab == "RandR_rates"', 
                              conditionalPanel(condition = 'input.RandR_sidebar == "trends"',
                                               linebreaks(1), 
                                               #dropdown menu to select geography for rates trend plot
                                               selectInput("randr_select_trend_rates",
                                                           label = "Select Health Board/Integration Authority to show in chart:",
                                                           choices = c("Scotland", boards, ijb_list))
                              ), #cond panel trends
             ),#cond panel part 2
                              #radio buttons to toggle between health boards and integration authority areas
                              radioButtons("select_hb_ijb_randr",
                                           label = "In the chart and table show:",
                                           choices = c("Health Boards", "Integration Authority Areas"),
                                           selected = "Health Boards"),
            
             width = 3
           ), #sidebar panel
           
           mainPanel(width = 9,
                     
                     fluidRow(column(
                       linebreaks(1),
                       #tabs to select Total Referrals and Rates per 10,000 Population
                       radioGroupButtons("RandR_tab", label = NULL, choices = RandR_tab_list,
                                         status = "tab",
                                         direction = "horizontal",
                                         justified = T,
                                         size = "normal"),
                       width = 12)
                     ), #fluidRow

                     uiOutput("rates_ui") #pages/rates.R
           )#main panel
         )#sidebar layout
), # tabpanel

##############################################.
# PAGE 2: Pathways ----
##############################################.
tabPanel(title = "Pathways",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("arrows-turn-to-dots"),
         value = "pathways",
         
         box(class = "header", h2("Dementia Post-Diagnostic Support; Pathways"),
             width = 12,
             collapsible = TRUE, collapsed = FALSE),
         
         sidebarLayout(
           sidebarPanel(
             #sidebar buttons
             radioGroupButtons("pathways_sidebar", label = NULL, choices = pathways_list,
                               status = "secondary",
                               direction = "vertical", 
                               justified = T,
                               size = "lg"
             ),
             
             conditionalPanel(condition = 'input.pathways_sidebar == "wait"',
                              linebreaks(1),
                              #dropdown to select financial year
                              selectInput("select_year_pathways",
                                          label = "Select Financial Year of Diagnosis:",
                                          choices = included_years_pathways,#CHANGE to included_years from 2026 onwards----
                                          selected= extra_referrals_year_sup)
             ), #conditionalPanel wait
             conditionalPanel(condition = 'input.pathways_sidebar == "trends"',  
                              linebreaks(1),
                              #drop down to select healthboard/IAA in trend chart
                              selectInput("select_hb_ijb_pathways_trend",
                                          label = "Select Health Board/Integration Authority to show in chart:",
                                          choices = c("Scotland", boards, ijb_list))
             ), #cond panel trends
             #radio buttons to toggle between health boards and integration authority areas
             radioButtons("select_hb_ijb_pathways",
                          label = "In the chart and table show:",
                          choices = c("Health Boards", "Integration Authority Areas"),
                          selected = "Health Boards"),
             width = 3
           ),#sidebarPanel
           
           mainPanel(width = 9,
                     uiOutput("pathways_ui")#pages/pathways.R
                     
           ) # mainPanel
         ) #sidebarLayout
), # tabpanel

##############################################.
# PAGE 3: LDP Standard  ----
##############################################.
tabPanel(title = "LDP Standard",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("clipboard-list"),
         value = "ldp-standard",
         
         box(class = "header", h2("Dementia Post-Diagnostic Support; Local Delivery Plan (LDP) Standard"),
             width = 12,
             collapsible = TRUE, collapsed = FALSE),
         
         sidebarLayout(
           sidebarPanel(
             #sidebar buttons
             radioGroupButtons("ldp_sidebar", label = NULL, choices = ldp_sidebar_list,
                               status = "secondary",
                               direction = "vertical", 
                               justified = T,
                               size = "lg"
             ),
             conditionalPanel(condition = 'input.ldp_sidebar == "outcomes"',
                              linebreaks(1),
                              #dropdown menu to select financial year
                              selectInput("select_year_ldp",
                                          label = "Select Financial Year of Diagnosis:",
                                          choices = included_years,
                                          selected= provisional_year_sup)  
             ), #cond panel outcomes
             conditionalPanel(condition = 'input.ldp_tab == "ldp_part_1"',
                              conditionalPanel(condition = 'input.ldp_sidebar == "trends"',
                                               linebreaks(1),  
                                               #dropdown menu to select health board to show in part 1 trend chart
                                               selectInput("select_hb_trend_part_1",
                                                           label = "Select Health Board to show in chart:",
                                                           choices = c("Scotland", boards))
                              ), #cond panel trends
             ),#cond panel ldp part 1
             conditionalPanel(condition = 'input.ldp_tab == "ldp_part_2"', 
                              conditionalPanel(condition = 'input.ldp_sidebar == "trends"',
                                               linebreaks(1), 
                                               #dropdown menu to select health board/IAA to show in part 2 trend chart
                                               selectInput("select_hb_ijb_trend_part_2",
                                                           label = "Select Health Board/Integration Authority to show in chart:",
                                                           choices = c("Scotland", boards, ijb_list))
                              ), #cond panel trends
                              #radio buttons to toggle between health boards and integration authority areas
                              radioButtons("select_hb_ijb",
                                           label = "In the chart and table show:",
                                           choices = c("Health Boards", "Integration Authority Areas"),
                                           selected = "Health Boards")
             ),#cond panel ldp part 2
             width = 3
           ), #sidebar panel
           
           mainPanel(width = 9,
                     
                     fluidRow(column(
                       linebreaks(1),
                       #tabs for selecting LDP Standard Part 1 and LDP Standard Part 2
                       radioGroupButtons("ldp_tab", label = NULL, choices = ldp_tab_list,
                                         status = "tab",
                                         direction = "horizontal",
                                         justified = T,
                                         size = "normal"),
                       width = 12)
                     ), #fluidRow
                     
                     uiOutput("ldp_ui")#pages/ldp_standard.R
           )#mainPanel
         )#sidebarLayout
), # tabpanel

##############################################.
# PAGE 4: Demographics ----
##############################################.
tabPanel(title = "Demographics",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("id-card"),
         value = "demo",
         
         box(class = "header", h2("Dementia Post-Diagnostic Support; Demographics"),
             width = 12,
             collapsible = TRUE, collapsed = FALSE),
      
         sidebarLayout(
           sidebarPanel(
             #sidebar buttons
             radioGroupButtons("select_data_demo", label = NULL, choices = demographics_list,
                                          status = "secondary",
                                          direction = "vertical",
                                          justified = T,
                                          size = "normal"
                                          ),
                        linebreaks(1),
                        #dropdown to select financial year
                        selectInput("select_year_demo",
                                           label = "Select Financial Year of Diagnosis:",
                                           choices = included_years_demographics,
                                           selected = provisional_year_sup),

                    width = 2
                    ),

           mainPanel(width = 10,
                   
           uiOutput("demo_ui")#pages/demographics.R
           
         ) #main panel
       ) #sidebar layout

      ), # tabpanel

##############################################.
# PAGE 5: Methodology ----
##############################################.
tabPanel(title = "Methodology",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("signs-post"),
         value = "method",
         
         box(class = "header", h2("Dementia Post-Diagnostic Support; Methodology"),
             width = 12,
             collapsible = TRUE, collapsed = FALSE),
         
         fluidRow(column(
           #tab for selecting LDP Classification, Number of Expected Diagnoses, Removal of Duplicate Records
           radioGroupButtons("method_tab", label = NULL, choices = method_list,
                             status = "tab",
                             direction = "horizontal",
                             justified = T,
                             size = "lg"), width = 12)),#fluidRow
         
         uiOutput("method_ui")#pages/methodology.R
      
), # tabPanel

##############################################.
# PAGE 6: Data Download----
##############################################.
tabPanel(title = "Data Download",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("download"),
         value = "dl",
         
         box(class = "header", h2("Dementia Post-Diagnostic Support; Download Data"),
             width = 12,
             collapsible = TRUE, collapsed = FALSE),
         
         uiOutput("download_ui")#pages/download.R
         
), # tabPanel

    collapsible = TRUE) # navbar
  ) # taglist
) # ui fluidpage

################################################################################.
# SERVER ----
################################################################################.

server <- function(input, output, session) {
  options(encoding="UTF-8")
 
  # Link to methodology page
  observeEvent(input$method_link, {
    updateTabsetPanel(session, "intabset", selected = "method")
  })
  
  # Link to home page
  observeEvent(input$home_link, {
    updateTabsetPanel(session, "intabset", selected = "intro")
    updateRadioGroupButtons(session, "home_select", selected = "about")
  })

  # Get content for individual pages
  source("pages/intro_page.R", local = TRUE)$value
  source("pages/ldp_standard.R", local = TRUE)$value
  source("pages/rates.R", local = TRUE)$value
  source("pages/demographics.R", local = TRUE)$value
  source("pages/pathways.R", local = TRUE)$value
  source("pages/methodology.R", local = TRUE)$value
  source("pages/download.R", local = TRUE)$value

  # Radio button label updates (ldp, pathways, RandR)
  observeEvent(input$ldp_sidebar, {
    req(input$ldp_sidebar)
    updateRadioButtons(session, "select_hb_ijb", label = if (input$ldp_sidebar == "trends") "In the table show:" else "In the chart and table show:")
  }, ignoreInit = TRUE)
  
  observeEvent(input$pathways_sidebar, {
    req(input$pathways_sidebar)
    updateRadioButtons(session, "select_hb_ijb_pathways", label = if (input$pathways_sidebar == "trends") "In the table show:" else "In the chart and table show:")
  }, ignoreInit = TRUE)
  
  observeEvent(input$RandR_sidebar, {
    req(input$RandR_sidebar)
    updateRadioButtons(session, "select_hb_ijb_randr", label = if (input$RandR_sidebar == "trends") "In the table show:" else "In the chart and table show:")
  }, ignoreInit = TRUE)
  
  # RandR: year select logic
  observeEvent(list(input$RandR_tab, input$select_year_randr), {
    req(input$RandR_tab, input$select_year_randr)
    updateSelectInput(session, "select_year_randr", label = "Select Financial Year of Diagnosis:",
                      choices = included_years_extra_referrals, selected = input$select_year_randr)
  }, ignoreInit = TRUE)
}

################################################################################.
# RUN APPLICATION ----
################################################################################.

if (AUTH_ENABLED) {
  ui <- shinymanager::secure_app(ui)
  secured_server <- function(input, output, session) {
    shinymanager::secure_server(check_credentials(credentials))
    server(input, output, session)
  }
  shinyApp(ui, secured_server)
} else {
  shinyApp(ui, server)
}

### END OF SCRIPT ###