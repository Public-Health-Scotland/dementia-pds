####
# dementia pds
# Original author(s): Abram McCormick
# Original date: 2024-09-24
# Written/run on RStudio R 4.1.2
# Description of content: Dementia PDS dashboard for annual publication
####


# Get packages
library(here)
source(here("dashboard/setup.R"))

# UI
ui <- fluidPage(
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
      sidebarPanel(radioGroupButtons("home_select", label = NULL, choices = home_list,
                                     status = "primary",
                                     direction = "vertical", 
                                     justified = T,
                                     size = "lg"), width = 3),
             mainPanel(
                uiOutput("intro_page_ui")
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
             #linebreaks(1),
             radioGroupButtons("RandR_sidebar", label = NULL, choices = RandR_sidebar_list,
                               status = "secondary",
                               direction = "vertical",
                               justified = T,
                               size = "lg"
             ),
             conditionalPanel(condition = 'input.RandR_sidebar == "referrals"',
                              linebreaks(1),
                              selectInput("select_year_randr",
                                          label = "Select Financial Year of Diagnosis:",
                                          choices = included_years_extra_referrals,
                                          selected= extra_referrals_year_sup),
             ), #cond panel total referrals
             conditionalPanel(condition = 'input.RandR_tab == "RandR_totals"',
                              conditionalPanel(condition = 'input.RandR_sidebar == "trends"',
                                               linebreaks(1),   
                                               selectInput("select_randr_trend_totals",
                                                           label = "Select Health Board/Integration Authority to show in chart:",
                                                           choices = c("Scotland", boards, ijb_list))
                              ), #cond panel trends
             ),#cond panel part 1
             conditionalPanel(condition = 'input.RandR_tab == "RandR_rates"', 
                              conditionalPanel(condition = 'input.RandR_sidebar == "trends"',
                                               linebreaks(1), 
                                               selectInput("randr_select_trend_rates",
                                                           label = "Select Health Board/Integration Authority to show in chart:",
                                                           choices = c("Scotland", boards, ijb_list))
                              ), #cond panel trends
             ),#cond panel part 2
                              radioButtons("select_hb_ijb_randr",
                                           label = "In the chart and table show:",
                                           choices = c("Health Boards", "Integration Authority Areas"),
                                           selected = "Health Boards"),
            
             width = 3
           ), #sidebar panel
           
           mainPanel(width = 9,
                     
                     fluidRow(column(
                       linebreaks(1),
                       radioGroupButtons("RandR_tab", label = NULL, choices = RandR_tab_list,
                                         status = "tab",
                                         direction = "horizontal",
                                         justified = T,
                                         size = "normal"),
                       width = 12)
                     ), #fluidRow

                     uiOutput("rates_ui")
           )#main panel
         )#sidebar layout
         
), # tabpanel

##############################################.
# PAGE 2: LDP Standard  ----
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
             #linebreaks(1),
             radioGroupButtons("ldp_sidebar", label = NULL, choices = ldp_sidebar_list,
                               status = "secondary",
                               direction = "vertical", 
                               justified = T,
                               size = "lg"
             ),
             conditionalPanel(condition = 'input.ldp_sidebar == "outcomes"',
                              linebreaks(1),
                              selectInput("select_year_ldp",
                                          label = "Select Financial Year of Diagnosis:",
                                          choices = included_years,
                                          selected= provisional_year_sup)  
             ), #cond panel outcomes
             conditionalPanel(condition = 'input.ldp_tab == "ldp_part_1"',
                              conditionalPanel(condition = 'input.ldp_sidebar == "trends"',
                                               linebreaks(1),   
                                               selectInput("select_hb_trend_part_1",
                                                           label = "Select Health Board to show in chart:",
                                                           choices = c("Scotland", boards))
                              ), #cond panel trends
             ),#cond panel ldp part 1
             conditionalPanel(condition = 'input.ldp_tab == "ldp_part_2"', 
                              conditionalPanel(condition = 'input.ldp_sidebar == "trends"',
                                               linebreaks(1), 
                                               selectInput("select_hb_ijb_trend_part_2",
                                                           label = "Select Health Board/Integration Authority to show in chart:",
                                                           choices = c("Scotland", boards, ijb_list))
                              ), #cond panel trends
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
                       radioGroupButtons("ldp_tab", label = NULL, choices = ldp_tab_list,
                                         status = "tab",
                                         direction = "horizontal",
                                         justified = T,
                                         size = "normal"),
                       width = 12)
                     ), #fluidRow
                     
                     uiOutput("ldp_ui")
           )#mainPanel
         )#sidebarLayout
), # tabpanel

##############################################.
# PAGE 3: Pathways ----
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
             #linebreaks(1),
             radioGroupButtons("pathways_sidebar", label = NULL, choices = pathways_list,
                               status = "secondary",
                               direction = "vertical", 
                               justified = T,
                               size = "lg"
             ),
             
             conditionalPanel(condition = 'input.pathways_sidebar == "wait"',
                              linebreaks(1),
                              selectInput("select_year_pathways",
                                          label = "Select Financial Year of Diagnosis:",
                                          choices = included_years_2025_gender_wait,#change to included_years from 2026 onwards
                                          selected= provisional_year_sup)
             ), #conditionalPanel wait
             conditionalPanel(condition = 'input.pathways_sidebar == "trends"',  
                              linebreaks(1),
                              selectInput("select_hb_ijb_pathways_trend",
                                          label = "Select Health Board/Integration Authority to show in chart:",
                                          choices = c("Scotland", boards, ijb_list))
             ), #cond panel trends
             radioButtons("select_hb_ijb_pathways",
                          label = "In the chart and table show:",
                          choices = c("Health Boards", "Integration Authority Areas"),
                          selected = "Health Boards"),
             width = 3
           ),#sidebarPanel
           mainPanel(width = 9,
                     uiOutput("pathways_ui")
           ) # mainPanel
         ) #sidebarLayout
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
      
         linebreaks(1),

         sidebarLayout(
           sidebarPanel(radioGroupButtons("select_data_demo", label = NULL, choices = demographics_list,
                                          status = "secondary",
                                          direction = "vertical",
                                          justified = T,
                                          size = "normal"
                                          ),
                        linebreaks(1),
                        selectInput("select_year_demo",
                                           label = "Select Financial Year of Diagnosis:",
                                           choices = included_years,
                                           selected = provisional_year_sup),

                    width = 2
                    ),

           mainPanel(width = 10,
                   
           uiOutput("demo_ui") 
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
           
           radioGroupButtons("method_tab", label = NULL, choices = method_list,
                             status = "tab",
                             direction = "horizontal",
                             justified = T,
                             size = "lg"), width = 12)),#fluidRow
         
         uiOutput("method_ui")
      
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
         
         uiOutput("download_ui")
         
), # tabPanel

    collapsible = TRUE) # navbar
  ) # taglist
) # ui fluidpage


# SERVER

server <- function(input, output, session) {
  
  ##link to methodology page----
  observeEvent(input$method_link, {
    updateTabsetPanel(session, "intabset", selected = "method")
  })
  
  ##link to home page----
  observeEvent(input$home_link, {
    updateTabsetPanel(session, "intabset", selected = "intro")
    updateRadioGroupButtons(session, "home_select", selected = "about")
  })
  

## Get content for individual pages----
    source(file.path(here("dashboard/pages/intro_page.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/ldp_standard.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/rates.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/demographics.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/pathways.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/methodology.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/download.R")), local = TRUE)$value


}

# Run the application
shinyApp(ui=ui, server=server)

### END OF SCRIPT ###
