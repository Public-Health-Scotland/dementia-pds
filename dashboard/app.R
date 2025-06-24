####
# dementia pds
# Original author(s): Original author(s)
# Original date: 2024-09-24
# Written/run on RStudio server 2022.7.2.576.12 and R 4.1.2
# Description of content
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
    
    h1("Dementia Post-Diagnostic Support (PDS)"),
    linebreaks(1),
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
# PAGE 1: Scotland LDP Standard  ----
##############################################.

tabPanel(title = "Scotland LDP Standard",
    # Look at https://fontawesome.com/search?m=free for icons
    icon = icon_no_warning_fn("clipboard"),
    value = "ldp-scot",
    
    box(h1("Dementia Post-Diagnostic Support; Local Delivery Plan (LDP) Standard - Scotland"),
        width = 12,
        collapsible = TRUE, collapsed = FALSE),
    #linebreaks(1),
    fluidRow(
      column(
        selectInput("select_year_p1",
                label = "Select Financial Year of Diagnosis:",
                choices = included_years,
                selected= provisional_year),
        width = 2, style = "width: auto; margin-top: -10px"),
            style = "border-bottom: solid; border-bottom-width: medium; border-bottom-color: var(--phs-purple-50);"),
      uiOutput("scotland_ui")
   ), # tabpanel



##############################################.
# PAGE 2: Health Boards LDP Standard  ----
##############################################.
tabPanel(title = "Health Boards LDP Standard",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("clipboard-list"),
         value = "ldp-hb",

            box(h1(htmlOutput("page_2_title")), width = 12,
                collapsible = TRUE, collapsed = FALSE),
         #linebreaks(1),
            fluidRow(
                column(
                  selectInput("select_year_p2",
                             label = "Select Financial Year of Diagnosis:",
                             choices = included_years,
                             selected = provisional_year),
                width = 2, style = "width: auto; margin-top: -10px"),
                column(
                  selectInput("select_hb_p2",
                            label = "Select Health Board:",
                            choices = boards),
                width = 2, style = "width: auto; margin-top: -10px"),
             style = "border-bottom: solid; border-bottom-width: medium; border-bottom-color: var(--phs-purple-50);"),
          uiOutput("hbs_ui")
        ), # tabpanel

##############################################.
# PAGE 3: Trend  ----
##############################################.
tabPanel(title = "Trends",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("chart-line"),
         value = "trends",
         
         box(h1("Dementia Post-Diagnostic Support - Trends"),
             width = 12,
             collapsible = TRUE, collapsed = FALSE),
         
        # linebreaks(1),
      fluidRow(column(
                radioGroupButtons("trend_tab", label = NULL, choices = trend_list,
                          status = "tab",
                          direction = "horizontal", 
                          justified = T,
                          size = "lg"), 
                  width = 12)
                ), #fluidRow
        
           uiOutput("trends_ui")
  
), # tabpanel


##############################################.
# PAGE 4: Demographics ----
##############################################.
tabPanel(title = "Demographics",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("id-card"),
         value = "demo",
         
         box(h1("Dementia Post-Diagnostic Support - Demographics"),
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
                                           selected = provisional_year),
                               
                               
                        selectInput("select_hb_ijb_demo",
                                           label = "Select Health Board/Integration Authority Area:",
                                           choices = c("Scotland", boards, ijb_list)),
                        
         conditionalPanel(condition = 'input.select_data_demo == "data_sex"',
                                         
                        selectInput("select_simd_demo",
                                           label = "Select SIMD Quintile:",
                                           choices = c("All",simd_list))), 
         
         conditionalPanel(condition = 'input.select_data_demo != "data_sex"',
                          
                        selectInput("select_sex_demo",
                                            label="Select Gender:",
                                            choices=c("All", "Female", "Male"))),
                        
                        
                        width = 2, style = "position:fixed; width: 16%; overflow-y: overlay; margin-left: -30px; height:-webkit-fill-available"),
           
           mainPanel(width = 10,
                   
                       
        
           uiOutput("demo_ui") 
           ) #main panel
        ) #sidebar layout

      ), # tabpanel


##############################################.
# PAGE 5: Rates Per 10,000 Population ----
##############################################.
tabPanel(title = "Rates",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("braille"),
         value = "rates",
         
         box(h1("Dementia Post-Diagnostic Support - Rates Per 10,000 Population"),
             width = 12,
             collapsible = TRUE, collapsed = FALSE),
         
         # linebreaks(1),
         fluidRow(column(
           # radioGroupButtons("trend_tab", label = NULL, choices = trend_list,
           #                   status = "tab",
           #                   direction = "horizontal", 
           #                   justified = T,
           #                   size = "lg"), 
           width = 12)
         ), #fluidRow
         
         uiOutput("rates_ui")
         
), # tabpanel

##############################################.
# PAGE 6: Pathways ----
##############################################.
tabPanel(title = "Pathways",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("arrows-turn-to-dots"),
         value = "pathways",
         
         box(h1("Dementia Post-Diagnostic Support - Pathways"),
             width = 12,
             collapsible = TRUE, collapsed = FALSE),
         
         # linebreaks(1),
         fluidRow(
           column(
             selectInput("select_year_pathways",
                         label = "Select Financial Year of Diagnosis:",
                         choices = included_years,
                         selected= provisional_year),
                      width = 2, style = "width: auto; margin-top: -10px"),
            style = "border-bottom: solid; border-bottom-width: medium; border-bottom-color: var(--phs-purple-50);"),
         uiOutput("pathways_ui")
         
), # tabpanel


##############################################.
# PAGE 7: Methodology ----
##############################################.
tabPanel(title = "Methodology",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("diagram-successor"),
         value = "method",
         
         box(h1("Dementia Post-Diagnostic Support - Methodology"),
             width = 12,
             collapsible = TRUE, collapsed = FALSE),
         
         fluidRow(
           box("This page includes more detail regarding the methodology used to arrive at the figures in this report. It is hoped that by sharing this, the reports will be easier to understand and local reporting will be more consistent.",
               background = "blue",
               width = 12
           )),
         linebreaks(1),
         fluidRow(column(
           
           radioGroupButtons("method_tab", label = NULL, choices = method_list,
                             status = "tab",
                             direction = "horizontal", 
                             justified = T,
                             size = "lg"), width = 12)),
         uiOutput("method_ui")
      
), # tabPanel

    collapsible = TRUE) # navbar
  ) # taglist
) # ui fluidpage


# SERVER

server <- function(input, output, session) {
  
  #for linking to methodology tab
  observe({
    query <- parseQueryString(session$clientData$url_search)
    query1 <- paste(names(query), query, sep = "=", collapse=", ")
    print(query1)
    if(query1 == "a=b"){
      updateTabItems(session, inputId = "intabset", selected = "method")
    }
  })
  
  
    # Get content for individual pages
    source(file.path(here("dashboard/pages/intro_page.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/page_1_scotland.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/page_2_healthboards.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/page_3_trends.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/page_4_demographics.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/page_5_rates.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/page_6_pathways.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/page_7_methodology.R")), local = TRUE)$value
   # source(file.path(here("dashboard/pages/page_7.R")), local = TRUE)$value

}

# Run the application
shinyApp(ui=ui, server=server)

### END OF SCRIPT ###
