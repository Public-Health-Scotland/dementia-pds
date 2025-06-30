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
# PAGE 1: LDP Standard  ----
##############################################.

tabPanel(title = "LDP Standard",
    # Look at https://fontawesome.com/search?m=free for icons
    icon = icon_no_warning_fn("clipboard-list"),
    value = "ldp-standard",
    
    box(h1("Dementia Post-Diagnostic Support; Local Delivery Plan (LDP) Standard"),
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
      linebreaks(1),
      
      conditionalPanel(condition = 'input.ldp_sidebar != "trends"',
     
          selectInput("select_year_p1",
                      label = "Select Financial Year of Diagnosis:",
                      choices = included_years,
                      selected= provisional_year_sup)
          ),#conditionalPanel
    width = 3, style = "position:fixed; width: 23%; overflow-y: overlay; margin-left: -30px; height:-webkit-fill-available"),
      
      mainPanel(width = 9,
                
      uiOutput("ldp_ui")
              )#mainPanel
         )#sidebarLayout
     ), # tabpanel


##############################################.
# PAGE 2: Referrals and Rates ----
##############################################.
tabPanel(title = "Referrals & Rates",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("braille"),
         value = "rates",
         
         box(h1("Dementia Post-Diagnostic Support; Referrals to PDS"),
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
# PAGE 3: Demographics ----
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
                                           selected = provisional_year_sup),
                               
                    width = 2, style = "position:fixed; width: 16%; overflow-y: overlay; margin-left: -30px; height:-webkit-fill-available"),
           
           mainPanel(width = 10,
                   
           uiOutput("demo_ui") 
           ) #main panel
        ) #sidebar layout

      ), # tabpanel



##############################################.
# PAGE 4: Pathways ----
##############################################.
tabPanel(title = "Pathways",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("arrows-turn-to-dots"),
         value = "pathways",
         
         box(h1("Dementia Post-Diagnostic Support - Pathways"),
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
             linebreaks(1),
             
             conditionalPanel(condition = 'input.pathways_sidebar == "wait"',
                              
                                selectInput("select_year_pathways",
                                            label = "Select Financial Year of Diagnosis:",
                                            choices = included_years,
                                            selected= provisional_year_sup),
                                                           
                                radioButtons("select_hb_ijb_pathways",
                                             label = "In the chart and table show Scotland and: ",
                                             choices = c("Health Boards", "Integration Authority Areas"),
                                             selected = "Health Boards",
                                             inline = FALSE)
                              ), #conditionalPanel
                         width = 3, style = "position:fixed; width: 23%; overflow-y: overlay; margin-left: -30px; height:-webkit-fill-available"),
           
           mainPanel(width = 9,
         uiOutput("pathways_ui")
           ) # mainPanel
         ) #sidebarLayout
), # tabpanel


##############################################.
# PAGE 5: Methodology ----
##############################################.
tabPanel(title = "Methodology",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("signs-post"),
         value = "method",
         
         box(h1("Dementia Post-Diagnostic Support - Methodology"),
             width = 12,
             collapsible = TRUE, collapsed = FALSE),
         
         # fluidRow(
         #   box("This page includes more detail regarding the methodology used to arrive at the figures in this report. It is hoped that by sharing this, the reports will be easier to understand and local reporting will be more consistent.",
         #       background = "blue",
         #       width = 12
         #   )),
         # linebreaks(1),
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
    source(file.path(here("dashboard/pages/page_1_ldp_standard.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/page_2_rates.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/page_3_demographics.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/page_4_pathways.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/page_5_methodology.R")), local = TRUE)$value
   # source(file.path(here("dashboard/pages/page_7.R")), local = TRUE)$value

}

# Run the application
shinyApp(ui=ui, server=server)

### END OF SCRIPT ###
