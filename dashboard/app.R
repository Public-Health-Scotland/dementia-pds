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
    header = tags$head(includeCSS("www/styles.css"),  # CSS stylesheet
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
    
    h1("Dementia Post-Diagnostic Support; Local Delivery Plan (LDP) Standard - Scotland"),
    linebreaks(1),
    fluidRow(selectInput("select_year_p1",
                label = "Select Financial Year of Diagnosis:",
                choices = included_years,
                selected= provisional_year
                )),
    linebreaks(1),
    uiOutput("page_1_ui")
   ), # tabpanel

##############################################.
# PAGE 2: Health Boards LDP Standard  ----
##############################################.
tabPanel(title = "Health Boards LDP Standard",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("clipboard-list"),
         value = "ldp-hb",

         h1(htmlOutput("page_2_title")),
         linebreaks(1),
         fluidRow(
            selectInput("select_year_p2",
                             label = "Select Financial Year of Diagnosis:",
                             choices = included_years,
                             selected = provisional_year)),
         fluidRow(
           selectInput("select_hb_p2",
                     label = "Select Health Board:",
                     choices = boards)),
         linebreaks(1),
         uiOutput("page_2_ui")
        
     ), # tabpanel

##############################################.
# PAGE 3: Trend  ----
##############################################.
tabPanel(title = "Trends",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("chart-line"),
         value = "trends",
         
         h1("Dementia Post-Diagnostic Support - Trends"),
         linebreaks(1),
      fluidRow(column(
        
        radioGroupButtons("trend_tab", label = NULL, choices = trend_list,
                          status = "tab",
                          direction = "horizontal", 
                          justified = T,
                          size = "lg"), width = 12)),
        
           uiOutput("page_3_ui")
  
), # tabpanel


##############################################.
# PAGE 4: Demographics ----
##############################################.
tabPanel(title = "Demographics",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("id-card"),
         value = "demo",
         
         h1("Dementia Post-Diagnostic Support - Demographics"),
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
                                           label = "Select Health Board/IJB:",
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
                   
                       
        
           uiOutput("page_4_ui") 
           ) #main panel
        ) #sidebar layout

      ), # tabpanel


##############################################.
# PAGE 5: Additional Analysis----
##############################################.
tabPanel(title = "Additional Analysis",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("magnifying-glass-chart"),
         value = "data",
         
         h1("Dementia Post-Diagnostic Support - Additional Analysis"),
         linebreaks(1),
         
         sidebarLayout(
           sidebarPanel(radioGroupButtons("select_data_add", label = NULL, choices = data_list,
                                          status = "secondary",
                                          direction = "vertical", 
                                          justified = T,
                                          size = "normal"), 
          linebreaks(1),              
                        selectInput("select_year_add",
                                           label = "Select Financial Year of Diagnosis:",
                                           choices = included_years,
                                           selected = provisional_year),
          
  conditionalPanel(condition= 'input.select_data_add == "data_subtype" || input.select_data_add == "data_stage" || input.select_data_add == "data_referral" || input.select_data_add == "data_model"',      
           
                       selectInput("select_hb_ijb_add",
                                           label = "Select Health Board/IJB:",
                                           choices = c("Scotland", boards, ijb_list))),
          
                        selectInput("select_sex_add",
                                           label="Select Gender:",
                                           choices=c("All", "Female", "Male")),
   
                        
                        width = 2, style = "position:fixed; width: 16%; overflow-y: overlay; margin-left: -30px; height:-webkit-fill-available"),
          
           
           mainPanel(width = 10,
            
               uiOutput("page_5_ui") 
           ) # main panel
         )  #sidebar layout
     ), # tabpanel


##############################################.
# PAGE 6: Methodology ----
##############################################.
tabPanel(title = "Methodology",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("diagram-successor"),
         value = "method",
         
         h1("Dementia Post-Diagnostic Support - Methodology"),
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
         uiOutput("page_6_ui")
      
) # tabPanel

##############################################.
# PAGE 7: Data Quality ----
##############################################.
# tabPanel(title = "Data Quality",
#          # Look at https://fontawesome.com/search?m=free for icons
#          icon = icon_no_warning_fn("circle-check"),
#          value = "quality",
#          h1("Dementia Post-Diagnostic Support - Data Quality"),
#               
#            uiOutput("page_7_ui")
#   
# ) # tabPanel



    ) # navbar
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
    source(file.path(here("dashboard/pages/page_1.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/page_2.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/page_3.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/page_4.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/page_5.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/page_6.R")), local = TRUE)$value
   # source(file.path(here("dashboard/pages/page_7.R")), local = TRUE)$value

}

# Run the application
shinyApp(ui=ui, server=server)

### END OF SCRIPT ###
