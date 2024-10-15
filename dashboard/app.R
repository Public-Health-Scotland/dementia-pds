##########################################################
# dementia pds
# Original author(s): Original author(s)
# Original date: 2024-09-24
# Written/run on RStudio server 2022.7.2.576.12 and R 4.1.2
# Description of content
##########################################################


# Get packages
library(here)
source(here("dashboard/setup.R"))

# UI
ui <- fluidPage(
tagList(
# Specify most recent fontawesome library - change version as needed
tags$style("@import url(https://use.fontawesome.com/releases/v6.1.2/css/all.css);"),
navbarPage(
  tags$style(".small-box.bg-blue { background-color: #3393DD !important; color: #000000 !important; }"),
    id = "intabset", # id used for jumping between tabs
    title = div(
        tags$a(img(src = "phs-logo.png", height = 40),
               href = "https://www.publichealthscotland.scot/",
               target = "_blank"), # PHS logo links to PHS website
    style = "position: relative; top: -5px;"),
    windowTitle = "Dementia PDS",# Title for browser tab
    header = tags$head(includeCSS("www/styles.css"),  # CSS stylesheet
    tags$link(rel = "shortcut icon", href = "favicon_phs.ico") # Icon for browser tab
), ##############################################.
# INTRO PAGE ----
##############################################.
tabPanel(title = "Home",
    icon = icon_no_warning_fn("circle-info"),
    value = "intro",

    h1("Dementia Post Diagnostic Support (PDS) Dashboard"),
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
# PAGE 1 ----
##############################################.
tabPanel(title = "Scotland LDP Standard",
    # Look at https://fontawesome.com/search?m=free for icons
    icon = icon_no_warning_fn("clipboard"),
    value = "intro",
    
    h1("Dementia Post-Diagnostic Support; Local Delivery Plan (LDP) Standard - Scotland"),
    linebreaks(1),
    box(selectInput("select_year_p1",
                label = "Financial Year",
                choices = included_years,
                selected= provisional_year
                )),
    linebreaks(1),
    uiOutput("page_1_ui"),
    linebreaks(2),
    h2("Scotland - Number of Individuals Diagnosed and Referred for PDS"),
    plotlyOutput("ldp_scotland"),
    linebreaks(2),
    h2("Scotland/Health Board - Number of Individuals relating to LDP Standard"),
    DT::dataTableOutput("table_hb"),
    linebreaks(2),
    h2("IJB - Number of Individuals relating to LDP Standard"),
    DT::dataTableOutput("table_ijb"),
    linebreaks(2)

    ), # tabpanel

# ----------------------------------------------

##############################################.
# PAGE 2 ----
##############################################.
tabPanel(title = "Health Boards LDP Standard",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("clipboard-list"),
         value = "intro",

         h1("Dementia Post-Diagnostic Support; Local Delivery Plan (LDP) Standard - Healthboard"),
         linebreaks(1),
         box(selectInput("select_hb_p2",
                     label = "Health board",
                     choices = boards
                     ),
         selectInput("select_year_p2",
                     label = "Financial Year",
                     choices = included_years,
                     selected = provisional_year)),
         linebreaks(1),
         uiOutput("page_2_ui"),
         linebreaks(2),
         h2("Number of Individuals relating to LDP Standard"),
         DT::dataTableOutput("table_hb_ijb"),
         linebreaks(2),
         h2("Number of Individuals Diagnosed and Referred for PDS"),
         plotlyOutput("hb_ijb_plot"),
         linebreaks(2)

     ), # tabpanel

# ----------------------------------------------

##############################################.
# PAGE 3 ----
##############################################.
tabPanel(title = "Demographics",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("id-card"),
         value = "intro",

         h1("Post Diagnostic Support - Demographics"),
         linebreaks(2),

         sidebarLayout(
           sidebarPanel(radioGroupButtons("select_data_p3", label = NULL, choices = demographics_list,
                                          status = "primary",
                                          direction = "vertical", 
                                          justified = T,
                                          size = "lg"), width = 3),
         mainPanel(
           # inputs
           fluidRow(column(selectInput("select_hb_p3",
                                           label = "Health board",
                                           choices = c("Scotland", boards)),width=6),
                    column(selectInput("select_year_p3",
                                           label = "Financial Year",
                                           choices = included_years,
                                           selected = provisional_year),width=6)),

           fluidRow(column(selectInput("select_sex_p3",
                                       label="Sex",
                                       choices = c("All", "Female", "Male")),width=6)),
                    
          
           fluidRow(
           # outputs
          #uiOutput("page_3_ui"),
          #linebreaks(2),
          h2(htmlOutput("table_title_p3")),
          DT::dataTableOutput("table_p3"),
          #linebreaks(2),
          h2(htmlOutput("chart_title_p3")),
          plotlyOutput("plot_p3", height = "800px"),
          linebreaks(2))
          ) # main panel
         ) #sidebar

      ), # tabpanel


# ----------------------------------------------

##############################################.
# PAGE 4 ----
##############################################.
tabPanel(title = "Additional Analysis",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("magnifying-glass-chart"),
         value = "intro",
         
         h1("Post Diagnostic Support - Additional Analysis"),
         linebreaks(2),
         
         sidebarLayout(
           sidebarPanel(radioGroupButtons("select_data_p4", label = NULL, choices = data_list,
                                          status = "primary",
                                          direction = "vertical", 
                                          justified = T,
                                          size = "lg"), width = 3),
           mainPanel(
             # inputs
             fluidRow(column(selectInput("select_hb_p4",
                                         label = "Health board",
                                         choices = c("Scotland", boards)),width=6),
                      column(selectInput("select_year_p4",
                                         label = "Financial Year",
                                         choices = included_years,
                                         selected = provisional_year),width=6)),
             
             fluidRow(column(selectInput("select_sex_p4",
                     label="Sex",
                  choices = c("All", "Female", "Male")),width=6)),
               #  column(selectInput("select_simd_p3",
               #label="SIMD",
               #  choices=unique(data_wait$simd)),width=6),
            fluidRow(
               # outputs
               #uiOutput("page_4_ui"),
               #linebreaks(2),
               h2(htmlOutput("table_title_p4")),
               DT::dataTableOutput("table_p4"),
               #linebreaks(2),
               h2(htmlOutput("chart_title_p4")),
               plotlyOutput("plot_p4", height = "800px"),
               linebreaks(2))
           ) # main panel
         ) #sidebar
         
     ) # tabpanel
    ) # navbar
  ) # taglist
) # ui fluidpage

# ----------------------------------------------

# Server

server <- function(input, output, session) {

    # Get functions
    source(here(here("dashboard/functions/core_functions.R")), local = TRUE)$value
    source(file.path(here("dashboard/functions/intro_page_functions.R")), local = TRUE)$value
    source(file.path(here("dashboard/functions/page_1_functions.R")), local = TRUE)$value

    # Get content for individual pages
    source(file.path(here("dashboard/pages/intro_page.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/page_1.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/page_2.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/page_3.R")), local = TRUE)$value
    source(file.path(here("dashboard/pages/page_4.R")), local = TRUE)$value

}

# Run the application
shinyApp(ui=ui, server=server)

### END OF SCRIPT ###
