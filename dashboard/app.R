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
#side bar buttons
tags$style(".btn.radiobtn.btn-primary {border-radius: 0px; }"),
tags$style(".btn.radiobtn.btn-primary.active { background-color: #4B999D ; border-radius: 0px; border: 2px solid #FFFFFF}"), 
#intro page ldp description box
tags$style(".box.box-solid.bg-blue { background-color: #E6F2FB ; padding: 2px; border: 3px solid #B3D7F2}"), 
#value box for page 1 and 2
tags$style(".small-box.bg-blue {background-color: #3393DD; border-radius: 0px; padding: 4px; margin-bottom: 10px; margin-right: 0px}"), 
# description box for page 1 and 2
tags$style(".box.box-solid.bg-black {background-color: #FFFFFF; padding: 0px; margin-bottom: 10px; margin-left: 0px}"), 
tags$style(".box-header {background-color: #E6F2FB; padding: 4px"),
tags$style(".box-title {margin-top: 3px; margin-bottom: -12px; padding: 4px"),
tags$style(".box-body {padding: 10px"),
tags$style(".col-sm-5 {padding-right: 4px; padding-left: 4px}"),
tags$style(".col-sm-7 {padding-right: 4px; padding-left: 4px}"),
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
# PAGE 1: Scotland LDP Standard  ----
##############################################.

tabPanel(title = "Scotland LDP Standard",
    # Look at https://fontawesome.com/search?m=free for icons
    icon = icon_no_warning_fn("clipboard"),
    value = "intro",
    
    h1("Dementia Post-Diagnostic Support; Local Delivery Plan (LDP) Standard - Scotland"),
    linebreaks(1),
    fluidRow(selectInput("select_year_p1",
                label = "Financial Year",
                choices = included_years,
                selected= provisional_year
                )),
    
    fluidRow(
    linebreaks(1),
    uiOutput("page_1_ui"),
    linebreaks(2),
    h2(htmlOutput("chart_title_p1")),
    plotlyOutput("ldp_scotland"),
    linebreaks(2),
    h2(htmlOutput("hb_table_title_p1")),
    DT::dataTableOutput("table_hb"),
    linebreaks(2),
    h2(htmlOutput("ijb_table_title_p1")),
    DT::dataTableOutput("table_ijb"),
    linebreaks(2)
    ),

    ), # tabpanel

##############################################.
# PAGE 2: Health Boards LDP Standard  ----
##############################################.
tabPanel(title = "Health Boards LDP Standard",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("clipboard-list"),
         value = "intro",

         h1("Dementia Post-Diagnostic Support; Local Delivery Plan (LDP) Standard - Healthboard"),
         linebreaks(1),
         fluidRow(
            selectInput("select_year_p2",
                             label = "Financial Year",
                             choices = included_years,
                             selected = provisional_year)),
         fluidRow(
           selectInput("select_hb_p2",
                     label = "Health board",
                     choices = boards)),
         fluidRow(
         linebreaks(1),
         uiOutput("page_2_ui"),
         linebreaks(2),
         h2(htmlOutput("table_title_p2")),
         DT::dataTableOutput("table_hb_ijb"),
         linebreaks(2),
         h2(htmlOutput("chart_title_p2")),
         plotlyOutput("hb_ijb_plot"),
         linebreaks(2)),

     ), # tabpanel

##############################################.
# PAGE 3: Trend  ----
##############################################.
tabPanel(title = "Trend",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("chart-line"),
         value = "intro",
         
         h1("Percentage of people referred for dementia PDS who received a minimum of one year’s support; Trend"),
      fluidRow(
         #  uiOutput("page_3_ui"),
          # linebreaks(2),
           h2("Scotland/Health Board"),
           DT::dataTableOutput("table_hb_trend"),
           h2("Integration Joint Board"),
           DT::dataTableOutput("table_ijb_trend"),
          h2(htmlOutput("chart_title_trend")),
          linebreaks(1)),
      fluidRow(
            column(selectInput("select_hb_ijb_trend",
                                label = "Select health board/IJB to show in chart:",
                                choices = c(boards, ijb_list), width = "100%"), width = 3)),
      
           plotlyOutput("trend_plot")
      
      
), # tabpanel


##############################################.
# PAGE 4: Demographics ----
##############################################.
tabPanel(title = "Demographics",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("id-card"),
         value = "intro",

         h1("Post Diagnostic Support - Demographics"),
         linebreaks(2),

         sidebarLayout(
           sidebarPanel(radioGroupButtons("select_data_demo", label = NULL, choices = demographics_list,
                                          status = "primary",
                                          direction = "vertical", 
                                          justified = T,
                                          size = "lg"), width = 3),
         mainPanel(
           # inputs
           fluidRow(
                    column(selectInput("select_year_demo",
                                           label = "Financial Year",
                                           choices = included_years,
                                           selected = provisional_year),width=6),
                    column(selectInput("select_sex_demo",
                                       label="Sex",
                                       choices=c("All", "Female", "Male")),width=6)),

           fluidRow(column(selectInput("select_hb_demo",
                                       label = "Health board",
                                       choices = c("Scotland", boards)),width=6)),
                    
          
             
           # outputs
          fluidRow(
          #uiOutput("page_3_ui"),
          #linebreaks(2),
          h2(htmlOutput("table_title_demo")),
          DT::dataTableOutput("table_demo"),
          #linebreaks(2),
          h2(htmlOutput("chart_title_demo_referrals")),
          plotlyOutput("plot_demo_referrals", height = "600px"),
          h2(htmlOutput("chart_title_demo_ldp")),
          plotlyOutput("plot_demo_ldp", height = "600px")),
        
          ) # main panel
         ) #sidebar layout

      ), # tabpanel


##############################################.
# PAGE 5: Additional Analysis----
##############################################.
tabPanel(title = "Additional Analysis",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("magnifying-glass-chart"),
         value = "intro",
         
         h1("Post Diagnostic Support - Additional Analysis"),
         linebreaks(2),
         
         sidebarLayout(
           sidebarPanel(radioGroupButtons("select_data_add", label = NULL, choices = data_list,
                                          status = "primary",
                                          direction = "vertical", 
                                          justified = T,
                                          size = "lg"), width = 3),
           
           mainPanel(
             fluidRow(
              column(selectInput("select_year_add",
                                                 label = "Financial Year",
                                                 choices = included_years,
                                                 selected = provisional_year),width=6),
             column(selectInput("select_sex_add",
                                label="Sex",
                                choices=c("All", "Female", "Male")),width=6)),
             conditionalPanel(
               condition= 'input.select_data_add != "waiting_times"',
             # inputs
             
             
             fluidRow(column(selectInput("select_hb_add",
                                         label = "Health board",
                                         choices = c("Scotland", boards)),width=6)),
                      
             
             #uiOutput("page_5_ui"),
             
             fluidRow(
               # outputs
               h2(htmlOutput("table_title_add")),
               DT::dataTableOutput("table_add"),
               h2(htmlOutput("chart_title_add_referrals")),
               plotlyOutput("plot_add_referrals", height = "600px"),
               h2(htmlOutput("chart_title_add")),
               plotlyOutput("plot_add", height = "600px"))
             ), #cond panel 1
             
             conditionalPanel(
               condition = 'input.select_data_add == "waiting_times"',
               
               fluidRow(
                 # outputs
                 h2(htmlOutput("hb_table_title_wait")),
                 DT::dataTableOutput("table_hb_wait"),
                 linebreaks(2),
                 h2(htmlOutput("ijb_table_title_wait")),
                 DT::dataTableOutput("table_ijb_wait"),
                 # h2(htmlOutput("")),
                 # plotlyOutput("", height = "600px"))
               
             ) #fluid Row
           ) #cond panel 2
         ) # main panel
         )  #sidebar layout
     ) # tabpanel
    ) # navbar
  ) # taglist
) # ui fluidpage


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
    source(file.path(here("dashboard/pages/page_5.R")), local = TRUE)$value

}

# Run the application
shinyApp(ui=ui, server=server)

### END OF SCRIPT ###
