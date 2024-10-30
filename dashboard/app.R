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
    h2(htmlOutput("pds_table_title_p1")),
    
    fluidRow(column(
      radioButtons("select_hb_ijb",
                         label = "In the table below show Scotland and: ",
                         choices = c("Health Boards", "Integration Joint Boards"),
                         selected = "Health Boards",
                         inline = TRUE
    ), width = 3)),
    
    DT::dataTableOutput("table_pds"),
    linebreaks(2),
    #h2(htmlOutput("ijb_table_title_p1")),
   # DT::dataTableOutput("table_ijb"),
   # linebreaks(2),
    h2(htmlOutput("hb_exp_table_title_p1")),
    DT::dataTableOutput("table_hb_exp"),
    linebreaks(2)
    ),

    ), # tabpanel

##############################################.
# PAGE 2: Health Boards LDP Standard  ----
##############################################.
tabPanel(title = "Health Boards LDP Standard",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("clipboard-list"),
         value = "ldp-hb",

        # h1("Dementia Post-Diagnostic Support; Local Delivery Plan (LDP) Standard - Health Board"),
         h1(htmlOutput("page_2_title")),
         linebreaks(1),
         fluidRow(
            selectInput("select_year_p2",
                             label = "Financial Year of Diagnosis",
                             choices = included_years,
                             selected = provisional_year)),
         fluidRow(
           selectInput("select_hb_p2",
                     label = "Health Board",
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
         value = "trend",
         
         h1("Dementia Post-Diagnostic Support - Trend"),
      fluidRow(column(
        
        radioGroupButtons("trend_tab", label = NULL, choices = trend_list,
                          status = "primary",
                          direction = "horizontal", 
                          justified = T,
                          size = "lg"), width = 12)),
        
         #  uiOutput("page_3_ui"),
          # linebreaks(2),
      conditionalPanel(
        condition= 'input.trend_tab == "pds_perc_trend"',
          fluidRow(
            column(
      h2(htmlOutput("table_pds_trend_title")), width = 12)),
      fluidRow(
        column(
        radioButtons("select_pds_trend_table",
                     label = "In the table below show Scotland and: ",
                     choices = c("Health Boards", "Integration Joint Boards"),
                     selected = "Health Boards",
                     inline = TRUE
        ), width = 3)),
           DT::dataTableOutput("table_hb_ijb_trend"),
           linebreaks(1),
     # h2("Percentage of people referred for dementia PDS who received a minimum of one year’s support; Integration Joint Board"),
        #   DT::dataTableOutput("table_ijb_trend"),
          h2(htmlOutput("chart_title_trend")),
      fluidRow(
            column(selectInput("select_hb_ijb_trend",
                                label = "Select Health Board/IJB to show in chart:",
                                choices = c(boards, ijb_list), width = "100%"), width = 3)),
      
           plotlyOutput("trend_plot")
      ), # cond panel 1
    conditionalPanel(
      condition= 'input.trend_tab == "exp_perc_trend"',
      fluidRow(
        column(
     h2("Percentage of people estimated to be newly diagnosed with dementia who were referred for post-diagnostic support; Scotland and Health Boards"),
        DT::dataTableOutput("table_hb_trend_2"),
        linebreaks(1),
        h2(htmlOutput("chart_title_trend_2")), width = 12)),
      #  linebreaks(1),
      fluidRow(
        column(selectInput("select_hb_ijb_trend_2",
                           label = "Select Health Board to show in chart:",
                           choices = boards, width = "100%"), width = 3)),
      
      plotlyOutput("trend_plot_2")
    ) # cond panel 2
      
      
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
                                          status = "primary",
                                          direction = "vertical", 
                                          justified = T,
                                          size = "lg"), width = 3),
         mainPanel(width = 9,
           # inputs
           fluidRow(
                    column(selectInput("select_year_demo",
                                           label = "Financial Year of Diagnosis",
                                           choices = included_years,
                                           selected = provisional_year),width=6),
                    column(selectInput("select_sex_demo",
                                        label="Gender",
                                        choices=c("All", "Female", "Male")),width=6)),

           fluidRow(column(selectInput("select_hb_demo",
                                       label = "Health Board",
                                       choices = c("Scotland", boards)),width=6)),
                    
          
             
           # outputs
          fluidRow(
          #uiOutput("page_3_ui"),
          #linebreaks(2),
          h2(htmlOutput("table_title_demo")),
          DT::dataTableOutput("table_demo"),
          linebreaks(1),
          h2(htmlOutput("chart_title_demo_referrals")),

          conditionalPanel(condition= 'input.select_sex_demo == "All"',
              radioButtons("select_sex_chart_1",
                              label="Choose how genders are displayed in chart:",
                              choices=c("show all genders combined" = "All",
                                        "show female/male comparison" = "Male/Female"),
                              inline =TRUE),
            plotlyOutput("plot_demo_referrals_all", height = "600px"),
            h2(htmlOutput("chart_title_demo_ldp_all")),
            radioButtons("select_sex_chart_2",
                         label="Choose how genders are displayed in chart:",
                         choices=c("show all genders combined" = "All",
                                   "show female/male comparison" = "Male/Female"),
                         inline =TRUE),
            plotlyOutput("plot_demo_ldp_all", height = "600px")),
          
          conditionalPanel(condition= 'input.select_sex_demo != "All"',
            plotlyOutput("plot_demo_referrals", height = "600px"),
            h2(htmlOutput("chart_title_demo_ldp")),
            plotlyOutput("plot_demo_ldp", height = "600px"))
          ),
      
          ) # main panel
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
                                          status = "primary",
                                          direction = "vertical", 
                                          justified = T,
                                          size = "lg"), width = 3),
           
           mainPanel(width = 9,
             fluidRow(
              column(selectInput("select_year_add",
                                                 label = "Financial Year of Diagnosis",
                                                 choices = included_years,
                                                 selected = provisional_year),width=6),
             column(selectInput("select_sex_add",
                                label="Gender",
                                choices=c("All", "Female", "Male")),width=6)),
             conditionalPanel(
               condition= 'input.select_data_add != "waiting_times"',
             # inputs
             
             
             fluidRow(column(selectInput("select_hb_add",
                                         label = "Health Board",
                                         choices = c("Scotland", boards)),width=6)),
                      
             
             #uiOutput("page_5_ui"),
             
             fluidRow(
               # outputs
               h2(htmlOutput("table_title_add")),
               DT::dataTableOutput("table_add"),
               linebreaks(1),
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
                 column(
                   radioButtons("select_wait_table",
                                label = "In the table below show Scotland and: ",
                                choices = c("Health Boards", "Integration Joint Boards"),
                                selected = "Health Boards",
                                inline = TRUE
                   ), width = 6),
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
     ), # tabpanel


##############################################.
# PAGE 6: Methodology ----
##############################################.
tabPanel(title = "Methodology",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("diagram-successor"),
         value = "method",
         
         h1("Dementia Post-Diagnostic Support - Methodology"),
         #linebreaks(1),
        
         fluidRow(
         uiOutput("page_6_ui")
         )# fluid Row
) # tabPanel

##############################################.
# PAGE 7: Data Quality ----
##############################################.
# tabPanel(title = "Data Quality",
#          # Look at https://fontawesome.com/search?m=free for icons
#          icon = icon_no_warning_fn("circle-check"),
#          value = "quality",
#          h1("Dementia Post-Diagnostic Support - Data Quality"),
#          #linebreaks(1),
# 
#          fluidRow(
#            uiOutput("page_7_ui")
#          )# fluid Row
# ) # tabPanel



    ) # navbar
  ) # taglist
) # ui fluidpage


# Server

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
    source(file.path(here("dashboard/pages/page_6.R")), local = TRUE)$value
   # source(file.path(here("dashboard/pages/page_7.R")), local = TRUE)$value

}

# Run the application
shinyApp(ui=ui, server=server)

### END OF SCRIPT ###
