####################### Intro Page #######################
#UI----
output$intro_page_ui <-  renderUI({
  
  div(
##About----
    conditionalPanel(condition = 'input.home_select == "about"',
                    column(
                     fluidRow(
                       ###about the dashboard----
                       h3(strong("About this Dashboard")),
                       p("This is the Public Health Scotland Dementia Post-Diagnostic Support (PDS) dashboard. 
                         This dashboard is supplementary to the full publication report which can be found ",
                         a("here.", href = paste0("https://publichealthscotland.scot/publications/dementia-post-diagnostic-support/dementia-post-diagnostic-support-local-delivery-plan-standard-figures-to-",
                                         str_replace(latest_fy, "/","")), target="_blank")),
                       p("This dashboard contains pages providing information on the following areas:",
                         tags$ul(tags$li("Total number of referrals to PDS and rates per 10,000 population"),
                                 tags$li("Pathways from diagnosis of dementia to first contact by a PDS practitioner"),
                                tags$li("Performance against the Local Delivery Plan (LDP) Standard"),
                                tags$li("Demographics (Gender, Age and Deprivation)"),
                                tags$li("Information on the methodology used in the report"),
                                tags$li("Downloading data from the report")
                                )
                                )
                     ), #fluidrow
                     
                     fluidRow(
                       ###about the standard----
                       h3(strong("About the LDP Standard")),
                       p("The Scottish Government published their third national dementia strategy in 2017. This included the commitment to extend and embed dementia post-diagnostic support. In order to effectively monitor the delivery of post-diagnosis support a national local delivery plan (LDP) standard was introduced for ",
                         ("all people newly diagnosed with dementia to receive a minimum of one year’s post-diagnostic support, coordinated by an appropriately trained Link Worker, including the building of a person-centred support plan. ")),
                         p("The LDP Standard is reported in two parts:"),
                       box(width = 12,
                           background = "blue",
                           tags$ol(tags$li(p("The percentage of people estimated to be newly diagnosed with dementia who were referred for post-diagnostic support.")),
                                   #  linebreaks(1),
                                     tags$li(p("The percentage of people referred who received a minimum of one year’s worth of post-diagnostic support coordinated by a Link Worker, including the building of a person-centred support plan."))),
                       ), #box
                     ), #fluidrow
                     linebreaks(1),
                        fluidRow(
                            p("For more detail on how the above is calculated, please see the",
                              ####link to methodology page----
                              a(href = "#",
                                "Methodology",
                                onclick = "Shiny.setInputValue('method_link', Math.random()); return false;"),
                              " page."),
                            p("In May 2023 the Scottish Government published their new dementia strategy for Scotland.  This strategy builds on previous strategies and continues to recognise the importance of dementia post-diagnostic support and the outcomes for people living with dementia. ",
                                a("New dementia strategy for Scotland: Everyone's Story - (www.gov.scot)", href = "https://www.gov.scot/publications/new-dementia-strategy-scotland-everyones-story/", target="_blank"))
                       ), # fluid Row
                     
                     fluidRow(
                       ###notes----
                       h3(strong("Notes")),
                       tags$ol(
                         tags$li(p(paste0("NHS Boards provide quarterly data submissions to Public Health Scotland (PHS) on individuals ",
                                          "diagnosed and referred for post-diagnostic support within their local areas and this dataset forms the basis ",
                                          "of the LDP standard calculation. These statistics are derived from quarterly post-diagnostic support data ",
                                          "submissions by NHS Boards as at ", format(end_date, "%d %B %Y"),".")),
                                 p("Further information regarding the PHS Dementia PDS dataset and submission process can be found on the ", 
                                   a("PHS Dementia PDS pages.", href ="https://publichealthscotland.scot/services/data-management/data-management-in-primary-social-and-community-care/dementia-post-diagnostic-support-pds/", target="_blank"))
                                 ),
                         tags$li(p("NHS Board level estimates of the number of people newly diagnosed with dementia (incidence) used in Tab 6 for the years 2016/17 to 2020/21, are from the paper ",
                                   a('Estimated and Projected Diagnosis Rates for Dementia in Scotland: 2014-2020', href = 'https://www.gov.scot/publications/estimated-projected-diagnosis-rates-dementia-scotland-2014-2020/', target="_blank"),
                                   " published by the Scottish Government in December 2016. Note that these estimations are subject to the limitations detailed within the paper."),
                                p("For financial years 2021/22 and 2022/23 the rates referenced in 'Estimated and Projected Diagnosis Rates for Dementia in Scotland 2014-2020' were used to create national, age ",
                                 "specific rates of dementia incidence per 1,000 population which were then applied to the Mid-2021 and Mid-2022 population estimates to obtain the incidence estimates. Population estimates taken from ",
                                  "National Records of Scotland (NRS) Mid-2021 and Mid-2022 Population Estimates at NHS Board Level."),
                                p("NRS Mid-year population estimates can be found at: ",
                                  a("Population, migration and households - National Records of Scotland (NRS)", href = "https://www.nrscotland.gov.uk/statistics-and-data/population-migration-and-households/#", target="_blank"))
                                ),
                        tags$li(p(paste0("Figures for ", provisional_year, " are provisional subject to all service users completing their support. Service users for whom it is not yet known if they have met the standard are excluded from the percentage figures."))
                               ),
                        tags$li(p("Information presented in this publication is not comparable to information published for time periods prior to 2016/17.")
                                ),
                        tags$li(p("Aberdeen City Integration Authority Area ceased its contract with Alzheimer Scotland during 2019 and introduced an in-house Dementia Post-Diagnostic Support (PDS) service. ",
                                  "This transition resulted in some PDS cases being terminated by Alzheimer Scotland earlier than 12 months and therefore not meeting the LDP standard. As part of the transition, ",
                                  "individuals were contacted to ask if they still wanted to receive PDS which was then provided by the in-house service, if required. This should be taken into account when ",
                                  "interpreting figures for Aberdeen City in 2018/19. Although figures for 2019/20 and 2020/21 include the total number of people referred to PDS across all Integration Authority Areas ",
                                  "in NHS Grampian, data for Aberdeen City is not included in the measurement figures for the LDP standard due to data quality issues and missing dates meaning it is not possible ",
                                  "to accurately measure these referrals against the LDP standard.")
                                ),
                        tags$li(p("NHS Tayside experienced challenges in capturing and reporting on electronic system data within Perth & Kinross over the period 2018 to 2019. ",
                                  "Despite local work undertaken to uncover the missing data, it has not been possible to fill this gap. This should be taken into account when interpreting figures for NHS Tayside and ",
                                  "Perth & Kinross for financial years 2018/19 and 2019/20. Subsequent measures put in place have successfully ",
                                  "allowed NHS Tayside to improve on data reporting from 2019 onwards.")
                                ),
                        tags$li(p("NHS Shetland / Shetland Islands did not have a PDS worker in post from 2022/23 Q1 through 2023/24 Q3. As a result not all people referred to PDS could be allocated or contacted ",
                                  "by a PDS worker within 12 months of diagnosis, hence they have been recorded as not having met the standard. A PDS worker was assigned in 2023/24 Q4 and the ",
                                  "PDS service in Shetland has resumed. This will also affect the pathway waiting times for NHS Shetland / Shetland Islands for 2022/23.")
                                ),
                        tags$li(p("The population estimates used in this report to calculate rates per 10,000 population are from the National Records of Scotland (NRS)
                                  Mid-year population estimates. As figures in this report are by financial year, the estimate for the calendar year
                                  with the majority of months in the selected financial year is used. For example, analysis for financial year 2018/19 
                                  uses estimates for the calendar year 2018."),
                                p(paste0("The rate per 10,000 population is calculated using the number of referrals with a dementia diagnosis date in the given
                                  financial year and the estimated population of the same year for each geographical area.
                                  The estimates used throughout the report are for the population that
                                  is 65 years and older, as ", perc_65_plus, "% of referrals to PDS are in this age group.")),
                                p("NRS Mid-year population estimates can be found at: ",
                                  a("Population, migration and households - National Records of Scotland (NRS)",
                                    href = "https://www.nrscotland.gov.uk/statistics-and-data/population-migration-and-households/#", target="_blank")),
                                ),
                        tags$li(p("The COVID-19 pandemic and the infection control measures put in place are likely to have had an impact on the number of people being diagnosed and referred, particularly in 2020/21.")
                                ),
                        tags$li(p("Some areas have reported difficulties in the COVID-19 pandemic affecting their ability and capacity to update their data systems which are required to provide the dementia PDS dataset, ",
                                  "however it is not anticipated that this has a large impact on the data presented in this report.")
                                )
                         )#tags$ol
                      ),#fluidRow
                     linebreaks(1),
                     width = 12,
                     #fix panel so sidebar and navigation bar do not scroll with content
                     style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; margin-left: 1px; height:-webkit-fill-available; background-color: white"
                    ) #column
                  
    ), #cond panel about
##Using the dashboard ----
    conditionalPanel(condition = 'input.home_select == "use"',
                     fluidRow(column(
                       h3(strong("Using the Dashboard")),
                       h4(strong("Interacting with the dashboard")),
                       p("The dashboard has seven pages across the top which can be selected: Home, Referrals & Rates, Pathways, LDP Standard, 
                         Demographics, Methodology, and Data Download. Most pages have buttons on the left hand side of the screen and/or 
                         tabs towards the top of the screen to navigate to areas within each page."),
                       p("Drop-down menus and radio buttons are located on the left hand side of the screen of most pages in the dashboard. Where these filters are available,
                         you can make choices on the data presented and the dashboard will update in response to the selection.
                         The charts are interactive and hovering the mouse over a specific data point will bring up more information."),
                       h4(strong("Downloading data")),
                       p("Data for each table can be downloaded as a .csv file by clicking the 'Download table data' button located above the table.
                         Additionaly, the Data Download page allows users to select the data from the report they wish to explore by using the drop-down menus and 
                         checkboxes provided, and then clicking the ‘Download data’ button to download a .csv file of the selected data."),
                       linebreaks(1),
                       width = 12,
                       #fix panel so sidebar and navigation bar do not scroll with content
                       style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; margin-left: 1px; height:-webkit-fill-available; background-color: white"
                     )
                     ) #fluidrow
    ), #cond panel using the dashboard
#Glossary ----
conditionalPanel(condition = 'input.home_select == "glossary"',
                 fluidRow(column(
                   h3(strong("Glossary")),
                   h4(strong("General Information")),
                   p(strong("Post-Diagnostic Support (PDS):"),"Support commissioned by the Scottish Government that everyone newly diagnosed with dementia is entitled
                        to a minimum of one year’s post-diagnostic support. Further information on post-diagnostic support can be found on the ",
                      a("Alzheimer Scotland", href ="https://www.alzscot.org/about-us/care/post-diagnostic-support/", target="_blank"),
                      " website."),
                   p(strong("Referral:"), "When a person receives a new diagnosis of dementia, a referral is made to a service that provides post-diagnostic support
                   and allocates a named person to work alongside the individual and those close to them."),
                   p(strong("Health Board:"), "This is the Health Board of the PDS Practitioner or Team providing care and support to the service user."),
                   p(strong("Integration Authority Area (IAA):"), "This is the IAA of the PDS Practitioner or Team providing care and support to the service user."),
                   p(strong("Provisional Years:"), "As it can take up to two years for a person to receive 12 months of PDS, the data reported by financial year
                   is deemed to be provisional for two years and PHS do not finalise the data until then. Therefore, the figures shown for",
                   provisional_year, "and", extra_referrals_year, "are currently provisional and subject to change in future versions of this report."),
                   h4(strong("Pathways")),
                   p(strong("Dementia Diagnosis Date:"), "The date on which the service user receives a confirmed diagnosis of dementia. This diagnosis must be confirmed
                     by a doctor or clinical practitioner with sufficient training and experience in the diagnosis of dementia."),
                   p(strong("First Contact:"), "The date on which an appropriate face to face direct contact took place with the service user by the PDS Practitioner 
                     or PDS Team with the knowledge and skills to introduce each model of care. Direct contact can be done in person or by video link and is not
                     restricted to both parties being in the same room."),
                   p(strong("Average (Median) Days:"), "The midpoint of the number of days between the given dates for each referral, such that there is an equal number
                     of values falling above and below it. The median number of days for each time period will be such that half of the people will
                     have experienced a wait time this long or longer."),
                   linebreaks(1),
                   width = 12,
                   #fix panel so sidebar and navigation bar do not scroll with content
                   style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; margin-left: 1px; height:-webkit-fill-available; background-color: white"
                 )
                 ) #fluidrow
), #cond panel glossary
#Accessibility-----
    conditionalPanel(condition = 'input.home_select == "access"',
                     fluidRow(column(
                       h3(strong("Accessibility")),
                       p("This website is run by ",
                         a("Public Health Scotland,", href ="https://www.publichealthscotland.scot/", target="_blank"),
                         "Scotland's national organisation for public health. Formed on 1 April 2020,
                         Public Health Scotland is currently reviewing its web estate. Public Health Scotland
                         is committed to making its website accessible, in accordance with the Public Sector Bodies (Websites and Mobile Applications)
                         (No. 2) Accessibility Regulations 2018. This accessibility statement applies to the dashboard
                         that accompanies the HSMR quarterly publication."),
                       p(a("AbilityNet", href = "https://mcmw.abilitynet.org.uk/", target="_blank"),
                         "has advice on making your device easier to use if you have a disability."),
                       h4(strong("How accessible this website is")),
                       p("This site has not yet been evaluated against WCAG 2.1 level AA."),
                       h4(strong("Reporting any accessibility problems with this website")),
                       p("If you wish to contact us about any accessibility issues you encounter on this site, please email",
                         a("phs.dementiapds@phs.scot", href="mailto:phs.dementiapds@phs.scot")),
                       h4(strong("Enforcement procedure")),
                       p("The Equality and Human Rights Commission (EHRC) is responsible for enforcing the
                       Public Sector Bodies (Websites and Mobile Applications) (No. 2) Accessibility Regulations 2018
                       (the ‘accessibility regulations’). If you’re not happy with how we respond to your complaint, contact the ",
                       a("Equality Advisory and Support Service (EASS).", href = "https://www.equalityadvisoryservice.com/", target="_blank")),
                      linebreaks(1),
                        width = 12,
                      #fix panel so sidebar and navigation bar do not scroll with content
                      style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; margin-left: 1px; height:-webkit-fill-available; background-color: white"
                       )
                   ) #fluidrow
              ), #cond panel accessibility

#Contact-----
conditionalPanel(condition = 'input.home_select == "contact"',
                 fluidRow(column(
                   h3(strong("Contact Us")),
                   p("If you have any questions or feedback regarding this dashboard, or have any questions relating to the data, please contact us at ",
                     a("phs.dementiapds@phs.scot", href="mailto:phs.dementiapds@phs.scot")),
                   linebreaks(1),
                   width = 12,
                   #fix panel so sidebar and navigation bar do not scroll with content
                   style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; margin-left: 1px; height:-webkit-fill-available; background-color: white"
                 )
                 ) #fluidrow
), #cond panel contact


) # div
      
  
}) # renderUI


#SERVER----

over_65_and_total <- left_join(

scot_65_plus_total <- data_age %>% filter(type != "59 and Under", type != "60 to 64") %>% 
  group_by(geog = "Scotland") %>% summarise(over_65_referrals = sum(referrals)),


scot_overall_total <- annual_table_data %>% group_by(geog = "Scotland") %>%  filter(ijb == "Scotland", ldp == "total") %>% summarise(total_referrals = sum(referrals))

)

perc_65_plus<-round(over_65_and_total[2]/over_65_and_total[3]*100,1)
