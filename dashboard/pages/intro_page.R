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
                         tags$ul(tags$li("Performance against the Local Delivery Plan (LDP) Standard"),
                                tags$li("Total number of referrals to PDS and rates per 10,000 population"),
                                tags$li("Demographics (Gender, Age and Deprivation)"),
                                tags$li("Pathways from diagnosis of dementia to first contact by a PDS practitioner"))
    
                                )
                     ), #fluidrow
                     
                     fluidRow(
                       ###about the standard----
                       h3(strong("About the LDP Standard")),
                       p("The Scottish Government published their third national dementia strategy in 2017. This included the commitment to extend and embed dementia post-diagnostic support. In order to effectively monitor the delivery of post-diagnosis support a national local delivery plan (LDP) standard was introduced for ",
                         em("all people newly diagnosed with dementia to receive a minimum of one year’s post-diagnostic support, coordinated by an appropriately trained Link Worker, including the building of a person-centred support plan. ")),
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
                                 p("Further information regarding the Dementia PDS dataset and submission process can be found on the ", 
                                   a("Dementia PDS pages.", href ="https://publichealthscotland.scot/services/data-management/data-management-in-primary-social-and-community-care/dementia-post-diagnostic-support-pds/", target="_blank"))
                                 ),
                         tags$li(p("NHS Board level estimates of the number of people newly diagnosed with dementia (incidence) used in Tab 6 for the years 2016/17 to 2020/21, are from the paper ",
                                   a('Estimated and Projected Diagnosis Rates for Dementia in Scotland: 2014-2020', href = 'https://www.gov.scot/publications/estimated-projected-diagnosis-rates-dementia-scotland-2014-2020/', target="_blank"),
                                   " published by the Scottish Government in December 2016. Note that these estimations are subject to the limitations detailed within the paper."),
                                p("For financial years 2021/22 and 2022/23 the rates referenced in 'Estimated and Projected Diagnosis Rates for Dementia in Scotland 2014-2020' were used to create national, age ",
                                 "specific rates of dementia incidence per 1,000 population which were then applied to the 2021 and 2022 population estimates to obtain the incidence estimates. Population estimates taken from ",
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
                        tags$li(p("NHS Tayside experienced challenges in capturing and reporting on electronic system data within Perth & Kinross over the period 2018/19 to 2019. ",
                                  "Despite local work undertaken to uncover the missing data, it has not been possible to fill this gap. Subsequent measures put in place have successfully ",
                                  "allowed NHS Tayside to improve on data reporting from 2019 onwards.")
                                ),
                        tags$li(p("NHS Shetland did not have a PDS worker in post from 2022/23 Q1 through 2023/24 Q3. As a result not all people referred to PDS could be allocated or contacted ",
                                  "by a PDS worker within 12 months of diagnosis, hence they have been recorded as not having met the standard. A PDS worker was assigned in 2023/24 Q4 and the ",
                                  "PDS service in Shetland has resumed. This will also affect the pathway waiting times for NHS Shetland /Shetland Islands for 2022/23.")
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
                     style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; margin-left: 1px; height:-webkit-fill-available; background-color: white"
                    ) #column
                  
    ), #cond panel about
##Using the dashboard ----
    conditionalPanel(condition = 'input.home_select == "use"',
                     fluidRow(
                       h3("How to use this dashboard"),
                       p("This dashboard..."),
                       p(strong("some bold text"))  
                     ) #fluidrow
    ), #cond panel using the dashboard
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
                       style = "position:fixed; width: -webkit-fill-available; overflow-y: overlay; margin-left: 1px; height:-webkit-fill-available; background-color: white"
                       )
                   ) #fluidrow
              ), #cond panel accessability

) # div
      
  
}) # renderUI

