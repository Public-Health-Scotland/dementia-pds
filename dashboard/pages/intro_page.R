####################### Intro Page #######################

output$intro_page_ui <-  renderUI({
if(input$home_select == "about"){
  div(
    fluidRow(
      h3("About This Dashboard"),
      p("This dashboard has been produced by Public Health Scotland (PHS) and contains analysis of performance against the Scottish Government’s Local Delivery Plan (LDP) Standard on provision of Dementia Post Diagnostic Support (PDS)."),
      p(strong(span("This dashboard is in development and should not be distributed widely.", style="color:red"))),
      p(strong("Please note that both the Dementia Post Diagnostic Support service provision and data submission to PHS have been affected by the COVID-19 pandemic.")),  
      p("The reports contained in this dashboard reflect the ", a("Dementia PDS dataset", href="https://publichealthscotland.scot/media/20921/2023-12-14-dementia-pds-definitions-and-recording-guidance-v14.pdf"),
        " effective 01 April 2019 and contain data for individuals diagnosed with dementia between ",
        strong("01 April 2016"), " and ", strong(paste0("31 March ", substr(last(included_years),1,2), substr(last(included_years),6,7))),  " who were referred for post diagnostic support."),
      # p("Data are submitted to PHS by health boards on a quarterly basis. Each health board provides updated information for all individuals referred for post diagnostic support with a diagnosis date from 01 April 2016 onwards. Therefore, data for diagnoses from this date onwards are refreshed in each management report and are based on the most recently submitted data."),
      # p("Information is shown at Scotland, Health Board and Integration Joint Board level using the drop down menus at the top of the page."),
      p("To ensure this dashboard is as useful as possible, we would welcome any comments or feedback you may have via the PHS Dementia PDS team mailbox at: ",
        a("phs.dementiapds@phs.scot", href="mailto:phs.dementiapds@phs.scot"))
    ), #fluidrow
	    
	   fluidRow(
	     h3("About the LDP Standard"),
	     p("The Local Delivery Plan (LDP) standard is that ",
	       em("everyone newly diagnosed with dementia will be offered a minimum of one year’s post-diagnostic support, coordinated by an appropriately trained Link Worker, including the building of a person-centred support plan. "),
	       "Performance is reported in two parts:"),
	     box(width = 12,
	         background = "blue",
	         p(tags$ol(tags$li("The percentage of people estimated to be newly diagnosed with dementia who were referred for post-diagnostic support."),
	                   linebreaks(1),
	                   tags$li("The percentage of people referred who received a minimum of one year’s worth of post-diagnostic support coordinated by a Link Worker, including the building of a person-centred support plan."))),
	         	         ), #box
	     	   ), #fluidrow
    linebreaks(1),
    fluidRow(p("For more detail on how the above is calculated, please see the", a("methodology", href = "?a=b"), " page."),
             p("Further information regarding the Dementia PDS dataset and submission process can be found on the ", 
               a("Dementia PDS pages.", href ="https://publichealthscotland.scot/services/data-management/data-management-in-primary-social-and-community-care/dementia-post-diagnostic-support-pds/")
                 )
             ), # fluid Row
   
   )} 
  
  else if(input$home_select == "use"){
    div(
      fluidRow(
        h3("How to use this dashboard"),
        p("This dashboard..."),
        p(strong(span("some red text", style="color:red"))),
        p(strong("some bold text"))  
      ), #fluidrow
      
    )}
  
}) # renderUI

