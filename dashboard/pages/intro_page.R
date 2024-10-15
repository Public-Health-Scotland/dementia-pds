####################### Intro Page #######################

output$intro_page_ui <-  renderUI({
if(input$home_select == "about"){
  div(
	     fluidRow(
            h3("About this report"),
	           p("This report has been produced by Public Health Scotland (PHS) and contains analysis of performance against the Scottish Government’s LDP Standard on provision of Dementia Post Diagnostic Support (PDS)."),
	           p(strong(span("The information contained in this report is for management information only and should not be distributed widely.", style="color:red"))),
	           p(strong("Please note that both the Dementia Post Diagnostic Support service provision and data submission to PHS have been affected by the COVID-19 pandemic."))  
	      ), #fluidrow
	    
	   fluidRow(
	     h3("About the LDP Standard"),
	     p("The Local Delivery Plan (LDP) standard is that everyone newly diagnosed with dementia will be offered a minimum of one year’s post-diagnostic support, coordinated by an appropriately trained Link Worker, including the building of a person-centred support plan. Performance is reported in two parts:")
	     
	   )
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

