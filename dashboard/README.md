# phs-dementiapds2025-app

## Scripts run outside of server
* To update data: deploy_app/1_prepare_data.R
* To update credentials: deploy_app/2_create_admin_credentials.R
* To deploy app: deploy_app/3_deploy_app.R

## Instructions for use
* Run the app by opening app.R and clicking 'Run' in the top right hand corner
* `setup.R` contains required packages and is where data and variables are read in
* `data` contains data to be read in
* `www` contains the app stylesheet and PHS icon images
* `pages` contains an R script for each tab, which are linked back to the ui in app.R
* `functions` contains R scripts with functions for the app

## PHS shiny app examples

* [COVID-19 dashboard](https://github.com/Public-Health-Scotland/COVID-19-Publication-Dashboard)
* [COVID-19 wider impacts dashboard](https://github.com/Public-Health-Scotland/covid-wider-impacts/tree/master/shiny_app)
* [ScotPHO profiles](https://github.com/Public-Health-Scotland/scotpho-profiles-tool)
