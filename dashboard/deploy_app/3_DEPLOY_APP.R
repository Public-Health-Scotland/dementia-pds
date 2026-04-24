################################################################################.
# SOURCE THIS SCRIPT TO DEPLOY APP
# VARIABLES TO UPDATE: "token", "secret", "filepath", "appName", "password_protect_deploy"
# "token" & "secret" ARE LOCATED IN "//conf/dementia/A&I/Outputs/dashboard/passwords.R"
################################################################################.

################################################################################.
# Name of file - 3_DEPLOY_APP.R
# Original Authors - Lucy Binsted
# Original Date - Dec 2025
# Written/run on - RStudio Server
# Version of R - 4.4.2
# Description - Deploy R shiny dashboard. 
################################################################################.

# Manual Variables - TO UPDATE ----
source("//conf/dementia/A&I/Outputs/dashboard/passwords.R")
token <- token # Defined in "//conf/dementia/A&I/Outputs/dashboard/passwords.R"
secret <- secret # Defined in "//conf/dementia/A&I/Outputs/dashboard/passwords.R"
filepath <- "/conf/dementia/A&I/Analysts/Lucy/dementia-pds/dashboard" # Absolute path to the folder the app is being deployed from
appName <- "phs-dementiapds2025-app" # Name of the app
password_protect_deploy <- "off" # "on" for pre-release, "off" for general release
  
# Update data and admin credentials
source(here::here("dashboard", "deploy_app", "1_PREPARE_DATA.R"))
source(here::here("dashboard", "deploy_app", "2_CREATE_ADMIN_CREDENTIALS.R"))

# Overwrite password_protect and AUTH_ENABLED set in 2_CREATE_ADMIN_CREDENTIALS.R
password_protect <- password_protect_deploy
if (password_protect == "on"){
  AUTH_ENABLED <- TRUE
} else if (password_protect == "off"){
  AUTH_ENABLED <- FALSE
} else {
  print("Please set password_protect_deploy to 'on' or 'off'")
}
save(AUTH_ENABLED, file = here::here("dashboard", "data", "AUTH_ENABLED.RData"))

# Ask user to check that end_date, pub_date and last_pub_date have been updated
check_msg1 <- paste0("Please check the data being used is correct.",
                     "\n\nYear: ", fy,
                     "\nQuarter: ", qt,
                     #"\nLast day in reporting period: ", end_date,
                     #"\nDate of publication: ", pub_date,
                     #"\nDate of last publication: ", last_pub_date,
                     "\nVersion: ", ifelse(test_output == FALSE, "Finalised", "Test"))
stop_msg1 <- "App deployment cancelled by user. Update 'end_date', 'pub_date', and 'last_pub_date' in 'dementia-pds/code/publication/00_setup-pub-environment.R'"
continue_msg <- "Continuing app deployment..."
  
# Ask user to check that password protect is on/off
check_msg2 <- paste0("Password protect is ", password_protect, ".")
stop_msg2 <- "App deployment cancelled by user. Update 'password_protect_deploy'"

# Option 1: RStudio pop-up message
# library(rstudioapi)
# if (!rstudioapi::showQuestion(title = "Publication Dates", message = paste0(check_msg1,"\n\nClick OK to continue app deployment."))) {
#   stop(stop_msg1)} else {message(continue_msg)}
# if (!rstudioapi::showQuestion(title = "Password Protect", message = paste0(check_msg2, "\n\nClick OK to continue app deployment."))) {
#   stop(stop_msg2)} else {message(continue_msg)}

# Option 2: Message in console
confirm_deployment <- function() {
  message(paste0(check_msg1, "\n\nType 'ok' to continue app deployment."))
  ans1 <- invisible(readline())
  if (ans1 != "ok") {stop(stop_msg1, call. = FALSE)} else {
    message(paste0(check_msg2, "\n\nType 'ok' to continue app deployment."))
    ans2 <- invisible(readline())
    if (ans2 != "ok") {stop(stop_msg2, call. = FALSE)} else {
      message(continue_msg)}}}
confirm_deployment()

# Connecting to the shiny.io account.
if (!require('remotes')) install.packages('remotes'); library('remotes')
if (!require('rsconnect')) install.packages('rsconnect'); library('rsconnect')
options(rsconnect.packrat = TRUE)
rsconnect::setAccountInfo(name='scotland', token=token, secret=secret)
rsconnect::deployApp(filepath, appName = appName)

# Copy data used in the dashboard to stats drive
files <- list.files(
  here::here("dashboard", "data"),
  full.names = TRUE
)

dest_dir <- file.path(
  "conf", "dementia", "A&I", "Outputs", "dashboard", "data", pub_date
)

dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

file.copy(files, dest_dir, overwrite = TRUE)

### END OF SCRIPT ###