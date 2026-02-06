################################################################################.
# SOURCE THIS SCRIPT TO UPDATE ADMIN CREDENTIALS FOR FOR DASHBOARD
# VARIABLES TO UPDATE: "user", "password", "password_protect"
# "user" & "password" ARE LOCATED IN "//conf/dementia/A&I/Outputs/dashboard/passwords.R"
################################################################################.

################################################################################.
# Name of file - 2_CREATE_ADMIN_CREDENTIALS.R
# Original Authors - Lucy Binsted
# Original Date - Dec 2025
# Written/run on - RStudio Server
# Version of R - 4.4.2
# Description - Create credentials for password protect for R shiny dashboard. 
################################################################################.

# Manual Variables - TO UPDATE ----
source("//conf/dementia/A&I/Outputs/dashboard/passwords.R")
user <- user # Defined in "//conf/dementia/A&I/Outputs/dashboard/passwords.R"
password <- password # Defined in "//conf/dementia/A&I/Outputs/dashboard/passwords.R"
password_protect <- "off" # "on" or "off"

# Create data folder if it does not exist
if (!dir.exists(here::here("dashboard/data"))) {dir.create(here::here("dashboard/data"))}

# Save password protect on/off
if (password_protect == "on"){
  AUTH_ENABLED <- TRUE
} else if (password_protect == "off"){
  AUTH_ENABLED <- FALSE
} else {
  print("Please set password_protect to 'on' or 'off'")
}

save(AUTH_ENABLED, file = here::here("dashboard", "data", "AUTH_ENABLED.RData"))

# Save credentials
credentials_df <- data.frame(
  user = c(user),
  password = c(password),
  stringsAsFactors = FALSE)

saveRDS(credentials_df, here::here("dashboard", "data", "credentials.rds"))

### END OF SCRIPT ###