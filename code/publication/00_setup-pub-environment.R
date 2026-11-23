################################################################################.
### Manual Variables - TO UPDATE FOR PUBLICATION ----
################################################################################.

# UPDATE - Date of the current publication (ddmmyyyy)
pub_date <- lubridate::dmy(21072026)

# UPDATE - Date of the last publication (ddmmyyyy)
last_pub_date <- lubridate::dmy(16122025)

# UPDATE - Financial Year (YYYY) and Quarter (Q) of the submission data 
# being used for the current publication
fy <- 2025
qt <- 3

# Source setup script
source(here::here("code/00_setup-environment-code.R"))
