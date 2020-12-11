#########################################################################
# Name of file - 01_create-figures.R
# Data release - Annual Dementia PDS Publication
# Original Authors - Alice Byers
# Original Date - December 2020
#
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Create all figures required for summary and report.
#########################################################################


### 1 - Load environment file and themes ----

source(here::here("code", "00_setup-environment.R"))

source(here("functions", "ggplot_themes.R"))


### 2 - Read in data ----

pds <- 
  read_rds(
    here("data", 
         glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}/Q{qt}"),
         glue("{fy}-{qt}_final-data.rds"))
  ) %>%
  
  # Select only FY to be incuded in pub
  filter(fy %in% fy_in_pub) %>%
  
  # Remove codes from board and IJB
  mutate(health_board = str_sub(health_board, 3, -1),
         ijb          = if_else(is.na(ijb),
                                "Unknown",
                                str_sub(ijb, 11, -1)))

# Load expected diagnoses reference file
exp <- read_csv(here("reference-files", "expected-diagnoses.csv")) %>%
  filter(health_board == "Scotland" & fy %in% fy_in_pub)


### 3 - Summary chart - age distribution ----

age_dist_data <-
  pds %>%
  filter(age_grp != "Unknown") %>%
  group_by(fy, age_grp) %>%
  summarise(ref = sum(referrals), .groups = "drop") %>%
  group_by(fy) %>%
  mutate(perc = ref / sum(ref) * 100) %>%
  ungroup() %>%
  filter(fy == last(fy_in_pub))

age_dist <-
  age_dist_data %>%
  ggplot(aes(x = age_grp, y = perc, fill = 1)) +
  geom_bar(stat = "identity", width = 0.5, fill = "#3f3685") +
  geom_text(aes(label = paste0(format(round_half_up(perc, 1), nsmall = 1), "%")), 
            vjust = -0.5,
            size = 3) +
  theme_dementia() +
  scale_y_continuous(
    limits = 
      c(0, ceiling(max(age_dist_data$perc) / 10) * 10),
    breaks = 
      seq(0, ceiling(max(age_dist_data$perc) / 10) * 10, 
          by = 5),
    labels = paste0(seq(0, ceiling(max(age_dist_data$perc) / 10) * 10, 
                        by = 5), "%"),
    expand = c(0, 0)) +
  scale_x_discrete(labels = str_wrap(age_dist_data$age_grp, width = 8)) +
  xlab("Age Group") +
  ylab(str_wrap("Percentage of total referrals", width = 10))

# Save chart to output folder
ggsave(
  here("publication", "output", pub_date, "figures", 
       paste0(pub_date, "_summary-chart.png")),
  plot = age_dist,
  width = 6.8,
  height = 3.5,
  device = "png",
  dpi = 600
)


### END OF SCRIPT ###