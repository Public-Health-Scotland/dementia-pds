################################################################################.
# Name of file - 08_dropped_records.R
# Data release - Quarterly Dementia PDS Management Reports
# Original Authors - Lucy Binsted
# Original Date - November 2026
#
# Written/run on - R Posit
# Version of R - 4.4.2
#
# Description - Check whether records have been dropped from one submission to the next.
################################################################################.

################################################################################.
### 1 - Read data ----
################################################################################.

# Source setup
rm(list = ls())
source(here::here("code", "00_setup-environment.R"))

years <- seq(2018, as.numeric(substr(current_fy, 1, 4)))
quarters <- seq(1, 4)
all_data <- list()

# Read in clean data for all quarters
for (year in years){
  for (qt in quarters){
    fy <- as.character(year)
    id <- paste0(fy, "_", qt)
    
    path <- tryCatch(
      get_mi_data_path(
        type = "clean_data",
        ext = "rds",
        fy = fy,
        qt = qt,
        test_output = FALSE),
      
      error = function(e) {
        tryCatch(
          get_mi_data_path(
            type = "clean_data",
            ext = "rds",
            fy = fy,
            qt = qt,
            test_output = TRUE),
          
          error = function(e) {
            message(
              "No clean_data file found for FY ", fy,
              ", quarter ", qt,
              ". Continuing...")
            NULL})})
    
    if (!is_null(path)){
      all_data[[id]] <- read_rds(path) %>% 
        select(chi_number, health_board, ijb) %>%
        mutate(fy = fy, qt = qt, id = id) %>%
        mutate(
          health_board = str_sub(health_board, 3, -1),
          ijb = if_else(is.na(ijb), "Unknown", str_sub(ijb, 11, -1)))
    }
  }
}

data <- bind_rows(all_data)

################################################################################.
### 2 - Prepare data ----
################################################################################.

# List unique CHI numbers in each IAA/Health Board and quarter
chi_by_quarter <- data %>%
  filter(ijb != "Unknown") %>%
  mutate(ijb = ifelse(ijb == "Clacks and Stirling", "Clackmannanshire and Stirling", ijb)) %>%
  distinct(health_board, ijb, id, chi_number) %>%
  summarise(
    chis = list(chi_number),
    n_chis = n(),
    .by = c(health_board, ijb, id)
  )

# Look at differences in CHI numbers between all pairs of quarters
quarter_comparisons <- chi_by_quarter %>%
  inner_join(
    chi_by_quarter,
    by = c("health_board", "ijb"),
    suffix = c("_1", "_2"),
    relationship = "many-to-many") %>%
  mutate(
    n_shared = map2_int(chis_1, chis_2, ~ length(intersect(.x, .y))),
    # Present in quarter 2 but not quarter 1
    n_added = map2_int(chis_1, chis_2, ~ length(setdiff(.y, .x))),
    pct_added = round(n_added / n_chis_1 * 100, 1),
    # Present in quarter 1 but not quarter 2
    n_removed = map2_int(chis_1, chis_2, ~ length(setdiff(.x, .y))),
    pct_removed = round(n_removed / n_chis_1 * 100, 1)) %>%
  select(
    health_board, ijb, previous_quarter = id_1, quarter = id_2,
    n_previous_quarter = n_chis_1, n_quarter = n_chis_2, n_shared, n_added,
    n_removed, pct_added, pct_removed) %>%
  arrange(health_board, ijb, quarter, previous_quarter)

# Keep only rows that are consecutive quarters and set factor levels
quarter_index <- function(x) {
  year <- as.integer(sub("_.*", "", x))
  quarter <- as.integer(sub(".*_", "", x))
  year * 4 + quarter
}

quarter_comparisons_filtered <- quarter_comparisons %>%
  filter((quarter_index(quarter) - quarter_index(previous_quarter) == 1)) %>%
  arrange(health_board, ijb) %>%
  mutate(ijb = factor(ijb, levels = unique(ijb))) %>%
  # Add a number for each ijb within each health board
  group_by(health_board) %>%
  mutate(num = dense_rank(ijb)) %>%
  ungroup()

################################################################################.
### 3 - Plots ----
################################################################################.

# Plot 1: Bubble plot of % removed
plot1 <- ggplot(
  quarter_comparisons_filtered, aes(
    x = quarter, y = ijb, text = paste0(
      health_board,
      "<br>IAA: ", ijb,
      "<br>Quarter: ", quarter,
      "<br>Total records: ", scales::comma(n_quarter),
      "<br>Records added: ", scales::comma(n_added),
      "<br>Records removed: ", scales::comma(n_removed),
      " (", pct_removed, "%)"))) +
  geom_point(aes(size = pct_removed, colour = health_board)) +
  scale_size_continuous(name = "Percentage of records removed", range = c(0, 10)) +
  scale_colour_manual(values = c(unname(phsstyles::phs_colors())[1:7], unname(phsstyles::phs_colors())[1:7])) + 
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(x = "Quarter", y = "Integration Authority Area")

ggplotly(plot1, tooltip = "text")

# Plot 2: Line plot showing total records facetted by health board
breaks_df <- tibble(q = unique(quarter_comparisons_filtered$quarter)) %>%
  arrange(q) %>%
  mutate(year = sub("_.*", "", q)) %>%
  group_by(year) %>%
  slice(1) %>%
  ungroup()

plot2 <- ggplot(
  quarter_comparisons_filtered, aes(
    x = quarter, y = n_quarter, text = paste0(
      "IAA: ", ijb,
      "<br>Quarter: ", quarter,
      "<br>Total records: ", scales::comma(n_quarter),
      "<br>Records added: ", scales::comma(n_added),
      "<br>Records removed: ", scales::comma(n_removed),
      " (", pct_removed, "%)"
    ))) +
  geom_line(aes(colour = as.factor(num), group = ijb)) + 
  scale_size_continuous(range = c(0, 5)) +
  scale_colour_manual(values = unname(phsstyles::phs_colors())[1:6]) + 
  theme_light()+
  scale_x_discrete(breaks = breaks_df$q, labels = breaks_df$year) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, size = 7, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(
      colour = "black",
      size = 8,
      face = "plain")) +
  labs(x = "Time Period", y = "Total Records") +
  facet_wrap(.~health_board, scales = "free_y", ncol = 4)

ggplotly(plot2, tooltip = "text")

################################ END OF SCRIPT #################################.
