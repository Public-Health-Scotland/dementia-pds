#########################################################################
# Name of file - 02_create-figures.R
# Data release - Annual Dementia PDS Publication
# Original Authors - Alice Byers
# Original Date - March 2021
# Updated by - Jennifer Thom
# Date - November 2023
#
# Written/run on - R Posit
# Version of R - 4.1.2
#
# Description - Create all figures required for summary and report.
#########################################################################


### 1 - Load environment file and themes ----

source(here::here("code", "publication", "00_setup-pub-environment.R"))

source(here("functions", "ggplot_themes.R"))


### 2 - Read in data ----

basefile <- read_rds(get_pub_data_path(test_output = test_output))


# Load expected diagnoses reference file
exp <- read_csv(get_exp_diagnoses_path()) %>% 
  filter(fy == max(fy_in_pub)) %>%
  select(health_board = health_board_label, fy, diagnoses)


# Aberdeen city lookup
# For populating charts with 2021 aberdeen city data. 
# Comment appropriate sections when this is not needed. e.g publishing 21/22 data onwards
# retain code for info 
# ac_lookup_hb <- read_xlsx(get_ac_lookup_path(), sheet = 'health_board') %>% 
#   filter(fy == "2020/21")
# 
# ac_lookup_simd <- read_xlsx(get_ac_lookup_path(), sheet = "simd") %>% 
#   filter(fy == "2020/21")
# 
# ac_lookup_age_group <- read_xlsx(get_ac_lookup_path(), sheet = "age_group") %>% 
#   filter(fy == "2020/21")

### 3 - Create figures ----

# Chart 1 - Incidence by Health Board

c1_data <-
  basefile %>%
  filter(fy == max(fy_in_pub)) %>%
  group_by(health_board) %>%
  summarise(across(c(referrals), sum),
            .groups = "drop") %>%
  adorn_totals(name = "Scotland") %>%
  left_join(exp, by = "health_board") %>%
  # Update the number of referrals using aberdeen city lookup for 2021 publication
  # Comment this code as not needed for 21/22 published data onwards
  #
  # left_join(ac_lookup_hb, by = c("health_board", "fy")) %>% 
  # mutate(referrals = if_else(health_board == "NHS Grampian", pub_referrals, referrals), 
  #        referrals = if_else(health_board == "Scotland", pub_referrals, referrals)) %>% 
  # select(-pub_referrals) %>% 
  mutate(
    perc = case_when(
      referrals == 0 ~ 0,
      TRUE ~ referrals / diagnoses * 100),
    perc_formatted = paste0(format(round_half_up(perc, 1), nsmall = 1), "%")
  )

c1 <-
  c1_data %>%
  mutate(health_board = fct_reorder(as_factor(health_board), perc)) %>%
  ggplot(aes(x = perc, y = health_board, fill = health_board == "Scotland")) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(x = perc, y = health_board, label = perc_formatted),
            nudge_x = -5, colour = "white", size = 3) +
  theme_dementia_pub() +
  scale_fill_manual(values = c("#3F3685", "#9B4393")) +
  theme(panel.grid.major.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  xlab(
    str_wrap(
      paste0("Percentage of Estimated Number of People Newly Diagnosed ",
             "with Dementia Referred to PDS"),
      width = 45)
  ) +
  ylab("")

ggsave(get_pub_figures_path(type = "c1", test_output = test_output),
  plot = c1,
  height = 6,
  width = 7,
  dpi = 600,
  device = "png"
)


# Chart 2 - One year PDS by Health Board

c2_data <-
  basefile %>%
  filter(fy == max(fy_in_pub)) %>%
  group_by(health_board) %>%
  summarise(across(referrals:denominator, sum),
            .groups = "drop") %>%
  adorn_totals(name = "Scotland") %>%
  mutate(
    perc = case_when(
      referrals == 0 ~ 0,
      TRUE ~ numerator / denominator * 100),
    perc_formatted = paste0(format(round_half_up(perc, 1), nsmall = 1), "%")
  )

c2 <-
  c2_data %>%
  mutate(health_board = fct_reorder(as_factor(health_board), perc)) %>%
  ggplot(aes(x = perc, y = health_board, fill = health_board == "Scotland")) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(x = perc, y = health_board, label = perc_formatted),
            nudge_x = -5, colour = "white", size = 3) +
  theme_dementia_pub() +
  scale_fill_manual(values = c("#3F3685", "#9B4393")) +
  theme(panel.grid.major.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  xlab("Percentage of Referrals Achieved LDP Standard") +
  ylab("")

ggsave(get_pub_figures_path(type = "c2", test_output = test_output),
  plot = c2,
  height = 6,
  width = 7,
  dpi = 600,
  device = "png"
)

# Save chart for twitter
ggsave(get_pub_figures_path(type = "twitter", test_output = test_output),
  plot = c2,
  height = 6,
  width = 7,
  dpi = 600,
  device = "png"
)

# Adjust text size and file dimensions for summary chart
summary <- c2
summary$layers[[2]] <- NULL

summary <-
  summary +
  geom_text(aes(x = perc, y = health_board, label = perc_formatted),
            nudge_x = -10.5, colour = "white", size = 3) +
  theme(axis.title.x = element_text(size = 7.5),
        axis.text = element_text(size = 7.5))

ggsave(get_pub_figures_path(type = "summary", test_output = test_output),
  plot = summary,
  height = 5,
  width = 5,
  dpi = 600,
  device = "png"
)


# Chart 3 - One year PDS by IJB

c3_data <-
  basefile %>%
  filter(fy == max(fy_in_pub)) %>% 
  # Remove Aberdeen city filter
  #filter(fy == max(fy_in_pub) & ijb != "Unknown" & ijb != "Aberdeen City") %>%
  group_by(ijb) %>%
  summarise(across(referrals:denominator, sum),
            .groups = "drop") %>%
  adorn_totals(name = "Scotland") %>%
  mutate(
    perc = case_when(
      referrals == 0 ~ 0,
      TRUE ~ numerator / denominator * 100),
    perc_formatted = paste0(format(round_half_up(perc, 1), nsmall = 1), "%")
  )

c3 <-
  c3_data %>%
  mutate(ijb = fct_reorder(as_factor(ijb), perc)) %>%
  ggplot(aes(x = perc, y = ijb, fill = ijb == "Scotland")) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(x = perc, y = ijb, label = perc_formatted),
            nudge_x = -5, colour = "white", size = 3) +
  theme_dementia_pub() +
  scale_fill_manual(values = c("#3F3685", "#9B4393")) +
  theme(panel.grid.major.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  xlab("Percentage of Referrals Achieved LDP Standard") +
  ylab("")

ggsave(get_pub_figures_path(type = "c3", test_output = test_output),
  plot = c3,
  height = 9,
  width = 7,
  dpi = 600,
  device = "png"
)


# Chart 4 - Referrals by age

c4_data <-
  basefile %>%
  filter(fy == max(fy_in_pub) & referrals > 0) %>%
  group_by(age_grp) %>%
  summarise(across(c(referrals), sum),
            .groups = "drop") %>%
  # Update the number of referrals using aberdeen city lookup for 2021 publication
  # Comment this code as not needed for 21/22 published data onwards
  #
  # left_join(ac_lookup_age_group, by = c("age_grp")) %>% 
  # mutate(referrals = case_when(age_grp == '59 and Under' ~ pub_referrals,  
  #                              age_grp == '60 to 64' ~ pub_referrals, 
  #                              age_grp == '65 to 69' ~ pub_referrals,
  #                              age_grp == '70 to 74' ~ pub_referrals,
  #                              age_grp == '75 to 79' ~ pub_referrals,
  #                              age_grp == '80 to 84' ~ pub_referrals,
  #                              age_grp == '85 to 89' ~ pub_referrals,
  #                              age_grp == '90+' ~ pub_referrals,
  #                              age_grp == 'Unknown' ~ pub_referrals)) %>% 
  # select(-c("fy", "pub_referrals")) %>% 
  mutate(
    perc = case_when(
      referrals == 0 ~ 0,
      TRUE ~ referrals / sum(referrals) * 100),
    perc_formatted = paste0(format(round_half_up(perc, 1), nsmall = 1), "%")
  )

c4_limit <- ceiling(max(c4_data$perc) / 10) * 10

c4 <-
  c4_data %>%
  ggplot(aes(x = age_grp, y = perc, fill = 1)) +
  geom_bar(stat = "identity", width = 0.5, fill = "#3F3685") +
  geom_text(aes(label = paste0(format(round_half_up(perc, 1), nsmall = 1), "%")), 
            vjust = -0.5,
            size = 3) +
  theme_dementia_pub() +
  scale_y_continuous(
    limits = c(0, c4_limit),
    breaks = seq(0, c4_limit, by = 5),
    labels = paste0(seq(0, c4_limit, by = 5), "%"),
    expand = c(0, 0)) +
  scale_x_discrete(labels = str_wrap(c4_data$age_grp, width = 8)) +
  xlab("Age Group") +
  ylab(str_wrap("Percentage of total referrals", width = 10))

# Save chart to output folder
ggsave(get_pub_figures_path(type = "c4", test_output = test_output),
  plot = c4,
  width = 6.8,
  height = 3.5,
  device = "png",
  dpi = 600
)


# Chart 5 - One year PDS by age

c5_data <-
  basefile %>%
  filter(fy == max(fy_in_pub) & referrals > 0) %>%
  group_by(age_grp) %>%
  summarise(across(referrals:denominator, sum),
            .groups = "drop") %>%
  # Update the number of referrals using aberdeen city lookup for 2021 publication
  # Comment this code as not needed for 21/22 published data onwards
  #
  # left_join(ac_lookup_age_group, by = c("age_grp")) %>% 
  # mutate(referrals = case_when(age_grp == '59 and Under' ~ pub_referrals,  
  #                              age_grp == '60 to 64' ~ pub_referrals, 
  #                              age_grp == '65 to 69' ~ pub_referrals,
  #                              age_grp == '70 to 74' ~ pub_referrals,
  #                              age_grp == '75 to 79' ~ pub_referrals,
  #                              age_grp == '80 to 84' ~ pub_referrals,
  #                              age_grp == '85 to 89' ~ pub_referrals,
  #                              age_grp == '90+' ~ pub_referrals,
  #                              age_grp == 'Unknown' ~ pub_referrals), 
  #        numerator = case_when(age_grp == '59 and Under' ~ pub_numerator,  
  #                                age_grp == '60 to 64' ~ pub_numerator, 
  #                                age_grp == '65 to 69' ~ pub_numerator,
  #                                age_grp == '70 to 74' ~ pub_numerator,
  #                                age_grp == '75 to 79' ~ pub_numerator,
  #                                age_grp == '80 to 84' ~ pub_numerator,
  #                                age_grp == '85 to 89' ~ pub_numerator,
  #                                age_grp == '90+' ~ pub_numerator,
  #                                age_grp == 'Unknown' ~ pub_numerator),
  #        denominator = case_when(age_grp == '59 and Under' ~ pub_denominator,  
  #                              age_grp == '60 to 64' ~ pub_denominator, 
  #                              age_grp == '65 to 69' ~ pub_denominator,
  #                              age_grp == '70 to 74' ~ pub_denominator,
  #                              age_grp == '75 to 79' ~ pub_denominator,
  #                              age_grp == '80 to 84' ~ pub_denominator,
  #                              age_grp == '85 to 89' ~ pub_denominator,
  #                              age_grp == '90+' ~ pub_denominator,
  #                              age_grp == 'Unknown' ~ pub_denominator)) %>% 
  # select(-c("pub_referrals", "pub_numerator", "pub_denominator")) %>% 
  mutate(
    perc = case_when(
      referrals == 0 ~ 0,
      TRUE ~ numerator / denominator * 100),
    perc_formatted = paste0(format(round_half_up(perc, 1), nsmall = 1), "%")
  )

c5 <-
  c5_data %>%
  ggplot(aes(x = age_grp, y = perc, fill = 1)) +
  geom_bar(stat = "identity", width = 0.5, fill = "#3F3685") +
  geom_text(aes(label = paste0(format(round_half_up(perc, 1), nsmall = 1), "%")), 
            vjust = -0.5,
            size = 3) +
  theme_dementia_pub() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_discrete(labels = str_wrap(c5_data$age_grp, width = 8)) +
  xlab("Age Group") +
  ylab(str_wrap("Percentage of Referrals Achieved LDP Standard", width = 10))

# Save chart to output folder
ggsave(get_pub_figures_path(type = "c5", test_output = test_output),
  plot = c5,
  width = 6.8,
  height = 3.5,
  device = "png",
  dpi = 600
)


# Chart 6 - Referrals by SIMD

c6_data <-
  basefile %>%
  filter(fy == max(fy_in_pub) & referrals > 0) %>%
  group_by(simd) %>%
  summarise(across(c(referrals), sum),
            .groups = "drop") %>%
  # Update the number of referrals using aberdeen city lookup for 2021 publication
  # Comment this code as not needed for 21/22 published data onwards
  #
  # left_join(ac_lookup_simd, by = c("simd")) %>% 
  # mutate(
  #   referrals = case_when(simd == '1 - Most Deprived' ~ pub_referrals, 
  #                         simd == '2' ~ pub_referrals, 
  #                         simd == '3' ~ pub_referrals,
  #                         simd == '4' ~ pub_referrals,
  #                         simd == '5 - Least Deprived' ~ pub_referrals,
  #                         simd == 'Unknown' ~ pub_referrals)) %>% 
  # select(-c("pub_referrals", "pub_numerator", "pub_denominator")) %>% 
  mutate(
    perc = case_when(
      referrals == 0 ~ 0,
      TRUE ~ referrals / sum(referrals) * 100),
    perc_formatted = paste0(format(round_half_up(perc, 1), nsmall = 1), "%")
  )

c6_limit <- ceiling(max(c6_data$perc) / 10) * 10

c6 <-
  c6_data %>%
  ggplot(aes(x = simd, y = perc, fill = 1)) +
  geom_bar(stat = "identity", width = 0.5, fill = "#3F3685") +
  geom_text(aes(label = paste0(format(round_half_up(perc, 1), nsmall = 1), "%")), 
            vjust = -0.5,
            size = 3) +
  theme_dementia_pub() +
  scale_y_continuous(
    limits = c(0, c6_limit),
    breaks = seq(0, c6_limit, by = 5),
    labels = paste0(seq(0, c6_limit, by = 5), "%"),
    expand = c(0, 0)) +
  scale_x_discrete(labels = str_wrap(c6_data$simd, width = 9)) +
  xlab("Deprivation Quintile") +
  ylab(str_wrap("Percentage of total referrals", width = 10))

# Save chart to output folder
ggsave(get_pub_figures_path(type = "c6", test_output = test_output),
  plot = c6,
  width = 6.8,
  height = 3.5,
  device = "png",
  dpi = 600
)


# Chart 7 - One year PDS by SIMD

c7_data <-
  basefile %>%
  filter(fy == max(fy_in_pub) & referrals > 0) %>%
  group_by(simd) %>%
  summarise(across(referrals:denominator, sum),
            .groups = "drop") %>%
  # Update the number of referrals using aberdeen city lookup for 2021 publication
  # Comment this code as not needed for 21/22 published data onwards
  #
  # left_join(ac_lookup_simd, by = c("simd")) %>% 
  # mutate(
  #   referrals = case_when(simd == '1 - Most Deprived' ~ pub_referrals, 
  #                         simd == '2' ~ pub_referrals, 
  #                         simd == '3' ~ pub_referrals,
  #                         simd == '4' ~ pub_referrals,
  #                         simd == '5 - Least Deprived' ~ pub_referrals,
  #                         simd == 'Unknown' ~ pub_referrals),
  #   numerator = case_when(simd == '1 - Most Deprived' ~ pub_numerator, 
  #                         simd == '2' ~ pub_numerator, 
  #                         simd == '3' ~ pub_numerator,
  #                         simd == '4' ~ pub_numerator,
  #                         simd == '5 - Least Deprived' ~ pub_numerator,
  #                         simd == 'Unknown' ~ pub_numerator),
  #   denominator = case_when(simd == '1 - Most Deprived' ~ pub_denominator, 
  #                         simd == '2' ~ pub_denominator, 
  #                         simd == '3' ~ pub_denominator,
  #                         simd == '4' ~ pub_denominator,
  #                         simd == '5 - Least Deprived' ~ pub_denominator,
  #                         simd == 'Unknown' ~ pub_denominator)) %>% 
  # select(-c("pub_referrals", "pub_numerator", "pub_denominator")) %>% 
  mutate(
    perc = case_when(
      referrals == 0 ~ 0,
      TRUE ~ numerator / denominator * 100),
    perc_formatted = paste0(format(round_half_up(perc, 1), nsmall = 1), "%")
  )

c7 <-
  c7_data %>%
  ggplot(aes(x = simd, y = perc, fill = 1)) +
  geom_bar(stat = "identity", width = 0.5, fill = "#3F3685") +
  geom_text(aes(label = paste0(format(round_half_up(perc, 1), nsmall = 1), "%")), 
            vjust = -0.5,
            size = 3) +
  theme_dementia_pub() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 110)) +
  scale_x_discrete(labels = str_wrap(c7_data$simd, width = 8)) +
  xlab("Deprivation Quintile") +
  ylab(str_wrap("Percentage of Referrals Achieved LDP Standard", width = 10))

# Save chart to output folder
ggsave(get_pub_figures_path(type = "c7", test_output = test_output),
  plot = c7,
  width = 6.8,
  height = 3.5,
  device = "png",
  dpi = 600
)

# Chart 8 - Referrals trend

trend_year <- paste0(as.numeric(substr(max(fy_in_pub),1,4)) + 1,
                           "/", as.numeric(substr(max(fy_in_pub),6,7)) + 1)

c8_data <-  read_rds(get_mi_data_path(type = "final_data", ext = "rds", test_output = test_output)) %>% 
  
  # Select FY to be included in rest of pub plus extra year
  filter(fy %in% c(fy_in_pub, trend_year)) %>%
  
  # Aggregate to year level (don't need month breakdown)
  group_by(across(c(health_board:simd, -month))) %>%
  summarise(referrals = sum(referrals),
            .groups = "drop")  %>% 
  # calculate total referrals by year
  group_by(geog = "Scotland", fy) %>% summarise(annual_referrals = sum(referrals))

c8 <-
  c8_data %>%
  ggplot(aes(x = fy, y = annual_referrals, group = geog)) +
  geom_line(colour = "#3F3685", linewidth = 1.3) +
 # geom_text(aes(label = format(annual_referrals, big.mark = ",")), 
         #   vjust = 1.5,
          #  size = 3) +
  theme_dementia_pub() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(c8_data$annual_referrals)+300)) +
  scale_x_discrete(labels = str_wrap(c8_data$fy, width = 8)) +
  xlab("Financial Year of Diagnosis") +
  ylab(str_wrap("Number of Referrals", width = 10)) #+
  #theme(
  #  axis.title.x = element_text(size = 14),  # Change x-axis label size
   # axis.title.y = element_text(size = 14))  # Change y-axis label size

c8


# Save chart to output folder
ggsave(get_pub_figures_path(type = "c8", test_output = test_output),
       plot = c8,
       width = 7,
       height = 3,
       device = "png",
       dpi = 600
)


# Chart 9 - Rates trend

trend_year <- paste0(as.numeric(substr(max(fy_in_pub),1,4)) + 1,
                     "/", as.numeric(substr(max(fy_in_pub),6,7)) + 1)

c9_data_pds <- read_rds(get_mi_data_path(type = "final_data", ext = "rds", test_output = test_output)) %>% 
  
  # Select FY to be included in rest of pub plus extra year
  filter(fy %in% c(fy_in_pub, trend_year)) %>%
  
  # Aggregate to year level (don't need month breakdown)
  group_by(across(c(health_board:simd, -month))) %>%
  summarise(referrals = sum(referrals),
            .groups = "drop")  %>% 
  # calculate total referrals by year
  group_by(geog = "Scotland", fy) %>% summarise(annual_referrals = sum(referrals)) %>% 
  # add year column to pds data to match pop data
  mutate(year = as.numeric(substr(fy, 1, 4)))

#read in pop_data
pop_data_trends <- pop_data <- read_rds("//conf/dementia/A&I/Outputs/management-report/lookups/pop_data.rds") %>%
  filter(geog == "Scotland", age_grp != "All", age_grp != "59 and Under", age_grp != "60 to 64", age_grp_2 == "All", sex == "All") %>% select(geog, year, pop_est) %>% 
  group_by(geog, year) %>% summarise(pop_est = sum(pop_est)) %>% ungroup()

c9_data <- left_join(c9_data_pds, pop_data_trends) %>% select(geog, fy, annual_referrals, pop_est) %>% 
  mutate(pop_rate_10000 = round((annual_referrals/pop_est)*10000, 1)) %>%
  select(geog, fy, pop_rate_10000) %>% 
  ungroup()

c9 <-
  c9_data %>%
  ggplot(aes(x = fy, y = pop_rate_10000, group = geog)) +
  geom_line(colour = "#3F3685", linewidth = 1.3) +
  # geom_text(aes(label = format(annual_referrals, big.mark = ",")), 
  #   vjust = 1.5,
  #  size = 3) +
  theme_dementia_pub() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(c9_data$pop_rate_10000)+15)) +
  scale_x_discrete(labels = str_wrap(c8_data$fy, width = 8)) +
  xlab("Financial Year of Diagnosis") +
  ylab(str_wrap("Number of Referrals", width = 10)) #+
#theme(
#  axis.title.x = element_text(size = 14),  # Change x-axis label size
# axis.title.y = element_text(size = 14))  # Change y-axis label size

c9


# Save chart to output folder
ggsave(get_pub_figures_path(type = "c9", test_output = test_output),
       plot = c9,
       width = 7,
       height = 3,
       device = "png",
       dpi = 600
)

### END OF SCRIPT ###