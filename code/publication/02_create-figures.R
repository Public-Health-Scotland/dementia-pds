#########################################################################
# Name of file - 02_create-figures.R
# Data release - Annual Dementia PDS Publication
# Original Authors - Alice Byers
# Original Date - March 2021
#
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Create all figures required for summary and report.
#########################################################################


### 1 - Load environment file and themes ----

source(here::here("code", "publication", "00_setup-pub-environment.R"))

source(here("functions", "ggplot_themes.R"))


### 2 - Read in data ----

basefile <-
  read_rds(
    here("data", "publication", glue("{pub_date}_pub-data.rds"))
  )

# Load expected diagnoses reference file
exp <- read_csv(here("reference-files", "expected-diagnoses.csv")) %>%
  filter(fy == max(fy_in_pub)) %>%
  select(health_board = health_board_label, fy, diagnoses)


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
  theme_dementia() +
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

ggsave(
  here("publication", "output", pub_date, "figures", 
       paste0(pub_date, "_incidence-hb.png")),
  plot = c1,
  height = 6,
  width = 6,
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
  theme_dementia() +
  scale_fill_manual(values = c("#3F3685", "#9B4393")) +
  theme(panel.grid.major.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  xlab("Percentage of Referrals Achieved LDP Standard") +
  ylab("")

ggsave(
  here("publication", "output", pub_date, "figures", 
       paste0(pub_date, "_12-months-hb.png")),
  plot = c2,
  height = 6,
  width = 6,
  dpi = 600,
  device = "png"
)

# Save chart for twitter
ggsave(
  here("publication", "output", pub_date,
       paste0(pub_date, "_dementia-pds_twitter-chart.png")),
  plot = c2,
  height = 6,
  width = 6,
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

ggsave(
  here("publication", "output", pub_date, "figures",
       paste0(pub_date, "_summary-chart.png")),
  plot = summary,
  height = 3.8,
  width = 4,
  dpi = 600,
  device = "png"
)


# Chart 3 - One year PDS by IJB

c3_data <-
  basefile %>%
  filter(fy == max(fy_in_pub) & ijb != "Unknown") %>%
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
  theme_dementia() +
  scale_fill_manual(values = c("#3F3685", "#9B4393")) +
  theme(panel.grid.major.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  xlab("Percentage of Referrals Achieved LDP Standard") +
  ylab("")

ggsave(
  here("publication", "output", pub_date, "figures", 
       paste0(pub_date, "_12-months-ijb.png")),
  plot = c3,
  height = 9,
  width = 6,
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
  theme_dementia() +
  scale_y_continuous(
    limits = c(0, c4_limit),
    breaks = seq(0, c4_limit, by = 5),
    labels = paste0(seq(0, c4_limit, by = 5), "%"),
    expand = c(0, 0)) +
  scale_x_discrete(labels = str_wrap(c4_data$age_grp, width = 8)) +
  xlab("Age Group") +
  ylab(str_wrap("Percentage of total referrals", width = 10))

# Save chart to output folder
ggsave(
  here("publication", "output", pub_date, "figures", 
       paste0(pub_date, "_age-dist.png")),
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
  theme_dementia() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_discrete(labels = str_wrap(c5_data$age_grp, width = 8)) +
  xlab("Age Group") +
  ylab(str_wrap("Percentage of Referrals Achieved LDP Standard", width = 10))

# Save chart to output folder
ggsave(
  here("publication", "output", pub_date, "figures", 
       paste0(pub_date, "_12-months-age.png")),
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
  theme_dementia() +
  scale_y_continuous(
    limits = c(0, c6_limit),
    breaks = seq(0, c6_limit, by = 5),
    labels = paste0(seq(0, c6_limit, by = 5), "%"),
    expand = c(0, 0)) +
  scale_x_discrete(labels = str_wrap(c6_data$simd, width = 9)) +
  xlab("Deprivation Quintile") +
  ylab(str_wrap("Percentage of total referrals", width = 10))

# Save chart to output folder
ggsave(
  here("publication", "output", pub_date, "figures", 
       paste0(pub_date, "_simd-dist.png")),
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
  theme_dementia() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_discrete(labels = str_wrap(c7_data$simd, width = 8)) +
  xlab("Deprivation Quintile") +
  ylab(str_wrap("Percentage of Referrals Achieved LDP Standard", width = 10))

# Save chart to output folder
ggsave(
  here("publication", "output", pub_date, "figures", 
       paste0(pub_date, "_12-months-simd.png")),
  plot = c7,
  width = 6.8,
  height = 3.5,
  device = "png",
  dpi = 600
)


### END OF SCRIPT ###