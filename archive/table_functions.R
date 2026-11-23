#creates table for outcomes sections for integration authority areas in subpage-demographics.Rmd
ijb_table_ldp <- function(data, measure){

perc <- data %>% filter(!grepl("NHS", geog)) %>% 
  select(geog,fy,{{measure}},perc_met) %>% 
  mutate(perc_met = if_else(is.na(perc_met) | (geog == "Aberdeen City" & fy %in% c("2019/20", "2020/21")), "-", paste0(perc_met,"%"))) %>%
  arrange({{measure}}) %>% 
  pivot_wider(names_from = {{measure}}, values_from = perc_met) %>%
  mutate(key = paste0(fy,geog), .before = geog) %>% 
  mutate(type = "% Met Standard/Exempt", .after = fy)

totals <- data %>% filter(!grepl("NHS", geog)) %>% 
  mutate(referrals_minus_ongoing = complete + exempt + fail + Aberdeen) %>% 
  select(geog,fy,{{measure}},referrals_minus_ongoing) %>% 
  mutate(across(where(is.numeric), ~if_else(is.na(.), "-", format(., big.mark = ",")))) %>%
  arrange({{measure}}) %>% 
  pivot_wider(names_from = {{measure}}, values_from = referrals_minus_ongoing) %>%
  mutate(key = paste0(fy,geog), .before = geog) %>% 
  mutate(type = if_else(fy %in% finalised_years,
                        "Number of Referrals", "Number of Referrals (excludes PDS Ongoing)"),
         .after = fy)

table <- bind_rows(totals, perc) %>% 
  arrange(geog, key, desc(type)) %>% 
  mutate(geog = if_else(type == "% Met Standard/Exempt", " ", geog)) %>% 
  rename(" " = "type") %>% 
  rename(`Integration Authority Area` = geog)

return(table)}


#creates table for outcomes sections for health boards subpage-demographics.Rmd
hb_table_ldp <- function(data, measure){
  
  perc <- data %>% filter(geog == "Scotland" | grepl("NHS", geog)) %>% 
    select(geog,fy,{{measure}},perc_met) %>% 
    mutate(perc_met = if_else(is.na(perc_met) | (geog == "NHS Grampian" & fy %in% c("2019/20", "2020/21")), "-", paste0(perc_met,"%"))) %>%
    arrange({{measure}}) %>% 
    pivot_wider(names_from = {{measure}}, values_from = perc_met) %>%
    mutate(key = paste0(fy,geog), .before = geog) %>% 
    mutate(across(everything(), ~replace_na(.x, "-")))  %>% 
    mutate(type = "% Met Standard/Exempt", .after = fy)
  
  totals <- data %>% filter(geog == "Scotland" | grepl("NHS", geog)) %>% 
    mutate(referrals_minus_ongoing = complete + exempt + fail + Aberdeen) %>% 
    select(geog,fy,{{measure}},referrals_minus_ongoing) %>% 
    mutate(across(where(is.numeric), ~if_else(is.na(.), "-", format(., big.mark = ",")))) %>%
    arrange({{measure}}) %>% 
    pivot_wider(names_from = {{measure}}, values_from = referrals_minus_ongoing) %>%
    mutate(key = paste0(fy,geog), .before = geog) %>% 
    mutate(across(everything(), ~replace_na(.x, "-"))) %>%  
    mutate(type = if_else(fy %in% finalised_years,
                          "Number of Referrals", "Number of Referrals (excludes PDS Ongoing)"),
           .after = fy)
  
  table <- bind_rows(totals, perc) %>% 
    arrange(geog, key, desc(type)) %>% 
    mutate(geog = if_else(type == "% Met Standard/Exempt", " ", geog)) %>% 
    rename(" " = "type") %>% 
    rename(`Health Board` = geog)
  
  return(table)}

#creates table for outcomes sections for trends subpage-demographics.Rmd
trend_table_ldp <- function(data, measure){

perc <- data %>% filter(!is.na(perc_met)) %>%
  select(geog, fy, {{measure}}, perc_met) %>%
  mutate(perc_met = if_else((geog == "Aberdeen City" & fy %in% c("2019/20", "2020/21")) | (geog == "NHS Grampian" & fy %in% c("2019/20", "2020/21")),  
          "-", paste0(perc_met, "%"))) %>%
  pivot_wider(names_from = fy, values_from = perc_met) %>%
  mutate(type = "% Met Standard/Exempt", .before = `2016/17`)

totals <- data %>% filter(!is.na(perc_met)) %>%
  mutate(referrals_minus_ongoing = complete + exempt + fail + Aberdeen) %>% 
  select(geog,fy,{{measure}},referrals_minus_ongoing) %>% 
  mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>%
  pivot_wider(names_from = fy, values_from = referrals_minus_ongoing, values_fill = "0") %>%  
  mutate(type = "Number of Referrals (excludes PDS Ongoing)",
         .before = `2016/17`)


table <- bind_rows(totals, perc) 

return(table)
}

