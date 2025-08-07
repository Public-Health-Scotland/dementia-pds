add_percent <- function(df, value, name) {
  
  data <- df %>%  mutate(perc =  if_else(total == 0, "-",
                                         paste0(round(100*{{value}}/total,1), "%")))
  
  colnames(data)[colnames(data) == 'perc'] <- name
  
  return(data)
  
}
