return_updated_manual_data <- function(service, table_name, df) {
  #Return data that is currently in Summary Repos on DB
  
  print(str(df))
  
  existing_data <- sql_manual_table_output(service,
                                           table_name)
  # Arrange by sites in alphabetical order
  existing_data <- existing_data %>%
    arrange(Site)
  
  print(existing_data)
  
  
  existing_data <- manual_table_month_order(existing_data)
  
  print(existing_data)
  
  existing_data <- existing_data %>%
    pivot_longer(cols = -contains(c("Site", "Metric")),
                 names_to = "Month",
                 values_to = "Value") %>%
    mutate(Value = as.numeric(Value),
           Month = as.Date(paste0(Month, "-01"),
                           format = "%m-%Y-%d"))

  print(str(existing_data))
  

  df <- anti_join(df,
                  existing_data,
                  by = c(
                  "SITE" = "Site",
                  "METRIC_NAME_SUBMITTED" = "Metric",
                  "REPORTING_MONTH" = "Month",
                  "VALUE" = "Value")
                  )
  print("-----------")
  print(df)
  
  df <- df %>% filter(!is.na(VALUE))
  
  print(df)
  
  
  return(df)
  
}
