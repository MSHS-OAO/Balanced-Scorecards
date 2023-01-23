return_updated_manual_data <- function(service, table_name, df) {
  #Return data that is currently in Summary Repos on DB
  con <- dbConnect(odbc::odbc(), dsn, timeout = 30)
  summary_repo_tbl <- tbl(con, "SUMMARY_REPO")
  manual_tbl_mapping <- tbl(con, "BSC_MANUAL_TABLE_MAPPING")
  
  max_month <- as.character(Sys.Date() - months(8))
  format <- "YYYY-MM-DD"
  
  existing_data <- summary_repo_tbl %>% filter(SERVICE == service,
                                               REPORTING_MONTH >= TO_DATE(max_month, format)) %>%
                    collect()
  dbDisconnect(con)
    
  
  # existing_data <- sql_manual_table_output(service,
  #                                          table_name)
  # # Arrange by sites in alphabetical order
  # existing_data <- existing_data %>%
  #   arrange(Site)
  # 
  # existing_data <- manual_table_month_order(existing_data)
  # 
  # existing_data <- existing_data %>%
  #   pivot_longer(cols = -contains(c("Site", "Metric")),
  #                names_to = "Month",
  #                values_to = "Value") %>%
  #   mutate(Value = as.numeric(Value),
  #          Month = as.Date(paste0(Month, "-01"),
  #                          format = "%m-%Y-%d"))
  df <- anti_join(df,
                  existing_data,
                  by = c(
                  "SITE" = "SITE",
                  "METRIC_NAME_SUBMITTED" = "METRIC_NAME_SUBMITTED",
                  "REPORTING_MONTH" = "REPORTING_MONTH",
                  "VALUE" = "VALUE")
                  )

    df <- df %>% filter(!is.na(VALUE))
 
    return(df)
  
}
