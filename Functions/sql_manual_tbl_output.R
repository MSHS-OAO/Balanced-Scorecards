con <- dbConnect(odbc::odbc(), driver_name, timeout = 30)
summary_repo_tbl <- tbl(con, "SUMMARY_REPO")

sql_manual_table_output <- function(service, table_name) {
  
  max_month <- (Sys.Date() - months(1)) - months(7)
  format <- "YYYY-MM-DD"
  
  df <- summary_repo_tbl %>% filter(SERVICE %in% service) %>%
          select(-SERVICE, -PREMIER_REPORTING_PERIOD, -UPDATE_TIME) %>%
          filter(MONTH >= TO_DATE(max_month, format)) %>%
          arrange(MONTH, SITE) %>%
          collect() %>%
          mutate(MONTH = format(MONTH, "%m-%Y"),
                 VALUE = as.character(VALUE)) %>%
          rename(Month = MONTH,
                 Site = SITE,
                 Metric = METRIC_NAME_SUBMITTED,
                 Value = VALUE) %>%
        pivot_wider(names_from = Month,
                    values_from = Value)
    
}
