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
