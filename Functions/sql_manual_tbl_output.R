poolcon <- dbPool(drv = odbc::odbc(), dsn = dsn, timeout = 30)
summary_repo_tbl <- tbl(poolcon, "SUMMARY_REPO")
manual_tbl_mapping <- tbl(poolcon, "BSC_MANUAL_TABLE_MAPPING")

sql_manual_table_output <- function(service, table_name) {
  
  # conn <- dbConnect(odbc::odbc(), dsn)
  # summary_repo_tbl <- tbl(conn, "SUMMARY_REPO")
  # manual_tbl_mapping <- tbl(conn, "BSC_MANUAL_TABLE_MAPPING")
  
  max_month <- as.character(Sys.Date() %m-% months(8))
  format <- "YYYY-MM-DD"
  
  manual_mapping_metrics <- manual_tbl_mapping %>% 
    filter(SERVICE %in% service, TABLE_NAME %in% table_name) %>% 
    collect()
  manual_mapping_metrics <- unique(manual_mapping_metrics$METRIC_NAME_SUBMITTED)
  
  df <- summary_repo_tbl %>%
    filter(SERVICE %in% service &
             METRIC_NAME_SUBMITTED %in% manual_mapping_metrics) %>%
    select(-SERVICE, -PREMIER_REPORTING_PERIOD, -UPDATED_TIME, -UPDATED_USER) %>%
    filter(REPORTING_MONTH >= TO_DATE(max_month, format)) %>% 
    arrange(REPORTING_MONTH, SITE) %>%
    collect() %>%
    mutate(REPORTING_MONTH = format(REPORTING_MONTH, "%m-%Y"),
           VALUE = as.character(VALUE)) %>%
    rename(Month = REPORTING_MONTH,
           Site = SITE,
           Metric = METRIC_NAME_SUBMITTED,
           Value = VALUE) %>%
    pivot_wider(names_from = Month,
                values_from = Value)
  
  # dbDisconnect(conn)
  
  df <- df[order(df$Metric, decreasing = TRUE),]
  

    
}
