mdf_from_db <- function(service_input, month_input) {
  min_month <- as.Date(paste0(month_input, "-01"), "%m-%Y-%d") %m-% months(12)
  
  # service_input <- "Lab"
  # month_input <- "05-2022"
  format <- "YYYY-MM-DD HH24:MI:SS"
  conn <- dbConnect(drv = odbc::odbc(),
                    dsn = dsn)
  mdf_tbl <- tbl(conn, "BSC_METRICS_FINAL_DF")
  metrics_final_df <- mdf_tbl %>% filter(SERVICE %in% service_input, 
                                         TO_DATE(min_month, format) <= REPORTING_MONTH) %>%
    select(-UPDATED_TIME, -UPDATED_USER, -METRIC_NAME_SUBMITTED) %>% collect() %>%
    rename(Service = SERVICE,
           Site = SITE,
           Metric_Group = METRIC_GROUP,
           Metric_Name = METRIC_NAME,
           Premier_Reporting_Period = PREMIER_REPORTING_PERIOD,
           value_rounded = VALUE,
           Reporting_Month_Ref = REPORTING_MONTH
    ) %>%
    mutate(Reporting_Month = format(Reporting_Month_Ref, "%m-%Y")) %>% 
    distinct()
  dbDisconnect(conn)
  
  return(metrics_final_df)
}
