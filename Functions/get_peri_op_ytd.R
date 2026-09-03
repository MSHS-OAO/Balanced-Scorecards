get_peri_op_ytd <- function(month_input) {
  min_month <- as.Date(paste0(month_input, "-01"), "%m-%Y-%d") %m-% months(18)
  
  # service_input <- "Lab"
  # month_input <- "06-2023"
  format <- "YYYY-MM-DD HH24:MI:SS"
  conn <- dbConnect(drv = odbc::odbc(),
                    dsn = dsn)
  sr_tbl <- tbl(conn, "SUMMARY_REPO")
  ytd_metrics <- sr_tbl %>% filter(SERVICE %in% 'Perioperative Services',
                                   TO_DATE(min_month, format) <= REPORTING_MONTH,
                                   SITE != 'SYSTEM') %>%
    select(-UPDATED_TIME, -UPDATED_USER) %>% collect() %>%
    filter(grepl('(FYTD)', METRIC_NAME_SUBMITTED)) %>%
    filter(!grepl("/.*/", PREMIER_REPORTING_PERIOD)) %>%
    rename(Service = SERVICE,
           Site = SITE,
           Premier_Reporting_Period = PREMIER_REPORTING_PERIOD,
           value_rounded = VALUE,
           Reporting_Month_Ref = REPORTING_MONTH
    ) 
  
  ytd_metrics$METRIC_NAME_SUBMITTED <- gsub(' \\(FYTD\\).*', '',ytd_metrics$METRIC_NAME_SUBMITTED)
  
  ytd_metrics <- ytd_metrics %>% mutate(Reporting_Month = format(Reporting_Month_Ref, "%m-%Y"),
           Metric_Group = 'Operational',
           Metric_Name = METRIC_NAME_SUBMITTED)%>% 
    select(-METRIC_NAME_SUBMITTED) %>%
    distinct() %>%
    group_by(
      Site,
      Metric_Name
    ) %>%
    filter(Reporting_Month_Ref <= as.Date(paste0(month_input, "-01"), "%m-%Y-%d")) %>%
    filter(Reporting_Month_Ref == max(Reporting_Month_Ref))
  dbDisconnect(conn)
  
  return(ytd_metrics)
}
