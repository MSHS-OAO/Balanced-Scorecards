get_budget_data <- function(service){
  
  
  conn <- dbConnect(drv = odbc::odbc(),
                    dsn = dsn)
  budget_data_repo_prelim <- tbl(conn, "SUMMARY_REPO") %>%
    filter(METRIC_NAME_SUBMITTED %in% budget_to_actual_summary_table_metrics,
           SERVICE == service) %>%
    select(SERVICE,SITE,METRIC_NAME_SUBMITTED,REPORTING_MONTH,VALUE) %>%
    collect() %>%
    rename(Metric_Name_Submitted = METRIC_NAME_SUBMITTED,
           Service = SERVICE,
           Site = SITE,
           Month = REPORTING_MONTH,
           Value = VALUE)
  
  budget_data_repo_ytd <- budget_data_repo_prelim %>%
    filter(grepl('(YTD)', Metric_Name_Submitted)) %>%
    mutate(Metric_Name_Submitted = str_sub(Metric_Name_Submitted,end = -6),
           Metric_Name_Submitted = str_trim(Metric_Name_Submitted))%>%
    rename(Value_ytd = Value)
  
  budget_data_repo_monthly <- budget_data_repo_prelim %>%
    filter(grepl('(Monthly)', Metric_Name_Submitted)) %>%
    mutate(Metric_Name_Submitted = str_sub(Metric_Name_Submitted,end = -10),
           Metric_Name_Submitted = str_trim(Metric_Name_Submitted))
  
  budget_data_repo_final <- left_join(budget_data_repo_monthly,
                                budget_data_repo_ytd,
                                by = c("Service",
                                       "Site",
                                       "Month",
                                       "Metric_Name_Submitted")) %>%
    mutate(Month = as.Date(Month, format = "%m-%Y-%d"))
  
  dbDisconnect(conn)
  
  return(budget_data_repo_final)
  
}