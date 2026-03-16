get_budget_data <- function(service,month_input){
  
  min_month <- as.Date(paste0(month_input, "-01"), "%m-%Y-%d") %m-% months(9)
  format <- "YYYY-MM-DD HH24:MI:SS"
  
  conn <- dbConnect(drv = odbc::odbc(),
                    dsn = dsn)
  budget_data_repo_prelim <- tbl(conn, "SUMMARY_REPO") %>%
    filter(METRIC_NAME_SUBMITTED %in% budget_to_actual_summary_table_metrics,
           SERVICE == service, 
           TO_DATE(min_month, format) <= REPORTING_MONTH) %>%
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
  
  if(service == "Nursing") {
    budget_data_labor_non_labor <- tbl(conn, "BSC_FINANCE_TABLE_VIEW") %>%
      filter(FUNCTION == service, 
             TO_DATE(min_month, format) <= MONTH) %>%
      collect() %>%
      mutate(SITE = case_when(
        SITE == 'MSH' ~ "MSH",
        SITE == 'MS STL' ~ "MSM",
        SITE == 'MS WEST' ~ "MSW",
        SITE == 'MS BIB' ~ "MSB",
        SITE == 'MSBHC' ~ "MSBHC",
        SITE == 'MS BI' ~ "MSBI",
        SITE == 'MS NYEE' ~ "NYEE",
        TRUE ~ SITE # The TRUE ~ condition serves as the "else" or default
      )) %>%
      select(SITE, FUNCTION,EXPTYPE,MONTH,SUM_OF_MONTH_BUDGET, SUM_OF_YTD_BUDGET) %>%
      group_by(SITE, MONTH, FUNCTION,EXPTYPE) %>%
      summarise(Value= sum(SUM_OF_MONTH_BUDGET),
                Value_ytd = sum(SUM_OF_YTD_BUDGET))%>%
      rename(Metric_Name_Submitted = EXPTYPE,
             Site = SITE,
             Month = MONTH,
             Service = FUNCTION) %>%
      mutate(Metric_Name_Submitted = case_when(
        Metric_Name_Submitted == 'Salaries' ~ "Budget_Total_Labor",
        Metric_Name_Submitted == 'Supplies' ~ "Budget_Total_Non_Labor",
        TRUE ~ Metric_Name_Submitted # The TRUE ~ condition serves as the "else" or default
      ))
    budget_data_repo_final <- rbind(budget_data_repo_final,
                                    budget_data_labor_non_labor)
  }
    
  
  dbDisconnect(conn)
  
  return(budget_data_repo_final)
  
}
