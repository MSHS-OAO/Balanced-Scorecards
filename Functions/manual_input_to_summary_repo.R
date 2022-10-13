to_summary_repos_form <- function(data,service,updated_user){
  
  summary_repo_kpi_format <- data %>%
    rename(SITE = Site,
           METRIC_NAME_SUBMITTED = Metric ) %>%
    pivot_longer(cols = c(-SITE,-METRIC_NAME_SUBMITTED),
                 names_to = "REPORTING_MONTH",
                 values_to = "VALUE") %>%
    mutate(REPORTING_MONTH = as.Date(format(parse_date_time(paste0("01-",REPORTING_MONTH),orders = "dmy"),"%Y-%m-%d")),
           SERVICE = service,
           PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
           #REPORTING_MONTH = format(REPORTING_MONTH,"%Y-%m-%d"),
           UPDATED_USER = updated_user,
           VALUE = as.numeric(VALUE))
  
  summary_repo_kpi_format <- as.data.frame(summary_repo_kpi_format)
  summary_repo_kpi_format <- summary_repo_kpi_format[complete.cases(summary_repo_kpi_format), ]  
  summary_repo_kpi_format <- as_tibble(summary_repo_kpi_format)
  
}