# function to append the new data to summary repo- KPIs & Disruptions and Issues -----
biomed_summary_repos_KPI <- function(data,updated_user){
  
      summary_repo_kpi_format <- data %>%
      rename(SITE = Site,
             METRIC_NAME_SUBMITTED = Metric ) %>%
      pivot_longer(cols = c(-SITE,-METRIC_NAME_SUBMITTED),
                   names_to = "REPORTING_MONTH",
                   values_to = "VALUE") %>%
      mutate(REPORTING_MONTH = as.Date(format(parse_date_time(paste0("01-",REPORTING_MONTH),orders = "dmy"),"%Y-%m-%d")),
             SERVICE = "Biomed / Clinical Engineering",
             PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
             #REPORTING_MONTH = format(REPORTING_MONTH,"%Y-%m-%d"),
             UPDATED_USER = updated_user,
             VALUE = as.numeric(VALUE))
      
      summary_repo_kpi_format <- as.data.frame(summary_repo_kpi_format)
      summary_repo_kpi_format <- summary_repo_kpi_format[complete.cases(summary_repo_kpi_format), ]  
      summary_repo_kpi_format <- as_tibble(summary_repo_kpi_format)

}


biomed_summary_repos_DI <- function(data,updated_user){
  
  
    summary_repo_di_format <- data %>%
      rename(SITE = Site) %>%
      select(-Metric) %>%
      #mutate(vars(col.names.to.numeric),as.numeric()) %>%
      pivot_longer(cols = c(-SITE),
                   names_to = "REPORTING_MONTH",
                   values_to = "VALUE") %>%
      mutate(REPORTING_MONTH = as.Date(format(parse_date_time(paste0("01-",REPORTING_MONTH),orders = "dmy"),"%Y-%m-%d")),
             SERVICE = "Biomed / Clinical Engineering",
             METRIC_NAME_SUBMITTED = "Total Disruptions or Equipment Issues",
             PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
             #REPORTING_MONTH = format(REPORTING_MONTH,"%Y-%m-%d"),
             UPDATED_USER = updated_user,
             VALUE = as.numeric(VALUE))
    
    summary_repo_di_format <- as.data.frame(summary_repo_di_format)
    summary_repo_di_format <- summary_repo_di_format[complete.cases(summary_repo_di_format), ] 
    summary_repo_di_format <- as_tibble(summary_repo_di_format)
    
    
}