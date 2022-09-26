engineering_repo_pull <- function(){

  operational_metrics_engineering <- read_excel(operational_metrics_engineering_path) %>% filter(Month >= max(Month) %m-% months(6)) %>%
    mutate_if(is.logical, as.character) %>%
    mutate_if(is.double, as.character) %>%
    pivot_longer(cols = c(-Month, -Site),
                 names_to = "Metric",
                 values_to = "Value") %>%
    pivot_wider(names_from = "Month", values_from = Value)
  
  
  
  # operational_metrics_engineering <- sql_summary_repo_data("ENGINEERING_SUMMARY_REPO", "2021-08-01", "2022-02-01")
  # 
  # operational_metrics_engineering <- operational_metrics_engineering %>%
  #                                       arrange(MONTH) %>%
  #                                       pivot_wider(names_from = "MONTH", values_from = VALUE)
  # 
  # operational_metrics_engineering <- operational_metrics_engineering %>% rename(Site = SITE,
  #                                                                               Metric = METRIC)
    
    return(operational_metrics_engineering)
}


operational_metrics_engineering <- engineering_repo_pull()

engineering_summary_repos_data <- read_excel(operational_metrics_engineering_path)

engineering_summary_repos <- function(data, updated_user){
  
  engineering_data <- data %>%
    pivot_longer(c(-Metric, -Site),
                 names_to = "Month",
                 values_to = "Value") %>%
                mutate(UPDATED_USER = updated_user,
                       REPORTING_MONTH = as.Date(paste0(Month, "-01"), format = "%m-%Y-%d"),
                       PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH, "%b %Y"),
                       SERVICE = "Engineering",
                       Value = as.numeric(Value)
                       ) %>%
      select(-Month) %>%
      rename(VALUE = Value,
             SITE = Site,
             METRIC_NAME_SUBMITTED = Metric)
  
}

cm_kpi <- function(data){
  
  raw_cm_df <- data
  
  
  ## Security CM KPI data pre-processing 
  cm_kpi_df <- raw_cm_df %>%
    mutate_at(vars(-Month, -Site), as.numeric) %>%
    pivot_longer(
      c(-Month, -Site), 
      names_to = "Metric_Name_Submitted",
      values_to = "value") %>%
    mutate(Service = "Engineering",
           Premier_Reporting_Period = format(as.Date(Month, format = "%Y-%m-%d"),"%b %Y"),
           Reporting_Month = format(as.Date(Month, format = "%Y-%m-%d"),"%m-%Y"),
           value_rounded = value)
  
  
  # Subset processed data for merge 
    metrics_final_df <- metrics_final_df_subset_and_merge(cm_kpi_df)
    return(metrics_final_df)
}