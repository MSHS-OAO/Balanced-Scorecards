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