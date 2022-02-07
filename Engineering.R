engineering_repo_pull <- function(){

    operational_metrics_engineering <- read_excel(operational_metrics_engineering_path) %>% filter(Month >= max(Month) %m-% months(6)) %>%
      mutate_if(is.logical, as.character) %>%
      mutate_if(is.double, as.character) %>%
      pivot_longer(cols = c(-Month, -Site),
                   names_to = "Metric",
                   values_to = "Value") %>%
      pivot_wider(names_from = "Month", values_from = Value)
    
    return(operational_metrics_engineering)
}


operational_metrics_engineering <- engineering_repo_pull()

engineering_summary_repos_data <- read_excel(operational_metrics_engineering_path)

engineering_summary_repos <- function(data){
  
  engineering_data <- data %>%
    pivot_longer(c(-Metric, -Site),
                 names_to = "Month",
                 values_to = "Value") %>%
    pivot_wider(names_from = "Metric", values_from = Value)
  
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
  
  cm_kpi_df$Metric_Group <- metric_group_mapping$Metric_Group[match(cm_kpi_df$Metric_Name_Submitted, metric_group_mapping$Metric_Name_Submitted)]
  cm_kpi_df$Metric_Name <- metric_group_mapping$Metric_Name[match(cm_kpi_df$Metric_Name_Submitted, metric_group_mapping$Metric_Name_Submitted)]
  
  ### Create Target Variance Column
  cm_kpi_target_status <- merge(cm_kpi_df[, c("Service","Site","Metric_Group","Metric_Name","Reporting_Month","value_rounded")],
                                target_mapping, 
                                by = c("Service","Site","Metric_Group","Metric_Name"),
                                all = TRUE)
  
  cm_kpi_target_status <- cm_kpi_target_status %>%
    mutate(Variance = between(value_rounded, Range_1, Range_2)) %>% # Target mapping
    filter(Variance == TRUE)
  
  cm_kpi_df_final <- merge(cm_kpi_df, cm_kpi_target_status[,c("Service","Site","Metric_Group","Metric_Name","Reporting_Month","Target","Status")],
                           all = TRUE)
  
  
  # Subset processed data for merge 
  cm_kpi_df_final <- cm_kpi_df_final[,processed_df_cols]
  
  cm_kpi_df_final$Reporting_Month_Ref <- as.Date(paste('01', as.yearmon(cm_kpi_df_final$Reporting_Month, "%m-%Y")), format='%d %b %Y')
  
  
  metrics_final_df <- full_join(metrics_final_df,cm_kpi_df_final)
}