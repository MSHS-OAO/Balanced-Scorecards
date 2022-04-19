metrics_final_df_subset_and_merge <- function(df) {
  df <- df[,processed_df_cols]
  df <- df %>% 
    mutate(Reporting_Month_Ref = as.Date(paste('01', as.yearmon(df$Reporting_Month, 
                                                                "%m-%Y")), 
                                         format='%d %b %Y'))
  updated_rows <- unique(df[c("Metric_Name", "Reporting_Month", 
                                      "Service", "Site")])
  metrics_final_df <- anti_join(metrics_final_df, updated_rows)
  metrics_final_df <- full_join(metrics_final_df, df)
  
  metrics_final_df <- metrics_final_df %>%
    arrange(Service,
            Site,
            Metric_Group,
            Reporting_Month_Ref)
  
  return(metrics_final_df)
}
