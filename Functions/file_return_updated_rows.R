file_return_updated_rows <- function(df) {
  #max_date_df <- max(df$REPORTING_MONTH)
  df <- df %>% filter(REPORTING_MONTH >= max(REPORTING_MONTH) %m-% months(6))
  #Get the identifiers for the data in Summary Repo
  service <- unique(df$SERVICE)
  reporting_period <- unique(df$PREMIER_REPORTING_PERIOD)
  metrics <- unique(df$METRIC_NAME_SUBMITTED)
  site <- unique(df$SITE)
  
  
  
  # filter out data based on the above results
  existing_data <- summary_repo_tbl %>% filter(SERVICE  %in% service, PREMIER_REPORTING_PERIOD %in% reporting_period, METRIC_NAME_SUBMITTED %in% metrics, SITE %in% site) %>% collect()
  
  #only retunr data that is not in Summary Repo or has been updated
  df <- anti_join(df,
                  existing_data,
                  by = c(
                    "SITE" = "SITE",
                    "METRIC_NAME_SUBMITTED" = "METRIC_NAME_SUBMITTED",
                    "REPORTING_MONTH" = "REPORTING_MONTH",
                    "VALUE" = "VALUE")
  )
  df <- df %>% filter(!is.na(VALUE))
  
  return(df)
}
