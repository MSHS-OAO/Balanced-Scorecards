save_append_join_updates <- function(summary_repos_data, updated_data, hist_file_name, unique_columns, summary_repos_path, metrics_final_df_function){
  # Save prior version of Dept Summary data
  write_xlsx(summary_repos_data,
             paste0(hist_archive_path,
                    hist_file_name, "",
                    format(Sys.time(), "%Y%m%d_%H%M%S"),
                    ".xlsx"))
  
  # Append summary with new data
  # First, identify the sites, months, and metrics in the new data
  new_data <- unique(
    updated_data[, unique_columns]
  )
  
  # Second, remove these sites, months, and metrics from the historical data, if they exist there
  # This allows us to ensure no duplicate entries for the same site, metric, and time period
  summary_repos_data  <- anti_join(summary_repos_data,
                                   new_data,
                                   by = (unique_columns = unique_columns))
  
  # Third, combine the updated historical data with the new data
  summary_repos_data  <- full_join(summary_repos_data,
                                   updated_data)
  
  
  # Lastly, save the updated summary data
  #write_xlsx(summary_repos_data, summary_repos_path)
  
  metrics_final_df <<- metrics_final_df_function(updated_data)
  
  # Save updated metrics_final_df
  #saveRDS(metrics_final_df, metrics_final_df_path)
}
