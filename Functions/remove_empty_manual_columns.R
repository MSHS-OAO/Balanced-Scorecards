remove_empty_manual_columns <- function(manual_table){
  # Identify columns with no data in them and remove before further processing
  # This ensures months with no data do not get added to the department summary
  # repo and metrics_final_df repository
  non_empty_cols <- !(apply(manual_table,
                            MARGIN = 2,
                            function(x) 
                              all(is.na(x))))
  
  manual_table <- manual_table[, non_empty_cols]
  
  return(manual_table)
}