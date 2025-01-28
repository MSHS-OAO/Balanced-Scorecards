#cashing

memoized_fetch_table <- memoise(function(table_name) {
  connection <- dbConnect(drv = odbc::odbc(), dsn = dsn)
  table_data <- tbl(connection, table_name) %>% collect()
  dbDisconnect(connection)
  table_data
})

#refresh
refresh_data <- function() {
  forget(memoized_fetch_table)
  
  list(
    future_state_data = memoized_fetch_table("BSC_FUTURE_FINANCE_VIEW"),
    current_state_data = memoized_fetch_table("BSC_CURRENT_FINANCE_VIEW"),
    status_data = memoized_fetch_table("BSC_TARGET_STATUS"),
    site_comparison = memoized_fetch_table("BSC_METRICS_FINAL_TESTING")
    
  )
}