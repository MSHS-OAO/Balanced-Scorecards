record_timestamp <- function(service){
  time_df <- read_excel(paste0(home_path, "time_updated.xlsx"))
  date_time <- data.frame(Updated = as.POSIXct(Sys.time()))
  date_time$Service = service
  date_time <- rbind(time_df, date_time)
  write_xlsx(date_time, paste0(home_path, "time_updated.xlsx"))
}