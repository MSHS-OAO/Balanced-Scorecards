library(DBI)
library(odbc)
# con <- dbConnect(odbc::odbc(), "OAO Cloud DB", timeout = 30)

# min_month <- "2022-01-01"
# max_month <- "2022-02-01"
# table <- "ENGINEERING_SUMMARY_REPO"

sql_summary_repo_data <- function(table, min_month, max_month){

  sql_statement <- paste0("SELECT * FROM ", table,
  " WHERE Month BETWEEN TO_DATE('",min_month, " 00:00:00', 'YYYY-MM-DD HH24:MI:SS') AND TO_DATE('", max_month, " 00:00:00', 'YYYY-MM-DD HH24:MI:SS')")

  results <- dbGetQuery(con, sql_statement)

}



test_df <- data.frame(Month = as.Date("2022-03-01"))
test_df$Site <- "MSM"
test_df$Metric <- "% of Critical PM's Completed on Time"
test_df$value <- 0.97

append_new_rows_to_db <- function(table, df){
  dbAppendTable(con,table,df)
}
