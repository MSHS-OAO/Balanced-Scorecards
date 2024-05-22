data <- read.xlsx("/SharedDrive//deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Finance Backend/Feb 2024 Access Data.xlsx")
updated_user = "Dheeraj Test"


process_finance_access_data <- function(data, updated_user){
  
  data  <- data%>%
    select(FDIV,
           SITE,
           CC,
           Name,
           EXPTYPE,
           Sub.Account,
           Sub.Account.Description,
           Month.Budget,
           YTD.Budget,
           Month.Actual,
           YTD.Actual,
           Annual.Budget,
           Time.Period)%>%
    rename(NAME = Name,
           SUB_ACCOUNT = Sub.Account,
           SUB_ACCOUNT_DESCRIPTION = Sub.Account.Description,
           TIME_PERIOD = Time.Period) %>%
    group_by(SITE,CC,SUB_ACCOUNT,SUB_ACCOUNT_DESCRIPTION,TIME_PERIOD) %>%
    summarise(MONTH_BUDGET = sum(Month.Budget),
              MONTH_ACTUAL = sum(Month.Actual),
              YTD_BUDGET = sum(YTD.Budget),
              YTD_ACTUAL = sum(YTD.Actual),
              ANNUAL_BUDGET = sum(Annual.Budget))%>%
    mutate(UPDATED_USER =  updated_user) %>%
    ungroup()
  
  key_cols = c("SITE","CC","SUB_ACCOUNT","SUB_ACCOUNT_DESCRIPTION","TIME_PERIOD")
  update_cols = names(data)
  update_cols = update_cols[! update_cols %in% key_cols]
  
  write_temporary_table_to_database_and_merge_updated(data,
                                                      c("SITE","CC","SUB_ACCOUNT","SUB_ACCOUNT_DESCRIPTION","TIME_PERIOD"),
                                                      "BSC_FINANCE_ACCESS_DATA",
                                                      "BSC_FINANCE_ACCESS_DATA_ST",
                                                      update_cols)
  
  
}
