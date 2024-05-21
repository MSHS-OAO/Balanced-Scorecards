
# data <- read.xlsx("/SharedDrive//deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Finance Backend/Cost Center Mapping Main File.xlsx",sheet=2)
# updated_user = "Dheeraj Test"


process_finance_cost_center_data <-  function(data, updated_user) {
  col_names <- make.unique(names(data))
  
  names(data) <- col_names
  
  data <- data %>%
    select(Site,ROLLUP,FSDIVISION,DIVISION,SUBDIV,Cost.Center,NAME,Entity,Fund.Type,Site.1,Cost.Center.1,Cloud.CC)%>%
    rename(SITE = Site,
           COST_CENTER = Cost.Center,
           ENTITY = Entity,
           FUND_TYPE = Fund.Type,
           SITE_CODE = Site.1,
           COST_CENTER_CODE = Cost.Center.1,
           CLOUD_CC = Cloud.CC) %>%
    mutate(UPDATED_USER = updated_user)
  
  col_names_new  <- names(data)
  update_cols <- col_names_new[col_names_new != "COST_CENTER"]
  update_cols <- update_cols[update_cols != "NAME"]
  
  write_temporary_table_to_database_and_merge_updated(data,
                                                      c("COST_CENTER", "NAME"),
                                                      "BSC_FINANCE_COST_CENTER_MAPPING",
                                                      "BSC_FINANCE_COST_CENTER_MAPPING_ST",
                                                      update_cols)
  
}

# process_finance_cost_center_data(data,updated_user)