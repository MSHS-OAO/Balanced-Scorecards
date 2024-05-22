
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
  
  
}

# process_finance_cost_center_data(data,updated_user)