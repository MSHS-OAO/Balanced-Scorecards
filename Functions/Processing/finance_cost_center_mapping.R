
# data <- read.xlsx("/SharedDrive//deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Finance Backend/Cost Center Mapping Main File.xlsx",sheet=2)
# updated_user = "Dheeraj Test"


process_finance_cost_center_data <-  function(data, updated_user) {
  # col_names <- make.unique(names(data))
  # 
  # names(data) <- col_names
  
  data <- data %>%
    select(Site...1,ROLLUP,FSDIVISION,DIVISION,SUBDIV,`Cost Center...6`,NAME,Entity,`Fund Type`,Site...18,`Cost Center...19`,`Cloud CC`, `Corporate Service Function`, 
           `Corporate Service Category`, `Corporate Service Overlap`, `HSO CC Flag`, `MSO CC Flag`)%>%
    rename(SITE = Site...1,
           COST_CENTER = `Cost Center...6`,
           ENTITY = Entity,
           FUND_TYPE = `Fund Type`,
           SITE_CODE = Site...18,
           COST_CENTER_CODE = `Cost Center...19`,
           CLOUD_CC = `Cloud CC`,
           FUNCTION = `Corporate Service Function`,
           CATEGORY = `Corporate Service Category`,
           `Corporate Service Overlap` = OVERLAP,
           `HSO CC Flag` = HSO_CC_FLAG,
           `MSO CC Flag` = MSO_CC_FLAG) %>%
    mutate(UPDATED_USER = updated_user)
  
  
}

# process_finance_cost_center_data(data,updated_user)