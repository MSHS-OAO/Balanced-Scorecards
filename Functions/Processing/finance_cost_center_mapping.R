
# data <- read.xlsx("/SharedDrive//deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Finance Backend/Cost Center Mapping Main File.xlsx",sheet=2)
# updated_user = "Dheeraj Test"


process_finance_cost_center_data <-  function(data, updated_user) {
  # col_names <- make.unique(names(data))
  # 
  # names(data) <- col_names
  
  data <- data %>%
    select(Site,ROLLUP,FSDIVISION,DIVISION,SUBDIV,`Cost Center`,NAME, `Corporate Service Function`, 
           `Corporate Service Category`, `Corporate Service Overlap`, `HSO CC Flag`, `MSO CC Flag`, MSBHC)%>%
    rename(SITE = Site,
           COST_CENTER = `Cost Center`,
           FUNCTION = `Corporate Service Function`,
           CATEGORY = `Corporate Service Category`,
           OVERLAP = `Corporate Service Overlap`,
           HSO_CC_FLAG = `HSO CC Flag`,
           MSO_CC_FLAG = `MSO CC Flag`) %>%
    mutate(UPDATED_USER = updated_user,
           SITE = case_when(MSBHC == 'MSBHC' ~ 'MSBHC',
                         TRUE ~ SITE),
           COST_CENTER = trimws(COST_CENTER),
           FUNCTION = str_to_title(FUNCTION)) %>%
    select(-MSBHC)
  
  
}

# process_finance_cost_center_data(data,updated_user)