# data <- read.xlsx("/SharedDrive//deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Finance Backend/Supply category to Sub-account mapping.xlsx")
# updated_user = "Laith Test"


process_finance_supplier_mapping_data <-  function(data, updated_user) {
 
  data <- data %>%
    rename(CATEGORY = Category) %>%
    mutate(UPDATED_USER = updated_user)%>%
    distinct()
  
 
}

# process_finance_supplier_mapping_data(data, updated_user)