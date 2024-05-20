data <- read.xlsx("/SharedDrive//deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Finance Backend/Supply category to Sub-account mapping.xlsx")
updated_user = "Laith Test"


process_finance_supplier_mapping_data <-  function(data, updated_user) {
 
  data <- data %>%
    rename(CATEGORY = Category) %>%
    mutate(UPDATED_USER = updated_user)
  
 
  write_temporary_table_to_database_and_merge_updated(data,
                                                      "ACCTNAME",
                                                      "BSC_FINANCE_SUPPLIER_MAPPING",
                                                      "BSC_FINANCE_SUPPLIER_MAPPING_ST",
                                                      c("CATEGORY","UPDATED_USER"))
  
}

process_finance_supplier_mapping_data(data, updated_user)