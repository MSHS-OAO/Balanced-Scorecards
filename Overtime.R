# start <- "J:" #Comment when publishing to RConnect
# # start <- "/SharedDrive"  #Uncomment when publishing to RConnect
# home_path <- paste0(start,"/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/")
# 
overtime_mapping_path <- paste0(home_path, "MSHS Scorecards Target Mapping.xlsx")
overtime_mapping <- read_excel(overtime_mapping_path, sheet = "Overtime")
# 
summary_repos_overtime_path <- paste0(home_path,"Summary Repos/Finance Overtime.xlsx")
summary_repos_overtime <- read_excel(summary_repos_overtime_path)

# data <- read_excel(paste0(home_path,"Input Data Raw/Finance/Overtime Hours/OT_extract_sample_2021_09.xlsx"))

overtime_file_processs <- function(data){
  
  data <- full_join(data,overtime_mapping)  
  
  data <- data %>% 
            filter(!(is.na(Service))) %>%
            filter(!(is.na(`Discharge Fisc Year-Period`))) %>%
            select(-Site,-VP,-`FLSA Category`, -`Uncontollable POT flag`)  %>%
            rename(Site = `Site Abbr`,
                   `Associated Dashboard Month` = `Discharge Fisc Year-Period`)
  
  data <- data %>% group_by(Site, Service, `Associated Dashboard Month`, Metric_Name) %>%
          mutate(Value = round((sum(`Actual Overtime Dollars`)/sum(`Actual Dollars`)),4)) %>%
          select(-`Cost Center Group`,-`Pay Category`,-`Actual Overtime Dollars`, -`Actual Dollars`) %>%
          distinct()
  
  data$`Associated Dashboard Month` <- as.Date(paste0(data$`Associated Dashboard Month`, "-01"), format = "%Y-%m-%d")
  #data$Metric_Name <- "Overtime Dollars - % (Finance)"
  data$Premier_Reporting_Period <- "FINANCE DATA"
  data$Metric <- "FINANCE DATA"
  
  data$Value[is.nan(data$Value)] <- 0
  
  data
  
}


overtime_metrics_final_df_process <- function(data){
  
  raw_finance_df <- data
  
  
  ## Finance overtime data pre-processing 
  finance_df_final <- raw_finance_df %>%
    rename(Metric_Name_Submitted = Metric_Name) %>%
    mutate(Reporting_Month = format(as.Date(`Associated Dashboard Month`, 
                                            format = "%Y-%m-%d"),"%m-%Y"),
           value_rounded = round(as.numeric(Value),2),
           Premier_Reporting_Period = format(as.Date(`Associated Dashboard Month`,
                                                     format = "%Y-%m-%d"),
                                             format = "%b %Y")) %>%
          filter(value_rounded != "NaN")
  
  # Subset processed data for merge 
  
  metrics_final_df <- metrics_final_df_subset_and_merge(finance_df_final)
  return(metrics_final_df)
}
