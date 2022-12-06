# start <- "J:" #Comment when publishing to RConnect
# # start <- "/SharedDrive"  #Uncomment when publishing to RConnect
# home_path <- paste0(start,"/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/")
# 
overtime_mapping_path <- paste0(home_path, "MSHS Scorecards Target Mapping 2022-04-13.xlsx")
overtime_mapping <- read_excel(overtime_mapping_path, sheet = "Overtime")
# 
summary_repos_overtime_path <- paste0(home_path,"Summary Repos/Finance Overtime.xlsx")
summary_repos_overtime <- read_excel(summary_repos_overtime_path)

# data <- read_excel(paste0(home_path,"Input Data Raw/Finance/Overtime Hours/OT_extract_sample_2021_09.xlsx"))

overtime_file_processs <- function(data, updated_user){
  
  data <- full_join(data,overtime_mapping)  
  
  data <- data %>% 
            filter(!(is.na(Service))) %>%
            filter(!(is.na(`Discharge Fisc Year-Period`))) %>%
            select(-Site,-VP,-`FLSA Category`, -`Uncontollable POT flag`)  %>%
            rename(Site = `Site Abbr`,
                   `Associated Dashboard Month` = `Discharge Fisc Year-Period`)
  
  data <- data %>% group_by(Site, Service, `Associated Dashboard Month`, Metric_Name) %>%
          mutate(Value = (sum(`Actual Overtime Dollars`)/sum(`Actual Dollars`))) %>%
          select(-`Cost Center Group`,-`Pay Category`,-`Actual Overtime Dollars`, -`Actual Dollars`) %>%
          distinct()
  
  data$`Associated Dashboard Month` <- as.Date(paste0(data$`Associated Dashboard Month`, "-01"), format = "%Y-%m-%d")
  #data$Metric_Name <- "Overtime Dollars - % (Finance)"
  data$Premier_Reporting_Period <- "FINANCE DATA"
  data$Metric <- "FINANCE DATA"
  
  data$Value[is.nan(data$Value)] <- 0
  
  data <- data %>% rename(SERVICE = Service,
                          SITE = Site,
                          REPORTING_MONTH = `Associated Dashboard Month`,
                          METRIC_NAME_SUBMITTED = Metric_Name,
                          VALUE = Value) %>%
                          mutate(PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH, "%b %Y"),
                                 UPDATED_USER = updated_user) %>%
                          select(SERVICE, 
                                 SITE, 
                                 REPORTING_MONTH,
                                 PREMIER_REPORTING_PERIOD, 
                                 METRIC_NAME_SUBMITTED,
                                 VALUE,
                                 UPDATED_USER)
  data
  
}


overtime_metrics_final_df_process <- function(data){
  
  raw_finance_df <- data
  
  
  ## Finance overtime data pre-processing 
  finance_df_final <- raw_finance_df %>%
    rename(Metric_Name_Submitted = Metric_Name) %>%
    mutate(Reporting_Month = format(as.Date(`Associated Dashboard Month`, 
                                            format = "%Y-%m-%d"),"%m-%Y"),
           value_rounded = as.numeric(Value),
           Premier_Reporting_Period = format(as.Date(`Associated Dashboard Month`,
                                                     format = "%Y-%m-%d"),
                                             format = "%b %Y")) %>%
          filter(value_rounded != "NaN")
  
  # Subset processed data for merge 
  
  metrics_final_df <- metrics_final_df_subset_and_merge(finance_df_final)
  return(metrics_final_df)
}
