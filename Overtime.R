# start <- "J:" #Comment when publishing to RConnect
# # start <- "/SharedDrive"  #Uncomment when publishing to RConnect
# home_path <- paste0(start,"/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/")
# 
# overtime_mapping_path <- paste0(home_path, "MSHS Scorecards Target Mapping.xlsx")
# overtime_mapping <- read_excel(overtime_mapping_path, sheet = "Overtime")
# 
# summary_repos_overtime_path <- paste0(home_path,"Summary Repos/Finance Overtime.xlsx")
# summary_repos_overtime <- read_excel(summary_repos_overtime_path)

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
    mutate(Reporting_Month = format(as.Date(`Associated Dashboard Month`, format = "%Y-%m-%d"),"%m-%Y"),
           value_rounded = round(as.numeric(Value),2),
           Metric_Group = "Overtime Hours")
  
  finance_target_status <-merge(finance_df_final[, c("Service",
                                               "Site",
                                               "Metric_Group",
                                               "Metric_Name",
                                               "Reporting_Month",
                                               "value_rounded")],
                                target_mapping,
                                by.x = c("Service",
                                         "Site",
                                         "Metric_Group",
                                         "Metric_Name"),
                                by.y = c("Service",
                                         "Site",
                                         "Metric_Group",
                                         "Metric_Name"),
                                all.x = TRUE) # Target mapping
  
  finance_target_status <- finance_target_status %>%
    mutate(Variance = between(value_rounded, Range_1, Range_2)) %>%
    filter(!is.na(Reporting_Month) &
             !(Variance %in% FALSE))
  
  finance_df_final <- merge(finance_df_final, finance_target_status[,c("Service","Site","Metric_Name","Target","Status", "Reporting_Month")],
                            all = FALSE)
  
  finance_df_final <- unique(finance_df_final) # Why are duplicates created from merging operation above?
  
  finance_df_final$Premier_Reporting_Period <- format(finance_df_final$`Associated Dashboard Month`, "%b %Y")
  
  # Subset processed data for merge 
  finance_df_merge <- finance_df_final[,processed_df_cols]
  finance_df_merge$Reporting_Month_Ref <- as.Date(paste('01', as.yearmon(finance_df_merge$Reporting_Month, "%m-%Y")), format='%d %b %Y')
  
  finance_df_merge <- finance_df_merge %>%
                      filter(value_rounded != "NaN")
  
  updated_rows <- unique(finance_df_merge[c("Metric_Name","Reporting_Month","Service", "Site")])
  metrics_final_df <- anti_join(metrics_final_df, updated_rows)

  metrics_final_df <- full_join(metrics_final_df,finance_df_merge)
  
}
