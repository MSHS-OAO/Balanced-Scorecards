# file_path <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/File for Testing 012323/Finance/Finance SystemWide/CSOR Template - 12 months.xlsx"
# data_all <- read_excel(file_path, sheet = "12 Month Pivot", skip = 3, col_types = c("text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text","numeric","numeric","numeric","numeric"))

# 
# file_path <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/File for Testing 012323/Finance/Finance SystemWide/Balanced Scorecard Update January 2024 YTD Financials v2.xlsx"
# data <- read_excel(file_path, sheet = "5-BSC Cost Center Detail", skip = 4, col_types = c("text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text" ,"numeric","numeric","numeric","numeric","numeric", "numeric" ,"text"))
# 
#   exclusions <- read_excel(file_path, sheet = "Exclusions") %>%
#                 select(-`...2`)
  
process_raw_finance_file <- function(data, updated_user, exclusions) {
  data_all <- data
  data <- data_all %>% filter(!(SITE %in% c("HSO", "MSO")))
  data <- data[!grepl("MSO:", data$Function),]
  
  ## split ed, radiology, Support Services and clinical nutrition data from the rest 
  data_rad <- data %>% filter(`Radiology?` == "Radiology")
  data_rad <- data_rad %>% mutate(Function = "Radiology")
  
  data_ed <- data %>% mutate(`Emergency Department?` = str_to_title(`Emergency Department?`)) %>% filter(`Emergency Department?` == "Emergency Department")
  data_ed <- data_ed %>% mutate(Function = "Emergency Department")
  
  
  # Contracting and Population Health include only MSO
  data_mso <- data_all[grepl("MSO:", data_all$Function),] %>%
              filter(SITE %in% ('MSO'))
  
  
  data <- data %>% filter(!(Function %in% c("Radiology", 
                                            "Emergency Department", 
                                            "Clinical Nutrition",
                                            "System CMO",
                                            "Support Services",
                                            "MSO: Contracting",
                                            "MSO: Population Health")))
  
  data_ss <- data %>% filter(Category %in% c("System CMO",
                                             "Support Services"))
  data_ss <- data_ss %>% 
    filter(!(Function %in% as.vector(exclusions$`System CMO`)) &
             !(Function %in% as.vector(exclusions$`Support Services`))) %>%
    mutate(Function =  Category)
  
  data_cn <- data %>% filter(`Clinical Nutrition?` == "Clinical Nutrition") %>%
    mutate(Function = "Clinical Nutrition")
  
  data <- bind_rows(data, data_ed, data_rad, data_cn,data_ss,data_mso)
  
  data <- data%>%
    filter(!Function %in% c('#N/A', '(blank)'))
  
  
  
  data <- data %>% mutate(Function = ifelse(Function == "Case Management", "Case Management / Social Work", Function))
  data <- data %>% mutate(Function = ifelse(Function == "Lab", "Lab and Blood Bank", Function))
  data <- data %>% mutate(Function = ifelse(Function == "Blood Bank", "Lab and Blood Bank", Function))
  # data <- data %>% mutate(Function = ifelse(Function == "MSO: Contracting", "Contracting", Function))
  # data <- data %>% mutate(Function = ifelse(Function == "MSO: Population Health", "Population Health", Function))
  
  site_data <- budget_raw_file_process_updated(data, updated_user)
  
  data <- data %>% mutate(Month_Updated = as.Date(paste0(Month, "-01"), "%B %Y-%d")) %>% select(Function, Category, SITE, CC, Name, EXPTYPE, `Sub Account`, `Sub Account Description`, `Supply Mapping File.Category`, Month_Updated, `Sum of Month Budget`, `Sum of Month Actual`, `Sum of YTD Budget`, `Sum of YTD Actual`, `Sum of Annual Budget`, `Sum of Remaining Budget YTD`) %>%
              filter(Function != "Grand Total") %>%
              mutate(across(where(is.numeric), round, digits=2)) %>%
          rename(Month = Month_Updated)
              
    colnames(data) <- toupper(colnames(data))
    colnames(data) <- gsub("[.]", " ", colnames(data))
    colnames(data) <- gsub(" ", "_", colnames(data))
  
  data <- data %>% select(FUNCTION, CATEGORY, SITE, CC, NAME, EXPTYPE, SUB_ACCOUNT, SUB_ACCOUNT_DESCRIPTION, SUPPLY_MAPPING_FILE_CATEGORY, MONTH, SUM_OF_MONTH_BUDGET, SUM_OF_MONTH_ACTUAL, SUM_OF_YTD_BUDGET, SUM_OF_YTD_ACTUAL, SUM_OF_ANNUAL_BUDGET, SUM_OF_REMAINING_BUDGET_YTD)
  
  # file_path_write <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/File for Testing 012323/Finance/Finance SystemWide/DNU/CSOR Jan 24.csv"
  # 
  # write_csv(data, file_path_write)
}



budget_raw_file_process_updated <- function(data, updated_user) {
  list_of_services <- c("Lab and Blood Bank", "Biomedical Engineering", "Emergency Department",
                        "Engineering", "Environmental Services", "Food Services", 
                        "Nursing", "Patient & Equipment Transport", "Security", 
                        "Radiology", "Perioperative Services", "Clinical Nutrition", "Case Management / Social Work")
  
  
  list_of_sites <- c("MS BI", "MS BIB", "MS STL", "MS WEST", "MSH", "MSQ")
  list_of_exptype <- c("Salaries", "Supplies")
  
  budget_data <- data %>% filter(Function %in% list_of_services) %>%
    filter(SITE %in% list_of_sites) %>%
    filter(EXPTYPE %in% list_of_exptype) %>%
    mutate_at(vars(c("SITE")), ~ifelse(SITE == "MS BI", "MSBI", 
                                       SITE)
    ) %>%
    mutate_at(vars(c("SITE")), ~ifelse(SITE == "MS BIB", "MSB", 
                                       SITE)
    ) %>%
    mutate_at(vars(c("SITE")), ~ifelse(SITE == "MS STL", "MSM", 
                                       SITE)
    ) %>%
    mutate_at(vars(c("SITE")), ~ifelse(SITE == "MS WEST", "MSW", 
                                       SITE)
    ) %>%
    mutate_at(vars(c("Function")), ~ifelse(Function == "Radiology", 
                                           "Imaging", Function)
    ) %>%
    mutate_at(vars(c("Function")), ~ifelse(Function == "Patient & Equipment Transport", 
                                           "Patient Transport", Function)
    ) %>%
    mutate_at(vars(c("Function")), ~ifelse(Function == "Emergency Department", 
                                           "ED", Function)
    ) %>%
    mutate_at(vars(c("Function")), ~ifelse(Function == "Biomedical Engineering", 
                                           "Biomed / Clinical Engineering", Function)
    ) %>%
    mutate_at(vars(c("Function")), ~ifelse(Function == "Food", 
                                           "Food Services", Function)
    ) %>%
    mutate_at(vars(c("Function")), ~ifelse(Function == "Lab and Blood Bank", 
                                           "Lab", Function)
    ) %>%
    mutate_at(vars(c("EXPTYPE")), ~ifelse(EXPTYPE == "Salaries", 
                                          "Budget to Actual Variance - Labor", 
                                          "Budget to Actual Variance - Non Labor"
    )
    ) %>%
    mutate(`Sum of Month Budget` = ifelse(is.na(`Sum of Month Budget`), 0 ,`Sum of Month Budget`), 
           `Sum of Month Actual` =  ifelse(is.na(`Sum of Month Actual`), 0 ,`Sum of Month Actual`)) 
  budget_data <- budget_data %>% fill(Month)
  
  
  budget_total_month <- budget_data %>% group_by(Function, SITE, Month) %>%
    summarise(VALUE = sum(as.numeric(`Sum of Month Budget`), na.rm = TRUE)) %>%
    mutate(Month = paste0(Month,"01")) %>%
    select(Function, SITE, Month, VALUE) %>%
    mutate(EXPTYPE = "Budget_Total (Monthly)") %>%
    rename(SERVICE = Function, 
           SITE = SITE,
           METRIC_NAME_SUBMITTED = EXPTYPE) %>%
    mutate(REPORTING_MONTH = as.Date(Month, format = "%b%Y%d")) %>%
    select(-Month) %>%
    distinct()
  
  budget_total_ytd <- budget_data %>% group_by(Function, SITE, Month) %>%
    summarise(VALUE = sum(as.numeric(`Sum of YTD Budget`), na.rm = TRUE)) %>%
    mutate(Month = paste0(Month,"01")) %>%
    select(Function, SITE, Month, VALUE) %>%
    mutate(EXPTYPE = "Budget_Total (YTD)") %>%
    rename(SERVICE = Function, 
           SITE = SITE,
           METRIC_NAME_SUBMITTED = EXPTYPE) %>%
    mutate(REPORTING_MONTH = as.Date(Month, format = "%b%Y%d")) %>%
    select(-Month) %>%
    distinct()
  
  budget_total <- bind_rows(budget_total_month, budget_total_ytd)
  
  
  budget_data_monthly <- budget_data %>%
    group_by(Function, SITE, Month, EXPTYPE) %>%
    summarise(VALUE = sum(`Sum of Month Budget`, na.rm = T) - sum(`Sum of Month Actual`, na.rm = T),
              Month = paste0(Month,"01")) %>%
    select(Function, SITE, EXPTYPE, Month, VALUE) %>%
    rename(SERVICE = Function, 
           SITE = SITE,
           METRIC_NAME_SUBMITTED = EXPTYPE) %>%
    mutate(REPORTING_MONTH = as.Date(Month, format = "%b%Y%d"),
           METRIC_NAME_SUBMITTED = paste0(METRIC_NAME_SUBMITTED, " (Monthly)")) %>%
    ungroup() %>%
    select(-Month) %>%
    distinct()
  
  budget_data_ytd <- budget_data %>%
    group_by(Function, SITE, Month, EXPTYPE) %>%
    summarise(VALUE = sum(`Sum of YTD Budget`, na.rm = T) - sum(`Sum of YTD Actual`, na.rm = T),
              Month = paste0(Month,"01")) %>%
    select(Function, SITE, EXPTYPE, Month, VALUE) %>%
    rename(SERVICE = Function, 
           SITE = SITE,
           METRIC_NAME_SUBMITTED = EXPTYPE) %>%
    mutate(REPORTING_MONTH = as.Date(Month, format = "%b%Y%d"),
           METRIC_NAME_SUBMITTED = paste0(METRIC_NAME_SUBMITTED, " (YTD)")) %>%
    ungroup() %>%
    select(-Month) %>%
    distinct()
  
  budget_data <- bind_rows(budget_data_ytd, budget_data_monthly)
  
  
  total_monthly <- budget_data %>% group_by(SERVICE, SITE, REPORTING_MONTH) %>%
    filter(grepl("(Monthly)", METRIC_NAME_SUBMITTED)) %>%
    summarise(VALUE = sum(VALUE)
    ) %>%
    mutate(METRIC_NAME_SUBMITTED = "Budget to Actual Variance - Total (Monthly)")
  
  total_ytd <- budget_data %>% group_by(SERVICE, SITE, REPORTING_MONTH) %>%
    filter(grepl("(YTD)", METRIC_NAME_SUBMITTED)) %>%
    summarise(VALUE = sum(VALUE)
    ) %>%
    mutate(METRIC_NAME_SUBMITTED = "Budget to Actual Variance - Total (YTD)")
  
  total <- bind_rows(total_monthly, total_ytd)
  
  
  budget_data_df <- full_join(budget_data, total)
  budget_data_df <- full_join(budget_data_df, budget_total)
  
  budget_data_df <- budget_data_df %>% 
    mutate(UPDATED_USER = updated_user,
           PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y")) %>%
    select(SERVICE, 
           SITE, 
           REPORTING_MONTH,
           PREMIER_REPORTING_PERIOD, 
           METRIC_NAME_SUBMITTED,
           VALUE,
           UPDATED_USER)
  
  write_temporary_table_to_database_and_merge(budget_data_df,
                                              "TEMP_BUDGET", NA)
}
