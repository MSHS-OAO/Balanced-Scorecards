file_path <- "Tests/CSOR Template - 12 months.xlsx"
data <- read_excel(file_path, sheet = "12 Month Pivot", skip = 3,
                   col_types = c("text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text","numeric","numeric","numeric","numeric"))


exclusions <- read_excel(file_path, sheet = "Exclusions")
# budget_to_actual_path_new <- paste0(home_path, "Summary Repos/Budget to Actual New.xlsx")
# 
# budget_data_repo <- read_excel(budget_to_actual_path_new)


budget_raw_file_process_sw <- function(data, exclusions, updated_user){
  
  exclusions <- exclusions %>%
    select(-`...2`)
  
  
  ## split ed, radiology, Support Services and clinical nutrition data from the rest 
  data_rad <- data %>% filter(`Radiology?` == "Radiology")
  data_rad <- data_rad %>% mutate(Function = "Radiology")
  
  data_ed <- data %>% filter(`Emergency Department?` == "Emergency Department")
  data_ed <- data_ed %>% mutate(Function = "Emergency Department")
  
  
  # Contracting and Population Health include only MSO
  data_mso <- data %>%
    filter(Function %in% c('MSO: Contracting',
                           'MSO: Population Health')) %>%
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
  data <- data %>% mutate(Function = ifelse(Function == "MSO: Contracting", "Contracting", Function))
  data <- data %>% mutate(Function = ifelse(Function == "MSO: Population Health", "Population Health", Function))
  
  
  
  list_of_services <- c("Lab and Blood Bank", "Biomedical Engineering", 
                        "Emergency Department","Engineering",
                        "Environmental Services", "Food Services", 
                        "Nursing", "Patient & Equipment Transport",
                        "Security", "Radiology", 
                        "Perioperative Services", "Clinical Nutrition", 
                        "Case Management / Social Work","Contracting","Population Health")
  
  
  list_of_sites <- c("MS BI", "MS BIB", "MS STL", "MS WEST", "MSH", "MSQ", "ISM")
  list_of_exptype <- c("Salaries", "Supplies")
  
  budget_data <- data %>% 
    filter(Function %in% list_of_services) %>%
    filter(SITE %in% list_of_sites) %>%
    filter(EXPTYPE %in% list_of_exptype) %>%
    mutate(
      
      SITE = case_when(
      SITE =="MS BI" ~ "MSBI",
      SITE == "MS BIB" ~ "MSB",
      SITE == "MS STL" ~ "MSM",
      SITE == "MS WEST" ~ "MSW",
      TRUE ~  SITE),
      
    Function = case_when(
      Function == "Radiology" ~ "Imaging",
      Function == "Patient & Equipment Transport" ~ "Patient Transport",
      Function == "Emergency Department" ~ "ED",
      Function == "Biomedical Engineering" ~  "Biomed / Clinical Engineering",
      Function == "Food" ~ "Food Services",
      Function == "Lab and Blood Bank" ~ "Lab",
      TRUE ~ Function
    ),
    
    EXPTYPE = if_else(EXPTYPE == "Salaries",
                      "Budget to Actual Variance - Labor",
                      "Budget to Actual Variance - Non Labor")) %>%
    mutate(`Sum of Month Budget` = ifelse(is.na(`Sum of Month Budget`), 0 ,`Sum of Month Budget`), 
           `Sum of Month Actual` =  ifelse(is.na(`Sum of Month Actual`), 0 ,`Sum of Month Actual`)) 
  
    # mutate_at(vars(c("SITE")), ~ifelse(SITE == "MS BI", "MSBI", 
    #                                    SITE)
    # ) %>%
    # mutate_at(vars(c("SITE")), ~ifelse(SITE == "MS BIB", "MSB", 
    #                                    SITE)
    # ) %>%
    # mutate_at(vars(c("SITE")), ~ifelse(SITE == "MS STL", "MSM", 
    #                                    SITE)
    # ) %>%
    # mutate_at(vars(c("SITE")), ~ifelse(SITE == "MS WEST", "MSW", 
    #                                    SITE)
    # ) %>%
    # mutate_at(vars(c("Function")), ~ifelse(Function == "Radiology", 
    #                                        "Imaging", Function)
    # ) %>%
    # mutate_at(vars(c("Function")), ~ifelse(Function == "Patient & Equipment Transport", 
    #                                        "Patient Transport", Function)
    # ) %>%
    # mutate_at(vars(c("Function")), ~ifelse(Function == "Emergency Department", 
    #                                        "ED", Function)
    # ) %>%
    # mutate_at(vars(c("Function")), ~ifelse(Function == "Biomedical Engineering", 
    #                                        "Biomed / Clinical Engineering", Function)
    # ) %>%
    # mutate_at(vars(c("Function")), ~ifelse(Function == "Food", 
    #                                        "Food Services", Function)
    # ) %>%
    # mutate_at(vars(c("Function")), ~ifelse(Function == "Lab and Blood Bank", 
    #                                        "Lab", Function)
    # ) %>%
    # mutate_at(vars(c("EXPTYPE")), ~ifelse(EXPTYPE == "Salaries", 
    #                                       "Budget to Actual Variance - Labor", 
    #                                       "Budget to Actual Variance - Non Labor"
    # )
    # ) %>%
  # budget_data <- budget_data %>% fill(Month) --- Ask Armando
  
  
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
  
  
  budget_data_monthly_percent <- budget_data %>%
    group_by(Function, SITE, Month, EXPTYPE) %>%
    summarise(VALUE = (sum(`Sum of Month Budget`, na.rm = T) - sum(`Sum of Month Actual`, na.rm = T))/sum(`Sum of Month Budget`, na.rm = T),
              Month = paste0(Month,"01")) %>%
    select(Function, SITE, EXPTYPE, Month, VALUE) %>%
    rename(SERVICE = Function, 
           SITE = SITE,
           METRIC_NAME_SUBMITTED = EXPTYPE) %>%
    mutate(REPORTING_MONTH = as.Date(Month, format = "%b%Y%d"),
           METRIC_NAME_SUBMITTED = ifelse(METRIC_NAME_SUBMITTED == "Budget to Actual Variance - Labor", "Budget to Actual Percent Variance - Labor" , "Budget to Actual Percent Variance - Non Labor"),
           METRIC_NAME_SUBMITTED = paste0(METRIC_NAME_SUBMITTED, "  (Monthly)")) %>%
    ungroup() %>%
    select(-Month) %>%
    distinct()

  
  budget_data_ytd <- budget_data %>%
    group_by(Function, SITE, Month, EXPTYPE) %>%
    summarise(VALUE = (sum(`Sum of YTD Budget`, na.rm = T) - sum(`Sum of YTD Actual`, na.rm = T))/sum(`Sum of YTD Budget`, na.rm = T),
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
  
  budget_data_ytd_percent <- budget_data %>%
    group_by(Function, SITE, Month, EXPTYPE) %>%
    summarise(VALUE = sum(`Sum of YTD Budget`, na.rm = T) - sum(`Sum of YTD Actual`, na.rm = T),
              Month = paste0(Month,"01")) %>%
    select(Function, SITE, EXPTYPE, Month, VALUE) %>%
    rename(SERVICE = Function, 
           SITE = SITE,
           METRIC_NAME_SUBMITTED = EXPTYPE) %>%
    mutate(REPORTING_MONTH = as.Date(Month, format = "%b%Y%d"),
           METRIC_NAME_SUBMITTED = ifelse(METRIC_NAME_SUBMITTED == "Budget to Actual Variance - Labor", "Budget to Actual Percent Variance - Labor" , "Budget to Actual Percent Variance - Non Labor"),
           METRIC_NAME_SUBMITTED = paste0(METRIC_NAME_SUBMITTED, " (YTD)")) %>%
    ungroup() %>%
    select(-Month) %>%
    distinct()
  
  
  budget_data <- bind_rows(budget_data_ytd, 
                           budget_data_monthly, 
                           budget_data_ytd_percent, 
                           budget_data_monthly_percent)
  
  
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
  
  
  
}

budget_to_actual_metrics_final_df <- function(data){
  data <- data %>% group_by(Service, Site, Month, Metric_Name_Submitted) %>% summarise(Value = sum(Value, na.rm = T))
  
  if(!("Premier_Reporting_Period" %in% names(data))){
    data <- data %>% mutate(Premier_Reporting_Period = format(as.Date(Month, format = "%Y-%m-%d"),"%b %Y"))
  }
  
  if(!("Reporting_Month" %in% names(data))){
    data <- data %>% mutate(Reporting_Month = format(as.Date(Month, format = "%Y-%m-%d"),"%m-%Y"))
  }
  
  if(!("value_rounded" %in% names(data))){
    data <- data %>% rename(value_rounded = Value)
  }
  metrics_final_df_subset_and_merge(data)
}

