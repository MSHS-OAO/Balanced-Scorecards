# data <- read_excel("C:/Users/villea04/Desktop/Back Office FiSRO Dashboard May Steering Committee (Apr 22 YTD).xlsx", sheet = "5-BSC Cost Center Detail", skip = 3)

budget_to_actual_path_new <- paste0(home_path, "Summary Repos/Budget to Actual New.xlsx")

budget_data_repo <- read_excel(budget_to_actual_path_new)


budget_raw_file_process <- function(data, updated_user){
  
  data_rad <- data %>% filter(`Radiology?` == "Radiology")
  data_rad <- data_rad %>% mutate(Function = "Radiology")
  #data_ed <- NULL
  data_ed <- data %>% filter(`Emergency Department?` == "Emergency Department")
  data_ed <- data_ed %>% mutate(Function = "Emergency Department")
  data <- data %>% filter(!(Function %in% c("Radiology", "Emergency Department"
                                            )
                            )
                          ) #%>%
    #filter(`Radiology?` != "Radiology") #%>%
  #filter(`Emergency Department?`!= "Emergency Department")
  
  data <- bind_rows(data,data_ed,data_rad)
  
  list_of_services <- c("Lab and Blood Bank", "Biomedical Engineering", "Emergency Department",
                        "Engineering", "Environmental Services", "Food Services", 
                        "Nursing", "Patient & Equipment Transport", "Security", 
                        "Radiology")
  
  
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

