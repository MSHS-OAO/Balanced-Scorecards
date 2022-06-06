#data <- read_excel("C:/Users/villea04/Desktop/Draft for BSC - Back Office FiSRO Dashboard Feb YTD.xlsx", sheet = "1-Pivot Summary by Site", skip = 5)

budget_to_actual_path_new <- paste0(home_path, "Summary Repos/Budget to Actual New.xlsx")
budget_raw_file_process <- function(data){
  list_of_services <- c("Blood Bank", "Clinical Engineering", "Emergency Department",
                        "Engineering", "Environmental Services", "Food", "Lab", 
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
                mutate_at(vars(c("Function")), ~ifelse(Function == "Clinical Engineering", 
                                                       "Biomed / Clinical Engineering", Function)
                ) %>%
                mutate_at(vars(c("Function")), ~ifelse(Function == "Food", 
                                                       "Food Services", Function)
                ) %>%
                mutate_at(vars(c("EXPTYPE")), ~ifelse(EXPTYPE == "Salaries", 
                                                      "Budget to Actual Variance - Labor", 
                                                      "Budget to Actual Variance - Non Labor"
                                                      )
                          ) %>%
                mutate_at(vars(c("EXPTYPE")), ~ifelse(EXPTYPE == "Budget to Actual Variance - Labor" & 
                                                        Function == "Lab", 
                                                       "Budget to Actual Variance - Labor (Lab)", 
                                                      EXPTYPE)
                ) %>%
                mutate_at(vars(c("EXPTYPE")), ~ifelse(EXPTYPE == "Budget to Actual Variance - Non Labor" & 
                                                      Function == "Lab", 
                                                      "Budget to Actual Variance - Non Labor (Lab)", 
                                                      EXPTYPE)
                ) %>%
              mutate_at(vars(c("EXPTYPE")), ~ifelse(EXPTYPE == "Budget to Actual Variance - Non Labor" & 
                                                      Function == "Blood Bank", 
                                                    "Budget to Actual Variance - Non Labor (Blood Bank)", 
                                                    EXPTYPE)
              ) %>%
            mutate_at(vars(c("EXPTYPE")), ~ifelse(EXPTYPE == "Budget to Actual Variance - Labor" & 
                                                    Function == "Blood Bank", 
                                                  "Budget to Actual Variance - Labor (Blood Bank)", 
                                                  EXPTYPE)
            )%>%
            mutate(Value = `Sum of Month Budget` - `Sum of Month Actual`,
                   Month = paste0(Month,"01")) %>%
            select(Function, SITE, EXPTYPE, Month, Value) %>%
            rename(Service = Function, 
                   Site = SITE,
                   Metric_Name_Submitted = EXPTYPE) %>%
          mutate(Month = as.Date(Month, format = "%b%Y%d"))
            
          
        total <- budget_data %>% group_by(Service, Site, Month) %>%
                 summarise(Value = sum(Value)) %>%
                 mutate(Metric_Name_Submitted = "Budget to Actual Variance - Total") %>%
                mutate_at(vars(c("Metric_Name_Submitted")), ~ifelse(Metric_Name_Submitted == "Budget to Actual Variance - Total" & 
                                                        Service == "Blood Bank", 
                                                      "Budget to Actual Variance - Total (Blood Bank)", 
                                                      Metric_Name_Submitted)
                ) %>%
              mutate_at(vars(c("Metric_Name_Submitted")), ~ifelse(Metric_Name_Submitted == "Budget to Actual Variance - Total" & 
                                                                    Service == "Lab", 
                                                                  "Budget to Actual Variance - Total (Lab)", 
                                                                  Metric_Name_Submitted)
              )
        budget_data_df <- full_join(budget_data, total)
        budget_data_df <- budget_data_df %>% mutate_at(vars(c("Service")), 
                                                       ~ifelse(Service == "Blood Bank", "Lab", Service)
                                                       )
            
}

budget_to_actual_metrics_final_df <- function(data){
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

