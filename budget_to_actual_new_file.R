# data <- read_excel("J:/deans/Presidents/HSPI-PM/Operations Planning/Corporate Service Financial Reporting/Monthly supplemental detail - Balanced Scorecards/Back Office FiSRO Dashboard Sept. Steering Committee (Aug 22 YTD) BSC.xlsx", sheet = "5-BSC Cost Center Detail", skip = 3)

budget_to_actual_path_new <- paste0(home_path, "Summary Repos/Budget to Actual New.xlsx")

budget_data_repo <- read_excel(budget_to_actual_path_new)


budget_raw_file_process <- function(data){
  
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
  
  budget_total <- budget_data %>% group_by(Function, SITE, Month) %>%
                    summarise(Value = sum(as.numeric(`Sum of Month Budget`), na.rm = TRUE),
                              Value_ytd = sum(as.numeric(`Sum of YTD Budget`), na.rm = TRUE)) %>%
                    mutate(Month = paste0(Month,"01")) %>%
                    select(Function, SITE, Month, Value, Value_ytd) %>%
                    mutate(EXPTYPE = "Budget_Total") %>%
                    rename(Service = Function, 
                           Site = SITE,
                           Metric_Name_Submitted = EXPTYPE) %>%
                    mutate(Month = as.Date(Month, format = "%b%Y%d")) %>%
                    distinct()
  
  
  budget_data <- budget_data %>%
            group_by(Function, SITE, Month, EXPTYPE) %>%
            summarise(Value = sum(`Sum of Month Budget`, na.rm = T) - sum(`Sum of Month Actual`, na.rm = T),
                      Value_ytd = sum(`Sum of YTD Budget`, na.rm = T) - sum(`Sum of YTD Actual`, na.rm = T),
                      Month = paste0(Month,"01")) %>%
            select(Function, SITE, EXPTYPE, Month, Value, Value_ytd) %>%
            rename(Service = Function, 
                   Site = SITE,
                   Metric_Name_Submitted = EXPTYPE) %>%
            mutate(Month = as.Date(Month, format = "%b%Y%d")) %>%
            distinct()
            
          
        total <- budget_data %>% group_by(Service, Site, Month) %>%
                  summarise(Value = sum(Value),
                            Value_ytd = sum(Value_ytd)
                            ) %>%
                mutate(Metric_Name_Submitted = "Budget to Actual Variance - Total")
           
        budget_data_df <- full_join(budget_data, total)
        budget_data_df <- full_join(budget_data_df, budget_total)

            
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

