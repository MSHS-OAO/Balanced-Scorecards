#data <- read_excel("/SharedDrive//deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Group 1/Case Management/MSHS UM review for the score cards January-February 2023 with Admissions  draft 4-14-2023.xlsx", skip = 5)

case_management_function <- function(data, updated_user) {
  required_ur_review_index_end <- which(data$`Row Labels` %in% "# of Admissions with UR Review  LOS 2+")
  completed_ur_review_within_three_days_index_end <- which(data$`Row Labels` %in% "Reviews completed within 3 days from Admission")
  data_end_index <- nrow(data)
  
  required_ur_review <- data[1:required_ur_review_index_end-1,] ##truncate df for first table
  completed_ur_review <- data[required_ur_review_index_end:completed_ur_review_within_three_days_index_end-1,]
  three_day_completed_ur_review <- data[completed_ur_review_within_three_days_index_end:data_end_index,]
  
  ##Create df for all required review admisions
  required_ur_review <- required_ur_review %>% 
                        select(-`Grand Total`) %>% 
                        rename(Site = `Row Labels`) %>% 
                        pivot_longer(!Site, names_to = "Month", values_to = "Value") %>%
                        filter(Site != "Grand Total") %>% 
                        filter(!is.na(Value)) %>%
                        filter(!is.na(as.numeric(Value))) %>%
                        mutate(Metric = "Required_UR_Review",
                               Value = as.numeric(Value))
  
  ##Create df for all completed review admisions
  completed_ur_review <- completed_ur_review %>% 
    select(-`Grand Total`) %>% 
    rename(Site = `Row Labels`) %>% 
    pivot_longer(!Site, names_to = "Month", values_to = "Value") %>%
    filter(Site != "Grand Total") %>% 
    filter(!is.na(Value)) %>%
    mutate(Site = toupper(trimws(Site))) %>%
    filter(!(Site %in% c("LOS", "REVIEW TYPE", "COUNT OF ACCOUNT NUMBER", "ROW LABELS"))) %>%
    filter(!is.na(as.numeric(Value))) %>%
    mutate(Metric = "Completed_UR_Review",
           Value = as.numeric(Value))
  
  ##Create df for all completed review admisions within 3 days
  three_day_completed_ur_review <- three_day_completed_ur_review %>% 
    select(-`Grand Total`) %>% 
    rename(Site = `Row Labels`) %>% 
    pivot_longer(!Site, names_to = "Month", values_to = "Value") %>%
    filter(Site != "Grand Total") %>% 
    filter(!is.na(Value)) %>%
    mutate(Site = toupper(trimws(Site))) %>%
    filter(!(Site %in% c("LOS", "REVIEW TYPE", "COUNT OF ACCOUNT NUMBER", "ROW LABELS"))) %>%
    filter(!is.na(as.numeric(Value))) %>%
    mutate(Metric = "Three_Day_Completed_UR_Review",
           Value = as.numeric(Value))
  
  
  ##Bind 3 tables together to calculate metrics
  df <- bind_rows(required_ur_review, completed_ur_review)
  df <- bind_rows(df, three_day_completed_ur_review)
  
  ##Calulate operational metrics
  df <- df %>% pivot_wider(names_from = "Metric", values_from = "Value") 
  df <- df %>% mutate(`Reviews Never Complete` = 1-(Completed_UR_Review/Required_UR_Review),
                      `Time to First Review` = Three_Day_Completed_UR_Review/Required_UR_Review,
                      `Review Completed on Time` = Three_Day_Completed_UR_Review/Completed_UR_Review) %>%
        select(Site, Month, `Reviews Never Complete`, `Time to First Review`, `Review Completed on Time`)
  
  ##Format data frame to standard summer repo format
  df <- df %>% pivot_longer(!c("Site", "Month"), names_to = "METRIC_NAME_SUBMITTED", values_to = "VALUE") %>%
      rename(SITE = Site,
             REPORTING_MONTH = Month) %>%
      mutate(REPORTING_MONTH = as.Date(paste0(REPORTING_MONTH, " 01"), format = "%B %Y %d"),
             PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH , "%b %Y"),
             UPDATED_USER = updated_user,
             SERVICE = "Case Management / Social Work")
  
  
  
  
  
}
