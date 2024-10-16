library(dplyr)
library(magrittr)
library(lubridate)
library(openxlsx)


#data <- read.xlsx("~/Balanced-Scorecards/Test/PT Transport All Data - System Template.xlsx")
#updated_user = "Laith Test"


process_patient_transport_data <-  function(data, updated_user) {
  
  data <- data %>%
    mutate(SERVICE = "Patient & Equipment Transport") %>%
    mutate(UPDATED_USER = updated_user) %>%
    mutate(REPORTING_MONTH = as.Date(REPORTING_MONTH, origin = "1899-12-30")) %>%
    mutate(PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y")) %>%
    mutate(VALUE = round(VALUE, 3)) %>%
    rename(METRIC_NAME_SUBMITTED=METRIC_NAME_SUMMARY) %>%
    select(SERVICE,SITE,REPORTING_MONTH,METRIC_NAME_SUBMITTED,VALUE,UPDATED_USER,PREMIER_REPORTING_PERIOD) 
  
  pt_transport_mapping <- metric_mapping_database %>% filter(Service == "Patient & Equipment Transport") %>% select(Metric_Name_Submitted, Metric_Name_Summary) %>% distinct()
  
  data <- left_join(data, pt_transport_mapping, c("METRIC_NAME_SUBMITTED" = "Metric_Name_Summary"))
  
  data <- data %>% select(-METRIC_NAME_SUBMITTED) %>% rename(METRIC_NAME_SUBMITTED = Metric_Name_Submitted)
  return(data)
  
}

# data <- process_patient_transport_data(data,updated_user)
# head(data)