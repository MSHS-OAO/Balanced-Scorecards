library(dplyr)
library(magrittr)
library(lubridate)

# data <- read.csv("/SharedDrive//deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Finance Backend/Patient Transport Jan-Apr 2024 data.csv")
# updated_user = "Laith Test"


process_patient_transport_data <-  function(data, updated_user) {
  
  data <- data %>%
    mutate(SERVICE = "Patient & Equipment Transport")
    mutate(UPDATED_USER = updated_user) %>%
    mutate(REPORTING_MONTH = as.Date(REPORTING_MONTH, format = "%Y-%m-%d")) %>%
    mutate(PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y")) %>%
    mutate(VALUE = round(VALUE, 3)) %>%
    rename(METRIC_NAME_SUBMITTED=METRIC_NAME_SUMMARY) %>%
    select(SERVICE,SITE,REPORTING_MONTH,METRIC_NAME_SUBMITTED,VALUE,UPDATED_USER,PREMIER_REPORTING_PERIOD) 
  
return(data)
  
}

# data <- process_patient_transport_data(data,updated_user)
# head(data)