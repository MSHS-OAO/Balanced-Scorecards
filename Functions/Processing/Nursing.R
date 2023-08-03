# datapath_new <- "Tests/MSHS Nursing Indicators 2021-2023 YTD (2023.07.07).xlsx"
# raw_data_new <- read_excel(datapath_new)
# updated_user <- "Test_DNU"

process_nursing_data <- function(data,updated_user){ #service_dept_summary
  
  data <- data %>%
    rename( REPORTING_MONTH =`Year-Month`,
            SERVICE = Unit,
            SITE = Facility) %>%
    mutate(REPORTING_MONTH = as.Date(paste0(REPORTING_MONTH,"-01"),"%Y-%m-%d"),
           SERVICE = "Nursing",
           `All Falls (per 1,000 PD)` = (`All Falls`/`Denominator (Patient Days)`)*1000,
           `Falls with Injury (per 1,000 PD)` = (`Falls with Injury`/`Denominator (Patient Days)`)*1000,
           `HAPU (per 1,000 PD)` = (HAPU/Denominator_HAPU)*1000,
           PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
           REPORTING_MONTH = as.Date(format(REPORTING_MONTH,"%Y-%m-%d")),
           UPDATED_USER = updated_user) %>%
    select(-`Denominator (Patient Days)`,-Denominator_HAPU) %>%
    pivot_longer(cols = c(-REPORTING_MONTH,-SERVICE,-SITE,-PREMIER_REPORTING_PERIOD,-UPDATED_USER),
                 names_to = "METRIC_NAME_SUBMITTED",
                 values_to = "VALUE")
  
  return(data)
  
}

#processed_data <- process_nursing_data(raw_data_new,updated_user)