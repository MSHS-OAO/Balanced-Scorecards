start <- "J:" #Comment when publishing to RConnect
home_path <- paste0(start,"/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/")
path_raw <- paste0(home_path, "File Examples/Nursing/MSHS Nursing Indicators 2021 YTD.xlsx")
data <- read_excel(path_raw)


process_nursing_data <- function(data,updated_user){ #service_dept_summary
  
  data <- data %>%
    rename( REPORTING_MONTH =`Year-Month`,
            SERVICE = Unit,
            SITE = Facility) %>%
    mutate(REPORTING_MONTH = as.Date(parse_date_time(paste0(REPORTING_MONTH,"-01"),orders = "ymd")), #get rid of parse_date_time
           SERVICE = "Nursing",
           `All Falls (per 1,000 PD)` = `All Falls`/(`Denominator (Patient Days)`/1000),
           `Falls with Injury (per 1,000 PD)` = `Falls with Injury`/(`Denominator (Patient Days)`/1000),
           `HAPU (per 1,000 PD)` = HAPU/(`Denominator (Patient Days)`/1000),
           PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
           REPORTING_MONTH = as.Date(format(REPORTING_MONTH,"%Y-%m-%d")),
           UPDATED_USER = updated_user) %>%
    select(-`Denominator (Patient Days)`) %>%
    pivot_longer(cols = c(-REPORTING_MONTH,-SERVICE,-SITE,-PREMIER_REPORTING_PERIOD,-UPDATED_USER),
                names_to = "METRIC_NAME_SUBMITTED",
                values_to = "VALUE")
  
  return(data)
  
}

data <- process_nursing_data(data,"TEST")



# data <- process_nursing_data(data,"TEST")
