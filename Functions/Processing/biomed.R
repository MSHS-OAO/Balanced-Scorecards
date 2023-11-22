# function to append the new data to summary repo- KPIs & Disruptions and Issues -----
# biomed_summary_repos_KPI <- function(data,updated_user){
#   
#       summary_repo_kpi_format <- data %>%
#       rename(SITE = Site,
#              METRIC_NAME_SUBMITTED = Metric ) %>%
#       pivot_longer(cols = c(-SITE,-METRIC_NAME_SUBMITTED),
#                    names_to = "REPORTING_MONTH",
#                    values_to = "VALUE") %>%
#       mutate(REPORTING_MONTH = as.Date(format(parse_date_time(paste0("01-",REPORTING_MONTH),orders = "dmy"),"%Y-%m-%d")),
#              SERVICE = "Biomed / Clinical Engineering",
#              PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
#              #REPORTING_MONTH = format(REPORTING_MONTH,"%Y-%m-%d"),
#              UPDATED_USER = updated_user,
#              VALUE = as.numeric(VALUE))
#       
#       summary_repo_kpi_format <- as.data.frame(summary_repo_kpi_format)
#       summary_repo_kpi_format <- summary_repo_kpi_format[complete.cases(summary_repo_kpi_format), ]  
#       summary_repo_kpi_format <- as_tibble(summary_repo_kpi_format)
# 
# }


# biomed_summary_repos_DI <- function(data,updated_user){
#   
#   
#     summary_repo_di_format <- data %>%
#       rename(SITE = Site) %>%
#       select(-Metric) %>%
#       #mutate(vars(col.names.to.numeric),as.numeric()) %>%
#       pivot_longer(cols = c(-SITE),
#                    names_to = "REPORTING_MONTH",
#                    values_to = "VALUE") %>%
#       mutate(REPORTING_MONTH = as.Date(format(parse_date_time(paste0("01-",REPORTING_MONTH),orders = "dmy"),"%Y-%m-%d")),
#              SERVICE = "Biomed / Clinical Engineering",
#              METRIC_NAME_SUBMITTED = "Total Disruptions or Equipment Issues",
#              PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
#              #REPORTING_MONTH = format(REPORTING_MONTH,"%Y-%m-%d"),
#              UPDATED_USER = updated_user,
#              VALUE = as.numeric(VALUE))
#     
#     summary_repo_di_format <- as.data.frame(summary_repo_di_format)
#     summary_repo_di_format <- summary_repo_di_format[complete.cases(summary_repo_di_format), ] 
#     summary_repo_di_format <- as_tibble(summary_repo_di_format)
#     
#     
# }

#File Upload processing  ----

# Tests ----
# datapath <- "Tests/MSHS Scorecard Data - Biomed.xlsx"
# raw_data <- read.xlsx(datapath)
# updated_user <- "TEST"

process_biomed_data <- function(raw_data,updated_user){
  
  ignore_rows <- which(raw_data[,1] =="NOTE:") -1
  
  processed_data <- raw_data %>%
    slice(1:ignore_rows) %>%
    rename(`Total Disruptions or Equipment Issues` = Disruptions) %>%
    pivot_longer(cols = c("All.Medical.Equipment.PM.Completion",
                          "High.Risk.medical.equipment.PM.Completion",
                          "Documented.Status",
                          "Total Disruptions or Equipment Issues",
                          "%.Corrective.repairs.closed.<=14.days",
                          "%.RTLS.Hardware.Offline"),
                 names_to = "METRIC_NAME_SUBMITTED",
                 values_to = "VALUE") %>%
    rename(SITE = Site) %>%
    mutate(METRIC_NAME_SUBMITTED = gsub("\\."," ",METRIC_NAME_SUBMITTED),
           METRIC_NAME_SUBMITTED = capitalize(METRIC_NAME_SUBMITTED),
           SERVICE = "Biomed / Clinical Engineering",
           REPORTING_MONTH = as.Date(paste('01',`Month.(Full.Name)`,Year),format= "%d %b %Y"),
           PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH, "%b %Y"),
           UPDATED_USER = updated_user,
           METRIC_NAME_SUBMITTED = if_else(METRIC_NAME_SUBMITTED == "All Medical Equipment PM Completion",
                                           "PM Compliance - All Medical Equipment",
                                           METRIC_NAME_SUBMITTED),
           METRIC_NAME_SUBMITTED = if_else(METRIC_NAME_SUBMITTED == "High Risk Medical Equipment PM Completion",
                                           "PM Compliance - High Risk Equipment",
                                           METRIC_NAME_SUBMITTED)) %>%
    select(SERVICE,SITE,REPORTING_MONTH,PREMIER_REPORTING_PERIOD,METRIC_NAME_SUBMITTED,VALUE,UPDATED_USER)
  
  
}
