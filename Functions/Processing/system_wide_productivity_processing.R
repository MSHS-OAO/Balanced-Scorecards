# datapath <- "Tests/sytemwide_premier.xlsx"
# datapath_old <- "Tests/DeptReportBuilderRPAVG.xlsx"
# raw_data <- read_excel(datapath,skip = 2)
# raw_data_old <- read_excel(datapath_old)
# updated_user <- "Test_DNU"
productivity_processing_system_wide <- function(raw_data, updated_user) {
  
  raw_data_longer <- raw_data %>%
    pivot_longer(cols = -c("Corp Time Period","...2") ,
                 names_to = "PREMIER_REPORTING_PERIOD", values_to = "VALUE")
  
  metrics <- raw_data_longer %>%
    filter(is.na(`...2`)) %>%
    select(c(PREMIER_REPORTING_PERIOD,VALUE)) %>%
    rename("METRIC_NAME_SUBMITTED" = VALUE)
    
  raw_data_longer <- raw_data_longer %>%
    filter(!is.na(`...2`))  %>%
    left_join(metrics) %>%
    mutate( PREMIER_REPORTING_PERIOD = gsub("\\...[0-9]+", "", PREMIER_REPORTING_PERIOD),
            PREMIER_REPORTING_PERIOD = format(as.Date(PREMIER_REPORTING_PERIOD,"%m/%d/%Y"),"%m/%d/%Y"),
            YTD = str_detect(METRIC_NAME_SUBMITTED, "FYTD"),
            METRIC_NAME_SUBMITTED =  unlist(str_split(METRIC_NAME_SUBMITTED,"-"))[[1]],
            METRIC_NAME_SUBMITTED = str_squish(METRIC_NAME_SUBMITTED)) %>%
    rename("SERVICE"= `...2`) %>%
    select(-`Corp Time Period`)
  
  # Create a dataframe to map old metrics to new names - Migration to 2.0
  metrics_mapper = list(Old_Metrics = c("Actual Worked FTE",
                                        "Agency Hours",
                                        "Overtime Hours",
                                        "Total Paid hours",
                                        "Total Worked Hours",
                                        "Volume",
                                        "Overtime Percent of Paid Hours",
                                        "Worked Hours Productivity Index",
                                        "Actual Worked Hours per Unit",
                                        "Total Target Worked FTE",
                                        "Total Paid hours"),
                        New_Metrics= c("Actual Worked FTE",
                                       "Agency Hours",
                                       "Actual Overtime Hrs",
                                       "Actual Paid Hrs",
                                       "Actual Worked Hrs",
                                       "Actual Measure Amount",
                                       "Actual Overtime % of Paid Hrs",
                                       "Worked Hours Productivity Index",
                                       "Actual Worked Hrs per Unit",
                                       "Total Target Wrked FTE",
                                       "Actual Paid Hours"))
  metrics_mapper_df <- as.data.frame(metrics_mapper)
  
  #Map the metrics to data
  raw_data_longer <- left_join(raw_data_longer, 
                           metrics_mapper_df,
                           by = c("METRIC_NAME_SUBMITTED" = "New_Metrics")) %>%
    select(-METRIC_NAME_SUBMITTED) %>%
    rename("METRIC_NAME_SUBMITTED" = "Old_Metrics")
  
  report_date_mapping <- report_date_mapping %>%
    mutate(`Report Data Updated until` = format(as.Date(`Report Data Updated until`,
                                                        format ="%m/%d/%Y"),
                                                "%m/%d/%Y"),
           `Report Release Date` = format(as.Date(`Report Release Date`,
                                                  format = "%m/%d/%Y"
           ),
           "%m/%d/%Y"
           ),
           `Dashboard Month` = format(as.Date(`Dashboard Month`,
                                              format = "%m/%d/%Y"
           ), "%m/%d/%Y")
    )
  
  raw_data_longer <- left_join(raw_data_longer, 
                               report_date_mapping[,c("Report Data Updated until","Dashboard Month")],
                               by = c("PREMIER_REPORTING_PERIOD" = "Report Data Updated until"))
  
  
  raw_data_longer <- raw_data_longer %>%
    rename(REPORTING_MONTH = `Dashboard Month`) %>%
    mutate(REPORTING_MONTH = as.Date(REPORTING_MONTH, 
                                         format = "%m/%d/%Y"),
           SITE = "SYSTEM",
           METRIC_NAME_SUBMITTED = case_when( YTD ~ paste0(METRIC_NAME_SUBMITTED, " (FYTD)"),
                                              TRUE ~ METRIC_NAME_SUBMITTED),
           SERVICE = str_to_title(str_squish(str_replace(SERVICE, "SYSTEM",""))),
           SERVICE =  case_when( 
                                 # SERVICE == "Biomedical Engineering" ~ "Biomed / Clinical Engineering",
                                 # SERVICE == "Radiology" ~ "Imaging",
                                 # SERVICE == "Transport" ~ "Patient & Equipment Transport",
                                 TRUE ~ SERVICE),
           UPDATED_USER = updated_user,
           VALUE = round(as.numeric(VALUE),2))  %>%
    select(-YTD) %>%
    drop_na()
  
  
  
}


#Test ----

# processed_new_data <- productivity_processing_system_wide(raw_data, "TEST")
