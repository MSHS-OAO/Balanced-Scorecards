# start <- "J:" #Comment when publishing to RConnect
# home_path <- paste0(start,"/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/")
# path_raw <- paste0(home_path, "Scorecards Final Jan2022/Files Received/Nursing/MSHS Nursing Indicators 2021 YTD.xlsx")

# data <- read_excel(path_raw)

NursingSummaryRepo <- read_excel(nursing_path) # change the variable name lower_case

NursingSummaryRepo <- NursingSummaryRepo %>% 
  mutate(`All Falls (per 1,000 PD)` = as.numeric(`All Falls (per 1,000 PD)`),
         `Falls with Injury (per 1,000 PD)` = as.numeric(`Falls with Injury (per 1,000 PD)`),
         `HAPU (per 1,000 PD)` = as.numeric(`HAPU (per 1,000 PD)`))


process_nursing_data <- function(data){ #service_dept_summary
  
  
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
           REPORTING_MONTH = format(REPORTING_MONTH,"%Y-%m-%d")) %>%
    select(-`Denominator (Patient Days)`) %>%
    pivot_longer(cols = c(-REPORTING_MONTH,-SERVICE,-SITE,-PREMIER_REPORTING_PERIOD),
                names_to = "METRIC_NAME_SUBMITTED",
                values_to = "VALUE")
  
  return(data)
  
}

nursing__metrics_final_df_process <- function(data){
  
  
  metrics_final_df_form <- data %>%
    rename(`Total Falls with Injury` = `Falls with Injury`,
           `Total HAPU` = HAPU,
           `Total Falls` = `All Falls`) %>%
    mutate(#Reporting_Month_Ref = parse_date_time(Reporting_Month_Ref,orders = "ymd"),
           Premier_Reporting_Period = format(parse_date_time(Month,orders = "ymd"),"%b %Y"),
           Reporting_Month = format(parse_date_time(Month,orders = "ymd"),"%m-%Y"))%>%
           #Metric_Group = "Nursing Ops") %>%
    select(-`Denominator (Patient Days)`,-Month) %>%
    pivot_longer(cols = c(-Site,-Service,-Premier_Reporting_Period,-Reporting_Month),
                 names_to = "Metric_Name_Submitted",
                 values_to = "value_rounded")
  
  # metrics_final_df_form <- left_join(metrics_final_df_form[, c("Service","Site","Metric_Group", "Metric_Name","Premier_Reporting_Period","Reporting_Month","value_rounded","Reporting_Month_Ref")],
  #                       target_mapping, 
  #                       by = c("Service","Site","Metric_Group", "Metric_Name"))
  # 
  # metrics_final_df_form <- metrics_final_df_form %>%
  #   mutate(Variance = between(value_rounded, Range_1, Range_2)) %>%
  #   filter(Variance %in% c(TRUE,NA)) #change once new target mapping 
  
  # metrics_final_df_form <- metrics_final_df_form %>% 
  #   select("Service","Site","Metric_Group", "Metric_Name_Submitted","Reporting_Month","Premier_Reporting_Period","value_rounded","Reporting_Month_Ref")
  # 
  # updated_rows <- unique(metrics_final_df_form[c("Metric_Name","Reporting_Month","Service", "Site")])
  # 
  # 
  # metrics_final_df <- anti_join(metrics_final_df, updated_rows)
  # 
  # metrics_final_df <- full_join(metrics_final_df,metrics_final_df_form)
  
  metrics_final_df <- metrics_final_df_subset_and_merge(metrics_final_df_form)
  
  return(metrics_final_df)
  
  
  
}


# data <- process_nursing_data(data)


data <- nursing__metrics_final_df_process(NursingSummaryRepo)
