# start <- "J:" #Comment when publishing to RConnect
# home_path <- paste0(start,"/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/")
# path_raw <- paste0(home_path, "Scorecards Final Jan2022/Files Received/Nursing/MSHS Nursing Indicators 2021 YTD.xlsx")
# 
# data <- read_excel(path_raw)

NursingSummaryRepo <- read_excel(nursing_path) # change the variable name lower_case

NursingSummaryRepo <- NursingSummaryRepo %>% 
  mutate(`All Falls (per 1,000 PD)` = as.numeric(`All Falls (per 1,000 PD)`),
         `Falls with Injury (per 1,000 PD)` = as.numeric(`Falls with Injury (per 1,000 PD)`),
         `HAPU (per 1,000 PD)` = as.numeric(`HAPU (per 1,000 PD)`))


process_nursing_data <- function(data){ #service_dept_summary
  
  
  data <- data %>%
    rename( Month =`Year-Month`,
            Service = Unit,
            Site = Facility) %>%
    mutate(Month = format(as.Date(parse_date_time(paste0(Month,"-01"),orders = "ymd")),"%Y-%m-%d"), #get rid of parse_date_time
           Service = "Nursing",
           `All Falls (per 1,000 PD)` = `All Falls`/(`Denominator (Patient Days)`/1000),
           `Falls with Injury (per 1,000 PD)` = `Falls with Injury`/(`Denominator (Patient Days)`/1000),
           `HAPU (per 1,000 PD)` = HAPU/(`Denominator (Patient Days)`/1000))
  
  return(data)
  
}

nursing__metrics_final_df_process <- function(data){
  
  
  metrics_final_df_form <- data %>%
    rename(Reporting_Month_Ref = Month,
           `Total Falls with Injury` = `Falls with Injury`,
           `Total HAPU` = HAPU,
           `Total Falls` = `All Falls`) %>%
    mutate(Reporting_Month_Ref = parse_date_time(Reporting_Month_Ref,orders = "ymd"),
           Premier_Reporting_Period = format(Reporting_Month_Ref,"%b %Y"),
           Reporting_Month = format(Reporting_Month_Ref,"%m-%Y"),
           Metric_Group = "Nursing Ops") %>%
    select(-`Denominator (Patient Days)`) %>%
    pivot_longer(cols = c(-Site,-Service,-Metric_Group,-Reporting_Month_Ref,-Premier_Reporting_Period,-Reporting_Month),
                 names_to = "Metric_Name",
                 values_to = "value_rounded")
  
  metrics_final_df_form <- left_join(metrics_final_df_form[, c("Service","Site","Metric_Group", "Metric_Name","Premier_Reporting_Period","Reporting_Month","value_rounded","Reporting_Month_Ref")],
                        target_mapping, 
                        by = c("Service","Site","Metric_Group", "Metric_Name"))
  
  metrics_final_df_form <- metrics_final_df_form %>%
    mutate(Variance = between(value_rounded, Range_1, Range_2)) %>%
    filter(Variance %in% c(TRUE,NA)) #change once new target mapping 
  
  metrics_final_df_form <- metrics_final_df_form %>% 
    select("Service","Site","Metric_Group", "Metric_Name","Premier_Reporting_Period","Reporting_Month","value_rounded","Target","Status","Reporting_Month_Ref")
  
  updated_rows <- unique(metrics_final_df_form[c("Metric_Name","Reporting_Month","Service", "Site")])
  
  
  metrics_final_df <- anti_join(metrics_final_df, updated_rows)
  
  metrics_final_df <- full_join(metrics_final_df,metrics_final_df_form)
  
  return(metrics_final_df)
  
  
  
}

# data <- process_nursing_data(data)
# data <- nursing__metrics_final_df_process(data)
