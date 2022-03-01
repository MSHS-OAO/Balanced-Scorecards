start <- "J:" #Comment when publishing to RConnect
home_path <- paste0(start,"/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/")
path_raw <- paste0(home_path, "Scorecards Final Jan2022/Files Received/ED/FTI1_data (13)_ED_for Dec Refresh.xlsx")

ed_data_ts <- read_excel(path_raw,sheet = "TimeStamps")
ed_data_percentiles <- read_excel(path_raw,sheet = "Percentiles")




ed_summary_repo <- read_excel(ed_path) # change the variable name lower_case


ed_dept_summary <- function(ed_data_ts,ed_data_percentiles){
  
  mapping <- tibble(`Measure Names`=c("Acuity Null",
                              "Acuity 5",
                              "Acuity 4",
                              "Acuity 3",
                              "Acuity 2",
                              "Acuity 1",
                              "SUM Admit to Depart (Boarder Hrs)",
                              "Median Admit to Depart (Boarder mins)",
                              "Median Arrival to Admit Decision",
                              "Median ED LOS Discharge (mins)",
                              "Median ED LOS Admit (mins)",
                              "LWBS",
                              "Volume",
                              "Percentile (90) of ED LOS Discharge",
                              "Percentile (90) of ED LOS Admit",
                              "Percentile (90) of Admit to Depart"), 
                    KPI=c("Acuity Null",
                          "Acuity 5",
                          "Acuity 4",
                          "Acuity 3",
                          "Acuity 2",
                          "Acuity 1",
                          "Total Boarder Hours",
                          "Admit to Depart (Median Boarder Hours)",
                          "Door to Admit (Median)",
                          "ED LOS Treat & Release (Median)",
                          "ED LOS Admit (Median)",
                          "LWBS %",
                          "Visit Volume (Epic)",
                          "ED LOS Treat&Release Patients (90th Percentile Hours)",
                          "ED LOS Admitted Patients (90th Percentile Hours)",
                          "Admit to Depart (90th Percentile Boarder Hours)"))
  
  summary_repo <- rbind(ed_data_ts,ed_data_percentiles)
  
  summary_repo <-left_join(summary_repo,
                            mapping)
  
  summary_repo <- summary_repo %>%
    rename(`Site` = `Arrv Dept (group)`,
           Month = `Month of Arrival Date`,
           Metric = `Measure Values`) %>%
    select(-`Measure Names`) %>%
    pivot_wider(names_from = "KPI",values_from = "Metric",values_fill=0) %>%
  mutate(`LWBS %` = round(`LWBS %`/`Visit Volume (Epic)`,4),
         `Admit to Depart (90th Percentile Boarder Hours)` = `Admit to Depart (90th Percentile Boarder Hours)`/60,
         `ED LOS Admitted Patients (90th Percentile Hours)` = `ED LOS Admitted Patients (90th Percentile Hours)`/60,
         `ED LOS Treat&Release Patients (90th Percentile Hours)` = `ED LOS Treat&Release Patients (90th Percentile Hours)`/60,
         `ED LOS Admit (Median)` = `ED LOS Admit (Median)`/60,
         `ED LOS Treat & Release (Median)` = `ED LOS Treat & Release (Median)`/60,
         `Door to Admit (Median)` = `Door to Admit (Median)`/60,
         `Admit to Depart (Median Boarder Hours)` = `Admit to Depart (Median Boarder Hours)`/60) %>%
    pivot_longer(cols = c(-Site,-Month),
                 names_to = "KPI",
                 values_to = "Metric")%>%
    mutate(Service = "ED")

  
}


ed__metrics_final_df_process <- function(summary_data){
  
  metrics_final_df_form <- summary_data %>%
    rename(Reporting_Month_Ref = Month,
           Metric_Name = KPI,
           value_rounded = Metric) %>%
    mutate(Reporting_Month_Ref = parse_date_time(Reporting_Month_Ref,orders = "ymd"),
           Premier_Reporting_Period = format(Reporting_Month_Ref,"%b %Y"),
           Reporting_Month = format(Reporting_Month_Ref,"%m-%Y"),
           Metric_Group = "Operational",
           Target = NA,
           Status = NA)
  
  metrics_final_df_form <- metrics_final_df_form %>% 
    select("Service","Site","Metric_Group", "Metric_Name","Premier_Reporting_Period","Reporting_Month","value_rounded","Target","Status","Reporting_Month_Ref")
  
  updated_rows <- unique(metrics_final_df_form[c("Metric_Name","Reporting_Month","Service", "Site")])
  
  
  metrics_final_df <- anti_join(metrics_final_df, updated_rows)
  
  metrics_final_df <- full_join(metrics_final_df,metrics_final_df_form)
  
  return(metrics_final_df)
  
}


data <- ed_dept_summary(ed_data_ts,ed_data_percentiles)
mdf <- ed__metrics_final_df_process(data)
