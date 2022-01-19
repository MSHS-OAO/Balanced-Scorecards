# start <- "J:" #Comment when publishing to RConnect
# start <- "/SharedDrive"  #Uncomment when publishing to RConnect
# home_path <- paste0(start,"/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/")
# pt_raw_data <- paste0(home_path, "Input Data Raw/Transport/PTET.xlsx")



process_PT_data <- function(pt_data_raw){
  
  
  cols_order = c("Service",
                 "Site",
                 "Metric_Group",
                 "Metric_Name",
                 "Premier_Reporting_Period",
                 "Reporting_Month",
                 "value_rounded")
  
  hospitals <- c("MSB","MSBI","MSM","MSQ","MSSL","MSW","NYEE","MSH")
  

  rows <- nrow(pt_data_raw)


  # pt_data_raw <- pt_data_raw %>%
  #   slice(3:end)
  pt_data_raw <- read_excel(pt_data_raw)
  
  hospitals_in_data <- c(unique(pull(pt_data_raw[,"...1"])))
  
  mask <- hospitals_in_data %in% hospitals
  
  hospitals_in_data <- hospitals_in_data[mask]
  

  rownums <- c()
  
  
  for (hospital in hospitals_in_data){
    
    rownums <- append(rownums,which(pt_data_raw["...1"] == hospital))
    
  }
  
  names(rownums) <- hospitals_in_data
  
  processed_data <- list()
  # data for metrics_final_df ----
  
  for(hospital in names(rownums)){
    
    rowstart <- rownums[hospital] +1
    rowend <- rownums[hospital] +4
    

    datap <- pt_data_raw[rowstart:rowend,] %>%
      rename("Metric_Group"=`...1`)  %>%
      filter(Metric_Group %in% c("Avg TAT","PT TAT >45 min #","# Transports")) %>%
      pivot_longer(cols =-Metric_Group,
                    names_to = "Day",
                    values_to = "value_rounded") %>%
      mutate(Day =  as.Date(as.numeric(as.character(Day)),origin= "1899-12-30"),
             Site = hospital,
             Service = "Patient Transport",
             Premier_Reporting_Period = format(Day,"%b %Y"),
             value_rounded = as.numeric(value_rounded),
             Reporting_Month = format(Day,"%m-%Y")) %>%
      pivot_wider(names_from = "Metric_Group",values_from = "value_rounded",values_fill=0)  %>%
      rename(NumTransports = `# Transports`) %>%
      select(-Day)%>%
      group_by(Site,Premier_Reporting_Period,Reporting_Month,Service) %>%
      summarise(totalTransports = sum(NumTransports,na.rm=TRUE),
                              "Turnaround Time" = mean(`Avg TAT`,na.rm=TRUE),
                               transportsMoreThan45min = sum(`PT TAT >45 min #`,na.rm=TRUE)) %>%
     ungroup() %>%
     mutate("% of Trips Over 45 Minutes" = round(transportsMoreThan45min/totalTransports,2)) %>%
     select(-totalTransports,-transportsMoreThan45min) %>%
     pivot_longer(cols = c(-Site,-Premier_Reporting_Period,-Reporting_Month,-Service),
                  names_to = "Metric_Group",
                  values_to = "value_rounded") %>%
     mutate(Metric_Name = ifelse(Metric_Group == "Turnaround Time","Patient  (All Trips)","Patient")) %>%
     select(all_of(cols_order))
    

             
             
             
    processed_data[[hospital]] <- datap
    
  }
   
  processed_data <- bind_rows(processed_data)
  processed_data <- as.data.frame(processed_data)
  processed_data <- processed_data[complete.cases(processed_data), ]
  
  

  ## code for summary repo ---
  summary_repo <- list()
  
  for(hospital in names(rownums)){
    
    rowstart <- rownums[hospital] +1
    rowend <- rownums[hospital] +4
    
    
    
    datas <- pt_data_raw[rowstart:rowend,] %>%
      rename("Metric_Group"=`...1`)  %>%
      filter(Metric_Group %in% c("Avg TAT","PT TAT >45 min #","# Transports")) %>%
      pivot_longer(cols =-Metric_Group,
                   names_to = "Date",
                   values_to = "value_rounded") %>%
      mutate(Date =  as.Date(as.numeric(as.character(Date)),origin= "1899-12-30"),
             value_rounded = as.numeric(value_rounded)) %>%
      pivot_wider(names_from = "Metric_Group",values_from = "value_rounded",values_fill=0) %>%
      mutate(Site = hospital,
            `Transport Type` = "Patient",
             Month = round_date(Date,"month")) %>%
      rename("Total Transports" = `# Transports`,
             "PT TAT > 45 min"=`PT TAT >45 min #`) %>%
      select(Site,Date,Month,`Total Transports`,`Avg TAT`,`PT TAT > 45 min`,`Transport Type`) %>%
      drop_na()
  
    
    
    
    summary_repo[[hospital]] <- datas
      
  }
  
  summary_repo <- bind_rows(summary_repo)
  summary_repo <- as.data.frame(summary_repo)
  summary_repo <- summary_repo[complete.cases(summary_repo), ]    
  
  

  
  return(list(processed_data, summary_repo))

}

# summary_repos_transport <- process_PT_data(pt_raw_data)




process_NPT_raw_data <- function(data){
  
  cols_order = c("Service",
                 "Site",
                 "Metric_Group",
                 "Metric_Name",
                 "Premier_Reporting_Period",
                 "Reporting_Month",
                 "value_rounded")
  
  month = paste(unique(data$TXPORT_MONTH),unique(data$TXPORT_YEAR_NUM))

  data <- data %>%
    select(REGION_NAME,
           TXPORT_TYPE_NAME,
           TXPORT_YEAR_NUM,
           TXPORT_MONTH,
           TXPORT_MONTH_NUM,
           COMP_JOBS_COUNT,
           COMP_JOB_OUTLIER_LAST_PND_CMP_COUNT,
           COMP_JOB_OUTLIER_LAST_PND_CMP_PCT,
           AVG_LAST_PND_TO_CMP) %>%
    rename(`No of Trips Over 45 Minutes` = COMP_JOB_OUTLIER_LAST_PND_CMP_COUNT,
           `No of Transports` = COMP_JOBS_COUNT,
           `Turnaround Time` = AVG_LAST_PND_TO_CMP,
           `Site` = REGION_NAME)
  
  data$Site <- ifelse(data$Site == "MSB Region", "MSB",
                      ifelse(data$Site == "MSBI Region", "MSBI",
                             ifelse(data$Site == " MSQ Region", "MSQ",
                                    ifelse(data$Site == "MSM Region", "MSM",
                                           ifelse(data$Site == "MSW Region", "MSW",
                                                  ifelse(data$Site == "MSH Region", "MSH", NA))))))
  
  data_metrics <- data%>%
    mutate(`% of Trips Over 45 Minutes` = round((`No of Trips Over 45 Minutes`)/`No of Transports`,2),
           Month = format(as.Date(paste(month, "01"), "%b %Y %d"), "%m/%d/%Y"),
           Service = "Patient Transport") %>%
    mutate(Premier_Reporting_Period = format(as.Date(Month, format = "%m/%d/%Y"),"%b %Y"),
           Reporting_Month = format(as.Date(Month, format = "%m/%d/%Y"),"%m-%Y")) %>%
    select(Service,Site,`% of Trips Over 45 Minutes`,`Turnaround Time`,Premier_Reporting_Period,Reporting_Month) %>%
    pivot_longer(cols = c(-Service,-Site,-Premier_Reporting_Period,-Reporting_Month),
                 names_to = "Metric_Group",
                 values_to = "value_rounded") %>%
    mutate(Metric_Name = ifelse(Metric_Group == "% of Trips Over 45 Minutes","Non-Patient","Non-Patient (All Trips)"))%>%
    select(all_of(cols_order))
  
  summary_repo <- data%>%
    mutate(Month = as.Date(paste(month, "01"), "%b %Y %d"),
           Date = Month,
           "Transport Type" = "Non-Patient") %>%
    rename("PT TAT > 45 min" = "No of Trips Over 45 Minutes",
           "Total Transports" ="No of Transports",
           "Avg TAT" = "Turnaround Time") %>%
    select(Site,Date,Month,`Total Transports`,`Avg TAT`,`PT TAT > 45 min`,`Transport Type`)
  
  summary_repo <- as.data.frame(summary_repo)
  data_metrics <- as.data.frame(data_metrics)
  summary_repo <- summary_repo[complete.cases(summary_repo), ]    
  data_metrics <- data_metrics[complete.cases(data_metrics), ]    
  

  return(list(data_metrics,summary_repo))
}

transport__metrics_final_df_process <- function(data){
### Create Target Variance Column
  TAT_Transport_target_status <- left_join(data[, c("Service","Site","Metric_Group", "Metric_Name","Reporting_Month","value_rounded")],
                                     target_mapping, 
                                     by = c("Service","Site","Metric_Group", "Metric_Name"))

  TAT_Transport_target_status <- TAT_Transport_target_status %>%
    mutate(Variance = between(value_rounded, Range_1, Range_2)) %>%
    filter(Variance == TRUE)

  TAT_Transport_df_final <- merge(data, 
                            TAT_Transport_target_status[,c("Service","Site","Metric_Group","Metric_Name","Reporting_Month","Target","Status")],
                            all = TRUE)
  
  TAT_Transport_df_merge <- TAT_Transport_df_final[,processed_df_cols]
  
  
  TAT_Transport_df_merge$Reporting_Month_Ref <- as.Date(paste('01', as.yearmon(TAT_Transport_df_merge$Reporting_Month, "%m-%Y")), format='%d %b %Y')
  

  updated_rows <- unique(TAT_Transport_df_merge[c("Metric_Name","Reporting_Month","Service", "Site")])
  
  
  metrics_final_df <- anti_join(metrics_final_df, updated_rows)

  metrics_final_df <- full_join(metrics_final_df,TAT_Transport_df_merge)
  
}


# NPT_Data <- paste0(home_path,"Input Data Raw/Transport/MSHS_Transport_Metrics_Report.xlsx")
# npt_data <- read_excel(NPT_Data)

# npt_datasum <- process_NPT_raw_data(npt_data)
# transport__metrics_final_df_process(npt_datasum[[1]])
