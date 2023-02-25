# data <- read_excel("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/File for Testing 012323/Transport/Support Services Data Collection Template v2.xlsx",
#                    sheet = "PTET")

process_PT_data <- function(pt_data_raw,updated_user){
  
  current_month <- floor_date(Sys.Date(),"month")

  hospitals <- c("MSB","MSBI","MSM","MSW","MSH")
  

  rows <- nrow(pt_data_raw)

  # pt_data_raw <- pt_data_raw %>%
  #   slice(3:end)
  # pt_data_raw <- read_excel(pt_data_raw, sheet = "PTET")
  # view(pt_data_raw)
  
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
             Reporting_Month = as.Date(format(Day,"%Y-%m-%d"))) %>%
      filter(Day<current_month) %>%
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
     mutate(Metric_Name = ifelse(Metric_Group == "Turnaround Time","Patient  (All Trips)","Patient"),
            UPDATED_USER = updated_user) %>%
      rename(SERVICE = Service,
             SITE = Site,
             PREMIER_REPORTING_PERIOD = Premier_Reporting_Period,
             REPORTING_MONTH = Reporting_Month,
             VALUE = value_rounded,
             METRIC_NAME_SUBMITTED = Metric_Name) %>%
      select(SERVICE,SITE,PREMIER_REPORTING_PERIOD,REPORTING_MONTH,VALUE,UPDATED_USER,METRIC_NAME_SUBMITTED)

    processed_data[[hospital]] <- datap
    
  }
   
  processed_data <- bind_rows(processed_data)
  processed_data <- as.data.frame(processed_data)
  processed_data <- processed_data[complete.cases(processed_data), ]
  
  processed_data <- processed_data %>%
    mutate(VALUE = replace(VALUE,which(is.infinite(processed_data$VALUE),arr.ind = TRUE),NA))
  
  trips_data <- processed_data %>%
    filter(METRIC_NAME_SUBMITTED == 'Patient  (All Trips)') %>%
    select(-REPORTING_MONTH) %>%
    group_by(SERVICE,SITE,PREMIER_REPORTING_PERIOD,UPDATED_USER,METRIC_NAME_SUBMITTED) %>%
    summarise(VALUE = mean(VALUE)) %>%
    ungroup() %>%
    mutate(REPORTING_MONTH =  as.Date(paste(PREMIER_REPORTING_PERIOD,"01"), format="%b %Y %d"))%>%
    select(SERVICE,SITE,PREMIER_REPORTING_PERIOD,REPORTING_MONTH,VALUE,UPDATED_USER,METRIC_NAME_SUBMITTED)
  
  tat_data <- processed_data %>%
    filter(METRIC_NAME_SUBMITTED == 'Patient') %>%
    select(-REPORTING_MONTH) %>%
    group_by(SERVICE,SITE,PREMIER_REPORTING_PERIOD,UPDATED_USER,METRIC_NAME_SUBMITTED) %>%
    summarise(VALUE = mean(VALUE)) %>%
    ungroup() %>%
    mutate(REPORTING_MONTH =  as.Date(paste(PREMIER_REPORTING_PERIOD,"01"), format="%b %Y %d"))%>%
    select(SERVICE,SITE,PREMIER_REPORTING_PERIOD,REPORTING_MONTH,VALUE,UPDATED_USER,METRIC_NAME_SUBMITTED)
  
  processed_data <- rbind(trips_data,tat_data)
  
  return(processed_data)

}
process_NPT_raw_data <- function(data,updated_user){
  
  
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
           `Site` = REGION_NAME)%>%
    mutate(Site = tolower(Site))
  
  data$Site <- ifelse(data$Site == "msb region", "MSB",
                      ifelse(data$Site == "msbi region", "MSBI",
                             ifelse(data$Site == "msq region", "MSQ",
                                    ifelse(data$Site == "msm region", "MSM",
                                           ifelse(data$Site == "msw region", "MSW",
                                                  ifelse(data$Site == "msh region", "MSH", NA))))))
  
  data_metrics <- data%>%
    mutate(`% of Trips Over 45 Minutes` = `No of Trips Over 45 Minutes`/`No of Transports`,
           Month = format(as.Date(paste(month, "01"), "%b %Y %d"), "%m/%d/%Y"),
           Service = "Patient Transport") %>%
    mutate(Premier_Reporting_Period = format(as.Date(Month, format = "%m/%d/%Y"),"%b %Y"),
           Reporting_Month = as.Date(format(as.Date(Month, format = "%m/%d/%Y"),"%Y-%m-%d"))) %>%
    select(Service,Site,`% of Trips Over 45 Minutes`,`Turnaround Time`,Premier_Reporting_Period,Reporting_Month) %>%
    pivot_longer(cols = c(-Service,-Site,-Premier_Reporting_Period,-Reporting_Month),
                 names_to = "Metric_Group",
                 values_to = "value_rounded") %>%
    mutate(Metric_Name = ifelse(Metric_Group == "% of Trips Over 45 Minutes","Non-Patient","Non-Patient (All Trips)"),
           UPDATED_USER = updated_user)%>%
    rename(SERVICE = Service,
           SITE = Site,
           PREMIER_REPORTING_PERIOD = Premier_Reporting_Period,
           REPORTING_MONTH = Reporting_Month,
           VALUE = value_rounded,
           METRIC_NAME_SUBMITTED = Metric_Name) %>%
    select(SERVICE,SITE,PREMIER_REPORTING_PERIOD,REPORTING_MONTH,VALUE,UPDATED_USER,METRIC_NAME_SUBMITTED)

  return(data_metrics)
}