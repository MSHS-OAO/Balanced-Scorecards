# start <- "J:" #Comment when publishing to RConnect
# home_path <- paste0(start,"/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/")
# xray <- paste0(home_path, "Input Data Raw/Imaging/E- CH _ 60m_ Monthly.xlsx")
# ctpath <- paste0(home_path, "Input Data Raw/Imaging/E- CT _ 60m_ Monthly.xlsx")
# 
# xraydata <- read.xlsx(xlsxFile = xray, fillMergedCells = TRUE,colNames = TRUE)
# ctdata <- read.xlsx(xlsxFile = ctpath, fillMergedCells = TRUE,colNames = TRUE)

ImagingSummaryRepo <- read_excel(imagingDR_path)



#  XRay Summary Repo Format ----
process_xray_data <- function(xraydata){
  
  current_month = format(Sys.Date(),format = "%b %Y")
  
  
  volume_info <- xraydata
  
  
  xraydata <- xraydata[, !duplicated(colnames(xraydata), fromLast = TRUE)]
  xraydata <- xraydata[2:nrow(xraydata),]
  xraydata <- xraydata %>%
    select(-X2) %>%
    rename(Month = X1) %>%
    filter(!Month==current_month) %>%
    pivot_longer(cols = -Month,
                 names_to = "Site",
                 values_to = "value_rounded") %>%
    mutate( value_rounded = as.numeric(value_rounded),
      Service="Imaging",
      Metric_Name_Submitted = "ED Chest X-Ray PA & Lateral (Exam Code CH2PAL) - Order to Scan Completed, % <= 60m")
  
  volume_info <- volume_info[, !duplicated(colnames(volume_info))]
  volume_info <- volume_info[2:nrow(volume_info),]
  volume_info <- volume_info %>%
    select(-X2) %>%
    rename(Month = X1) %>%
    filter(!Month==current_month) %>%
    pivot_longer(cols = -Month,
                 names_to = "Site",
                 values_to = "value_rounded") %>%
    mutate( value_rounded = as.numeric(value_rounded),
            Service="Imaging",
            Metric_Name_Submitted = "ED Chest X-Ray PA & Lateral (Exam Code CH2PAL) - Order to Scan Completed, Total Volume")
  
  summary_repo <- rbind(volume_info, xraydata)

}
#  CT Summary Repo Format ----
process_ctdata_data <- function(ctdata){
  
  current_month = format(Sys.Date(),format = "%b %Y")
  
  
  volume_info <- ctdata
  
  ctdata <- ctdata[, !duplicated(colnames(ctdata), fromLast = TRUE)]
  ctdata <- ctdata[2:nrow(ctdata),]
  ctdata <- ctdata %>%
    select(-X2) %>%
    rename(REPORTING_MONTH = X1) %>%
    filter(!REPORTING_MONTH==current_month) %>%
    pivot_longer(cols = -REPORTING_MONTH,
                 names_to = "SITE",
                 values_to = "VALUE") %>%
    mutate( VALUE = as.numeric(VALUE),
            SERVICE="Imaging",
            METRIC_NAME_SUBMITTED = "ED Head CT Without Contrast (Exam Code CTNHEAD0) - Ordered to Scan Completed, % <= 60m")
  
  volume_info <- volume_info[, !duplicated(colnames(volume_info))]
  volume_info <- volume_info[2:nrow(volume_info),]
  volume_info <- volume_info %>%
    select(-X2) %>%
    rename(REPORTING_MONTH = X1) %>%
    filter(!REPORTING_MONTH==current_month) %>%
    pivot_longer(cols = -Month,
                 names_to = "SITE",
                 values_to = "VALUE") %>%
    mutate( VALUE = as.numeric(VALUE),
            SERVICE="Imaging",
            METRIC_NAME_SUBMITTED = "ED Head CT Without Contrast (Exam Code CTNHEAD0) - Ordered to Scan Completed, Total Volume",
            PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
            REPORTING_MONTH = format(REPORTING_MONTH,"%Y-%m-%d"))
  
  summary_repo <- rbind(volume_info, ctdata)
  
  
}

# CT metrics_final_df processing function ----
imagingdrct__metrics_final_df_process <- function(ctdata){
  
  
  ctdata <- ctdata %>%
    rename(Premier_Reporting_Period = Month)%>%
           #Metric_Name = Metric_Name_Submitted) %>%
    mutate( Reporting_Month_Ref = as.Date(paste(Premier_Reporting_Period,"01"), format="%b %Y %d"),
            Reporting_Month = format(Reporting_Month_Ref, "%m-%Y")) %>%
    select(-Reporting_Month_Ref)
  
  # Subset processed data for merge 
  metrics_final_df <- metrics_final_df_subset_and_merge(ctdata)
  return(metrics_final_df)
  

  # ctdata <- left_join(ctdata[, c("Service","Site","Metric_Group", "Metric_Name","Premier_Reporting_Period","Reporting_Month","value_rounded","Reporting_Month_Ref")],
  #                             target_mapping, 
  #                             by = c("Service","Site","Metric_Group", "Metric_Name"))
  # 
  # 
  # ctdata <- ctdata %>%
  #   mutate(Variance = between(value_rounded, Range_1, Range_2)) %>%
  #   filter(Variance %in% c(TRUE,NA))
  # 
  # ctdata <- ctdata %>% 
  # select("Service","Site","Metric_Group", "Metric_Name","Premier_Reporting_Period","Reporting_Month","value_rounded","Target","Status","Reporting_Month_Ref")
  # 
  # updated_rows <- unique(ctdata[c("Metric_Name","Reporting_Month","Service", "Site")])
  # 
  # 
  # metrics_final_df <- anti_join(metrics_final_df, updated_rows)
  # 
  # metrics_final_df <- full_join(metrics_final_df,ctdata)
  
}
# XRay metrics_final_df processing function ----
imagingdrxray__metrics_final_df_process <- function(xraydata){
  
  xraydata <- xraydata %>%
    rename(Premier_Reporting_Period = Month)%>%
           #Metric_Name = Metric_Name_Submitted) %>%
    mutate( Reporting_Month_Ref = as.Date(paste(Premier_Reporting_Period,"01"), format="%b %Y %d"),
            Reporting_Month = format(Reporting_Month_Ref, "%m-%Y")) %>%
    select(-Reporting_Month_Ref)
  
  # Subset processed data for merge 
  metrics_final_df <- metrics_final_df_subset_and_merge(xraydata)
  return(metrics_final_df)
  
  
  
  # xraydata <- left_join(xraydata[, c("Service","Site","Metric_Group", "Metric_Name","Premier_Reporting_Period","Reporting_Month","value_rounded","Reporting_Month_Ref")],
  #                     target_mapping, 
  #                     by = c("Service","Site","Metric_Group", "Metric_Name"))
  # xraydata <- xraydata %>%
  #   mutate(Variance = between(value_rounded, Range_1, Range_2)) %>%
  #   filter(Variance %in% c(TRUE,NA))
  # 
  # xraydata <- xraydata %>% 
  #   select("Service","Site","Metric_Group", "Metric_Name","Premier_Reporting_Period","Reporting_Month","value_rounded","Target","Status","Reporting_Month_Ref")
  # 
  # updated_rows <- unique(xraydata[c("Metric_Name","Reporting_Month","Service", "Site")])
  # 
  # 
  # metrics_final_df <- anti_join(metrics_final_df, updated_rows)
  # 
  # metrics_final_df <- full_join(metrics_final_df,xraydata)
  # 
  # return(metrics_final_df)
  
}

# xraydata <- process_xray_data(xraydata)
# ctdata <- process_ctdata_data(ctdata)
# 
# xraydata <- imagingdrxray__metrics_final_df_process(xraydata)
# ctdata <- imagingdrct__metrics_final_df_process(ctdata)

