# start <- "J:" #Comment when publishing to RConnect
# home_path <- paste0(start,"/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/")
# xray <- paste0(home_path, "Input Data Raw/Imaging/E- CH _ 60m_ Monthly.xlsx")
# ctpath <- paste0(home_path, "Input Data Raw/Imaging/E- CT _ 60m_ Monthly.xlsx")
# 
# xraydata <- read.xlsx(xlsxFile = xray, fillMergedCells = TRUE,colNames = TRUE)
# ctdata <- read.xlsx(xlsxFile = ctpath, fillMergedCells = TRUE,colNames = TRUE)

ImagingSummaryRepo <- read_excel(imagingDR_path)

ImagingSummaryRepo$Month <- format(ImagingSummaryRepo$Month,format = "%b %Y")

current_month = format(Sys.Date(),format = "%b %Y")

#  XRay Summary Repo Format ----
process_xray_data <- function(xraydata){
  xraydata <- xraydata[, !duplicated(colnames(xraydata), fromLast = TRUE)]
  xraydata <- xraydata[2:nrow(xraydata),]
  xraydata <- xraydata %>%
    select(-X2) %>%
    rename(Month = X1) %>%
    filter(!Month==current_month) %>%
    pivot_longer(cols = -Month,
                 names_to = "Site",
                 values_to = "value_rounded") %>%
    mutate( value_rounded = round(as.numeric(value_rounded),2),
      Service="Imaging",
      Metric_Name_Submitted = "ED Chest X-Ray PA & Lateral Order to Scan Completed (Median TAT -Exam Code CH2PAL)")
}
#  CT Summary Repo Format ----
process_ctdata_data <- function(ctdata){
  ctdata <- ctdata[, !duplicated(colnames(ctdata), fromLast = TRUE)]
  ctdata <- ctdata[2:nrow(ctdata),]
  ctdata <- ctdata %>%
    select(-X2) %>%
    rename(Month = X1) %>%
    filter(!Month==current_month) %>%
    pivot_longer(cols = -Month,
                 names_to = "Site",
                 values_to = "value_rounded") %>%
    mutate( value_rounded = round(as.numeric(value_rounded),2),
            Service="Imaging",
            Metric_Name_Submitted = "ED Head CT Without Contrast (Exam Code CTNHEAD0) - Ordered to Scan Completed, % <= 60m")
}

# CT metrics_final_df processing function ----
imagingdrct__metrics_final_df_process <- function(ctdata){
  ctdata <- ctdata %>%
    rename(Premier_Reporting_Period = Month) %>%
    mutate( Reporting_Month_Ref = as.Date(paste(Premier_Reporting_Period,"01"), format="%b %Y %d"),
            Reporting_Month = format(Reporting_Month_Ref, "%m-%Y"),
            Metric_Group = "DR - Ops",
            Metric_Name = "ED Neuro CT Without Contrast - Ordered to Scan Completed")
  
  ctdata <- left_join(ctdata[, c("Service","Site","Metric_Group", "Metric_Name","Premier_Reporting_Period","Reporting_Month","value_rounded","Reporting_Month_Ref")],
                              target_mapping, 
                              by = c("Service","Site","Metric_Group", "Metric_Name"))
  ctdata <- ctdata %>%
    mutate(Variance = between(value_rounded, Range_1, Range_2)) %>%
    filter(Variance == TRUE)
  
  ctdata <- ctdata %>% 
  select("Service","Site","Metric_Group", "Metric_Name","Premier_Reporting_Period","Reporting_Month","value_rounded","Target","Status","Reporting_Month_Ref")
  
  updated_rows <- unique(ctdata[c("Metric_Name","Reporting_Month","Service", "Site")])
  
  
  metrics_final_df <- anti_join(metrics_final_df, updated_rows)
  
  metrics_final_df <- full_join(metrics_final_df,ctdata)
  
  return(metrics_final_df)
  
}
# XRay metrics_final_df processing function ----
imagingdrxray__metrics_final_df_process <- function(xraydata){
  
  xraydata <- xraydata %>%
    rename(Premier_Reporting_Period = Month) %>%
    mutate( Reporting_Month_Ref = as.Date(paste(Premier_Reporting_Period,"01"), format="%b %Y %d"),
            Reporting_Month = format(Reporting_Month_Ref, "%m-%Y"),
            Metric_Group = "DR - Ops",
            Metric_Name = "ED X-Ray Order to Scan Completed")
  
  xraydata <- left_join(xraydata[, c("Service","Site","Metric_Group", "Metric_Name","Premier_Reporting_Period","Reporting_Month","value_rounded","Reporting_Month_Ref")],
                      target_mapping, 
                      by = c("Service","Site","Metric_Group", "Metric_Name"))
  xraydata <- xraydata %>%
    mutate(Variance = between(value_rounded, Range_1, Range_2)) %>%
    filter(Variance == TRUE)
  
  xraydata <- xraydata %>% 
    select("Service","Site","Metric_Group", "Metric_Name","Premier_Reporting_Period","Reporting_Month","value_rounded","Target","Status","Reporting_Month_Ref")
  
  updated_rows <- unique(xraydata[c("Metric_Name","Reporting_Month","Service", "Site")])

  
  metrics_final_df <- anti_join(metrics_final_df, updated_rows)
  
  metrics_final_df <- full_join(metrics_final_df,xraydata)
  
  return(metrics_final_df)
  
}

# xraydata <- process_xray_data(xraydata)
# ctdata <- process_ctdata_data(ctdata)
# 
# xraydata <- imagingdrxray__metrics_final_df_process(xraydata)
# ctdata <- imagingdrct__metrics_final_df_process(ctdata)

