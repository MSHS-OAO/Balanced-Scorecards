ImagingSummaryRepo <- read_excel(imagingDR_path)



#  XRay Summary Repo Format ----
process_xray_data <- function(xraydata, updated_user){
  
  current_month = format(Sys.Date(),format = "%b %Y")
  
  
  volume_info <- xraydata
  
  
  xraydata <- xraydata[, !duplicated(colnames(xraydata), fromLast = TRUE)]
  xraydata <- xraydata[2:nrow(xraydata),]
  xraydata <- xraydata %>%
    select(-X2) %>%
    rename(REPORTING_MONTH = X1) %>%
    filter(!REPORTING_MONTH==current_month) %>%
    mutate(REPORTING_MONTH = as.Date(paste(REPORTING_MONTH,"01"), format="%b %Y %d"))%>%
    pivot_longer(cols = -REPORTING_MONTH,
                 names_to = "SITE",
                 values_to = "VALUE") %>%
    mutate( VALUE = as.numeric(VALUE),
            SERVICE="Imaging",
            METRIC_NAME_SUBMITTED = "ED Chest X-Ray PA & Lateral (Exam Code CH2PAL) - Order to Scan Completed, % <= 60m",
            PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
            REPORTING_MONTH = as.Date(format(REPORTING_MONTH,"%Y-%m-%d"), "%Y-%m-%d"),
            UPDATED_USER = updated_user
                                      )
  
  volume_info <- volume_info[, !duplicated(colnames(volume_info))]
  volume_info <- volume_info[2:nrow(volume_info),]
  volume_info <- volume_info %>%
    select(-X2) %>%
    rename(REPORTING_MONTH = X1) %>%
    filter(!REPORTING_MONTH==current_month) %>%
    mutate(REPORTING_MONTH = as.Date(paste(REPORTING_MONTH,"01"), format="%b %Y %d"))%>%
    pivot_longer(cols = -REPORTING_MONTH,
                 names_to = "SITE",
                 values_to = "VALUE") %>%
    mutate( VALUE = as.numeric(VALUE),
            SERVICE="Imaging",
            METRIC_NAME_SUBMITTED = "ED Chest X-Ray PA & Lateral (Exam Code CH2PAL) - Order to Scan Completed, Total Volume",
            PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
            REPORTING_MONTH = as.Date(format(REPORTING_MONTH,"%Y-%m-%d"), "%Y-%m-%d"),
            UPDATED_USER = updated_user
                                      )
  
  summary_repo <- rbind(volume_info, xraydata)

}
#  CT Summary Repo Format ----
process_ctdata_data <- function(ctdata, updated_user){
  
  current_month = format(Sys.Date(),format = "%b %Y")
  
  
  volume_info <- ctdata
  
  ctdata <- ctdata[, !duplicated(colnames(ctdata), fromLast = TRUE)]
  ctdata <- ctdata[2:nrow(ctdata),]
  ctdata <- ctdata %>%
    select(-X2) %>%
    rename(REPORTING_MONTH = X1) %>%
    filter(!REPORTING_MONTH==current_month) %>%
    mutate(REPORTING_MONTH = as.Date(paste(REPORTING_MONTH,"01"), format="%b %Y %d"))%>%
    pivot_longer(cols = -REPORTING_MONTH,
                 names_to = "SITE",
                 values_to = "VALUE") %>%
    mutate( VALUE = as.numeric(VALUE),
            SERVICE="Imaging",
            METRIC_NAME_SUBMITTED = "ED Head CT Without Contrast (Exam Code CTNHEAD0) - Ordered to Scan Completed, % <= 60m",
            PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
            REPORTING_MONTH = as.Date(format(REPORTING_MONTH,"%Y-%m-%d"), "%Y-%m-%d"),
            UPDATED_USER = updated_user
            )
  
  volume_info <- volume_info[, !duplicated(colnames(volume_info))]
  volume_info <- volume_info[2:nrow(volume_info),]
  volume_info <- volume_info %>%
    select(-X2) %>%
    rename(REPORTING_MONTH = X1) %>%
    filter(!REPORTING_MONTH==current_month) %>%
    mutate(REPORTING_MONTH = as.Date(paste(REPORTING_MONTH,"01"), format="%b %Y %d"))%>%
    pivot_longer(cols = -REPORTING_MONTH,
                 names_to = "SITE",
                 values_to = "VALUE") %>%
    mutate( VALUE = as.numeric(VALUE),
            SERVICE="Imaging",
            METRIC_NAME_SUBMITTED = "ED Head CT Without Contrast (Exam Code CTNHEAD0) - Ordered to Scan Completed, Total Volume",
            PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
            REPORTING_MONTH = as.Date(format(REPORTING_MONTH,"%Y-%m-%d"), "%Y-%m-%d"),
            UPDATED_USER = updated_user
            )
  
  summary_repo <- rbind(volume_info, ctdata)
  
  
}
