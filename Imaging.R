# data <- read_excel("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/Imaging/FTI-BalancedScorecard-2021-Jan1-Nov30 (1).xlsx")
# 
# imaging_repo <- read_excel(paste0(home_path, "Summary Repos/Imaging-IR.xlsx"))

imaging_dept_summary <- function(data){
  
  data <- data %>% row_to_names(row_number = 1)
  data <- subset(data, select = -c(Measurement))

  #Figure out where MSW data begins
  site_index <- which(data$Category== "MSW Interventional Radiology Dashboard")
  
  ytd_index <- length(data)
  
  months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  year <- year(Sys.Date() %m-% months(1))
  
  months <- sprintf(paste0("%s-",year), months)
  
  ## Get the number of months in the data
  num_months <- ytd_index - 3
  colnames(data)[3:(ytd_index-1)] <- months[1:num_months]
  colnames(data)[ytd_index] <- paste0("YTD ", year)
  
  delete_rows <- which(data$Description == "Description")
  
  data <- data[-c(delete_rows),]
  
  delete_rows <- which(data$Description == "8:30A-5P, M,T,R,F and 9:30A-5P W")
  data <- data[-c(delete_rows),]
  
  data <- filter(data, rowSums(is.na(data)) != ncol(data))
  
  site_index <- which(data$Category== "MSW Interventional Radiology Dashboard")
  data$Site <- ""
  data[1:(site_index-1),length(data)] <- "MSH"
  data[site_index:nrow(data),length(data)] <- "MSW"
  
  data <- data[-c(site_index),]
  
  data <- data %>% fill(Category)
  
  data$Service <- "Imaging"
  
  data <- data %>% relocate(Site, .before = Category) %>%
                    relocate(Service, .before = (Site))
  
  data <- data[,-c(length(data))]
  
  data <- data %>%
            pivot_longer(cols = -c(Service, Site, Category, Description),
                         names_to = "Reporting_Month",
                         values_to = "value_rounded")
  
  data$Reporting_Month_Ref <- as.Date(paste0(data$Reporting_Month, "-01"), format = "%m-%Y-%d")
  
  data <- data %>% rename(Metric_Name_Submitted = Description)
  
  data$value_rounded <- round(data$value_rounded, digits = 2)
  
  data
}


imaging_metrics_final_df <- function(data){
  
  
  imaging_df <- merge(data,
                        metric_group_mapping[c("Metric_Group",
                                               "Metric_Name",
                                               "Metric_Name_Submitted")],
                        by = c("Metric_Name_Submitted"))
  
  
  
  # Combine with target mapping to include status definitions and targets
  imaging_target_status <- merge(imaging_df[, c("Service",
                                                "Site",
                                                "Metric_Group",
                                                "Metric_Name",
                                                "Reporting_Month",
                                                "value_rounded")],
                                 target_mapping,
                                 by.x = c("Service",
                                          "Site",
                                          "Metric_Group",
                                          "Metric_Name"),
                                 by.y = c("Service",
                                          "Site",
                                          "Metric_Group",
                                          "Metric_Name"),
                                 all.x = TRUE)
  
  # Determine status based on target ranges
  imaging_target_status <- imaging_target_status %>%
    mutate(Variance = between(value_rounded, Range_1, Range_2)) %>%
    filter(!is.na(Reporting_Month) &
             (Variance %in% TRUE))
  
  
  # Combine two dataframes
  imaging_df_merge <- merge(imaging_df,
                            imaging_target_status[, c("Service",
                                                      "Site",
                                                      "Metric_Group",
                                                      "Metric_Name",
                                                      "Reporting_Month",
                                                      "Target",
                                                      "Status")],
                            all.x = TRUE)
  
  imaging_df_merge$Premier_Reporting_Period <- format(imaging_df_merge$Reporting_Month_Ref, "%b-%Y")
  
  imaging_df_merge <- imaging_df_merge %>% select(-Category)
  
  # Select relevant columns
  imaging_df_merge <- imaging_df_merge[, processed_df_cols]
  
  imaging_df_merge <- imaging_df_merge %>%
    mutate(Reporting_Month_Ref = as.Date(paste("01",
                                               as.yearmon(Reporting_Month,
                                                          "%m-%Y")),
                                         format = "%d %b %Y"))
  
  
  new_rows <- unique(imaging_df_merge[, c("Metric_Name",
                                          "Reporting_Month",
                                          "Service",
                                          "Site")])
  
  metrics_final_df <- anti_join(metrics_final_df,
                                new_rows)
  
  metrics_final_df <- full_join(metrics_final_df,
                                imaging_df_merge)
  
  
}
