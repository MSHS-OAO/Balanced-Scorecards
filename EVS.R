start <- "J:" #Comment when publishing to RConnect
# start <- "/SharedDrive"  #Uncomment when publishing to RConnect
home_path <- paste0(start,"/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/")
operational_metrics_environmental_path <- paste0(home_path, "Summary Repos/TAT - EVS.xlsx")


summary_repos_environmental <- read_excel(operational_metrics_environmental_path) %>% filter(Month >= max(Month) %m-% months(12)) %>%
  mutate_if(is.logical, as.character) %>%
  mutate_if(is.double, as.character) %>%
  select(-Hospital) %>%
  pivot_longer(cols = c(-Month, -Site, -Service),
               names_to = "Metric",
               values_to = "Value") %>%
  pivot_wider(names_from = "Month", values_from = Value)


evs_file_process <- function(data, month){

  
  data <- na.omit(data, na.action = "omit")  
  
  data <- data %>%
    mutate(Hrs = ifelse(
      # First check to see if the letter h followed by a space is detected
      grepl("h\\s", TAT...4),
      # If hours are detected in the TAT, extract the characters before "h"
      # and convert to an integer
      as.integer(str_extract(TAT...4, "[0-9]*(?=h\\s)")),
      # Otherwise, set hours to 0
      0), 
      Mins = # Extract the numbers preceeding "m" and convert to integer
        as.integer(
          str_extract(TAT...4, "[0-9]+(?=m)")
        ),
      TATMin = Hrs*60 + Mins) %>%
    select(-Hrs, -Mins, -TAT...4) %>%
    rename(`Non-IsolationAverage TAT` = TATMin)
  
  data <- data %>%
    mutate(Hrs = ifelse(
      # First check to see if the letter h followed by a space is detected
      grepl("h\\s", TAT...7),
      # If hours are detected in the TAT, extract the characters before "h"
      # and convert to an integer
      as.integer(str_extract(TAT...7, "[0-9]*(?=h\\s)")),
      # Otherwise, set hours to 0
      0), 
      Mins = # Extract the numbers preceeding "m" and convert to integer
        as.integer(
          str_extract(TAT...7, "[0-9]+(?=m)")
        ),
      TATMin = Hrs*60 + Mins) %>%
    select(-Hrs, -Mins, -TAT...7) %>%
    rename(`Isolation Average TAT` = TATMin)
  
  
  
  data <- data %>%
    rename(`Non-Isolation Requests` = `Normal Requests`,
           `Non-Isolation  % > 90 mins` = `% > 90 mins...3`,
           `Isolation % > 90 mins` = `% > 90 mins...6`)
  
  data$Month <- format(as.Date(paste(month, "01"), "%b %Y %d"), "%m/%d/%Y")
  data$Site <- ifelse(data$Hospital == "Mount Sinai Bi Brooklyn", "MSB",
                      ifelse(data$Hospital == "Mount Sinai Bi Petrie", "MSBI",
                             ifelse(data$Hospital == "Mount Sinai Queens Hospital", "MSQ",
                                    ifelse(data$Hospital == "Mount Sinai St. Luke's", "MSM",
                                           ifelse(data$Hospital == "Mount Sinai Morningside", "MSM",
                                              ifelse(data$Hospital == "Mount Sinai West", "MSW",
                                                  ifelse(data$Hospital == "The Mount Sinai Hospital", "MSH", NA)))))))
  data$Service <- "Environmental Services"
  
  data <- data %>% 
    relocate(Service, .before = Hospital) %>%
    relocate(Site, .after = Hospital) %>%
    relocate(Month, .after = Site) %>%  
    select(-Hospital)
  
  data$`Non-Isolation  % > 90 mins` <- data$`Non-Isolation  % > 90 mins`*100
  data$`Isolation % > 90 mins` <- data$`Isolation % > 90 mins`*100
  
  data
}


evs__metrics_final_df_process <- function(data){
  raw_TAT_EVS_df <- data
  ## TAT - EVS processing 
  #raw_TAT_EVS_df[,5:length(raw_TAT_EVS_df)] <- sapply(raw_TAT_EVS_df[,5:length(raw_TAT_EVS_df)], as.numeric)
  TAT_EVS_df <- raw_TAT_EVS_df %>%
    mutate(`% Isolation Turns` = round((`Isolation Requests` / (`Isolation Requests` + `Non-Isolation Requests`)),2),
           `% Non-Isolation Turns` = round((`Non-Isolation Requests` / (`Isolation Requests` + `Non-Isolation Requests`)),2)) %>%
    pivot_longer(4:11,
                 names_to = "Metric_Name_Submitted",
                 values_to = "value_rounded") %>%
    mutate(value_rounded = round(value_rounded,2),
           Premier_Reporting_Period = format(as.Date(Month, format = "%m/%d/%Y"),"%b %Y"),
           Reporting_Month = format(as.Date(Month, format = "%m/%d/%Y"),"%m-%Y")) %>%
           select(-Month)
  
  TAT_EVS_df <- merge(TAT_EVS_df, metric_group_mapping[c("Metric_Group","Metric_Name","Metric_Name_Submitted")],
                      by = c("Metric_Name_Submitted"))
  
  ### Create Target Variance Column
  TAT_EVS_target_status <- merge(TAT_EVS_df[, c("Service","Site","Metric_Group", "Metric_Name","Reporting_Month","value_rounded")],
                                 target_mapping, 
                                 by.x = c("Service","Site","Metric_Group", "Metric_Name"),
                                 by.y = c("Service","Site","Metric_Group", "Metric_Name"),
                                 all = TRUE)
  
  TAT_EVS_target_status <- TAT_EVS_target_status %>%
    mutate(Variance = between(value_rounded, Range_1, Range_2)) %>% # Target mapping
    filter(!is.na(Reporting_Month)) %>%
    filter(!(Variance %in% FALSE))
  
  TAT_EVS_df_final <- merge(TAT_EVS_df, 
                            TAT_EVS_target_status[,c("Service","Site","Metric_Group","Metric_Name","Reporting_Month","Target","Status")],
                            all = FALSE)
  
  
  # Subset processed data for merge 
  TAT_EVS_df_merge <- TAT_EVS_df_final[,processed_df_cols]
  
  TAT_EVS_df_merge$Reporting_Month_Ref <- as.Date(paste('01', as.yearmon(TAT_EVS_df_merge$Reporting_Month, "%m-%Y")), format='%d %b %Y')
  
  
  updated_rows <- unique(TAT_EVS_df_merge[c("Metric_Name","Reporting_Month","Service", "Site")])
  metrics_final_df <- anti_join(metrics_final_df, updated_rows)
  
  metrics_final_df <- full_join(metrics_final_df,TAT_EVS_df_merge)
  
}


# test <- read_excel("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/EVS/MSHS Normal Clean vs Iso Clean TAT_March21.xlsx")
# month <- excel_sheets("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/EVS/MSHS Normal Clean vs Iso Clean TAT_March21.xlsx")[1]
# 
# data <- evs_file_process(test,month)
