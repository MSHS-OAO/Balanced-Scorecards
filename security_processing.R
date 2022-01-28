# Source code for processing Security KPIs

# Security Incident Reports -------------
# Import historical summary
security_incident_reports <- read_excel(security_incident_reports_path)

# Reformat "Month" column in Proficiency Testing data for merging
security_incident_reports <- security_incident_reports %>%
  mutate(Month = date(Month))


# Determine last month and next month for Security Incident Reports
sec_inc_rpts_last_month <- max(security_incident_reports$Month)

# Identify Security Incident Report metrics to include in KPI breakout tab
sec_inc_rpt_metrics_incl <- metric_grouping %>%
  filter(Metric_Group %in% "Incident Reports" &
           Security %in% "Y") %>%
  select(`Data Table`, Metric_Group, Metric_Name, Metric_Name_Submitted) %>%
  distinct()

# Reformat Security Incident Reports data into wider format for manual entries
sec_inc_rpts_manual_table <- security_incident_reports %>%
  select(-Service) %>%
  filter(Month >= sec_inc_rpts_last_month - months(7) &
           Metric %in% sec_inc_rpt_metrics_incl$Metric_Name_Submitted) %>%
  arrange(Month,
          Site,
          Metric) %>%
  mutate(Month = format(Month, "%m-%Y"),
         Number = as.character(Number)) %>%
  pivot_wider(names_from = Month,
              values_from = Number) #%>%
  # Add a column with the next month for the user to enter data
  # mutate('{format(sec_inc_rpts_last_month + months(1), "%m-%Y")}' := "")


# Custom function for converting data from manual table input to Security Incident Reports Dept Summary format
sec_inc_rpts_dept_summary <- function(data) {
  sec_inc_rpts_summary <- data %>%
    # Convert from wide to long format for consistency with department summary
    pivot_longer(cols = c(-Metric, -Site),
                 names_to = "Month",
                 values_to = "Number") %>%
    mutate(
      # Change format to be consistent with department summary repo
      Number = as.numeric(Number),
      Month = as.Date(my(Month)),
      Service = "Security") %>%
    # Reorder columns
    relocate(Service) %>%
    relocate(Month, .before = Metric) %>%
    arrange(Month,
            Site,
            Metric)
      
}

# test <- sec_inc_rpts_dept_summary(sec_inc_rpts_manual_table)

# Custom function for processing and formatting department summary into metrics_final_df format
sec_inc_rpts_metrics_final_df <- function(sec_inc_rpts_summary) {
  
  # Format for metrics_final_df
  sec_inc_rpts_df <- sec_inc_rpts_summary %>%
    # Remove empty metrics
    # filter(!is.na(Number)) %>%
    arrange(Month,
            desc(Metric),
            Site) %>%
    # Start formatting for metrics_final_df format
    rename(Metric_Name_Submitted = Metric) %>%
    mutate(value_rounded = round(Number, digits = 2),
           Premier_Reporting_Period = format(Month, "%b %Y"),
           Reporting_Month = format(Month, "%m-%Y"),
           Month = NULL,
           Number = NULL)
  
  # Merge with metric group mapping data for included metrics to get
  # "Metric_Group" and "Metric_Name" columns
  sec_inc_rpts_df <- merge(sec_inc_rpts_df,
                        metric_group_mapping[c("Metric_Group",
                                               "Metric_Name",
                                               "Metric_Name_Submitted")],
                        by = c("Metric_Name_Submitted"))
  
  # Combine with target mapping to include status definitions and targets
  sec_inc_rpts_target_status <- merge(sec_inc_rpts_df[, c("Service",
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
  sec_inc_rpts_target_status <- sec_inc_rpts_target_status %>%
    mutate(Variance = between(value_rounded, Range_1, Range_2)) %>%
    filter(!is.na(Reporting_Month) &
             !(Variance %in% FALSE))
  
  # Combine two dataframes
  sec_inc_rpts_df_merge <- merge(sec_inc_rpts_df,
                                 sec_inc_rpts_target_status[, c("Service",
                                                          "Site",
                                                          "Metric_Group",
                                                          "Metric_Name",
                                                          "Reporting_Month",
                                                          "Target",
                                                          "Status")],
                              all = FALSE)
  
  # Select relevant columns
  sec_inc_rpts_df_merge <- sec_inc_rpts_df_merge[, processed_df_cols]
  
  # Add reporting month back in
  sec_inc_rpts_df_merge <- sec_inc_rpts_df_merge %>%
    mutate(Reporting_Month_Ref = as.Date(paste("01",
                                               as.yearmon(Reporting_Month,
                                                          "%m-%Y")),
                                         format = "%d %b %Y"))
  
  new_rows <- unique(sec_inc_rpts_df_merge[, c("Metric_Name",
                                            "Reporting_Month",
                                            "Service",
                                            "Site")])
  
  metrics_final_df <- anti_join(metrics_final_df,
                                new_rows)
  
  metrics_final_df <- full_join(metrics_final_df,
                                sec_inc_rpts_df_merge)
  
  metrics_final_df <- metrics_final_df %>%
    arrange(Service,
            Site,
            Metric_Group,
            Reporting_Month_Ref)
  
  return(metrics_final_df)
  
}

# Security Events -----------------------------
# Import historical summary
security_events <- read_excel(security_events_path)

# Reformat "Month" column in Proficiency Testing data for merging
security_events <- security_events %>%
  mutate(Month = date(Month))


# Determine last month and next month for Security Incident Reports
sec_events_last_month <- max(security_events$Month)

# Reformat Security Incident Reports data into wider format for manual entries
sec_events_manual_table <- security_events %>%
  select(-Service) %>%
  filter(Month >= sec_events_last_month - months(7) &
           !(Metric %in% c("Total Security Events (12-mo Rolling)"))) %>%
  arrange(Month,
          Site) %>%
  mutate(Month = format(Month, "%m-%Y"),
         Number = as.character(Number)) %>%
  pivot_wider(names_from = Month,
              values_from = Number) #%>%
  # # Add a column with the next month for the user to enter data
  # mutate('{format(sec_events_last_month + months(1), "%m-%Y")}' := "")

# Custom function for converting data from manual table input to Security Events Dept Summary format
sec_events_dept_summary <- function(data) {
  sec_events_monthly_totals <- data %>%
    # Convert from wide to long format for consistency with department summary
    pivot_longer(cols = c(-Metric, -Site),
                 names_to = "Month",
                 values_to = "Number") %>%
    mutate(
      # Change format to be consistent with department summary repo
      Number = as.numeric(Number),
      Month = as.Date(my(Month)),
      Service = "Security") %>%
    # Reorder columns
    relocate(Service) %>%
    relocate(Month, .before = Metric)
  
  # Calculate 12-mo rolling sum for data in tables ---------------
  # Find the months included in the manual table data
  manual_table_months <- sort(unique(sec_events_monthly_totals$Month))
  
    # Start by finding total security events for months not included in manual table
  # This is needed to calculate 12-month rolling sum
  sec_events_prior_months <- security_events %>%
    filter(!(Month %in% manual_table_months) &
             Metric %in% c("Total Security Events"))
  
  # Combine total security events from manual table and prior months
  total_monthly_sec_events <- rbind(sec_events_monthly_totals,
                                    sec_events_prior_months)
  
  # Split data frame into list of dataframes with last 12 months in each list item
  sec_events_12mo_rolling <- map(
    .x = manual_table_months,
    .f = ~total_monthly_sec_events %>%
      filter(Month >= (.x - months(11)) &
               Month <= .x) %>%
      dplyr::group_by(Site) %>%
      dplyr::summarize(Month = max(Month),
                       TotalSecurityEvents_12mo = sum(Number))) %>%
    # Combine list of dataframes into single dataframe
    bind_rows() %>%
    # Add relevant columns and reorder
    mutate(Service = "Security",
           Metric = "Total Security Events (12-mo Rolling)") %>%
    rename(Number = TotalSecurityEvents_12mo) %>%
    relocate(Service) %>%
    relocate(Metric, .before = Number)
  
  sec_events_summary <- rbind(sec_events_monthly_totals,
                              sec_events_12mo_rolling)
  
  return(sec_events_summary)
  
}

# Custom function for processing and formatting department summary into metrics_final_df format
sec_events_metrics_final_df <- function(sec_events_summary) {
  
  # Format for metrics_final_df
  sec_events_df <- sec_events_summary %>%
    # Remove empty metrics
    filter(!is.na(Number)) %>%
    arrange(Month,
            desc(Metric),
            Site) %>%
    # Start formatting for metrics_final_df format
    rename(Metric_Name_Submitted = Metric) %>%
    mutate(value_rounded = round(Number, digits = 2),
           Premier_Reporting_Period = format(Month, "%b %Y"),
           Reporting_Month = format(Month, "%m-%Y"),
           Month = NULL,
           Number = NULL)
  
  # Merge with metric group mapping data for included metrics to get
  # "Metric_Group" and "Metric_Name" columns
  sec_events_df <- merge(sec_events_df,
                           metric_group_mapping[c("Metric_Group",
                                                  "Metric_Name",
                                                  "Metric_Name_Submitted")],
                           by = c("Metric_Name_Submitted"))
  
  # Combine with target mapping to include status definitions and targets
  sec_events_target_status <- merge(sec_events_df[, c("Service",
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
  sec_events_target_status <- sec_events_target_status %>%
    mutate(Variance = between(value_rounded, Range_1, Range_2)) %>%
    filter(!is.na(Reporting_Month) &
             !(Variance %in% FALSE))
  
  # Combine two dataframes
  sec_events_df_merge <- merge(sec_events_df,
                               sec_events_target_status[, c("Service",
                                                                "Site",
                                                                "Metric_Group",
                                                                "Metric_Name",
                                                                "Reporting_Month",
                                                                "Target",
                                                                "Status")],
                               all = FALSE)
  
  # Select relevant columns
  sec_events_df_merge <- sec_events_df_merge[, processed_df_cols]
  
  # Add reporting month back in
  sec_events_df_merge <- sec_events_df_merge %>%
    mutate(Reporting_Month_Ref = as.Date(paste("01",
                                               as.yearmon(Reporting_Month,
                                                          "%m-%Y")),
                                         format = "%d %b %Y"))
  
  new_rows <- unique(sec_events_df_merge[, c("Metric_Name",
                                             "Reporting_Month",
                                             "Service",
                                             "Site")])
  
  metrics_final_df <- anti_join(metrics_final_df,
                                new_rows)
  
  metrics_final_df <- full_join(metrics_final_df,
                                sec_events_df_merge)
  
  metrics_final_df <- metrics_final_df %>%
    arrange(Service,
            Site,
            Metric_Group,
            Reporting_Month_Ref)
  
  return(metrics_final_df)
  
}



# # Calculate 12 month rolling sum for security events
# sec_events_rolling_df <- security_events %>%
#   filter(Site == "MSH") %>%
#   mutate(Month_12mo = Month - months(11),
#          RollSumFunction = rollsum(Number,
#                                    k = 2,
#                                    fill = NA))
# 
# sec_events_rolling_df$RollingSum <- sapply(1:nrow(sec_events_rolling_df),
#                                           # 1,
#                                           function(x) {
#                                             # paste(x)
#                                             # paste(sec_events_rolling_df$Month_12mo[x])
#                                             
#                                             
#                                             sum(sec_events_rolling_df$Number[which(sec_events_rolling_df$Month >= sec_events_rolling_df$Month_12mo[x] &
#                                                           sec_events_rolling_df$Month <= sec_events_rolling_df$Month[x])],
#                                                 na.rm = TRUE)
#                                           },
#                                           simplify = FALSE)
# 
# 
# for(i in 1:nrow(sec_events_rolling_df)) {
#   print(i)
#   sec_events_rolling_df$RollingSum[i] <- sum(
#     sec_events_rolling_df$Number[which(sec_events_rolling_df$Month >= sec_events_rolling_df$Month_12mo[i] &
#                                        sec_events_rolling_df$Month <= sec_events_rolling_df$Month[i])],
#     na.rm = TRUE)
# }


# # Code for  calculating Total Security Events 12-mo Rolling Sum for all historical data
# # Note: This should only need to be completed once and then updated each time
# # the user enters data.
# sec_events_split_df <- unique(security_events$Month) %>%
#   map(~security_events %>%
#         filter(Month >= (.x - months(11)) &
#                  Month <= .x) %>%
#         dplyr::group_by(Site) %>%
#         dplyr::summarize(Month = max(Month),
#                          TotalSecurityEvents_12mo = sum(Number))
#       ) %>%
#   bind_rows() %>%
#   mutate(Service = "Security",
#          Metric = "Total Security Events (12-mo Rolling)") %>%
#   rename(Number = TotalSecurityEvents_12mo) %>%
#   relocate(Service) %>%
#   relocate(Metric, .before = Number)
# 
# # Add rolling sum to existing historical data
# security_events <- rbind(security_events,
#                          sec_events_split_df)
# 
# # Save to department summary
# write_xlsx(security_events, security_events_path)







