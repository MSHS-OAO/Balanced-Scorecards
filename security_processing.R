# Source code for processing Security KPIs

# Security Incident Reports -------------
# Import historical summary
security_incident_reports <- read_excel(security_incident_reports_path)

# Reformat "Month" column in Proficiency Testing data for merging
security_incident_reports <- security_incident_reports %>%
  mutate(Month = date(Month))


# Determine last month and next month for Security Incident Reports
sec_inc_rpts_last_month <- max(security_incident_reports$Month)
sec_next_month <- sec_inc_rpts_last_month + months(1)

# Reformat Security Incident Reports data into wider format for manual entries
sec_inc_rpts_manual_table <- security_incident_reports %>%
  select(-Service) %>%
  filter(Month >= sec_inc_rpts_last_month - months(7)) %>%
  arrange(Month,
          Site) %>%
  mutate(Month = format(Month, "%m-%Y"),
         Number = as.character(Number)) %>%
  pivot_wider(names_from = Month,
              values_from = Number) %>%
  # Add a column with the next month for the user to enter data
  mutate('{format(sec_inc_rpts_last_month + months(1), "%m-%Y")}' := "")


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
    relocate(Month, .before = Metric)
      
}

# test <- sec_inc_rpts_dept_summary(sec_inc_rpts_manual_table)

# Custom function for processing and formatting department summary into metrics_final_df format
sec_inc_rpts_metrics_final_df <- function(sec_inc_rpts_summary) {
  
  # Format for metrics_final_df
  sec_inc_rpts_df <- sec_inc_rpts_summary %>%
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
  filter(Month >= sec_events_last_month - months(7)) %>%
  arrange(Month,
          Site) %>%
  mutate(Month = format(Month, "%m-%Y"),
         Number = as.character(Number)) %>%
  pivot_wider(names_from = Month,
              values_from = Number) %>%
  # Add a column with the next month for the user to enter data
  mutate('{format(sec_events_last_month + months(1), "%m-%Y")}' := "")

# Custom function for converting data from manual table input to Security Events Dept Summary format
sec_events_dept_summary <- function(data) {
  sec_events_summary <- data %>%
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
