# Source code for processing Security KPIs

# Security Incident Reports -------------
# Custom function for converting data from manual table input to Security Incident Reports Dept Summary format
sec_inc_rpts_dept_summary <- function(data, updated_user) {
  sec_inc_rpts_summary <- data %>%
    rename(SITE = Site,
           METRIC_NAME_SUBMITTED = Metric) %>%
    # Convert from wide to long format for consistency with department summary
    pivot_longer(cols = c(-METRIC_NAME_SUBMITTED, -SITE),
                 names_to = "REPORTING_MONTH",
                 values_to = "VALUE") %>%
    mutate(
      # Change format to be consistent with dept summary repo
      VALUE = as.numeric(VALUE),
      REPORTING_MONTH = as.Date(my(REPORTING_MONTH)),
      SERVICE = "Security",
      PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH, "%b %Y"),
      UPDATED_USER = updated_user) %>%
    # Reorder columns
    relocate(SERVICE) %>%
    relocate(REPORTING_MONTH, .before = METRIC_NAME_SUBMITTED)
  
  return(sec_inc_rpts_summary)
      
}

# test <- sec_inc_rpts_dept_summary(sec_inc_rpts_manual_table)



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
    mutate(value_rounded = Number,
           Premier_Reporting_Period = format(Month, "%b %Y"),
           Reporting_Month = format(Month, "%m-%Y"),
           Month = NULL,
           Number = NULL)
  
  # Use custom function for updating metrics_final_df using standard process
  metrics_final_df <- metrics_final_df_subset_and_merge(sec_events_df)
  
  # # Merge with metric group mapping data for included metrics to get
  # # "Metric_Group" and "Metric_Name" columns
  # sec_events_df <- merge(sec_events_df,
  #                        metric_mapping_breakout[c("Metric_Group",
  #                                                  "Metric_Name",
  #                                                  "Metric_Name_Submitted")],
  #                          # metric_group_mapping[c("Metric_Group",
  #                          #                        "Metric_Name",
  #                          #                        "Metric_Name_Submitted")],
  #                          by = c("Metric_Name_Submitted"))
  # 
  # # Select relevant columns
  # sec_events_df <- sec_events_df[, processed_df_cols]
  # 
  # # Add reporting month back in
  # sec_events_df <- sec_events_df %>%
  #   mutate(Reporting_Month_Ref = as.Date(paste("01",
  #                                              as.yearmon(Reporting_Month,
  #                                                         "%m-%Y")),
  #                                        format = "%d %b %Y"))
  # 
  # new_rows <- unique(sec_events_df[, c("Metric_Name",
  #                                      "Reporting_Month",
  #                                      "Service",
  #                                      "Site")])
  # 
  # metrics_final_df <- anti_join(metrics_final_df,
  #                               new_rows)
  # 
  # metrics_final_df <- full_join(metrics_final_df,
  #                               sec_events_df)
  # 
  # metrics_final_df <- metrics_final_df %>%
  #   arrange(Service,
  #           Site,
  #           Metric_Group,
  #           Reporting_Month_Ref)
  # 
  # return(metrics_final_df)
  
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







