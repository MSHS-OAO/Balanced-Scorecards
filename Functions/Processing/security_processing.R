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
# Custom function for converting data from manual table input to Security Events Dept Summary format
sec_events_dept_summary <- function(data, updated_user) {
  
  # Convert manual table to long format
  sec_events_monthly_totals <- data %>%
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
  

  # Calculate 12-mo rolling sum for data in tables ---------------
  # Find the months included in the manual table data
  manual_table_months <- sort(unique(sec_events_monthly_totals$REPORTING_MONTH))
  
  # Start by finding total security events for months not included in manual table
  # This is needed to calculate 12-month rolling sum
  sec_events_prior_months <- summary_repo_tbl %>%
    filter(SERVICE %in% "Security" &
             METRIC_NAME_SUBMITTED %in% "Total Security Events") %>%
    collect() %>%
    select(-UPDATED_TIME) %>%
    filter(!(REPORTING_MONTH %in% manual_table_months)) %>%
    relocate(PREMIER_REPORTING_PERIOD, .after = VALUE)
  
  # Combine total security events from manual table and prior months
  total_monthly_sec_events <- rbind(sec_events_monthly_totals,
                                    sec_events_prior_months)
  
  # Split data frame into list of dataframes with last 12 months in each list item
  sec_events_12mo_rolling <- map(
    .x = manual_table_months,
    .f = ~total_monthly_sec_events %>%
      filter(REPORTING_MONTH >= (.x - months(11)) &
               REPORTING_MONTH <= .x) %>%
      dplyr::group_by(SITE) %>%
      dplyr::summarize(REPORTING_MONTH = max(REPORTING_MONTH),
                       TotalSecurityEvents_12mo = sum(VALUE))) %>%
    # Combine list of dataframes into single dataframe
    bind_rows() %>%
    # Add relevant columns and reorder
    mutate(SERVICE = "Security",
           METRIC_NAME_SUBMITTED = "Total Security Events (12-mo Rolling)",
           PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH, "%b %Y"),
           UPDATED_USER = updated_user) %>%
    rename(VALUE = TotalSecurityEvents_12mo) %>%
    relocate(SERVICE) %>%
    relocate(METRIC_NAME_SUBMITTED, .before = VALUE)
  
  sec_events_summary <- rbind(sec_events_monthly_totals,
                              sec_events_12mo_rolling)
  
  return(sec_events_summary)
  
}





