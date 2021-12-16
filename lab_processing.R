# Code for processing Lab KPI data

# Reference data --------------------------------
lab_sites <- read_excel(target_mapping_path,
                        sheet = "Lab_Sites")

lab_test_codes <- read_excel(target_mapping_path,
                             sheet = "Lab_TestCodes")

lab_icu <- read_excel(target_mapping_path,
                      sheet = "Lab_ICU")

# Select relevant columns from ICU mappings
mshs_icu <- lab_icu %>%
  select(Hospital, LocCode, LocName) %>%
  mutate(HospLoc = paste(Hospital, LocCode))

# Create vector with order of sites for department summary output
lab_sites_ordered <- c("MSH", "MSQ", "MSW", "MSM", "MSBI", "MSB", "NYEE")

# Import historical repositories -------------------------
# Read in lab department summary repos for both TAT and Proficiency testing
ops_metrics_lab_tat <- read_excel(ops_metrics_lab_tat_path)
ops_metrics_lab_pt <- read_excel(ops_metrics_lab_prof_test_path)

# Fix format of imported data for easier exporting ------------
# Reformat "Month" column in TAT data for merging
ops_metrics_lab_tat <- ops_metrics_lab_tat %>%
  mutate(Month = date(Month))

prof_test_last_month <- max(ops_metrics_lab_pt$Month)
next_month <- prof_test_last_month + months(1)

# Reformat Proficiency Testing data into wider format for manual entries
prof_test_manual_table <- ops_metrics_lab_pt %>%
  select(-Service) %>%
  filter(Month >= as.Date("12/1/2020", format("%m/%d/%Y"))) %>%
  # mutate(Number = percent(Number, 1)) %>%
  arrange(Month,
          Site) %>%
  mutate(Month = format(Month, "%m-%Y"),
         Number = as.character(Number)) %>%
  pivot_wider(names_from = Month,
              values_from = Number) %>%
  # Add a column with the next month for the user to enter data
  mutate('{format(prof_test_last_month + months(1), "%m-%Y")}' := "")


# Custom functions for processing monthly raw data for TAT analysis --------------
# Custom function for processing raw SCC data
lab_scc_tat_dept_summ_processing <- function(scc_raw_data) {
  scc_df <- scc_raw_data
  
  # Crosswalk sites
  scc_df <- left_join(scc_df,
                      lab_sites,
                      by = c("SITE" = "Data_Hosp"))
  
  # Crosswalk test codes
  scc_df <- left_join(scc_df,
                      lab_test_codes,
                      by = c("TEST_ID" = "TestCode"))
  
  scc_df <- scc_df %>%
    mutate(
      # Create a column combining site, location code, and location name
      HospLoc = paste(Site, WARD),
      # Determine whether unit is an ICU
      MSHS_ICU = HospLoc %in% mshs_icu$HospLoc,
      # Determine if unit is an ICU or an ED to be used for Troponin analysis
      ED_ICU = CLINCTYPE %in% "E" | MSHS_ICU,
      # Convert timestamps to posix times
      Receive_DateTime = as.POSIXct(RECEIVE_DT,
                                    tz = "UTC",
                                    form = "%Y-%m-%d %H:%M:%OS"),
      Result_DateTime = as.POSIXct(VERIFIED_DT,
                                   tz = "UTC",
                                   format = "%Y-%m-%d %H:%M:%OS"),
      # Calculate receive to result TAT
      ReceiveResultTAT = as.numeric(Result_DateTime - Receive_DateTime,
                                    units = "mins"),
      # Determine whether or not to include TAT
      TATInclude = !is.na(ReceiveResultTAT) & ReceiveResultTAT >= 0,
      # Determine whether or not to include specimen in calculation based
      SpecimenInclude = 
        # Include specimens with a valid TAT
        TATInclude &
        # Include HGB Stat labs or Troponin ED/ICU labs
        ((Test == "HGB" & PRIORITY == "S") |
           (Test == "Troponin" & ED_ICU)),
      # Determine target TAT based on specimen type
      TargetTAT = case_when(Test == "HGB" ~ 60,
                            Test == "Troponin" ~ 50),
      # Determine if lab meets target TAT
      ReceiveResultInTarget = ReceiveResultTAT <= TargetTAT,
      # Determine month and year of resulted lab
      ResultMonthYr = as.Date(
        paste0(month(Result_DateTime), "/1/", year(Result_DateTime)),
        format = "%m/%d/%Y"),
      # Create a column with metric summary
      Metric = paste0(Test, " (<=", TargetTAT, " min)")
    )
  
  # Determine primary month of report and remove any labs resulted the following month.
  scc_monthly_volume <- scc_df %>%
    filter(SpecimenInclude) %>%
    group_by(ResultMonthYr) %>%
    summarize(Count = n()) %>%
    arrange(-Count) %>%
    ungroup()
  
  scc_report_month <- scc_monthly_volume$ResultMonthYr[1]
  
  scc_df <- scc_df %>%
    filter(ResultMonthYr %in% scc_report_month)
  
  scc_summary <- scc_df %>%
    filter(SpecimenInclude) %>%
    group_by(Site,
             Test,
             ResultMonthYr,
             Metric) %>%
    summarize(LabsWithinTarget = sum(ReceiveResultInTarget),
              TotalLabs = n(),
              PercentInTarget = round(LabsWithinTarget / TotalLabs,
                                      digits = 4),
              .groups = "keep") %>%
    ungroup() %>%
    # Format for department summary repo structure
    mutate(Service = "Lab",
           LabsWithinTarget = NULL,
           TotalLabs = NULL,
           Test = NULL) %>%
    rename(Month = ResultMonthYr,
           Number = PercentInTarget) %>%
    relocate(Service)
  
  return(scc_summary)
  
}

# Custom function for processing raw Sunquest data
lab_sun_tat_dept_summ_processing <- function(sun_raw_data) {
  sun_df <- sun_raw_data
  
  # Crosswalk sites
  sun_df <- left_join(sun_df,
                      lab_sites,
                      by = c("HospCode" = "Data_Hosp"))
  
  # Crosswalk test codes
  sun_df <- left_join(sun_df,
                      lab_test_codes,
                      by = c("TestCode" = "TestCode"))
  
  sun_df <- sun_df %>%
    # Remove MSSN
    filter(Site != "MSSN") %>%
    mutate(
      # Create a column combining site, location code, and location name
      HospLoc = paste(HospCode, LocCode),
      # Determine whether unit is an ICU
      MSHS_ICU = HospLoc %in% mshs_icu$HospLoc,
      # Determine if unit is an ICU or an ED to be used for Troponin analysis
      ED_ICU = LocType %in% "ER" | MSHS_ICU,
      # Convert timestamps to posix times
      Receive_DateTime = as.POSIXct(ReceiveDateTime,
                                    tz = "UTC",
                                    form = "%m/%d/%Y %H:%M:%S"),
      Result_DateTime = as.POSIXct(ResultDateTime,
                                   tz = "UTC",
                                   format = "%m/%d/%Y %H:%M:%S"),
      # Calculate receive to result TAT
      ReceiveResultTAT = as.numeric(Result_DateTime - Receive_DateTime,
                                    units = "mins"),
      # Determine whether or not to include TAT
      TATInclude = !is.na(ReceiveResultTAT) & ReceiveResultTAT >= 0,
      # Determine whether or not to include specimen in calculation based
      SpecimenInclude = 
        # Include specimens with a valid TAT
        TATInclude &
        # Include HGB Stat labs or Troponin ED/ICU labs
        ((Test == "HGB" & SpecimenPriority == "S") |
           (Test == "Troponin" & ED_ICU)),
      # Determine target TAT based on specimen type
      TargetTAT = case_when(Test == "HGB" ~ 60,
                            Test == "Troponin" ~ 50),
      # Determine if lab meets target TAT
      ReceiveResultInTarget = ReceiveResultTAT <= TargetTAT,
      # Determine month and year of resulted lab
      ResultMonthYr = as.Date(
        paste0(month(Result_DateTime), "/1/", year(Result_DateTime)),
        format = "%m/%d/%Y"),
      # Create a column with metric summary
      Metric = paste0(Test, " (<=", TargetTAT, " min)")
    )
  
  # Sunquest data often includes labs resulted the following day/month.
  # Determine primary month of report and remove any labs resulted the following month.
  sun_monthly_volume <- sun_df %>%
    filter(SpecimenInclude) %>%
    group_by(ResultMonthYr) %>%
    summarize(Count = n()) %>%
    arrange(-Count) %>%
    ungroup()
  
  sun_report_month <- sun_monthly_volume$ResultMonthYr[1]
  
  sun_df <- sun_df %>%
    filter(ResultMonthYr %in% sun_report_month)
  
  sun_summary <- sun_df %>%
    filter(SpecimenInclude) %>%
    group_by(Site,
             Test,
             ResultMonthYr,
             Metric) %>%
    summarize(LabsWithinTarget = sum(ReceiveResultInTarget),
              TotalLabs = n(),
              PercentInTarget = round(LabsWithinTarget / TotalLabs,
                                      digits = 4),
              .groups = "keep") %>%
    ungroup() %>%
    # Format for department summary repo structure
    mutate(Service = "Lab",
           LabsWithinTarget = NULL,
           TotalLabs = NULL,
           Test = NULL) %>%
    rename(Month = ResultMonthYr,
           Number = PercentInTarget) %>%
    relocate(Service)
  
  return(sun_summary)
  
}


# Custom functions for formatting summarized data into metrics_final_df structure -------------------
# Custom function for SCC data
lab_scc_metrics_final_processing <- function(scc_summary) {
  
  # Format for metrics_final_df
  scc_tat_df <- scc_summary %>%
    # Reorder for better visualization
    mutate(Site = factor(Site,
                         levels = lab_sites_ordered,
                         ordered = TRUE)) %>%
    arrange(Month,
            desc(Metric),
            Site) %>%
    mutate(Site = as.character(Site)) %>%
    # Start formatting for metrics_final_df format
    rename(Metric_Name_Submitted = Metric) %>%
    mutate(value_rounded = round(Number, digits = 2),
           Premier_Reporting_Period = format(Month, "%b %Y"),
           Reporting_Month = format(Month, "%m-%Y"),
           Month = NULL,
           Number = NULL)
  
  # Merge with metric group mapping data for included metrics to get
  # "Metric_Group" and "Metric_Name" columns
  scc_tat_df <- merge(scc_tat_df,
                      metric_group_mapping[c("Metric_Group",
                                             "Metric_Name",
                                             "Metric_Name_Submitted")],
                      by = c("Metric_Name_Submitted"))
  
  # Combine with target mapping to include status definitions and targets
  scc_tat_target_status <- merge(scc_tat_df[, c("Service",
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
  scc_tat_target_status <- scc_tat_target_status %>%
    mutate(Variance = between(value_rounded, Range_1, Range_2)) %>%
    filter(!is.na(Reporting_Month) &
             !(Variance %in% FALSE))
  
  # Combine two dataframes
  scc_tat_df_merge <- merge(scc_tat_df,
                            scc_tat_target_status[, c("Service",
                                                      "Site",
                                                      "Metric_Group",
                                                      "Metric_Name",
                                                      "Reporting_Month",
                                                      "Target",
                                                      "Status")],
                            all = FALSE)
  
  # Select relevant columns
  scc_tat_df_merge <- scc_tat_df_merge[, processed_df_cols]
  
  # Add reporting month back in
  scc_tat_df_merge <- scc_tat_df_merge %>%
    mutate(Reporting_Month_Ref = as.Date(paste("01",
                                               as.yearmon(Reporting_Month,
                                                          "%m-%Y")),
                                         format = "%d %b %Y"))
  
  new_rows <- unique(scc_tat_df_merge[, c("Metric_Name",
                                          "Reporting_Month",
                                          "Service",
                                          "Site")])
  
  metrics_final_df <- anti_join(metrics_final_df,
                                new_rows)
  
  metrics_final_df <- full_join(metrics_final_df,
                                scc_tat_df_merge)
  
  metrics_final_df <- metrics_final_df %>%
    arrange(Service,
            Site,
            Metric_Group,
            Reporting_Month_Ref)
  
  return(metrics_final_df)
  
}

# Custom function for Sunquest data
lab_sun_metrics_final_processing <- function(sun_summary) {
  
  # Format for metrics_final_df
  sun_tat_df <- sun_summary %>%
    # Reorder for better visualization
    mutate(Site = factor(Site,
                         levels = lab_sites_ordered,
                         ordered = TRUE)) %>%
    arrange(Month,
            desc(Metric),
            Site) %>%
    mutate(Site = as.character(Site)) %>%
    # Start formatting for metrics_final_df format
    rename(Metric_Name_Submitted = Metric) %>%
    mutate(value_rounded = round(Number, digits = 2),
           Premier_Reporting_Period = format(Month, "%b %Y"),
           Reporting_Month = format(Month, "%m-%Y"),
           Month = NULL,
           Number = NULL)
  
  # Merge with metric group mapping data for included metrics to get
  # "Metric_Group" and "Metric_Name" columns
  sun_tat_df <- merge(sun_tat_df,
                      metric_group_mapping[c("Metric_Group",
                                             "Metric_Name",
                                             "Metric_Name_Submitted")],
                      by = c("Metric_Name_Submitted"))
  
  # Combine with target mapping to include status definitions and targets
  sun_tat_target_status <- merge(sun_tat_df[, c("Service",
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
  sun_tat_target_status <- sun_tat_target_status %>%
    mutate(Variance = between(value_rounded, Range_1, Range_2)) %>%
    filter(!is.na(Reporting_Month) &
             !(Variance %in% FALSE))
  
  # Combine two dataframes
  sun_tat_df_merge <- merge(sun_tat_df,
                            sun_tat_target_status[, c("Service",
                                                      "Site",
                                                      "Metric_Group",
                                                      "Metric_Name",
                                                      "Reporting_Month",
                                                      "Target",
                                                      "Status")],
                            all = FALSE)
  
  # Select relevant columns
  sun_tat_df_merge <- sun_tat_df_merge[, processed_df_cols]
  
  # Add reporting month back in
  sun_tat_df_merge <- sun_tat_df_merge %>%
    mutate(Reporting_Month_Ref = as.Date(paste("01",
                                               as.yearmon(Reporting_Month,
                                                          "%m-%Y")),
                                         format = "%d %b %Y"))
  
  new_rows <- unique(sun_tat_df_merge[, c("Metric_Name",
                                          "Reporting_Month",
                                          "Service",
                                          "Site")])
  
  metrics_final_df <- anti_join(metrics_final_df,
                                new_rows)
  
  metrics_final_df <- full_join(metrics_final_df,
                                sun_tat_df_merge)
  
  metrics_final_df <- metrics_final_df %>%
    arrange(Service,
            Site,
            Metric_Group,
            Reporting_Month_Ref)
  
  return(metrics_final_df)
  
}


# Proficiency Testing ----------------
lab_prof_test_dept_summ_processing <- function(data) {
  prof_test_summary <- data %>%
    # Convert from wide to long format for consistency with department summary
    pivot_longer(cols = c(-Metric, -Site),
                 names_to = "Month",
                 values_to = "Number") %>%
    mutate(
      # Change format to be consistent with dept summary repo
      Number = as.numeric(Number),
      Month = as.Date(my(Month)),
      Service = "Lab") %>%
    # Reorder columns
    relocate(Service) %>%
    relocate(Month, .before = Metric)
  
  return(prof_test_summary)
  
}


# # Code for testing custom functions and approach ----------------------
# # Comment out code below here for deployed tool
# # Reference data for site, test, and ICU mappings. This can be commented out when using global.R
# # lab_sites <- read_excel(
# #   paste0("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization",
# #          "/Projects/System Operations/Balanced Scorecards Automation",
# #          "/Data_Dashboard/MSHS Scorecards Target Mapping.xlsx"),
# #   sheet = "Lab_Sites"
# # )
# # 
# # lab_test_codes <- read_excel(
# #   paste0("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization",
# #          "/Projects/System Operations/Balanced Scorecards Automation",
# #          "/Data_Dashboard/MSHS Scorecards Target Mapping.xlsx"),
# #   sheet = "Lab_TestCodes"
# # )
# # 
# # lab_icu <- read_excel(
# #   paste0("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization",
# #          "/Projects/System Operations/Balanced Scorecards Automation",
# #          "/Data_Dashboard/MSHS Scorecards Target Mapping.xlsx"),
# #   sheet = "Lab_ICU"
# # )
# 
# # Test custom functions and methodology -----------------------
# scc_test_file_path <- paste0("J:/deans/Presidents/HSPI-PM",
#                              "/Operations Analytics and Optimization/Projects",
#                              "/System Operations/Balanced Scorecards Automation",
#                              "/Data_Dashboard/Input Data Raw/Lab & Blood Bank",
#                              "/SCC/2021.03_SCC.xlsx")
# 
# sun_test_file_path <- paste0("J:/deans/Presidents/HSPI-PM",
#                              "/Operations Analytics and Optimization/Projects",
#                              "/System Operations/Balanced Scorecards Automation",
#                              "/Data_Dashboard/Input Data Raw/Lab & Blood Bank",
#                              "/SUNQUEST/2021.03_SUNQ.xlsx")
# 
# 
# # SCC Data Processing and Appending --------------
# # Read in raw data Excel file
# scc_data <- read_excel(scc_test_file_path)
# 
# # Save prior version of Lab TAT Dept Summary data in Hist Archive folder
# write_xlsx(ops_metrics_lab_tat,
#            paste0(hist_archive_path,
#                   "Lab TAT Metrics Pre-SCC Updates ",
#                   format(Sys.time(), "%Y%m%d_%H%M%S"),
#                   ".xlsx"))
# 
# # Process raw data using custom functions
# scc_summary_data <- lab_scc_tat_process(scc_data)
# 
# # write_xlsx(scc_summary_data,
# #            paste0(hist_archive_path,
# #                   "Lab TAT Metrics Date Test",
# #                   ".xlsx"))
# 
# # Add new SCC data to existing repository 
# # First, identify the sites, months, and metrics in the new data
# scc_new_data <- unique(
#   scc_summary_data[  c("Service", "Site", "Month", "Metric")]
# )
# 
# # Second, remove these sites, months, and metrics from the historical data, if they exist there.
# # This allows us to ensure no duplicate entries for the same site, metric, and time period
# ops_metrics_lab_tat <- anti_join(ops_metrics_lab_tat,
#                                          scc_new_data,
#                                          by = c("Service" = "Service",
#                                                 "Site" = "Site",
#                                                 "Month" = "Month",
#                                                 "Metric" = "Metric")
# )
# 
# # Third, combine the updated historical data with the new data
# ops_metrics_lab_tat <- full_join(ops_metrics_lab_tat,
#                                  scc_summary_data)
# 
# # Next, arrange the department summary by month, metric name, and site
# ops_metrics_lab_tat <- ops_metrics_lab_tat %>%
#   mutate(Site = factor(Site, levels = lab_sites_ordered, ordered = TRUE)) %>%
#   arrange(Month,
#           desc(Metric),
#           Site) %>%
#   mutate(Site = as.character(Site))
# 
# # Lastly, save the updated summary data
# write_xlsx(ops_metrics_lab_tat, ops_metrics_lab_tat_path)
# 
# # Update metrics_final_df with SCC data
# metrics_final_df <- lab_scc_metrics_final_processing(scc_summary_data)
# 
# 
# # Sunquest Data Processing and Appending ----------------------
# # Read in raw data Excel file
# sun_data <- read_excel(sun_test_file_path,
#                        col_types = "text")
# 
# # Save prior version of Lab TAT Dept Summary data in Hist Archive folder
# write_xlsx(ops_metrics_lab_tat,
#            paste0(hist_archive_path,
#                   "Lab TAT Metrics Pre-Sun Updates ",
#                   format(Sys.time(), "%Y%m%d_%H%M%S"),
#                   ".xlsx"))
# 
# # Process raw data using custom function
# sun_summary_data <- lab_sun_tat_process(sun_data)
# 
# # First, identify the sites, months, and metrics in the new data
# sun_new_data <- unique(
#   sun_summary_data[c("Service", "Site", "Month", "Metric")]
# )
# 
# # Second, remove these sites, months, and metrics from the historical data, if they exist there.
# # This allows us to ensure no duplicate entries for the same site, metric, and time period
# ops_metrics_lab_tat <- anti_join(ops_metrics_lab_tat,
#                                   sun_new_data,
#                                   by = c("Service" = "Service",
#                                          "Site" = "Site",
#                                          "Month" = "Month",
#                                          "Metric" = "Metric")
# )
# 
# # Third, combine the updated historical data with the new data
# ops_metrics_lab_tat <- full_join(ops_metrics_lab_tat,
#                                  sun_summary_data)
# 
# # Next, arrange the department summary by month, metric name, and site
# ops_metrics_lab_tat <- ops_metrics_lab_tat %>%
#   mutate(Site = factor(Site, levels = lab_sites_ordered, ordered = TRUE)) %>%
#   arrange(Month,
#           desc(Metric),
#           Site) %>%
#   mutate(Site = as.character(Site))
# 
# # Lastly, save the updated summary data
# write_xlsx(ops_metrics_lab_tat, ops_metrics_lab_tat_path)
# 
# # Update metrics_final_df with Sunquest data
# metrics_final_df <- lab_sun_metrics_final_processing(sun_summary_data)



