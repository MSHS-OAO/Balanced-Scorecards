

# Proficiency Testing ----------------
# Custom function for processing and formatting manual inputs into department summary format
lab_prof_test_dept_summary <- function(data, updated_user) {
  prof_test_summary <- data %>%
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
      SERVICE = "Lab",
      PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH, "%b %Y"),
      UPDATED_USER = updated_user) %>%
    # Reorder columns
    relocate(SERVICE) %>%
    relocate(REPORTING_MONTH, .before = METRIC_NAME_SUBMITTED)
  
  
  return(prof_test_summary)
  
}







# Retiring the processing for SCC/SUN Data, Pulling metrics from LAB_KPI_tables ----
# Code for processing Lab KPI SCC and SUNQUEST data -----

# Reference data --------------------------------
# lab_sites <- read_excel(target_mapping_path,
#                         sheet = "Lab_Sites")

# lab_test_codes <- read_excel(target_mapping_path,
#                              sheet = "Lab_TestCodes")
# 
# lab_icu <- read_excel(target_mapping_path,
#                       sheet = "Lab_ICU")

# lab_sites <- tbl(conn, "BSC_LAB_SITES") %>% 
#   rename(Data_Hosp = DATA_HOSP,
#          Site = SITE) %>% collect()
# 
# lab_test_codes <- tbl(conn, "BSC_LAB_TEST_CODES") %>%
#                     rename(TestCode = TESTCODE,
#                            Test = TEST) %>% collect()
# 
# lab_icu <- tbl(conn, "BSC_LAB_ICU") %>%
#                     rename(`Data Type` = DATA_TYPE,
#                            Hospital = HOSPITAL,
#                            `ICU Type` = ICU_TYPE,
#                            LocCode = LOCCODE,
#                            LocName = LOCNAME,
#                            Combined = COMBINED) %>% collect()
# 
# 
# # Select relevant columns from ICU mappings
# mshs_icu <- lab_icu %>%
#   select(Hospital, LocCode, LocName) %>%
#   mutate(HospLoc = paste(Hospital, LocCode))
# 
# # Create vector with order of sites for department summary output
# lab_sites_ordered <- c("MSH", "MSQ", "MSW", "MSM", "MSBI", "MSB", "NYEE")
# 
# # Custom functions for processing monthly raw data for TAT analysis --------------
# # Custom function for processing raw SCC data
# lab_scc_tat_dept_summary <- function(scc_raw_data, updated_user) {
#   scc_df <- scc_raw_data
#   
#   # Crosswalk sites
#   scc_df <- left_join(scc_df,
#                       lab_sites,
#                       by = c("SITE" = "Data_Hosp"))
#   
#   # Crosswalk test codes
#   scc_df <- left_join(scc_df,
#                       lab_test_codes,
#                       by = c("TEST_ID" = "TestCode"))
#   
#   scc_df <- scc_df %>%
#     mutate(
#       # Create a column combining site, location code, and location name
#       HospLoc = paste(Site, WARD),
#       # Determine whether unit is an ICU
#       MSHS_ICU = HospLoc %in% mshs_icu$HospLoc,
#       # Determine if unit is an ICU or an ED to be used for Troponin analysis
#       ED_ICU = CLINCTYPE %in% "E" | MSHS_ICU,
#       # Convert timestamps to posix times
#       Receive_DateTime = as.POSIXct(RECEIVE_DT,
#                                     tz = "UTC",
#                                     form = "%Y-%m-%d %H:%M:%OS"),
#       Result_DateTime = as.POSIXct(VERIFIED_DT,
#                                    tz = "UTC",
#                                    format = "%Y-%m-%d %H:%M:%OS"),
#       # Calculate receive to result TAT
#       ReceiveResultTAT = as.numeric(Result_DateTime - Receive_DateTime,
#                                     units = "mins"),
#       # Determine whether or not to include TAT
#       TATInclude = !is.na(ReceiveResultTAT) & ReceiveResultTAT >= 0,
#       # Determine whether or not to include specimen in calculation based
#       SpecimenInclude = 
#         # Include specimens with a valid TAT
#         TATInclude &
#         # Include HGB Stat labs or Troponin ED/ICU labs
#         ((Test == "HGB" & PRIORITY == "S") |
#            (Test == "Troponin" & ED_ICU)),
#       # Determine target TAT based on specimen type
#       TargetTAT = case_when(Test == "HGB" ~ 60,
#                             Test == "Troponin" ~ 50),
#       # Determine if lab meets target TAT
#       ReceiveResultInTarget = ReceiveResultTAT <= TargetTAT,
#       # Determine month and year of resulted lab
#       ResultMonthYr = as.Date(
#         paste0(month(Result_DateTime), "/1/", year(Result_DateTime)),
#         format = "%m/%d/%Y"),
#       # Create a column with metric summary
#       Metric = paste0(Test, " (<=", TargetTAT, " min)")
#     )
#   
#   # Determine primary month of report and remove any labs resulted the following month.
#   scc_monthly_volume <- scc_df %>%
#     filter(SpecimenInclude) %>%
#     group_by(ResultMonthYr) %>%
#     summarize(Count = n()) %>%
#     arrange(-Count) %>%
#     ungroup()
#   
#   scc_report_month <- scc_monthly_volume$ResultMonthYr[1]
#   
#   scc_df <- scc_df %>%
#     filter(ResultMonthYr %in% scc_report_month)
#   
#   scc_summary <- scc_df %>%
#     filter(SpecimenInclude) %>%
#     group_by(Site,
#              Test,
#              ResultMonthYr,
#              Metric) %>%
#     summarize(LabsWithinTarget = sum(ReceiveResultInTarget),
#               TotalLabs = n(),
#               PercentInTarget = round(LabsWithinTarget / TotalLabs,
#                                       digits = 4),
#               .groups = "keep") %>%
#     ungroup() %>%
#     # Format for standardized department summary repo structure
#     select(-LabsWithinTarget,
#            -TotalLabs,
#            -Test) %>%
#     rename(SITE = Site,
#            REPORTING_MONTH = ResultMonthYr,
#            METRIC_NAME_SUBMITTED = Metric,
#            VALUE = PercentInTarget) %>%
#     mutate(SERVICE = "Lab",
#            PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH, "%b %Y"),
#            UPDATED_USER = updated_user) %>%
#     relocate(SERVICE) %>%
#     relocate(PREMIER_REPORTING_PERIOD, .after = REPORTING_MONTH)
#   
#   return(scc_summary)
#   
# }
# 
# # Custom function for processing raw Sunquest data
# lab_sun_tat_dept_summary <- function(sun_raw_data, updated_user) {
#   sun_df <- sun_raw_data
#   
#   # Crosswalk sites
#   sun_df <- left_join(sun_df,
#                       lab_sites,
#                       by = c("HospCode" = "Data_Hosp"))
#   
#   # Crosswalk test codes
#   sun_df <- left_join(sun_df,
#                       lab_test_codes,
#                       by = c("TestCode" = "TestCode"))
#   
#   sun_df <- sun_df %>%
#     # Remove MSSN
#     filter(Site != "MSSN") %>%
#     mutate(
#       # Create a column combining site, location code, and location name
#       HospLoc = paste(HospCode, LocCode),
#       # Determine whether unit is an ICU
#       MSHS_ICU = HospLoc %in% mshs_icu$HospLoc,
#       # Determine if unit is an ICU or an ED to be used for Troponin analysis
#       ED_ICU = LocType %in% "ER" | MSHS_ICU,
#       # Convert timestamps to posix times
#       Receive_DateTime = as.POSIXct(ReceiveDateTime,
#                                     tz = "UTC",
#                                     form = "%m/%d/%Y %H:%M:%S"),
#       Result_DateTime = as.POSIXct(ResultDateTime,
#                                    tz = "UTC",
#                                    format = "%m/%d/%Y %H:%M:%S"),
#       # Calculate receive to result TAT
#       ReceiveResultTAT = as.numeric(Result_DateTime - Receive_DateTime,
#                                     units = "mins"),
#       # Determine whether or not to include TAT
#       TATInclude = !is.na(ReceiveResultTAT) & ReceiveResultTAT >= 0,
#       # Determine whether or not to include specimen in calculation based
#       SpecimenInclude = 
#         # Include specimens with a valid TAT
#         TATInclude &
#         # Include HGB Stat labs or Troponin ED/ICU labs
#         ((Test == "HGB" & SpecimenPriority == "S") |
#            (Test == "Troponin" & ED_ICU)),
#       # Determine target TAT based on specimen type
#       TargetTAT = case_when(Test == "HGB" ~ 60,
#                             Test == "Troponin" ~ 50),
#       # Determine if lab meets target TAT
#       ReceiveResultInTarget = ReceiveResultTAT <= TargetTAT,
#       # Determine month and year of resulted lab
#       ResultMonthYr = as.Date(
#         paste0(month(Result_DateTime), "/1/", year(Result_DateTime)),
#         format = "%m/%d/%Y"),
#       # Create a column with metric summary
#       Metric = paste0(Test, " (<=", TargetTAT, " min)")
#     )
#   
#   # Sunquest data often includes labs resulted the following day/month.
#   # Determine primary month of report and remove any labs resulted the following month.
#   sun_monthly_volume <- sun_df %>%
#     filter(SpecimenInclude) %>%
#     group_by(ResultMonthYr) %>%
#     summarize(Count = n()) %>%
#     arrange(-Count) %>%
#     ungroup()
#   
#   sun_report_month <- sun_monthly_volume$ResultMonthYr[1]
#   
#   sun_df <- sun_df %>%
#     filter(ResultMonthYr %in% sun_report_month)
#   
#   sun_summary <- sun_df %>%
#     filter(SpecimenInclude) %>%
#     group_by(Site,
#              Test,
#              ResultMonthYr,
#              Metric) %>%
#     summarize(LabsWithinTarget = sum(ReceiveResultInTarget),
#               TotalLabs = n(),
#               PercentInTarget = round(LabsWithinTarget / TotalLabs,
#                                       digits = 4),
#               .groups = "keep") %>%
#     ungroup() %>%
#     # Format for standardized department summary repo structure
#     select(-LabsWithinTarget,
#            -TotalLabs,
#            -Test) %>%
#     rename(SITE = Site,
#            REPORTING_MONTH = ResultMonthYr,
#            METRIC_NAME_SUBMITTED = Metric,
#            VALUE = PercentInTarget) %>%
#     mutate(SERVICE = "Lab",
#            PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH, "%b %Y"),
#            UPDATED_USER = updated_user) %>%
#     relocate(SERVICE) %>%
#     relocate(PREMIER_REPORTING_PERIOD, .after = REPORTING_MONTH)
#   
#   return(sun_summary)
#   
# }
# 
# 
# # Custom functions for formatting summarized data into metrics_final_df structure -------------------
# # Custom function for SCC data
# lab_scc_tat_metrics_final_df <- function(scc_summary) {
#   
#   # Format for metrics_final_df
#   scc_tat_df <- scc_summary %>%
#     # Reorder for better visualization
#     mutate(Site = factor(Site,
#                          levels = lab_sites_ordered,
#                          ordered = TRUE)) %>%
#     arrange(Month,
#             desc(Metric),
#             Site) %>%
#     mutate(Site = as.character(Site)) %>%
#     # Start formatting for metrics_final_df format
#     rename(Metric_Name_Submitted = Metric) %>%
#     mutate(value_rounded = Number,
#            Premier_Reporting_Period = format(Month, "%b %Y"),
#            Reporting_Month = format(Month, "%m-%Y"),
#            Month = NULL,
#            Number = NULL)
#   
#   # Use custom function for updating metrics_final_df using standard process
#   metrics_final_df <- metrics_final_df_subset_and_merge(scc_tat_df)
#   
#   return(metrics_final_df)
#   
# }
# 
# # Custom function for Sunquest data
# lab_sun_tat_metrics_final_df <- function(sun_summary) {
#   
#   # Format for metrics_final_df
#   sun_tat_df <- sun_summary %>%
#     # Reorder for better visualization
#     mutate(Site = factor(Site,
#                          levels = lab_sites_ordered,
#                          ordered = TRUE)) %>%
#     arrange(Month,
#             desc(Metric),
#             Site) %>%
#     mutate(Site = as.character(Site)) %>%
#     # Start formatting for metrics_final_df format
#     rename(Metric_Name_Submitted = Metric) %>%
#     mutate(value_rounded = Number,
#            Premier_Reporting_Period = format(Month, "%b %Y"),
#            Reporting_Month = format(Month, "%m-%Y"),
#            Month = NULL,
#            Number = NULL)
#   
#   # Use custom function for updating metrics_final_df using standard process
#   metrics_final_df <- metrics_final_df_subset_and_merge(sun_tat_df)
#   
#   return(metrics_final_df)
#   
# }


