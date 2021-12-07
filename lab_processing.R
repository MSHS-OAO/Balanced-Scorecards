# Code for processing Lab KPI data


# Reference data for site, test, and ICU mappings. This can be commented out when using global.R
lab_sites <- read_excel(
  paste0("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization",
         "/Projects/System Operations/Balanced Scorecards Automation",
         "/Data_Dashboard/MSHS Scorecards Target Mapping.xlsx"),
  sheet = "Lab_Sites"
)

lab_test_codes <- read_excel(
  paste0("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization",
         "/Projects/System Operations/Balanced Scorecards Automation",
         "/Data_Dashboard/MSHS Scorecards Target Mapping.xlsx"),
  sheet = "Lab_TestCodes"
)

lab_icu <- read_excel(
  paste0("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization",
         "/Projects/System Operations/Balanced Scorecards Automation",
         "/Data_Dashboard/MSHS Scorecards Target Mapping.xlsx"),
  sheet = "Lab_ICU"
)

# Select relevant columns from ICU mappings
mshs_icu <- lab_icu %>%
  select(Hospital, LocCode, LocName) %>%
  mutate(HospLoc = paste(Hospital, LocCode))

lab_scc_tat_process <- function(scc_raw_data) {
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
      Metric = paste0(Test, " (<= ", TargetTAT, " min)")
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
              PercentInTarget = percent(LabsWithinTarget / TotalLabs, accuracy = 1),
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
