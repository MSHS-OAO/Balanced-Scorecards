# Code for processing Lab KPI data

# Import historical repositories -------------------------
# Read in lab summary repos
ops_metrics_lab_tat <- read_excel(ops_metrics_lab_tat_path)
ops_metrics_lab_pt <- read_excel(ops_metrics_lab_prof_test_path)

# Change format of Lab Proficiency Testing historical daata for HandsOnTable and manual input formatting
ops_metrics_lab_pt <- ops_metrics_lab_pt %>%
  filter(Month >= "2020-12-01") %>%
  mutate_if(is.logical, as.character) %>%
  mutate_if(is.double, as.character) %>%
  pivot_wider(names_from = "Month", values_from = Number)

# Reference data --------------------------------
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

# Custom functions for processing raw TAT data from SCC and Sunquest --------------
# Custom function for processing raw SCC data
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

# Custom function for processing raw Sunquest data
lab_sun_tat_process <- function(sun_raw_data) {
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
  
  return(sun_summary)
  
}

# Test custom functions -----------------------
scc_test_file_path <- paste0("J:/deans/Presidents/HSPI-PM",
                             "/Operations Analytics and Optimization/Projects",
                             "/System Operations/Balanced Scorecards Automation",
                             "/Data_Dashboard/Input Data Raw/Lab & Blood Bank",
                             "/SCC/2021.03_SCC.xlsx")

sun_test_file_path <- paste0("J:/deans/Presidents/HSPI-PM",
                             "/Operations Analytics and Optimization/Projects",
                             "/System Operations/Balanced Scorecards Automation",
                             "/Data_Dashboard/Input Data Raw/Lab & Blood Bank",
                             "/SUNQUEST/2021.03_SUNQ.xlsx")

scc_data <- read_excel(scc_test_file_path)
sun_data <- read_excel(sun_test_file_path)

scc_summary_data <- lab_scc_tat_process(scc_data)
sun_summary_data <- lab_sun_tat_process(sun_data)

scc_summary_data <- scc_summary_data %>%
  mutate(Concate = paste(Site, Month, Metric))

sun_summary_data <- sun_summary_data %>%
  mutate(Concate = paste(Site, Month, Metric))


ops_metrics_lab_tat <- ops_metrics_lab_tat %>%
  mutate(Concate = paste(Site, Month, Metric),
         Dupl = Concate %in% scc_summary_data$Concate)

# Custom function for formatting SCC data into appropriate format for metrics_final_df.RDS structure
lab_scc_metrics_final_processing <- function(scc_summary) {
  
  
  
  
  
  
  
  
}



# Proficiency Testing ----------------



