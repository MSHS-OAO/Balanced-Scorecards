# Code to validate/replicate FTI calculations for Lab KPIs
rm(list = ls())

# Install and load packages ----------------------------------------
suppressMessages({
  # library(xlsx)
  library(assertr)
  library(readxl)
  library(writexl)
  library(plyr)
  library(dplyr)
  library(data.table)
  library(zoo)
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyWidgets)
  library(htmlwidgets)
  library(lubridate)
  library(tcltk)
  library(tidyverse)
  library(plotly)
  library(knitr)
  library(kableExtra)
  library(leaflet)
  library(grid)
  library(gridExtra)
  library(eeptools)
  library(ggQC)
  library(utils)
  library(scales)
  library(chron)
  library(bupaR)
  library(shiny)
  library(DT)
  library(DiagrammeR)
  library(shinyalert)
  library(edeaR)
  library(processmapR)
  library(processmonitR)
  library(processanimateR)
  library(tidyr)
  library(lubridate)
  library(RColorBrewer)
  library(DiagrammeR)
  library(ggplot2)
  library(leaflet)
  library(readr)
  library(highcharter)
  library(ggforce) # for 'geom_arc_bar'
  library(packcircles) # for packed circle graph
  library(viridis)
  library(ggiraph)
  library(treemapify)
  library(treemap)
  library(broom)
  library(extrafont)
  library(tis) # for US holidays
  library(vroom)
  library(sjmisc)
  library(tools)
  library(here)
  library(shinyBS)
  library(shinyscreenshot)
  library(fasttime)
  library(shinycssloaders)
  library(fontawesome)
  library(rhandsontable)
  library(janitor)
  library(stringr)
})

# First, reproduce FTI's Excel analysis using their data
# Second, create compilation from monthly files on SharePoint and compare

# Import FTI SCC and Sunquest compilations
fti_sun_comp <- read_excel(
  paste0("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization",
         "/Projects/System Operations/Balanced Scorecards Automation",
         "/Data_Dashboard/Input Data Raw/Lab & Blood Bank",
         "/Sunquest_Compiled_throughJuly2021.xlsx"),
  sheet = "Raw Data"
)

fti_scc_comp <- read_excel(
  paste0("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization",
         "/Projects/System Operations/Balanced Scorecards Automation",
         "/Data_Dashboard/Input Data Raw/Lab & Blood Bank",
         "/Soft Sites_Compiled_throughJuly2021.xlsx"),
  sheet = "Compiled Data"
)

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
mshs_icu <- icu_mapping %>%
  select(Hospital, LocCode, LocName) %>%
  mutate(HospLoc = paste(Hospital, LocCode))

# Crosswalk sites
fti_sun_df <- fti_sun_comp

fti_sun_df <- left_join(fti_sun_df,
                        lab_sites,
                      by = c("HospCode" = "Data_Hosp"))

fti_scc_df <- fti_scc_comp

fti_scc_df <- left_join(fti_scc_df,
                        lab_sites,
                        by = c("SITE" = "Data_Hosp"))

# Crosswalk test codes
fti_sun_df <- left_join(fti_sun_df,
                        lab_test_codes,
                        by = c("TestCode" = "TestCode"))

fti_scc_df <- left_join(fti_scc_df,
                        lab_test_codes,
                        by = c("TEST_ID" = "TestCode"))

fti_sun_df <- fti_sun_df %>%
  mutate(
    # Remove columns FTI added
    File = NULL,
    `Date Validation_Result` = NULL,
    `Result_Month Year` = NULL,
    `Negative Value` = NULL,
    `Receive to Result Target (<=60)` = NULL,
    `Receive to Result Target (<=50)` = NULL,
    `Time Stamp Calculated` = NULL,
    `Time Stamp Variance` = NULL,
    `Location Include` = NULL,
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
    Metric = paste0(Test, " (<= ", TargetTAT, " min)")
  )

fti_scc_df <- fti_scc_df %>%
  mutate(
    # Remove columns FTI added
    File = NULL,
    `Location Include` = NULL,
    `Date Validation` = NULL,
    `Negative Value` = NULL,
    `Receive to Result Target (<=60)` = NULL,
    `Receive to Result Target (<=50)` = NULL,
    `Time Stamp Calculated` = NULL,
    `Time Stamp Variance` = NULL,
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


sun_summary_fti <- fti_sun_df %>%
  filter(SpecimenInclude) %>%
  group_by(Site,
           Test,
           ResultMonthYr,
           Metric) %>%
  summarize(LabsWithinTarget = sum(ReceiveResultInTarget),
            TotalLabs = n(),
            PercentInTarget = percent(LabsWithinTarget / TotalLabs, accuracy = 0.01),
            .groups = "keep") %>%
  ungroup()

scc_summary_fti <- fti_scc_df %>%
  filter(SpecimenInclude) %>%
  group_by(Site,
           Test,
           ResultMonthYr,
           Metric) %>%
  summarize(LabsWithinTarget = sum(ReceiveResultInTarget),
            TotalLabs = n(),
            PercentInTarget = percent(LabsWithinTarget / TotalLabs, accuracy = 0.001),
            .groups = "keep") %>%
  ungroup()

# Now import the monthly Sunquest and SCC files from OneDrive and compare to FTI's results
scc_folder <- paste0("J:/deans/Presidents/HSPI-PM",
                     "/Operations Analytics and Optimization/Projects",
                     "/System Operations/Balanced Scorecards Automation",
                     "/Data_Dashboard/Input Data Raw/Lab & Blood Bank/SCC")

sun_folder <- paste0("J:/deans/Presidents/HSPI-PM",
                     "/Operations Analytics and Optimization/Projects",
                     "/System Operations/Balanced Scorecards Automation",
                     "/Data_Dashboard/Input Data Raw/Lab & Blood Bank/SUNQUEST")

scc_files <- list.files(scc_folder)
sun_files <- list.files(sun_folder)

scc_list <- lapply(scc_files,
                   function(x) read_excel(
                     paste0(scc_folder, "/", x)
                   )
)

sun_list <- lapply(sun_files,
                   function(x) read_excel(
                     paste0(sun_folder, "/", x)
                   )
)

process_scc <- function(scc_raw_data) {
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

process_sun <- function(sun_raw_data) {
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

test_output <- process_scc(scc_list)

scc_summary_stats <- bind_rows(lapply(scc_list, process_scc))
sun_summary_stats <- bind_rows(lapply(sun_list, process_sun))

# sun_summary_stats_v2 <- sun_summary_stats %>%
#   group_by(Site,
#            Test,
#            ResultMonthYr,
#            Metric) %>%
#   summarize(LabsWithinTarget = sum(LabsWithinTarget),
#             TotalLabs = sum(TotalLabs),
#             PercentInTarget = percent(LabsWithinTarget / TotalLabs, accuracy = 0.01),
#             .groups = "keep") %>%
#   ungroup()

cp_summary_stats <- bind_rows(scc_summary_stats, sun_summary_stats_v2)

export_list <- list("SCC - FTI Method" = scc_summary_fti,
                    "SUN - FTI Method" = sun_summary_fti,
                    "SCC - OAO Code" = scc_summary_stats,
                    "SUN - OAO Code" = sun_summary_stats)

write_xlsx(export_list,
           paste0("J:/deans/Presidents/HSPI-PM",
                  "/Operations Analytics and Optimization/Projects",
                  "/System Operations/Balanced Scorecards Automation",
                  "/Data_Dashboard/HSO FTI Data Validation",
                  "/Lab KPI Analysis Validation Corrected ", Sys.Date(), ".xlsx")
)
