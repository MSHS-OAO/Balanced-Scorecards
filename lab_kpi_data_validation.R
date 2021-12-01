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
  library(zipcodeR)
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
  
ref_file <- read_excel(
  paste0("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization",
         "/Projects/System Operations/Balanced Scorecards Automation",
         "/Data_Dashboard/MSHS Scorecards Target Mapping.xlsx"),
  sheet = "Lab Mapping"
)

# Select relevant columns from ICU mappings
mshs_icu <- ref_file %>%
  select(Hospital, LocCode, LocName) %>%
  mutate(HospLoc = paste(Hospital, LocCode))

fti_sun_df <- fti_sun_comp %>%
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
                                 format = "%m/%d/%Y %H:%M%S"),
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
      ((TestCode == "HGB" & SpecimenPriority == "S") |
         (TestCode == "TROP" & ED_ICU)),
    # Determine target TAT based on specimen type
    TargetTAT = case_when(TestCode == "HGB" ~ 60,
                          TestCode == "TROP" ~ 50),
    # Determine if lab meets target TAT
    ReceiveResultInTarget = ReceiveResultTAT <= TargetTAT,
    # Determine month and year of resulted lab
    ResultMonthYr = as.Date(
      paste0(month(Result_DateTime), "/1/", year(Result_DateTime)),
      format = "%m/%d/%Y"),
    # Create a column with metric summary
    Metric = case_when(
      TestCode == "HGB" ~ paste0("HGB (<=", TargetTAT, " min)"),
      TestCode == "TROP" ~ paste0("Troponin (<=", TargetTAT, " min)")
    )
  )

fti_scc_df <- fti_scc_comp %>%
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
    # Create a column with site
    Site = case_when(SITE == "Sinai" ~ "MSH",
                     SITE == "Queens" ~ "MSQ"),
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
      ((TEST_ID == "HGB" & PRIORITY == "S") |
         (TEST_ID == "TROI" & ED_ICU)),
    # Determine target TAT based on specimen type
    TargetTAT = case_when(TEST_ID == "HGB" ~ 60,
                          TEST_ID == "TROI" ~ 50),
    # Determine if lab meets target TAT
    ReceiveResultInTarget = ReceiveResultTAT <= TargetTAT,
    # Determine month and year of resulted lab
    ResultMonthYr = as.Date(
      paste0(month(Result_DateTime), "/1/", year(Result_DateTime)),
      format = "%m/%d/%Y"),
    # Create a column with metric summary
    Metric = case_when(
      TEST_ID == "HGB" ~ paste0("HGB (<=", TargetTAT, " min)"),
      TEST_ID == "TROI" ~ paste0("Troponin (<=", TargetTAT, " min)")
    )
  )



# Logic used by FTI:
# HGB: Stat orders only, regardless of setting; exclude negative or blank TAT
# Troponin: ED and Critical Care Units only; exclude negative or blank TAT

sun_summary <- fti_sun_df %>%
  filter(SpecimenInclude) %>%
  group_by(HospCode,
           TestCode,
           ResultMonthYr,
           Metric) %>%
  summarize(LabsWithinTarget = sum(ReceiveResultInTarget),
            TotalLabs = n(),
            PercentInTarget = percent(LabsWithinTarget / TotalLabs, accuracy = 0.01)) %>%
  ungroup()

scc_summary <- fti_scc_df %>%
  filter(SpecimenInclude) %>%
  group_by(Site,
           TEST_ID,
           ResultMonthYr,
           Metric) %>%
  summarize(LabsWithinTarget = sum(ReceiveResultInTarget),
            TotalLabs = n(),
            PercentInTarget = percent(LabsWithinTarget / TotalLabs, accuracy = 0.001)) %>%
  ungroup()



  