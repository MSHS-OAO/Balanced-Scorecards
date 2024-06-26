
# Install and load packages ----------------------------------------
suppressMessages({
  library(pool)
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
  #library(tcltk)
  library(tidyverse)
  library(plotly)
  library(knitr)
  library(kableExtra)
  library(leaflet)
  library(grid)
  library(gridExtra)
  #library(eeptools)
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
  library(openxlsx)
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
  library(glue)
  library(magrittr)
  library(shinyjs)
  library(DBI)
  library(odbc)
  #library(reshape2)
  library(formattable)
})




print("0")
dsn <- "OAO Cloud DB Production"
dsn_oracle <- paste0(dsn, " Oracle")
print("1")


options(shiny.maxRequestSize=500*1024^2)

# Maximize R Memory Size 
#memory.limit(size = 8000000)

# # Increase allowable file size (Sunquest monthly files are too large for default)
# if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=100*1024^2)

# Set aesthetics theme -----------------------------------------------------------------------------

# Color Functions for Graphs =====================================
theme_set(theme_minimal())

# Mount Sinai corporate colors 
MountSinai_colors <- c(
  `dark purple`  = "#212070",
  `dark pink`    = "#d80b8c",
  `dark blue`    = "#00aeef",
  `dark grey`    = "#7f7f7f",
  `yellow`       = "#ffc000",
  `purple`       = "#7030a0",
  `med purple`   = "#5753d0",
  `med pink`     = "#f75dbe",
  `med blue`     = "#5cd3ff",
  `med grey`     = "#a5a7a5",
  `light purple` = "#c7c6ef",
  `light pink`   = "#fcc9e9",
  `light blue`   = "#c9f0ff",
  `light grey`   = "#dddedd"
)

# Function to extract Mount Sinai colors as hex codes
# Use Character names of MountSinai_colors

MountSinai_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (MountSinai_colors)
  
  MountSinai_colors[cols]
}

# Create palettes 
MountSinai_palettes <- list(
  `all`   = MountSinai_cols("dark purple","dark pink","dark blue","dark grey",
                            "med purple","med pink","med blue","med grey", 
                            "light purple","light pink","light blue","light grey"),
  
  `dark`  = MountSinai_cols("dark purple","dark grey",
                            "yellow","med pink","dark pink","dark blue",
                            "med purple","med grey","med blue"),
  
  `main`  = MountSinai_cols("dark purple","dark grey","dark pink","dark blue","med purple","med pink","med blue","med grey"),
  
  `purple`  = MountSinai_cols("dark purple","med purple","light purple"),
  
  `pink`  = MountSinai_cols("dark pink","med pink","light pink"),
  
  `blue`  = MountSinai_cols("dark blue", "med blue", "light blue"),
  
  `grey`  = MountSinai_cols("dark grey", "med grey", "light grey"),
  
  `purpleGrey` = MountSinai_cols("dark purple", "dark grey"),
  
  `pinkBlue` = MountSinai_cols("dark pink", "dark blue")
  
)

# MountSinai_palettes
# Return function to interpolate a Mount Sinai color palette
# default value is the main palette, reverse = True will change the order
print("2")

MountSinai_pal <- function(palette = "all", reverse = FALSE, ...) {
  pal <- MountSinai_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


# Scale Function for ggplot can be used instead of scale_color_manual
scale_color_MountSinai <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- MountSinai_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("MountSinai_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

# Scale Fill for ggplot insetead of scale_fill_manual 
scale_fill_MountSinai <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- MountSinai_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("MountSinai_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
#### Global Filepaths
#### Global Filepaths
if(file.exists("J:/")){
  start <- "J:" #Comment when publishing to RConnect
  home_path <- paste0(start,"/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/")
  start_shared <- "J:"
  #dsn <- "OAO Cloud DB Staging"
} else{
  start <- "/data"  #Uncomment when publishing to RConnect
  home_path <- paste0(start,"/Scorecards_Staging/")
  start_shared <- "/SharedDrive"
  #install.packages("reshape2", repos = "http://cran.us.r-project.org")
  #dsn <- "OAO Cloud DB Staging" 
}

# metrics_final_df_path <- paste0(home_path, "metrics_final_df.rds")
# budget_to_actual_path <- paste0(home_path, "Summary Repos/Budget to Actual.xlsx")
# target_mapping_path <- paste0(home_path, "MSHS Scorecards Target Mapping 2022-04-13.xlsx")
# operational_metrics_path <- paste0(home_path, "Balanced Scorecards Data Input.xlsx")
# operational_metrics_engineering_path <- paste0(home_path, 'Summary Repos/CM KPI.xlsx')
# operational_metrics_environmental_path <- paste0(home_path, "Summary Repos/TAT - EVS.xlsx")
# census_days_path <- paste0(home_path, "Finance/Monthly Stats Summary for benchmarking 20211013.xlsx")
# 
# 
# # File path for BME/CE KPI metrics
# bmekpi_table_path <- paste0(home_path, "Summary Repos/KPIs.xlsx")
# bmedi_table_path <- paste0(home_path, "Summary Repos/DisruptionsAndIssuesMonthly.xlsx")
# 
# # File path for Imaging DR Ops metrics
# imagingDR_path <- paste0(home_path, "Summary Repos/Imaging-DR.xlsx")
# 
# # File path for Nursing metrics
# nursing_path <- paste0(home_path, "Summary Repos/Nursing.xlsx")
# 
# # File path for ED metrics
# ed_path <- paste0(home_path, "Summary Repos/EDSummary.xlsx")
# 
# 
# # File path for Lab KPI metrics
# ops_metrics_lab_tat_path <- paste0(home_path, "Summary Repos/Lab TAT Metrics.xlsx")
# ops_metrics_lab_prof_test_path <- paste0(home_path, "Summary Repos/Lab Prof Testing Metrics.xlsx")
# 
# # File path for Security KPI metrics
# security_incident_reports_path <- paste0(home_path,
#                                            "Summary Repos/",
#                                            "Security Incident Reports.xlsx")
# 
# security_events_path <- paste0(home_path,
#                                "Summary Repos/",
#                                "Security Monthly Events.xlsx")
# 
# 
# # File path for saving the prior version of Dept Summary data
# hist_archive_path <- paste0(home_path, "Summary Repos/Hist Archive/")
# 
# #
# engineering_table_path <- paste0(home_path, "Summary Repos/CM KPI.xlsx")
# pt_exp_table_path <- paste0(home_path, "Summary Repos/Patient Experience.xlsx")
# evs_table_path <- paste0(home_path, "Summary Repos/TAT - EVS.xlsx")
# transport_table_path <- paste0(home_path, "Summary Repos/TAT - Transport.xlsx")

# Read in processed data ---------------------------------------------------------------------------
## Set data path ==================================================================================
conn <- dbConnect(odbc(), dsn)
print("3")
# data_path <- here()
# metrics_final_df <- readRDS(metrics_final_df_path) # Load processed Premier productivity data 


key_volume_mapping_path <- paste0(start_shared, "/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Mapping/MSHS_Reporting_Definition_Mapping.xlsx")

target_mapping <- tbl(conn, "BSC_TARGET_STATUS") %>% collect() %>%
                  rename(Service = SERVICE,
                         Site = SITE,
                         Metric_Group = METRIC_GROUP,
                         Metric_Name = METRIC_NAME,
                         Metric_Name_Submitted = METRIC_NAME_SUBMITTED,
                         Target = TARGET,
                         Green_Status = GREEN_STATUS,
                         Yellow_Status = YELLOW_STATUS,
                         Red_Status = RED_STATUS,
                         Green_Start = GREEN_START,
                         Green_End = GREEN_END,
                         Yellow_Start = YELLOW_START,
                         Yellow_End = YELLOW_END,
                         Red_Start = RED_START,
                         Red_End = RED_END)
metric_mapping_database <- tbl(conn, "BSC_MAPPING_TABLE") %>% collect() %>%
                            rename(Service = SERVICE,
                                   General_Group = GENERAL_GROUP,
                                   Metric_Group = METRIC_GROUP,
                                   Metric_Name_Summary = METRIC_NAME_SUMMARY,
                                   Metric_Name = METRIC_NAME,
                                   Metric_Name_Submitted = METRIC_NAME_SUBMITTED,
                                   Metric_Unit = METRIC_UNIT,
                                   Reporting_Tab = REPORTING_TAB,
                                   Display_Order = DISPLAY_ORDER
                                   )



# target_mapping <- read_excel(target_mapping_path, sheet = "Targets and Status") # Import updated target mapping file
# # metric_mapping_raw <- read_excel(target_mapping_path, sheet = "Metric Mapping")
# metric_mapping_database <- read_excel(target_mapping_path, sheet = "Metric Mapping Database")
# # metric_grouping <-  read_excel(target_mapping_path, sheet = "Metric Group v2") # Import Metric Group
# # summary_metrics <- read_excel(target_mapping_path, sheet = "Summary Metrics v2") # Import Summary Metrics
# budget_mapping <- read_excel(target_mapping_path, sheet = "Budget")

# Sites included -----------------------------------------------------------------------------------
sites_inc <- c("MSB","MSBI","MSH","MSM","MSQ","MSW","NYEE")
print("4")

dttm <- function(x) {
  as.POSIXct(x,format="%m/%d/%Y",tz=Sys.timezone(),origin = "1970-01-01")
}


#site_mapping <- tbl(conn, "BSC_SITE_LPM") %>% collect()
report_date_mapping <- tbl(conn, "BSC_REPORT_DATES_LPM") %>% collect() %>% 
                        rename(`Report Data Updated until` = REPORT_DATA_UPDATED_UNTIL,
                               `Dashboard Month` = DASHBOARD_MONTH,
                               `Report Release Date` = REPORT_RELEASE_DATE)

cost_rev_mapping <- tbl(conn, "BSC_COST_REV_MAPPING") %>% collect() %>%
                      rename(Metric = METRIC,
                             Metric_Name_Submitted = METRIC_NAME_SUBMITTED,
                             Metric_Group = METRIC_GROUP,
                             Metric_Name = METRIC_NAME)


key_vol_mapping <- read_excel(key_volume_mapping_path,
                              sheet = "Sheet1", col_names = TRUE, na = c("", "NA")) # Premier Reporting ID-Key Volume mapping
key_vol_mapping <- key_vol_mapping %>% filter(!is.na(DEFINITION.CODE))
key_vol_mapping_oracle <- tbl(conn, "BSC_KEY_VOLUME_MAPPING_ORACLE") %>% collect()

processed_df_cols <- c("Service", "Site", "Metric_Group", "Metric_Name",
                       "Premier_Reporting_Period", "Reporting_Month",
                       "value_rounded") # All columns needed in final merged data set
print("5")


# Code for making name filed Mandatory ----
# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

fieldsAll <- c("name_1")

# which fields are mandatory
fieldsMandatory <- fieldsAll

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    h2(label, style = "display: inline;"),
    span("*", class = "mandatory_star")
  )
}

print("6")

# operational_metrics <- read_excel(operational_metrics_path, sheet = "Sheet1", na = "")

transform_dt <- function(dt, names_to, values_to){
  # dt <- data
  # names_to <- "Month"
  # values_to <- "Actual Revenue"
  drop <- c("Service")#, "Metric")
  dt <- dt[ , !(names(dt) %in% drop)]
  pivot_longer(dt, c(Site, Metric), names_to = as.character(names_to), values_to = as.character(values_to)) %>%
    drop_na(values_to)
}

print("7")

engineering_data_process <- function(data){
  engineering_data <- data %>%
    pivot_longer(c(-Metric, -Site),
                 names_to = "Month",
                 values_to = "Value") %>%
    pivot_wider(names_from = "Metric", values_from = Value)
  
}

target_mapping_analysis <- target_mapping %>%
  select(-contains("Status")) %>%
  filter(!(Target %in% c("Budget"))) %>%
  mutate_at(vars(contains(c("Target", "_Start", "_End"))), as.numeric) %>%
  distinct()

target_mapping_reference <- target_mapping %>%
  select(-contains("_Start"), -contains("_End")) %>%
  # filter(Target != "Remove") %>%
  distinct()

print("8")

# target_mapping_reference <- left_join(target_mapping_reference,
#                                       metric_unit_filter_new)

high_level_order <- c("Premier", "Budget", "Operational", "Patient Experience")

# metric_mapping_summary_site <- metric_mapping_raw %>%
#   # Remove Data Table column since it isn't used anywhere
#   # Remove columns ending in _Incl since this is used for the KPI Breakout tab
#   select(-`Data Table`, -contains("_Breakout_Order")) %>%
#   # Pivot longer based on service line columns
#   pivot_longer(cols = contains("_Summary_Site_Order"),
#                names_to = "Service",
#                values_to = "Display_Order") %>%
#   # Remove any metrics that are not displayed in the Summary or Site tabs
#   filter(!is.na(Display_Order)) %>%
#   # Convert General_Group to an ordered factor
#   # This ensures consistent visualization even if numbers entered in Display_Order are incorrect (ie, ED and Nursing)
#   mutate(General_Group = factor(General_Group,
#                                 levels = high_level_order,
#                                 ordered = TRUE)) %>%
#   # Arrange by service line, general grouping, and display order to ensure consistency
#   arrange(Service, General_Group, Display_Order) %>%
#   mutate(
#     # Remove _Order from service column
#     Service = str_extract(Service, ".*(?=_Summary_Site_Order)"),
#     # Fix abbreviated service names
#     Service = ifelse(str_detect(Service, "EVS"), "Environmental Services",
#                      ifelse(str_detect(Service, "Biomed"),
#                             "Biomed / Clinical Engineering",
#                             ifelse(str_detect(Service, "Food"), "Food Services",
#                                    ifelse(str_detect(Service, "Transport"),
#                                           "Patient Transport",
#                                           Service)))),
#     # Convert General_Group from factor back to character now that data is ordered properly
#     General_Group = as.character(General_Group),
#     # # Update Overtime Hours - % (Premier)
#     # across(.cols = everything (),
#     #        .fns = function(x) {
#     #          str_replace(x,
#     #                      "\\ %\\ \\(Premier\\)",
#     #                      "\\ Hours\\ \\-\\ %\\ \\(Premier\\)")
#     #          })
#     )

print("9")
metric_mapping_summary_site <- metric_mapping_database %>%
  filter(Reporting_Tab %in% "Summary and Site") %>%
  select(-Reporting_Tab) %>%
  mutate(General_Group = factor(General_Group,
                                levels = high_level_order,
                                ordered = TRUE)) %>%
  arrange(Service, General_Group, Display_Order) %>%
  mutate(General_Group = as.character(General_Group))

# vector for budget_data_repo function for summary and site tabs in server.R

budget_to_actual_summary_table_metrics <- c("Budget to Actual Variance - Non Labor (Monthly)",
                                            "Budget to Actual Variance - Total (Monthly)",
                                            "Budget to Actual Variance - Labor (Monthly)",
                                            "Budget to Actual Variance - Non Labor (YTD)",
                                            "Budget to Actual Variance - Total (YTD)",
                                            "Budget to Actual Variance - Labor (YTD)",
                                            "Budget_Total (Monthly)",
                                            "Budget_Total (YTD)")



# metric_mapping_breakout <- metric_mapping_raw %>%
#   # Remove Data Table column since it isn't used anywhere
#   # Remove columns ending in _Incl since this is used for the KPI Breakout tab
#   select(-`Data Table`, -contains("_Summary_Site_Order")) %>%
#   # Pivot longer based on service line columns
#   pivot_longer(cols = contains("_Breakout_Order"),
#                names_to = "Service",
#                values_to = "Display_Order") %>%
#   # Remove any metrics that are not displayed in the Summary or Site tabs
#   filter(!is.na(Display_Order)) %>%
#   # Convert General_Group to an ordered factor
#   # This ensures consistent visualization even if numbers entered in Display_Order are incorrect (ie, ED and Nursing)
#   mutate(General_Group = factor(General_Group,
#                                 levels = high_level_order,
#                                 ordered = TRUE),
#          # Remove _Order from service column
#          Service = str_extract(Service, ".*(?=_Breakout_Order)"),
#          # Fix abbreviated service names
#          Service = ifelse(str_detect(Service, "EVS"), "Environmental Services",
#                           ifelse(str_detect(Service, "Biomed"),
#                                  "Biomed / Clinical Engineering",
#                                  ifelse(str_detect(Service, "Food"), "Food Services",
#                                         ifelse(str_detect(Service, "Transport"),
#                                                "Patient Transport",
#                                                Service))))) %>%
#   # Arrange by service line, general grouping, and display order to ensure consistency
#   arrange(Service, General_Group, Display_Order) %>%
#   mutate(
#     # # Remove _Order from service column
#     # Service = str_extract(Service, ".*(?=_Breakout_Order)"),
#     # # Fix abbreviated service names
#     # Service = ifelse(str_detect(Service, "EVS"), "Environmental Services",
#     #                  ifelse(str_detect(Service, "Biomed"),
#     #                         "Biomed / Clinical Engineering",
#     #                         ifelse(str_detect(Service, "Food"), "Food Services",
#     #                                ifelse(str_detect(Service, "Transport"),
#     #                                       "Patient Transport",
#     #                                       Service)))),
#     # Convert General_Group from factor back to character now that data is ordered properly
#     General_Group = as.character(General_Group),
#     # # Update Overtime Hours - % (Premier)
#     # across(.cols = everything (),
#     #        .fns = function(x) {
#     #          str_replace(x,
#     #                      "\\ %\\ \\(Premier\\)",
#     #                      "\\ Hours\\ \\-\\ %\\ \\(Premier\\)")
#     #        })
#     )

print("10")
metric_mapping_breakout <- metric_mapping_database %>%
  filter(Reporting_Tab %in% "Breakout") %>%
  select(-Reporting_Tab) %>%
  mutate(General_Group = factor(General_Group,
                                levels = high_level_order,
                                ordered = TRUE)) %>%
  arrange(Service, General_Group, Display_Order) %>%
  mutate(General_Group = as.character(General_Group))
print("11")


system_productivity <- tbl(conn, "BSC_SYSTEM_WIDE_PRODUCTIVITY_FINANCE") %>% group_by(SERVICE) %>% summarise(max = max(REPORTING_MONTH)) %>% collect()
# Source files for processing service line data -------------------
function_sources <- list.files("Functions", full.names = T, recursive = T)
sapply(function_sources, source, echo = T)
source(paste0("Functions/metrics_final_df_subset_and_merge.R"))
source(paste0("Functions/manual_format_check.R"))
# source("ClinicalNurtrition.R")
# source("OvertimeNew.R")
# source("lab_processing.R")
# source("EVS.R")
# source("patient_experience.R")
# source("security_processing.R")
# source("Transportation.R")
# source("biomed.R")
# source("ImagingDR.R")
# source("Imaging.R")
# source("Engineering.R")
# source("Overtime.R")
# source("Census Days.R")
# source("nursing.R")
# source("ED.R")
# source("productivity.R")
# source("budget_to_actual_new_file.R")
# source("productivity_update.R")
# source("peri_op_processing.R")
dbDisconnect(conn)
