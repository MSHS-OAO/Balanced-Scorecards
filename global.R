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
})

# source("EVS.R")
# source("press_ganey.R")
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

# Color Function that can be used to call all colors is "MountSinai_cols()"
# Use in ggplot 

#MountSinai_cols()       # will provide all colors and their hex codes in a table 
#MountSinai_cols("pink") # will provide color name and the hex code for the pink color

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
# budget_to_actual_path <- here::here("Data/Summary Repos/Budget to Actual.xlsx")
# metrics_final_df_path <- here::here("Data/metrics_final_df.rds")
# key_volume_mapping_path <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Mapping/MSHS_Reporting_Definition_Mapping.xlsx"
# target_mapping_path <- here::here("Data/MSHS Scorecards Target Mapping.xlsx")
# operational_metrics_path <- here::here("Data/Balanced Scorecards Data Input.xlsx")
# operational_metrics_engineering_path <- here("Data/Summary Repos/CM KPI.xlsx")
# operational_metrics_environmental_path <- here("Data/Summary Repos/TAT - EVS.xlsx")
# census_days_path <- "Data/Finance/Monthly Stats Summary for benchmarking 20211013.xlsx"
# operational_metrics_lab_path <- here("Data/Summary Repos/Lab - Metrics.xlsx")


start <- "J:" #Comment when publishing to RConnect
home_path <- paste0(start,"/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/")

# start <- "/data"  #Uncomment when publishing to RConnect
# home_path <- paste0(start,"/Scorecards_Staging/")
# start_shared <- "/SharedDrive"
start_shared <- "J:"
metrics_final_df_path <- paste0(home_path, "metrics_final_df.rds")
budget_to_actual_path <- paste0(home_path, "Summary Repos/Budget to Actual.xlsx")
target_mapping_path <- paste0(home_path, "MSHS Scorecards Target Mapping KEN.xlsx")
# target_mapping_path <- ("C:/Users/villea04/Downloads/MSHS Scorecards Target Mapping.xlsx")
operational_metrics_path <- paste0(home_path, "Balanced Scorecards Data Input.xlsx")
operational_metrics_engineering_path <- paste0(home_path, 'Summary Repos/CM KPI.xlsx')
operational_metrics_environmental_path <- paste0(home_path, "Summary Repos/TAT - EVS.xlsx")
census_days_path <- paste0(home_path, "Finance/Monthly Stats Summary for benchmarking 20211013.xlsx")


# File path for BME/CE KPI metrics
bmekpi_table_path <- paste0(home_path, "Summary Repos/KPIs.xlsx")
bmedi_table_path <- paste0(home_path, "Summary Repos/DisruptionsAndIssuesMonthly.xlsx")

# File path for Imaging DR Ops metrics
imagingDR_path <- paste0(home_path, "Summary Repos/Imaging-DR.xlsx")

# File path for Nursing metrics
nursing_path <- paste0(home_path, "Summary Repos/Nursing.xlsx")

# File path for ED metrics
ed_path <- paste0(home_path, "Summary Repos/EDSummary.xlsx")


# File path for Lab KPI metrics
ops_metrics_lab_tat_path <- paste0(home_path, "Summary Repos/Lab TAT Metrics.xlsx")
ops_metrics_lab_prof_test_path <- paste0(home_path, "Summary Repos/Lab Prof Testing Metrics.xlsx")

# File path for Security KPI metrics
security_incident_reports_path <- paste0(home_path,
                                           "Summary Repos/",
                                           "Security Incident Reports.xlsx")

security_events_path <- paste0(home_path,
                               "Summary Repos/",
                               "Security Monthly Events.xlsx")


# File path for saving the prior version of Dept Summary data
hist_archive_path <- paste0(home_path, "Summary Repos/Hist Archive/")

#
key_volume_mapping_path <- paste0(start, "/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Mapping/MSHS_Reporting_Definition_Mapping.xlsx")
engineering_table_path <- paste0(home_path, "Summary Repos/CM KPI.xlsx")
press_ganey_table_path <- paste0(home_path, "Summary Repos/Press Ganey.xlsx")
evs_table_path <- paste0(home_path, "Summary Repos/TAT - EVS.xlsx")
transport_table_path <- paste0(home_path, "Summary Repos/TAT - Transport.xlsx")

# Read in processed data ---------------------------------------------------------------------------
## Set data path ===================================================================================
data_path <- here()
metrics_final_df <- readRDS(metrics_final_df_path) # Load processed Premier productivity data 

target_mapping <- read_excel(target_mapping_path, sheet = "Target") # Import target mapping file
target_mapping_v2 <- read_excel(target_mapping_path, sheet = "Target v2") # Import updated target mapping file
metric_grouping <-  read_excel(target_mapping_path, sheet = "Metric Group v2") # Import Metric Group
summary_metrics <- read_excel(target_mapping_path, sheet = "Summary Metrics v2") # Import Summary Metrics
budget_mapping <- read_excel(target_mapping_path, sheet = "Budget")

# metric_grouping_order <- as.factor(unique(metric_grouping$Metric_Group)) # Define order of metrics displayed
# 
# metric_grouping_filter <- metric_grouping %>%
#   pivot_longer(
#     6:length(metric_grouping),
#     names_to = "Service",
#     values_to = "Inclusion"
#   ) %>%
#   filter(!is.na(Inclusion)) %>%
#   arrange(Service)
# 
# 
# 
# summary_metric_filter <- summary_metrics %>%
#   pivot_longer(
#     7:length(summary_metrics),
#     names_to = "Service",
#     values_to = "Order"
#   ) %>%
#   filter(!is.na(Order)) %>%
#   arrange(Order) 
# 
# metric_unit_filter <- unique(metric_grouping[, c("Metric_Group","Metric_Name","Metric_Unit")])
# metric_unit_filter_summary <- unique(summary_metric_filter[, c("Summary_Metric_Name","Metric_Unit")])
# metric_unit_perc <- unique((metric_unit_filter_summary %>% filter(Metric_Unit == "Percent"))$Summary_Metric_Name)
# # Fix naming of Overtime Hours - % (Premier)
# metric_unit_filter_summary_new <- metric_unit_filter_summary %>%
#   mutate(Summary_Metric_Name = str_replace(Summary_Metric_Name,
#                                            "\\ %\\ \\(Premier\\)",
#                                            "\\ Hours\\ \\-\\ %\\ \\(Premier\\)"))
# 
# metric_unit_perc_new <- str_replace(metric_unit_perc,
#                                     "\\ %\\ \\(Premier\\)",
#                                     "\\ Hours\\ \\-\\ %\\ \\(Premier\\)")
# 
# # Need to create a new metric_unit_filter that includes Metric_Name_Submitted
# # This is needed since this column is used in the KPI Breakout tab and is not
# # in metric_unit_filte, resulting in metrics not being formatted properly
# metric_unit_filter_new <- unique(metric_grouping[, c("Metric_Group",
#                                                      "Metric_Name",
#                                                      "Metric_Name_Submitted",
#                                                      "Metric_Unit")])
# 
# metric_unit_filter_new <- metric_unit_filter_new %>%
#   mutate(Metric_Name = str_replace(Metric_Name,
#                                    "\\ %\\ \\(Premier\\)",
#                                    "\\ Hours\\ \\-\\ %\\ \\(Premier\\)")) %>%
#   select(-Metric_Name)
# 
# # Reactive Data Functions --------------------------------------------------------------------------
# ## CAN THESE VARIABLES/FUNCTIONS BE REMOVED? THEY DON'T APPEAR TO BE USED ANYWHERE.
# ## Summary Tab Data
# groupByFilters_1 <- function(dt, campus, service){
#   result <- dt %>% filter(Site %in% campus, Service %in% service)
#   return(result)
# }
# 
# ## Comparison and Breakout Tab Data
# groupByFilters_2 <- function(dt, campus, service, metric){
#   result <- dt %>% filter(Site %in% campus, Service %in% service, Metric_Name %in% metric)
#   return(result)
# }

# Sites included -----------------------------------------------------------------------------------
sites_inc <- c("MSB","MSBI","MSH","MSM","MSQ","MSW","NYEE")

# Metric Group Order ------------------------------------------------------------------------------- 
# metric_group_order <- c("Productivity", "Overtime Hours", "Budget to Actual")
## CAN THIS BE REMOVED? WE REDEFINE THIS IN SERVER.R
# metric_group_order <- metric_grouping_order

# Summary Tab Metrics ------------------------------------------------------------------------------ 
# summary_tab_metrics <- c("Worked Hours Productivity Index", "Overtime Percent of Paid Hours", 
#                               "Overtime Dollars - % (Finance)", "Actual Worked FTE",
#                               "Budget to Actual Variance - Total")
# summary_tab_metrics <- as.factor(unique(summary_metric_filter$Metric_Group))

## CAN THIS BE REMOVED? NOT USED ANYWHERE
summary_tab_target <- c("Worked Hours Productivity Index", "Overtime Percent of Paid Hours", 
                        "Overtime Dollars - % (Finance)") 



library(DBI)
 #con <- dbConnect(odbc::odbc(), "OAO_Data", timeout = 30)



dttm <- function(x) {
  as.POSIXct(x,format="%m/%d/%Y",tz=Sys.timezone(),origin = "1970-01-01")
}


# Import all reference / mapping files needed ----
site_path <- here() # Set path to new data (raw data)
site_mapping <- read_excel(target_mapping_path, 
                           sheet = "Site_New",  col_names = TRUE, na = c("", "NA")) # Premier site-service mapping

report_date_mapping <- read_excel(target_mapping_path, 
                                  sheet = "Report Dates",  col_names = TRUE, na = c("", "NA")) # Premier reporting-dashboard date mapping 


## Redundant to metric_grouping_filter
# metric_group_mapping <- read_excel(target_mapping_path,
#                                    sheet = "Metric Group v2",  col_names = TRUE, na = c("", "NA")) # Metric group mapping
# metric_group_mapping <- metric_group_mapping %>% # Processing metric group mapping file
#   pivot_longer(
#     6:length(metric_group_mapping),
#     names_to = "Service",
#     values_to = "Inclusion"
#   ) %>%
#   filter(!is.na(Inclusion))

cost_rev_mapping <- read_excel(target_mapping_path, 
                               sheet = "Cost and Rev Mapping",  col_names = TRUE, na = c("", "NA")) # Metric group mapping

key_vol_mapping <- read_excel(key_volume_mapping_path,
                              sheet = "Sheet1", col_names = TRUE, na = c("", "NA")) # Premier Reporting ID-Key Volume mapping  
key_vol_mapping <- key_vol_mapping %>% filter(!is.na(DEFINITION.CODE))

processed_df_cols <- c("Service","Site","Metric_Group","Metric_Name","Premier_Reporting_Period",
                       "Reporting_Month","value_rounded","Target","Status") # All columns needed in final merged data set




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



operational_metrics <- read_excel(operational_metrics_path, sheet = "Sheet1", na = "")

transform_dt <- function(dt, names_to, values_to){
  # dt <- data
  # names_to <- "Month"
  # values_to <- "Actual Revenue"
  drop <- c("Service")#, "Metric")
  dt <- dt[ , !(names(dt) %in% drop)]
  pivot_longer(dt, c(Site, Metric), names_to = as.character(names_to), values_to = as.character(values_to)) %>%
    drop_na(values_to)
}








options(shiny.maxRequestSize=500*1024^2)





engineering_data_process <- function(data){
  engineering_data <- data %>%
    pivot_longer(c(-Metric, -Site),
                 names_to = "Month",
                 values_to = "Value") %>%
    pivot_wider(names_from = "Metric", values_from = Value)
  
}


# Code for processing and using new target mapping file and metrics_final_df without Target and Status included
metrics_final_df_new <- metrics_final_df %>%
  select(-Target, -Status) %>%
  mutate(Metric_Name = ifelse(Metric_Name %in% "Overtime % (Premier)",
                              "Overtime Hours - % (Premier)",
                              str_replace(Metric_Name, "\\sMOM", "")),
         Metric_Group = ifelse(str_detect(Metric_Group,
                                          "(Press Ganey)|(HCAHPS)"),
                               "Patient Experience", Metric_Group))

target_mapping_analysis <- target_mapping_v2 %>%
  select(-Range_1, -Range_2, -contains("Status")) %>%
  filter(!(Target %in% c("Remove", "Budget"))) %>%
  mutate_at(vars(contains(c("Target", "_Start", "_End"))), as.numeric) %>%
  distinct()

target_mapping_reference <- target_mapping_v2 %>%
  select(-Range_1, -Range_2, -Status, -contains("_Start"), -contains("_End")) %>%
  filter(Target != "Remove") %>%
  distinct()

# target_mapping_reference <- left_join(target_mapping_reference,
#                                       metric_unit_filter_new)

metric_mapping_raw <- read_excel(target_mapping_path, sheet = "Metric Mapping")

high_level_order <- c("Premier", "Budget", "Operational", "Patient Experience")

metric_mapping_summary_site <- metric_mapping_raw %>%
  # Remove Data Table column since it isn't used anywhere
  # Remove columns ending in _Incl since this is used for the KPI Breakout tab
  select(-`Data Table`, -contains("_Breakout_Order")) %>%
  # Pivot longer based on service line columns
  pivot_longer(cols = contains("_Summary_Site_Order"),
               names_to = "Service",
               values_to = "Display_Order") %>%
  # Remove any metrics that are not displayed in the Summary or Site tabs
  filter(!is.na(Display_Order)) %>%
  # Convert General_Group to an ordered factor
  # This ensures consistent visualization even if numbers entered in Display_Order are incorrect (ie, ED and Nursing)
  mutate(General_Group = factor(General_Group,
                                levels = high_level_order,
                                ordered = TRUE)) %>%
  # Arrange by service line, general grouping, and display order to ensure consistency
  arrange(Service, General_Group, Display_Order) %>%
  mutate(
    # Remove _Order from service column
    Service = str_extract(Service, ".*(?=_Summary_Site_Order)"),
    # Fix abbreviated service names
    Service = ifelse(str_detect(Service, "EVS"), "Environmental Services",
                     ifelse(str_detect(Service, "Biomed"),
                            "Biomed / Clinical Engineering",
                            ifelse(str_detect(Service, "Food"), "Food Services",
                                   ifelse(str_detect(Service, "Transport"),
                                          "Patient Transport",
                                          Service)))),
    # Convert General_Group from factor back to character now that data is ordered properly
    General_Group = as.character(General_Group),
    # Update Overtime Hours - % (Premier)
    across(.cols = everything (),
           .fns = function(x) {
             str_replace(x,
                         "\\ %\\ \\(Premier\\)",
                         "\\ Hours\\ \\-\\ %\\ \\(Premier\\)")
             }))

metric_mapping_breakout <- metric_mapping_raw %>%
  # Remove Data Table column since it isn't used anywhere
  # Remove columns ending in _Incl since this is used for the KPI Breakout tab
  select(-`Data Table`, -contains("_Summary_Site_Order")) %>%
  # Pivot longer based on service line columns
  pivot_longer(cols = contains("_Breakout_Order"),
               names_to = "Service",
               values_to = "Display_Order") %>%
  # Remove any metrics that are not displayed in the Summary or Site tabs
  filter(!is.na(Display_Order)) %>%
  # Convert General_Group to an ordered factor
  # This ensures consistent visualization even if numbers entered in Display_Order are incorrect (ie, ED and Nursing)
  mutate(General_Group = factor(General_Group,
                                levels = high_level_order,
                                ordered = TRUE)) %>%
  # Arrange by service line, general grouping, and display order to ensure consistency
  arrange(Service, General_Group, Display_Order) %>%
  mutate(
    # Remove _Order from service column
    Service = str_extract(Service, ".*(?=_Breakout_Order)"),
    # Fix abbreviated service names
    Service = ifelse(str_detect(Service, "EVS"), "Environmental Services",
                     ifelse(str_detect(Service, "Biomed"),
                            "Biomed / Clinical Engineering",
                            ifelse(str_detect(Service, "Food"), "Food Services",
                                   ifelse(str_detect(Service, "Transport"),
                                          "Patient Transport",
                                          Service)))),
    # Convert General_Group from factor back to character now that data is ordered properly
    General_Group = as.character(General_Group),
    # Update Overtime Hours - % (Premier)
    across(.cols = everything (),
           .fns = function(x) {
             str_replace(x,
                         "\\ %\\ \\(Premier\\)",
                         "\\ Hours\\ \\-\\ %\\ \\(Premier\\)")
           }))


  


# Source files for processing service line data -------------------
source("lab_processing.R")
source("EVS.R")
source("press_ganey.R")
source("security_processing.R")
source("Transportation.R")
source("biomed.R")
source("ImagingDR.R")
source("Imaging.R")
source("Engineering.R")
source("Overtime.R")
source("Census Days.R")
source("nursing.R")
source("ED.R")
source("productivity.R")
source("budget_to_actual.R")
