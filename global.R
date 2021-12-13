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

source("EVS.R")
source("press_ganey.R")
# Maximize R Memory Size 
memory.limit(size = 8000000)

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
# start <- "/SharedDrive"  #Uncomment when publishing to RConnect
 home_path <- paste0(start,"/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/")
#home_path <- "/data/Scorecards_Data/"
metrics_final_df_path <- paste0(home_path, "metrics_final_df.rds")
budget_to_actual_path <- paste0(home_path, "Summary Repos/Budget to Actual.xlsx")
target_mapping_path <- paste0(home_path, "MSHS Scorecards Target Mapping.xlsx")
operational_metrics_path <- paste0(home_path, "Balanced Scorecards Data Input.xlsx")
operational_metrics_engineering_path <- paste0(home_path, 'Summary Repos/CM KPI.xlsx')
operational_metrics_environmental_path <- paste0(home_path, "Summary Repos/TAT - EVS.xlsx")
census_days_path <- paste0(home_path, "Finance/Monthly Stats Summary for benchmarking 20211013.xlsx")

# File path for Lab KPI metrics
ops_metrics_lab_tat_path <- paste0(home_path, "Summary Repos/Lab TAT Metrics.xlsx")
ops_metrics_lab_prof_test_path <- paste0(home_path, "Summary Repos/Lab Prof Testing Metrics.xlsx")

# File path for saving the prior version of Dept Summary data
hist_archive_path <- paste0(home_path, "Summary Repos/Hist Archive/")

#
key_volume_mapping_path <- paste0(start, "/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Mapping/MSHS_Reporting_Definition_Mapping.xlsx")
engineering_table_path <- paste0(home_path, "Summary Repos/CM KPI.xlsx")
press_ganey_table_path <- paste0(home_path, "Summary Repos/Press Ganey.xlsx")
evs_table_path <- paste0(home_path, "Summart Repos/TAT - EVS.xlsx")
# Read in processed data ---------------------------------------------------------------------------
## Set data path ===================================================================================
data_path <- here()
metrics_final_df <- readRDS(metrics_final_df_path) # Load processed Premier productivity data 

target_mapping <- read_excel(target_mapping_path, sheet = "Target") # Import target mapping file
metric_grouping <-  read_excel(target_mapping_path, sheet = "Metric Group") # Import Metric Group
summary_metrics <- read_excel(target_mapping_path, sheet = "Summary Metrics") # Import Summary Metrics

metric_grouping_order <- as.factor(unique(metric_grouping$Metric_Group)) # Define order of metrics displayed

metric_grouping_filter <- metric_grouping %>%
  pivot_longer(
    6:length(metric_grouping),
    names_to = "Service",
    values_to = "Inclusion"
  ) %>%
  filter(!is.na(Inclusion)) %>%
  arrange(Service)

summary_metric_filter <- summary_metrics %>%
  pivot_longer(
    7:length(metric_grouping),
    names_to = "Service",
    values_to = "Order"
  ) %>%
  filter(!is.na(Order)) %>%
  arrange(Order) 

metric_unit_filter <- unique(metric_grouping[, c("Metric_Group","Metric_Name","Metric_Unit")])
metric_unit_filter_summary <- unique(summary_metric_filter[, c("Summary_Metric_Name","Metric_Unit")])
metric_unit_perc <- unique((metric_unit_filter_summary %>% filter(Metric_Unit == "Percent"))$Summary_Metric_Name)

# Reactive Data Functions --------------------------------------------------------------------------
## Summary Tab Data
groupByFilters_1 <- function(dt, campus, service){
  result <- dt %>% filter(Site %in% campus, Service %in% service)
  return(result)
}

## Comparison and Breakout Tab Data
groupByFilters_2 <- function(dt, campus, service, metric){
  result <- dt %>% filter(Site %in% campus, Service %in% service, Metric_Name %in% metric)
  return(result)
}

# Sites included -----------------------------------------------------------------------------------
sites_inc <- c("MSB","MSBI","MSH","MSM","MSQ","MSW","NYEE")

# Metric Group Order ------------------------------------------------------------------------------- 
# metric_group_order <- c("Productivity", "Overtime Hours", "Budget to Actual")
metric_group_order <- metric_grouping_order

# Summary Tab Metrics ------------------------------------------------------------------------------ 
# summary_tab_metrics <- c("Worked Hours Productivity Index", "Overtime Percent of Paid Hours", 
#                               "Overtime Dollars - % (Finance)", "Actual Worked FTE",
#                               "Budget to Actual Variance - Total")
# summary_tab_metrics <- as.factor(unique(summary_metric_filter$Metric_Group))

summary_tab_target <- c("Worked Hours Productivity Index", "Overtime Percent of Paid Hours", 
                        "Overtime Dollars - % (Finance)") 





library(DBI)
 #con <- dbConnect(odbc::odbc(), "OAO_Data", timeout = 30)



dttm <- function(x) {
  as.POSIXct(x,format="%m/%d/%Y",tz=Sys.timezone(),origin = "1970-01-01")
}


# Import all reference / mapping files needed
site_path <- here() # Set path to new data (raw data)
site_mapping <- read_excel(target_mapping_path, 
                           sheet = "Site_New",  col_names = TRUE, na = c("", "NA")) # Premier site-service mapping

report_date_mapping <- read_excel(target_mapping_path, 
                                  sheet = "Report Dates",  col_names = TRUE, na = c("", "NA")) # Premier reporting-dashboard date mapping 



metric_group_mapping <- read_excel(target_mapping_path, 
                                   sheet = "Metric Group",  col_names = TRUE, na = c("", "NA")) # Metric group mapping
metric_group_mapping <- metric_group_mapping %>% # Processing metric group mapping file
  pivot_longer(
    6:length(metric_group_mapping),
    names_to = "Service",
    values_to = "Inclusion"
  ) %>%
  filter(!is.na(Inclusion))

cost_rev_mapping <- read_excel(target_mapping_path, 
                               sheet = "Cost and Rev Mapping",  col_names = TRUE, na = c("", "NA")) # Metric group mapping

key_vol_mapping <- read_excel(key_volume_mapping_path,
                              sheet = "Sheet1", col_names = TRUE, na = c("", "NA")) # Premier Reporting ID-Key Volume mapping  
key_vol_mapping <- key_vol_mapping %>% filter(!is.na(DEFINITION.CODE))

processed_df_cols <- c("Service","Site","Metric_Group","Metric_Name","Premier_Reporting_Period",
                       "Reporting_Month","value_rounded","Target","Status") # All columns needed in final merged data set

press_ganey_mapping <- read_excel(target_mapping_path, sheet = "Press Ganey")

### Process Productivity Data 

productivity_process <- function(data, data_nursing_radiology){
  # Import productivity data for depts excluding Nursing and Radiology 
  raw_prod_df <- data
  #raw_prod_df <- read_excel("Data/Dept Report Builder.xlsx")
  raw_prod_df <- raw_prod_df %>% select(!`Entity Time Period Desc`)
  
  ## Productivity data pre-processing
  ### Aggregate reporting period columns
  prod_df <- raw_prod_df %>% 
    mutate(id = seq_len(n())) %>% 
    melt(id.var = c('Corporation Code','Corporation Name', 'Entity Code', 'Entity', 'Department Reporting Definition ID',
                    'Department Reporting Definition Name', 'Key Volume', 'Mapped Facility Cost Centers', 'id'), na.rm = F) %>% 
    select(-c('id'))
  
  
  ### Process Reporting Period to Reporting Month
  prod_df$variable <- gsub("\\..*","", prod_df$variable) # Remove underscores at end of reporting period
  names(prod_df)[names(prod_df) == 'variable'] <- 'Premier_Reporting_Period' # Rename Reporting Period column
  prod_df <- prod_df %>%
    mutate(Reporting_Month_Start = as.Date(dttm(gsub(" .*$", "", Premier_Reporting_Period))))
  
  prod_df_final <- prod_df %>% # Fill in Metric name in a column
    mutate(Metrics = as.numeric(value),
           Metrics = ifelse(is.na(Metrics), value, ""),
           Metrics = ifelse(Metrics == "", NA, Metrics)) %>%
    fill(Metrics) %>%
    filter(!is.na(`Corporation Code`)) %>%
    unique()
  
  
  # Import productivity data fo Nursing and Radiology 
  raw_prod_nursing_rad_df <- data_nursing_radiology
  #raw_prod_nursing_rad_df <- read_excel("Data/Dept Report Builder_nursing_radiology.xlsx")
  raw_prod_nursing_rad_df <- raw_prod_nursing_rad_df %>% select(!`Entity Time Period Desc`)
  
  
  ## Productivity data pre-processing
  ### Aggregate reporting period columns
  prod_nursing_rad_df <- raw_prod_nursing_rad_df %>% 
    mutate(id = seq_len(n())) %>% 
    melt(id.var = c('Corporation Code','Corporation Name', 'Entity Code', 'Entity', 'Department Reporting Definition ID',
                    'Department Reporting Definition Name', 'Key Volume', 'Mapped Facility Cost Centers', 'id'), na.rm = F) %>% 
    select(-c('id'))
  
  
  ### Process Reporting Period to Reporting Month
  prod_nursing_rad_df$variable <- gsub("\\..*","", prod_nursing_rad_df$variable) # Remove underscores at end of reporting period
  names(prod_nursing_rad_df)[names(prod_nursing_rad_df) == 'variable'] <- 'Premier_Reporting_Period' # Rename Reporting Period column
  prod_nursing_rad_df <- prod_nursing_rad_df %>%
    mutate(Reporting_Month_Start = as.Date(dttm(gsub(" .*$", "", Premier_Reporting_Period))))
  
  prod_nursing_rad_df_final <- prod_nursing_rad_df %>% # Fill in Metric name in a column
    mutate(Metrics = as.numeric(value),
           Metrics = ifelse(is.na(Metrics), value, ""),
           Metrics = ifelse(Metrics == "", NA, Metrics)) %>%
    fill(Metrics) %>%
    filter(!is.na(`Corporation Code`)) %>%
    unique()
  
  
  # Merge two data imports and exclude duplicates 
  prod_df_merged <- rbind(prod_df_final, prod_nursing_rad_df_final)
  prod_df_merged <- prod_df_merged %>%
    unique()
  
  
  ## Map Site and Service Group
  prod_df_all <- merge(prod_df_merged, site_mapping[,c("DEFINITION.CODE","KEY.VOLUME","SITE", "Balanced.Scorecards.Groupings")], 
                       by.x = c("Department Reporting Definition ID", "Key Volume"),
                       by.y = c("DEFINITION.CODE", "KEY.VOLUME"), all.x = TRUE)
  
  names(prod_df_all)[names(prod_df_all) == "Balanced.Scorecards.Groupings"] <- "Service"
  names(prod_df_all)[names(prod_df_all) == "SITE"] <- "Site"
  
  
  prod_df_all <- prod_df_all %>% # Process Metric Group column
    filter(!is.na(Service)) %>%
    mutate(Metric_Name = trim(gsub("-.*","", Metrics)),
           Metric_Group = ifelse(grepl("Overtime", Metric_Name), "Overtime Hours", "Productivity"),
           value_rounded = round(parse_number(value),2))
  
  ### Map Premier Reporting Period -> Dashboard Month 
  report_date_mapping <- report_date_mapping %>%
    mutate(`Report Data Updated until` = format(as.Date(`Report Data Updated until`, "%m/%d/%Y"), "%m/%d/%Y"),
           `Report Release Date` = format(as.Date(`Report Release Date`, "%m/%d/%Y"), "%m/%d/%Y"),
           `Dashboard Month` = format(as.Date(`Dashboard Month`, "%m/%d/%Y"), "%m/%d/%Y"))
  
  report_data <- as.data.frame(report_date_mapping$`Report Data Updated until`)
  report_data <- col_concat(report_data, sep = "|")
  report_data <- paste(report_data, collapse = "|")
  
  prod_df_all <- prod_df_all %>% filter(grepl(report_data, Premier_Reporting_Period))
  prod_df_all <- prod_df_all %>% separate(Premier_Reporting_Period, c("Report_Start","Report_End"), sep = " - ", remove = FALSE)
  prod_df_all$Reporting_Month_Ref <- report_date_mapping$`Dashboard Month`[match(prod_df_all$`Report_End`,report_date_mapping$`Report Data Updated until`)]
  prod_df_all$Reporting_Month_Ref <- as.Date(prod_df_all$Reporting_Month_Ref, format = "%m/%d/%Y")
  
  prod_df_all <- prod_df_all[, !(names(prod_df_all) %in% c("Report_Start","Report_End"))]
  prod_df_all <- prod_df_all %>% unique()
  
  
  ## Calculate aggregate Overtime % and Productivity Index for Nursing and Imaging
  prod_df_aggregate <- prod_df_all %>%
    group_by(Service, Site, Metric_Group, Metric_Name, Premier_Reporting_Period, Reporting_Month_Ref) %>%
    summarise(value_rounded = round(mean(value_rounded, na.rm = TRUE),2)) %>%
    filter(Metric_Name != "Total Target Worked FTE") %>%
    filter(!(Service %in% c("Nursing","Imaging") & 
               Metric_Name %in% c("Overtime Percent of Paid Hours","Worked Hours Productivity Index")))
  
  
  nursing_rad_metric_calc <- prod_df_all %>% # Calculate Productivity and Overtime % separately 
    filter(Service %in% c("Nursing","Imaging")) %>%
    filter(Metric_Name %in% c("Total Target Worked FTE","Actual Worked FTE",
                              "Overtime Hours", "Total Paid hours")) %>%
    group_by(Service, Site, Metric_Name, Premier_Reporting_Period, Reporting_Month_Ref) %>%
    summarise(value_rounded = round(sum(value_rounded, na.rm = TRUE),2)) %>%
    pivot_wider(names_from = Metric_Name,
                values_from = value_rounded) %>%
    mutate(`Worked Hours Productivity Index` = round(`Total Target Worked FTE`/`Actual Worked FTE`,2),
           `Overtime Percent of Paid Hours` = round(`Overtime Hours`/`Total Paid hours`,2)) %>%
    pivot_longer(5:9,
                 names_to = "Metric_Name",
                 values_to = "value_rounded") %>%
    filter(Metric_Name %in% c("Worked Hours Productivity Index","Overtime Percent of Paid Hours")) %>%
    mutate_at(vars(value_rounded), ~replace(., is.nan(.), 0)) %>%
    mutate(Metric_Group = ifelse(Metric_Name == "Worked Hours Productivity Index", "Productivity", "Overtime Hours"))
  
  
  prod_df_aggregate_all <- bind_rows(prod_df_aggregate, nursing_rad_metric_calc) # Merge newly calculated productivity index and overtime % for nursing and radiology 
  
  prod_df_aggregate_all$Metric_Name <- metric_group_mapping$Metric_Name[match(prod_df_aggregate_all$Metric_Name, 
                                                                              metric_group_mapping$Metric_Name_Submitted)] # Map final Metric_Name
  
  
  ### Create Target Variance Column
  prod_target_status <- merge(prod_df_aggregate_all[, c("Service","Site","Metric_Name","Reporting_Month_Ref","value_rounded")],
                              target_mapping, by = c("Service","Site","Metric_Name"))
  
  prod_target_status <- prod_target_status %>%
    mutate(Variance = between(value_rounded, Range_1, Range_2)) %>% # Target mapping
    filter(Variance == TRUE)
  
  prod_df_final <- merge(prod_df_aggregate_all, prod_target_status[,c("Service","Site","Metric_Name","Reporting_Month_Ref","Target","Status")],
                         all = TRUE)
  
  prod_df_final$Reporting_Month <- format(as.Date(prod_df_final$Reporting_Month_Ref, format = "%Y-%m-%d"),"%m-%Y")
  prod_df_final$Reporting_Month_Ref <- NULL
  
  prod_df_final$Premier_Reporting_Period <- sub(".*- ", "", prod_df_final$Premier_Reporting_Period)
  
  # Subset processed data for merge 
  prod_df_merge <- prod_df_final[,processed_df_cols] 
  
  prod_df_merge$Reporting_Month_Ref <- as.Date(paste('01', as.yearmon(prod_df_merge$Reporting_Month, "%m-%Y")), format='%d %b %Y')
  
  metrics_final_df <- full_join(metrics_final_df,prod_df_merge)
}


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


operational_metrics_engineering <- read_excel(operational_metrics_engineering_path) %>% filter(Month >= "2020-12-01") %>%
                                    mutate_if(is.logical, as.character) %>%
                                    mutate_if(is.double, as.character) %>%
                                    select(-Hospital) %>%
                                    pivot_longer(cols = c(-Month, -Site),
                                                 names_to = "Metric",
                                                 values_to = "Value") %>%
                                    pivot_wider(names_from = "Month", values_from = Value)




cm_kpi <- function(data){
  
  #data <- operational_metrics_engineering
  #data$`2021-07-01` <- data$`2021-06-01`
  engineering_data <- data %>%
            pivot_longer(c(-Metric, -Site),
                         names_to = "Month",
                         values_to = "Value") %>%
            pivot_wider(names_from = "Metric", values_from = Value)
  
  
  raw_cm_df <- engineering_data
  
  
  ## Security CM KPI data pre-processing 
  cm_kpi_df <- raw_cm_df %>%
    mutate_at(vars(-Month, -Site), as.numeric) %>%
    pivot_longer(
      c(-Month, -Site), 
      names_to = "Metric_Name_Submitted",
      values_to = "value") %>%
    mutate(Service = "Engineering",
           Premier_Reporting_Period = format(as.Date(Month, format = "%Y-%m-%d"),"%b %Y"),
           Reporting_Month = format(as.Date(Month, format = "%Y-%m-%d"),"%m-%Y"),
           value_rounded = value)
  
  cm_kpi_df$Metric_Group <- metric_group_mapping$Metric_Group[match(cm_kpi_df$Metric_Name_Submitted, metric_group_mapping$Metric_Name_Submitted)]
  cm_kpi_df$Metric_Name <- metric_group_mapping$Metric_Name[match(cm_kpi_df$Metric_Name_Submitted, metric_group_mapping$Metric_Name_Submitted)]
  
  ### Create Target Variance Column
  cm_kpi_target_status <- merge(cm_kpi_df[, c("Service","Site","Metric_Group","Metric_Name","Reporting_Month","value_rounded")],
                                target_mapping, 
                                by = c("Service","Site","Metric_Group","Metric_Name"),
                                all = TRUE)
  
  cm_kpi_target_status <- cm_kpi_target_status %>%
    mutate(Variance = between(value_rounded, Range_1, Range_2)) %>% # Target mapping
    filter(Variance == TRUE)
  
  cm_kpi_df_final <- merge(cm_kpi_df, cm_kpi_target_status[,c("Service","Site","Metric_Group","Metric_Name","Reporting_Month","Target","Status")],
                           all = TRUE)
  
  
  # Subset processed data for merge 
  cm_kpi_df_final <- cm_kpi_df_final[,processed_df_cols]
  
  cm_kpi_df_final$Reporting_Month_Ref <- as.Date(paste('01', as.yearmon(cm_kpi_df_final$Reporting_Month, "%m-%Y")), format='%d %b %Y')
  
  
  metrics_final_df <- full_join(metrics_final_df,cm_kpi_df_final)
}


bislr_preprocess <- function(finance_bislr){
  ### Split out to actual data
  actual_index <- which(colnames(finance_bislr) == "Actual")
  finance_bislr_actual <- finance_bislr[,1:actual_index]
  col_names <- colnames(finance_bislr_actual)
  year_index <- str_find(col_names, "ACTUAL", 0)[1]
  year <- gsub(".*ACTUAL", "", col_names[6])
  
  finance_bislr_actual <- finance_bislr_actual %>% row_to_names(row_number = 1)  
  
  colname <- names(finance_bislr_actual)
  colname[7:length(finance_bislr_actual)-1] <- paste0(colname[7:length(finance_bislr_actual)-1], "-01-",year)
  colname[7:length(finance_bislr_actual)-1] <- format(as.Date(colname[7:length(finance_bislr_actual)-1], "%B-%d-%Y"),"%Y-%m-%d")
  
  
  colnames(finance_bislr_actual) <- colname
  
  names(finance_bislr_actual)[length(names(finance_bislr_actual))] <- "YTD" 
  
  colname <- names(finance_bislr_actual)
  
  finance_bislr_actual <- merge(finance_bislr_actual, site_mapping[,c("Balanced.Scorecards.Groupings", "SITE","COST.CENTER")],
                            by.x = c("Cost Center2"), by.y = c("COST.CENTER"))
  
  finance_bislr_actual <- finance_bislr_actual %>%
                            select(-`Functional Level`, -`Cost Center Desc`) %>%
                            rename(Service = Balanced.Scorecards.Groupings) %>%
                            rename(Site = SITE) %>%
                            select(-YTD)
  finance_bislr_actual <- finance_bislr_actual %>%
    pivot_longer(cols = c(-`Cost Center2`, -`Division Level 3 Desc`, -`Account Category Desc2`, -Service, -Site),
                 names_to = "Month",
                 values_to = "Month Actual") 
  finance_bislr_actual <- finance_bislr_actual[,c("Service", "Site", "Division Level 3 Desc", "Cost Center2", "Account Category Desc2", "Month", "Month Actual")]
  
  
  ###Split out to Budget Data
  budget_index <- which(colnames(finance_bislr) == "Budget")
  index_start <- actual_index+1
  
  finance_bislr_budget <- finance_bislr[,c(1:5,(actual_index+1):(budget_index-1))]
  
  col_names <- colnames(finance_bislr_budget)
  year_index <- str_find(col_names, "BUDGET", 0)[1]
  
  year <- gsub(".*BUDGET", "", col_names[6])
  
  finance_bislr_budget <- finance_bislr_budget %>% row_to_names(row_number = 1)  
  
  colname <- names(finance_bislr_budget)
  colname[6:length(finance_bislr_budget)] <- paste0(colname[6:length(finance_bislr_budget)], "-01-",year)
  colname[6:length(finance_bislr_budget)] <- format(as.Date(colname[6:length(finance_bislr_budget)], "%B-%d-%Y"),"%Y-%m-%d")
  
  colnames(finance_bislr_budget) <- colname
  
  finance_bislr_budget <- merge(finance_bislr_budget, site_mapping[,c("Balanced.Scorecards.Groupings", "SITE","COST.CENTER")],
                                by.x = c("Cost Center2"), by.y = c("COST.CENTER"))
  
  finance_bislr_budget <- finance_bislr_budget %>%
    select(-`Functional Level`, -`Cost Center Desc`) %>%
    rename(Service = Balanced.Scorecards.Groupings) %>%
    rename(Site = SITE) 
  
  finance_bislr_budget <- finance_bislr_budget %>%
    pivot_longer(cols = c(-`Cost Center2`, -`Division Level 3 Desc`, -`Account Category Desc2`, -Service, -Site),
                 names_to = "Month",
                 values_to = "Month Budget") 
  finance_bislr_budget <- finance_bislr_budget[,c("Service", "Site", "Division Level 3 Desc", "Cost Center2", "Account Category Desc2", "Month", "Month Budget")]
  
  finance_bislr_budget <- finance_bislr_budget %>% filter(!is.na(Service))
  
  finance_bislr_actual <- finance_bislr_actual %>% filter(!is.na(Service))
  
  finance_bislr_comb <- full_join(finance_bislr_actual,finance_bislr_budget)
}

#finance_bislr_comb <- bislr_preprocess(finance_bislr)

##Read in data from ExpTrend
# sheet_names <- excel_sheets("Data/FTI/ExpTrendSepMo2021.xlsx")
# sheet_names <- sheet_names[sheet_names != "Sheet3"]
# 
# 
# for (i in 1:length(sheet_names)){
#   
#   data <- read_excel("Data/FTI/ExpTrendSepMo2021.xlsx", sheet = sheet_names[i])
#   sheet_month <- colnames(data)[1]
#   data <- data %>% row_to_names(row_number = 1)
#   data$Month <- sheet_month
#   
#   print(i)
#   if(i == 1){
#     prev <- data
#   } else{
#     data <- full_join(data,prev)
#   }
#   
#   prev <- data
# }

exptrend_process <- function(finance_exptrend){

  finance_exptrend <-  finance_exptrend %>%
                        select(-FDIV,-SUBACCT,-ACCTNAME,-NAME, -YTD_BUD, -YTD_ACT) %>%
                        rename(`Account Category Desc2` = EXPTYPE,
                               Site = SITE,
                               `Cost Center2` = CC,
                               `Month Budget` = MO_BUD,
                               `Month Actual` = MO_ACT)
  
  
  finance_exptrend$`Account Category Desc2` <- gsub(".*2-", "", finance_exptrend$`Account Category Desc2`)
  finance_exptrend$`Account Category Desc2` <- gsub(".*1-", "", finance_exptrend$`Account Category Desc2`)
  
  finance_exptrend$`Account Category Desc2` <- gsub(".*SUPPLIES", "Supplies", finance_exptrend$`Account Category Desc2`)
  finance_exptrend$`Account Category Desc2` <- gsub(".*SALARIES", "Salary", finance_exptrend$`Account Category Desc2`)
  
  finance_exptrend$Month <- paste(finance_exptrend$Month, "01")
  finance_exptrend$Month <- format(as.Date(finance_exptrend$Month, format = '%B %Y %d'),"%Y-%m-%d")
  
  finance_exptrend$`Cost Center2` <- stringr::str_replace(finance_exptrend$`Cost Center2`, '\\-', "")
  
  finance_exptrend <- merge(finance_exptrend, site_mapping[,c("Balanced.Scorecards.Groupings","COST.CENTER")],
                                 by.x = c("Cost Center2"), by.y = c("COST.CENTER"))
  finance_exptrend <- finance_exptrend %>% filter(!is.na(Balanced.Scorecards.Groupings))
  
  finance_exptrend <- finance_exptrend %>% rename(Service = Balanced.Scorecards.Groupings)
}



## Read in file for Census Days
# census_days <- read_excel(census_days_path, sheet = "System Summary", col_types = "text")#, stringsAsFactors = F)
# census_days_index <- which(colnames(census_days) == "Census Days")
# census_days <- census_days[,c(1,census_days_index:(census_days_index+6))]
# 
# census_days <- census_days %>% row_to_names(row_number = 1) %>%
#                 rename(Month = 1)
# 
# census_days <- census_days %>%
#                 pivot_longer(cols = c(-Month),names_to = "Site",
#                             values_to = "Census Days")
# 
# census_days <- census_days %>% filter(!is.na(Month))
# 
# census_days$Month <- paste(census_days$Month, "01")
# 
# census_days$Month <- gsub(",", "", census_days$Month)
# 
# 
# census_days$Month <- format(as.Date(census_days$Month, "%B %Y %d"), "%m/%d/%Y")
#   
# census_days <- census_days %>% filter(!is.na(Month))




options(shiny.maxRequestSize=30*1024^2)


budget_to_actual_process <- function(data){
  raw_budget_actual <- data

  ## Budget to Actual data pre-processing
  budget_actual_df <- raw_budget_actual %>%
    mutate(Month_Var = ifelse(`Month Budget` == "Not Received", " ", 
                              round(as.numeric(`Month Budget`) - as.numeric(`Month Actual`))),
           Metric_Name = ifelse(`Account Category Desc2` == "Salary", 
                                "Budget to Actual Variance - Labor", "Budget to Actual Variance - Non Labor")) %>%
    group_by(Service, Site, Metric_Name, Month) %>%
    summarise(Month_Var= sum(as.numeric(Month_Var), na.rm = TRUE)) %>%
    pivot_wider(
      names_from = Metric_Name,
      values_from = Month_Var) %>%
    select(Service, Site, Month, `Budget to Actual Variance - Labor`, `Budget to Actual Variance - Non Labor`) %>%
    mutate(`Budget to Actual Variance - Total` = 
             `Budget to Actual Variance - Labor` + `Budget to Actual Variance - Non Labor`)
  
  ## Calculate Target and Status
  budget_actual_target <- raw_budget_actual %>%
    group_by(Service, Site, Month) %>%
    summarise(Budget_Total = sum(as.numeric(`Month Budget`), na.rm = TRUE)) 
  
  
  budget_actual_df <- merge(budget_actual_df, budget_actual_target)
  budget_actual_df <- budget_actual_df %>% 
    mutate(Target = round(`Budget to Actual Variance - Total`/Budget_Total,2),
           Status = ifelse(Target >= 0, "Green", ifelse(Target < -0.02, "Red", "Yellow"))) %>%
    pivot_longer(4:7,
                 names_to = "Metric_Name",
                 values_to = "value_rounded") %>%
    mutate(Metric_Group = "Budget to Actual",
           Reporting_Month = format(as.Date(Month, format = "%Y-%m-%d"),"%m-%Y"),
           Premier_Reporting_Period = format(as.Date(Month, format = "%Y-%m-%d"),"%b %Y")) %>%
    filter(!is.na(Reporting_Month))
  
  budget_actual_df$Metric_Name_Submitted <- metric_group_mapping$Metric_Name[match(budget_actual_df$Metric_Name, metric_group_mapping$Metric_Name_Submitted)]
  budget_actual_df <- budget_actual_df %>% 
    mutate(Metric_Name = ifelse(is.na(Metric_Name_Submitted), Metric_Name, Metric_Name_Submitted))
  
  ### Subset processed data for merge
  budget_actual_df_merge <- budget_actual_df[,processed_df_cols] 
  
  budget_actual_df_merge$Reporting_Month_Ref <- as.Date(paste('01', as.yearmon(budget_actual_df_merge$Reporting_Month, "%m-%Y")), format='%d %b %Y')
  
  
  
  updated_rows <- unique(budget_actual_df_merge[c("Metric_Name","Reporting_Month","Service", "Site")])
  metrics_final_df <- anti_join(metrics_final_df, updated_rows)

  metrics_final_df <- full_join(metrics_final_df,budget_actual_df_merge)
  
  return(metrics_final_df)
}


engineering_data_process <- function(data){
  engineering_data <- data %>%
    pivot_longer(c(-Metric, -Site),
                 names_to = "Month",
                 values_to = "Value") %>%
    pivot_wider(names_from = "Metric", values_from = Value)
  
}


source("EVS.R")
source("press_ganey.R")

