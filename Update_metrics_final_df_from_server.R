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

# Reference paths and files -----------------------
start <- "J:" #Comment when publishing to RConnect
home_path <- paste0(start,"/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/")

# Import metrics_final_df
metrics_final_df_path <- paste0(home_path, "metrics_final_df_server_5-12.rds")
metrics_final_df <- readRDS(metrics_final_df_path) # Load processed Premier productivity data 

# Import reference data for targets and metric mapping
target_mapping_path <- paste0(home_path, "MSHS Scorecards Target Mapping 2022-04-13.xlsx")

target_mapping <- read_excel(target_mapping_path, sheet = "Targets and Status") # Import updated target mapping file
metric_mapping_database <- read_excel(target_mapping_path, sheet = "Metric Mapping Database")

target_mapping_analysis <- target_mapping %>%
  select(-contains("Status")) %>%
  filter(!(Target %in% c("Budget"))) %>%
  mutate_at(vars(contains(c("Target", "_Start", "_End"))), as.numeric) %>%
  distinct()

target_mapping_reference <- target_mapping %>%
  select(-contains("_Start"), -contains("_End")) %>%
  # filter(Target != "Remove") %>%
  distinct()

high_level_order <- c("Premier", "Budget", "Operational", "Patient Experience")

metric_mapping_summary_site <- metric_mapping_database %>%
  filter(Reporting_Tab %in% "Summary and Site") %>%
  select(-Reporting_Tab) %>%
  mutate(General_Group = factor(General_Group,
                                levels = high_level_order,
                                ordered = TRUE)) %>%
  arrange(Service, General_Group, Display_Order) %>%
  mutate(General_Group = as.character(General_Group))

metric_mapping_breakout <- metric_mapping_database %>%
  filter(Reporting_Tab %in% "Breakout") %>%
  select(-Reporting_Tab) %>%
  mutate(General_Group = factor(General_Group,
                                levels = high_level_order,
                                ordered = TRUE)) %>%
  arrange(Service, General_Group, Display_Order) %>%
  mutate(General_Group = as.character(General_Group))

# Create dataframes with unique metric names for each service line
metrics_final_df_naming <- metrics_final_df %>%
  select(Service,
         Metric_Group,
         Metric_Name) %>%
  distinct()

target_mapping_naming <- target_mapping %>%
  select(Service,
         Metric_Group,
         Metric_Name,
         Metric_Name_Submitted) %>%
  distinct()

metric_mapping_naming <- metric_mapping_database %>%
  select(Service,
         Metric_Group,
         Metric_Name_Summary,
         Metric_Name,
         Metric_Name_Submitted) %>%
  distinct()

# Identify missing/incorrect names in metrics_final_df ----
metrics_final_df_naming <- metrics_final_df_naming %>%
  mutate(Group_Target_Mapping = Metric_Group %in% target_mapping_naming$Metric_Group,
         Name_Target_Mapping = Metric_Name %in% target_mapping_naming$Metric_Name,
         Group_Metric_Mapping = Metric_Group %in% metric_mapping_naming$Metric_Group,
         Name_Metric_Mapping = Metric_Name %in% metric_mapping_naming$Metric_Name)

service_metrics_to_fix <- metrics_final_df_naming %>%
  filter(!Group_Metric_Mapping | !Name_Metric_Mapping)

# Verify naming consistency between target mapping and metric mapping ----
target_mapping_naming <- target_mapping_naming %>%
  mutate(Group_Metric_Mapping = Metric_Group %in% metric_mapping_naming$Metric_Group,
         Name_Metric_Mapping = Metric_Name %in% metric_mapping_naming$Metric_Name,
         Submitted_Metric_Mapping = Metric_Name_Submitted %in% metric_mapping_naming$Metric_Name_Submitted)

# test2 <- metrics_final_df_naming %>%
#   mutate(StrDetectTest = str_detect(Metric_Name,
#                                     "(Overtime \\% \\(Premier\\))"))

# # Update appropriate metric groups and metric names ----
metrics_final_df <- metrics_final_df %>%
  mutate(
    # Fix Patient Experience metric group
    Metric_Group = str_replace(Metric_Group,
                               "((Press Ganey)|(HCAHPS)).*",
                               "Patient Experience"),
    Metric_Name2 = Metric_Name,
    # Standardize Overtime Hours - % Premier
    Metric_Name2 = str_replace(Metric_Name2,
                               "Overtime \\% \\(Premier\\)",
                               "Overtime Hours - % (Premier)"),
    # Remove MOM from Budget to Actual
    Metric_Name2 = str_replace(Metric_Name2, "\\sMOM", ""),
    # Fix typos for Budget to Actual Non Labor
    Metric_Name2 = str_replace(Metric_Name2, "Actual\\-\\sNon\\sLabor",
                               "Actual - Non Labor"),
    # Standardize Lab metric names
    Metric_Name2 = ifelse(Service %in% "Lab" &
                            str_detect(Metric_Name2, "Budget to Actual") &
                            !str_detect(Metric_Name2, "Blood Bank"),
                          paste(Metric_Name2, "(Lab)"), Metric_Name2),
    Metric_Name = Metric_Name2) %>%
  select(-Metric_Name2)

# Biomedical Update "Daily Avg. # of Disruptions and Issues" to "Total Disruptions or Equipment Issues" ----
metrics_final_df <- metrics_final_df %>% 
  mutate(Metric_Name = str_replace(Metric_Name,
                                   "Daily Avg. # of Disruptions and Issues",
                                   "Total Disruptions or Equipment Issues"))

# Update Nursing Metric Group ----
NursingOps <-  metrics_final_df %>% 
  filter(Metric_Group %in% c("Nursing Ops")) %>%
  select(-Metric_Group) 

metrics_final_df_naming_nursing <- metric_mapping_naming %>%
  select(Service,Metric_Group,Metric_Name)

NursingOps <- left_join(NursingOps,
                    metrics_final_df_naming_nursing,
                    by=c("Service", "Metric_Name"))

metrics_final_df <- metrics_final_df %>% 
  filter(!Metric_Group %in% c("Nursing Ops"))

metrics_final_df <- rbind(metrics_final_df,NursingOps)

# Change the ED's treat & release to maintain consistency ----
# Tibble to map new KPI Names ----

mapping <- tibble(KPI=c("Acuity Null",
                        "Acuity 5",
                        "Acuity 4",
                        "Acuity 3",
                        "Acuity 2",
                        "Acuity 1",
                        "Total Boarder Hours",
                        "Admit to Depart (Median Boarder Hours)",
                        "Door to Admit (Median)",
                        "ED LOS Treat & Release (Median)",
                        "ED LOS Admit (Median)",
                        "LWBS",
                        "Visit Volume (Epic)",
                        "ED LOS Treat&Release Patients (90th Percentile Hours)",
                        "ED LOS Admitted Patients (90th Percentile Hours)",
                        "Admit to Depart (90th Percentile Boarder Hours)",
                        "Acuity 1 count AAAEM",
                        "Acuity 2 count AAAEM",
                        "Acuity 3 count AAAEM",
                        "Acuity 4 count AAAEM",
                        "Acuity 5 count AAAEM",
                        "Acuity Null count AAAEM",
                        "LWBS %"), 
                  KPINew=c("Acuity Null",
                           "Acuity 5",
                           "Acuity 4",
                           "Acuity 3",
                           "Acuity 2",
                           "Acuity 1",
                           "Total Boarder Hours",
                           "Admit to Depart Boarder Hours (Median)",
                           "Door to Admit (Median)",
                           "ED LOS T&R Patients (Median)",
                           "ED LOS Admitted Patients (Median)",
                           "LWBS",
                           "Visit Volume (Epic)",
                           "ED LOS T&R Patients (90th Percentile)",
                           "ED LOS Admitted Patients (90th Percentile)",
                           "Admit to Depart Boarder Hours (90th Percentile)",
                           "Acuity 1 count AAAEM",
                           "Acuity 2 count AAAEM",
                           "Acuity 3 count AAAEM",
                           "Acuity 4 count AAAEM",
                           "Acuity 5 count AAAEM",
                           "Acuity Null count AAAEM",
                           "LWBS %"))
# ED metrics to remap ----
KPI=c("Acuity Null",
      "Acuity 5",
      "Acuity 4",
      "Acuity 3",
      "Acuity 2",
      "Acuity 1",
      "Total Boarder Hours",
      "Admit to Depart (Median Boarder Hours)",
      "Door to Admit (Median)",
      "ED LOS Treat & Release (Median)",
      "ED LOS Admit (Median)",
      "LWBS",
      "Visit Volume (Epic)",
      "ED LOS Treat&Release Patients (90th Percentile Hours)",
      "ED LOS Admitted Patients (90th Percentile Hours)",
      "Admit to Depart (90th Percentile Boarder Hours)",
      "Acuity 1 count AAAEM",
      "Acuity 2 count AAAEM",
      "Acuity 3 count AAAEM",
      "Acuity 4 count AAAEM",
      "Acuity 5 count AAAEM",
      "Acuity Null count AAAEM",
      "LWBS %")
metrics_final_df_from_server_ED <- metrics_final_df %>% 
  filter(Metric_Name %in% KPI)

metrics_final_df <- metrics_final_df %>% 
  filter(!Metric_Name %in% KPI)

# Code to change the metric names in metrics final df ----
metrics_final_df_from_server_ED <- merge(metrics_final_df_from_server_ED,
                                         mapping,
                                         by.x = "Metric_Name",
                                         by.y = "KPI") %>%
  select(-Metric_Name) %>%
  rename(Metric_Name = KPINew) %>%
  select(names(metrics_final_df))

metrics_final_df <- rbind(metrics_final_df,metrics_final_df_from_server_ED)


# Update ED Metric Group ----
EDOps <-  metrics_final_df %>% 
  filter(Metric_Group %in% c("Operational" ,"Left Without Being Seen (LWBS)") & Service %in% c("ED")) %>%
  select(-Metric_Group) 

metrics_final_df_naming_ED <- metric_mapping_naming %>%
  select(Service,Metric_Group,Metric_Name)

EDOps <- left_join(EDOps,
                   metrics_final_df_naming_ED,
                   by=c("Service", "Metric_Name"))

metrics_final_df <- metrics_final_df %>% 
  filter(!(Metric_Group %in% c("Operational","Left Without Being Seen (LWBS)") & Service %in% c("ED")))

EDOps <- EDOps %>%
  select(names(metrics_final_df))

metrics_final_df <- rbind(metrics_final_df,EDOps)


# Identify missing/incorrect names in metrics_final_df ----
metrics_final_df_naming2 <- metrics_final_df %>%
  select(Service,
         Metric_Group,
         Metric_Name) %>%
  distinct() %>%
  mutate(Group_Target_Mapping = Metric_Group %in% target_mapping_naming$Metric_Group,
         Name_Target_Mapping = Metric_Name %in% target_mapping_naming$Metric_Name,
         Group_Metric_Mapping = Metric_Group %in% metric_mapping_naming$Metric_Group,
         Name_Metric_Mapping = Metric_Name %in% metric_mapping_naming$Metric_Name)

service_metrics_to_fix2 <- metrics_final_df_naming2 %>%
  filter(!Group_Metric_Mapping | !Name_Metric_Mapping) #%>%
  # mutate(
  #   # Fix Patient Experience metric group
  #   Metric_Group = str_replace(Metric_Group,
  #                              "((Press Ganey)|(HCAHPS)).*",
  #                              "Patient Experience"),
  #   Metric_Name2 = Metric_Name,
  #   # Remove MOM from Budget to Actual
  #   Metric_Name2 = str_replace(Metric_Name2, "\\sMOM", ""),
  #   # Fix typos for Budget to Actual Non Labor
  #   Metric_Name2 = str_replace(Metric_Name2, "Actual\\-\\sNon\\sLabor",
  #                              "Actual - Non Labor"),
  #   # Standardize Lab metric names
  #   Metric_Name2 = ifelse(Service %in% "Lab" &
  #                           Metric_Group %in% "Budget to Actual" &
  #                           !str_detect(Metric_Name2, "Blood Bank"),
  #                         paste(Metric_Name2, "(Lab)"), Metric_Name2))


# # Update metrics_final_df with Lab TAT data with greater accuracy --------
# metrics_final_df <- lab_scc_tat_metrics_final_df(ops_metrics_lab_tat)
# 
#saveRDS(metrics_final_df, metrics_final_df_path)

