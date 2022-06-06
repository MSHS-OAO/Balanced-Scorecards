# Code for converting local department summary repositories into single standard table for database

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

# Import service line summary repos ------------------
## File path for Lab KPI metrics --------------
ops_metrics_lab_tat_path <- paste0(home_path, "Summary Repos/Lab TAT Metrics.xlsx")
ops_metrics_lab_prof_test_path <- paste0(home_path, "Summary Repos/Lab Prof Testing Metrics.xlsx")

lab_tat_repo <- read_excel(ops_metrics_lab_tat_path)
lab_prof_test_repo <- read_excel(ops_metrics_lab_prof_test_path)

## File path for Security KPI metrics ------------
security_incident_reports_path <- paste0(home_path,
                                         "Summary Repos/",
                                         "Security Incident Reports.xlsx")

security_events_path <- paste0(home_path,
                               "Summary Repos/",
                               "Security Monthly Events.xlsx")

sec_inc_reports_repo <- read_excel(security_incident_reports_path)
sec_events_repo <- read_excel(security_events_path)

# Create new or import existing standard repo
existing_repo <- FALSE

if (existing_repo) {
  service_line_repo_file <- choose.files(default = paste0(home_path,
                                                   "Summary Repos for Database/*.*"))
  
  service_line_repo_database <- read_excel(service_line_repo_file)

} else {
  service_line_repo_database <- NULL
}

# Custom function for appending service line repo with selected service line
append_service_line_repo <- function(existing_dept_repo) {
  standard_service_line_repo <- rbind(service_line_repo_database,
                                      existing_dept_repo)
}

# service_line_repo_database <- append_service_line_repo(lab_tat_repo)
service_line_repo_database <- append_service_line_repo(lab_prof_test_repo)
service_line_repo_database <- append_service_line_repo(sec_inc_reports_repo)
service_line_repo_database <- append_service_line_repo(sec_events_repo)
