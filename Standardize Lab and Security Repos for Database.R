# Code for reformatting Metric Mapping spreadsheet for database compatible structure

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

#### Global Filepaths ----------

start <- "J:" #Comment when publishing to RConnect
home_path <- paste0(start,
                    "/deans/Presidents/HSPI-PM",
                    "/Operations Analytics and Optimization/Projects",
                    "/System Operations",
                    "/Balanced Scorecards Automation/Data_Dashboard")

start_shared <- "J:"

# Import data with last time service line was updated
time_updated <- read_excel(path = paste0(home_path,
                                         "/Summary Repos Production 7-20",
                                         "/time_updated.xlsx"))

# Import Lab and Security repositories -------------------
lab_tat_repo_name <- "Lab TAT Metrics.xlsx"
lab_prof_test_repo_name <- "Lab Prof Testing Metrics.xlsx"
sec_events_repo_name <-"Security Monthly Events.xlsx"
sec_incidents_repo_name <- "Security Incident Reports.xlsx"

# test <- read_excel(paste0(home_path,
#                    "/Summary Repos Production 7-20",
#                    "/",
#                    lab_tat_repo_name))
# 
# test <- test %>%
#   mutate(Month = date(Month)) %>%
#   rename(SERVICE = Service,
#          SITE = Site,
#          REPORTING_MONTH = Month,
#          METRIC_NAME_SUBMITTED = Metric,
#          VALUE = Number) %>%
#   mutate(PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH, "%b %Y"),
#          UPDATED_TIME = NA,
#          UPDATED_USER = NA) %>%
#   relocate(PREMIER_REPORTING_PERIOD, .after = REPORTING_MONTH)


# Generalized function for putting Lab and Security repositories into standard format ---------
standardize_repo <- function(filename) {
  
  data <- read_excel(path = paste0(home_path,
                                   "/Summary Repos Production 7-20",
                                   "/",
                                   filename))
  
  last_updated <- max(
    time_updated[which(time_updated$Service %in% unique(data$Service)), ]$Updated)
  
  
  data <- data %>%
    mutate(Month = date(Month)) %>%
    rename(SERVICE = Service,
           SITE = Site,
           REPORTING_MONTH = Month,
           METRIC_NAME_SUBMITTED = Metric,
           VALUE = Number) %>%
    mutate(PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH, "%b %Y"),
           UPDATED_TIME = last_updated,
           UPDATED_USER = NA) %>%
    relocate(PREMIER_REPORTING_PERIOD, .after = REPORTING_MONTH)
  
  write_xlsx(data,
             path = paste0(home_path,
                           "/Summary Repos for Database",
                           "/",
                           filename))
  
}

updated_lab_tat_repo <- standardize_repo(lab_tat_repo_name)
updated_lab_prof_testing_repo <- standardize_repo(lab_prof_test_repo_name)
updated_sec_events_repo <- standardize_repo(sec_events_repo_name)
updated_sec_incident_repo <- standardize_repo(sec_incidents_repo_name)