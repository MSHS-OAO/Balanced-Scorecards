# Code for reformatting Metric Mapping spreadsheet for database compatible structure

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

#### Global Filepaths

start <- "J:" #Comment when publishing to RConnect
home_path <- paste0(start,"/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/")
start_shared <- "J:"

target_mapping_path <- paste0(home_path, "MSHS Scorecards Target Mapping 2022-04-13.xlsx")

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
    # # Update Overtime Hours - % (Premier)
    # across(.cols = everything (),
    #        .fns = function(x) {
    #          str_replace(x,
    #                      "\\ %\\ \\(Premier\\)",
    #                      "\\ Hours\\ \\-\\ %\\ \\(Premier\\)")
    #          })
  )

metric_mapping_db_format <- metric_mapping_raw %>%
  pivot_longer(cols = contains("_Order"),
               names_to = "Service_Tab",
               values_to = "Display_Order") %>%
  mutate(Service = str_extract(Service_Tab, ".+?(?=_)"),
         Service = ifelse(str_detect(Service, "EVS"), "Environmental Services",
                          ifelse(str_detect(Service, "Biomed"),
                                 "Biomed / Clinical Engineering",
                                 ifelse(str_detect(Service, "Food"), "Food Services",
                                        ifelse(str_detect(Service, "Transport"),
                                               "Patient Transport",
                                               Service)))),
         Reporting_Tab = ifelse(str_detect(Service_Tab, "Summary_Site"),
                                "Summary and Site",
                                ifelse(str_detect(Service_Tab,
                                                  "Breakout"),
                                       "Breakout", NA))) %>%
  select(-Service_Tab, -`Data Table`) %>%
  filter(!is.na(Display_Order)) %>% 
  relocate(Display_Order, .after = Reporting_Tab) %>%
  relocate(Service) %>%
  mutate(General_Group = factor(General_Group,
                                levels = high_level_order,
                                ordered = TRUE)) %>%
  arrange(Service,
          desc(Reporting_Tab),
          General_Group,
          Display_Order)

write_xlsx(metric_mapping_db_format,
           path = paste0(home_path,
                         "Metric Mapping Database Sturcture ",
                         format(Sys.Date(), "%Y-%m-%d"),
                         ".xlsx"))

