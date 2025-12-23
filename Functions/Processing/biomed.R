# filepath  <- "Test/Biomed Updated file June.xlsx"
# data <- read_excel(filepath)

# function to append the new data to summary repo- KPIs & Disruptions and Issues -----
biomed_summary_repos_KPI <- function(data,updated_user){
  
  data <- biomed_file_transform(data)
  summary_repo_kpi_format <- data %>%
  rename(SITE = Site,
         METRIC_NAME_SUBMITTED = Metric ) %>%
  mutate(REPORTING_MONTH = as.Date(REPORTING_MONTH,"%Y-%m-%d"),
         SERVICE = "Biomed / Clinical Engineering",
         PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
         #REPORTING_MONTH = format(REPORTING_MONTH,"%Y-%m-%d"),
         UPDATED_USER = updated_user,
         VALUE = as.numeric(VALUE))
  
  summary_repo_kpi_format <- as.data.frame(summary_repo_kpi_format)
  summary_repo_kpi_format <- summary_repo_kpi_format[complete.cases(summary_repo_kpi_format), ]  
  summary_repo_kpi_format <- as_tibble(summary_repo_kpi_format)

}



biomed_file_transform <- function(data) {
  biomed_mapping <- metric_mapping_database %>% filter(Service == 'Biomed / Clinical Engineering') %>%
    filter(General_Group == 'Operational') %>% select(Metric_Name_Summary, Metric_Name_Submitted) %>% distinct()
  
  data <- left_join(data, biomed_mapping, by = c("METRIC_NAME_SUMMARY" = "Metric_Name_Summary")) %>% select(-METRIC_NAME_SUMMARY, -SERVICE) %>% 
            rename(Site = SITE, Metric = Metric_Name_Submitted) 
}

# processed_data <- biomed_summary_repos_KPI(data,"Dheeraj")
