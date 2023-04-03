cn_dept_summary <- function(raw_data,updated_user){
  
  processed_data <- raw_data %>%
    pivot_longer(-c("Site", "Metric"),names_to = "REPORTING_MONTH", values_to = "VALUE")
  processed_data <- processed_data %>% 
    mutate(REPORTING_MONTH = as.Date(as.integer(REPORTING_MONTH), origin = "1899-12-30"),
           SERVICE = 'Clinical Nutrition',
           UPDATED_USER = updated_user,
           PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
           VALUE = case_when(Metric  == "RD pended orders"~ VALUE/100,
                             Metric  == "Nutrition assessment for patient w/ PI"~ VALUE/100,
                             Metric  == "Malnutrition Identification"~ VALUE/100,
                             Metric  == "Malnutrition Revenue"~ VALUE)) %>%
    rename(`SITE` = `Site`,
           `METRIC_NAME_SUBMITTED` = `Metric`) %>%
    drop_na() %>%
    select(SERVICE,SITE,REPORTING_MONTH,PREMIER_REPORTING_PERIOD,METRIC_NAME_SUBMITTED,VALUE,UPDATED_USER)
  
}