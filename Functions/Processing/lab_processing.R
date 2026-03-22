
# lab_data <- "Test/ProficiencyTesting.xlsx"
# updated_user <- "Dheeraj01202025"
# data <-  read_excel(lab_data ,sheet = "Reformatted")

# Proficiency Testing ----------------
# Custom function for processing and formatting manual inputs into department summary format
lab_prof_test_dept_summary <- function(data, updated_user) {
  
  data <- data %>%
    mutate(PREMIER_REPORTING_PERIOD = format(as.Date(REPORTING_MONTH),"%b %Y"),
           UPDATED_USER = updated_user,
           METRIC_NAME_SUBMITTED = case_when(
             METRIC_NAME_SUBMITTED == 'Proficiency Testing (YTD)' ~ "Proficiency Testing (FYTD)",
             .default = METRIC_NAME_SUBMITTED
           ),
           VALUE = round(VALUE,4)) %>%
    drop_na(VALUE) %>%
    unique()
  
  data

}