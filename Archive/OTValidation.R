


processed_data <- read_xlsx("C:/Users/tommad01/Desktop/OT Validation/ProcessedProductivityData06172023.xlsx")
processed_data <- processed_data %>%
  filter(PREMIER_REPORTING_PERIOD == "05/20/2023",
         METRIC_NAME_SUBMITTED %in% c("Actual Worked FTE",
                                      "Worked Hours Productivity Index",
                                      "Overtime Hours",
                                      "Agency Hours",
                                      "Overtime Percent of Paid Hours"),
         !SERVICE %in% c("ED",
                         "Imaging",
                         "Lab",
                         "Nursing")) %>%
  mutate(VALUE = round(VALUE,4))


report <- read_xlsx("C:/Users/tommad01/Desktop/OT Validation/MSHS_Department Performance Breakdown_05-20-2023.xlsx",skip = 1,sheet = "05.20.2023 Corporate Rollup" )


report <- report %>% mutate(Service = ifelse(grepl("Radiology", `Corporate Service Line`), "Imaging",
                                                               ifelse(grepl("Biomed", `Corporate Service Line`), "Biomed / Clinical Engineering",
                                                                      ifelse(`Corporate Service Line` == "Support Services - Engineering", "Engineering",
                                                                             ifelse(`Corporate Service Line` == "Support Services - Environmental Services", "Environmental Services",
                                                                                    ifelse(`Corporate Service Line` == "Support Services - Food Services", "Food Services",
                                                                                           ifelse(grepl("Nursing", `Corporate Service Line`), "Nursing",
                                                                                                  ifelse(`Corporate Service Line` == "Support Services - Patient Transport", "Patient Transport",
                                                                                                         ifelse(`Corporate Service Line` == "Support Services - Security", "Security", 
                                                                                                                ifelse(`Corporate Service Line` == "Perioperative Services", "Perioperative Services",
                                                                                                                       ifelse(`Corporate Service Line` == "Support Services - Clinical Nutrition", "Clinical Nutrition", NA
                                                                                                                       )
                                                                                                                )
                                                                                                         )
                                                                                                  )
                                                                                           )
                                                                                    )
                                                                             )
                                                                      )
                                                               )
)
) %>%
  filter(!is.na(Service)) %>%
  select(Service,Hospital,`05/20/2023 FTE`,`05/20/2023 Productivity Index`,`05/20/2023 Overtime %`,`05/20/2023 Overtime Hours`,`05/20/2023 Agency Hours`) %>%
  mutate(`05/20/2023 Productivity Index` = as.numeric(`05/20/2023 Productivity Index`),
         `05/20/2023 Overtime %` = as.numeric(`05/20/2023 Overtime %`)) %>%
  pivot_longer(c(-Service,-Hospital),
               names_to = "Metric",
               values_to = "Value") %>%
  mutate(Value = round(as.numeric(Value),4),
         Metric = ifelse(Metric == "05/20/2023 Agency Hours","Agency Hours",
                         ifelse(Metric == "05/20/2023 Overtime Hours","Overtime Hours",
                                ifelse(Metric == "05/20/2023 Overtime %","Overtime Percent of Paid Hours",
                                       ifelse(Metric == "05/20/2023 Productivity Index","Worked Hours Productivity Index","Actual Worked FTE")))))


combined_data <- left_join(processed_data,
                           report,
                           by = c("SITE" = "Hospital",
                                  "METRIC_NAME_SUBMITTED" = "Metric",
                                  "SERVICE" = "Service"))
combined_data <- combined_data %>%
  mutate(Difference = (VALUE-Value)*100/Value)


write_xlsx(combined_data,"C:/Users/tommad01/Desktop/OT Validation/OTValidation05202023.xlsx")
