library(dplyr)
library(readxl)

mdf <- readRDS("mdf_pull_2_8.rds")
mapping_file <- read_csv("BSC_MAPPING_TABLE.csv")
budget_metrics <- c("Budget to Actual Variance - Total", "Budget to Actual Variance - Labor", "Budget to Actual Variance - Non Labor")
budget_file <- read_excel("budget_repo_2_22.xlsx")

budget_monthly <- budget_file %>% select(-Value_ytd) %>% mutate(Metric_Name_Submitted = paste0(Metric_Name_Submitted, " (Monthly)"))
budget_ytd <- budget_file %>% select(-Value) %>% mutate(Metric_Name_Submitted = paste0(Metric_Name_Submitted, " (YTD)")) %>%
                rename(Value = Value_ytd)
budget_complete <- bind_rows(budget_monthly, budget_ytd)

mdf <- left_join(mdf, mapping_file[c("SERVICE", "METRIC_NAME_SUBMITTED", "METRIC_NAME")], by = c("Service" = "SERVICE",
                                                                   "Metric_Name" = "METRIC_NAME"))
mdf <- mdf %>% filter(!(METRIC_NAME_SUBMITTED %in% budget_metrics))
mdf <- bind_rows(mdf, budget_complete)



mdf <- mdf %>% mutate(Premier_Reporting_Period = ifelse(Metric_Group == "Premier", Premier_Reporting_Period, format(Reporting_Month_Ref, "%b %Y"))) %>% 
  select(-Metric_Group, -Month, -Reporting_Month, -Metric_Name) %>% rename(Reporting_Month = Reporting_Month_Ref, value = value_rounded) %>%
  mutate(METRIC_NAME_SUBMITTED = ifelse(METRIC_NAME_SUBMITTED == 'Non-IsolationAverage TAT' &
                                            Service == 'Environmental Services', 'Non-Isolation Average TAT', METRIC_NAME_SUBMITTED)) %>%
  mutate(METRIC_NAME_SUBMITTED = ifelse(METRIC_NAME_SUBMITTED == 'Prime Time Room Utilization ESTIMATE All Rooms (%)' &
                                        Service == 'Imaging', 'Prime Time Room Utilization ESTIMATE All Rooms (%) 8:30A-5:00P, M-F', METRIC_NAME_SUBMITTED)
         )



time_updated <- read_excel("time_updated_2_8.xlsx")
time_updated <- time_updated %>% group_by(Service) %>% filter(Updated == max(Updated))
str(time_updated)

mdf_upt <- left_join(mdf, time_updated)
