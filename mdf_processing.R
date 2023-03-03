library(dplyr)
library(readxl)

mdf_raw <- readRDS("BSC Update/mdf_3_3.rds") %>% filter(Service == "Environmental Services", !(Metric_Group  %in% c("Productivity", "Overtime Hours"))) %>% filter(Metric_Group == "Patient Experience")
mdf_raw <- mdf_raw %>% group_by(Service, Site, Metric_Name,Metric_Group, Reporting_Month) %>% mutate(id = row_number()) %>% ungroup() %>% filter(Metric_Group != "Budget to Actual")
mdf <- mdf_raw %>% filter(id == 1) %>% select(-id)
mapping_file <- read_csv("BSC Update/BSC_MAPPING_TABLE.csv")
mapping_file <- mapping_file  %>% filter(REPORTING_TAB == "Breakout")
budget_metrics <- c("Budget to Actual Variance - Total", "Budget to Actual Variance - Labor", "Budget to Actual Variance - Non Labor")
budget_file <- read_excel("BSC Update/budget_3_3.xlsx")
time_updated <- read_excel("BSC Update/time_upt_3_3.xlsx")

budget_monthly <- budget_file %>% select(-Value_ytd) %>% mutate(Metric_Name_Submitted = paste0(Metric_Name_Submitted, " (Monthly)"))
budget_ytd <- budget_file %>% select(-Value) %>% mutate(Metric_Name_Submitted = paste0(Metric_Name_Submitted, " (YTD)")) %>%
                rename(Value = Value_ytd)
budget_complete <- bind_rows(budget_monthly, budget_ytd)
budget_complete <- budget_complete %>% rename(value_rounded = Value,
                                              METRIC_NAME_SUBMITTED = Metric_Name_Submitted,
                                              Reporting_Month_Ref = Month) %>%
                                        mutate(Premier_Reporting_Period = format(Reporting_Month_Ref, "%b %Y"),
                                                Metric_Group = "Budget")

mapping_test <- left_join(mdf, mapping_file[c("SERVICE", "METRIC_GROUP", "METRIC_NAME")], by = c("Service" = "SERVICE",
                                                                                             "Metric_Group" = "METRIC_GROUP"#,
                                                                                             #"Metric_Name" = "METRIC_NAME"
                                                                                             ))
#mdf <- mapping_test %>% select(-Metric_Name) %>% rename(Metric_Name = METRIC_NAME)

mdf <- left_join(mdf, mapping_file[c("SERVICE", "METRIC_NAME_SUBMITTED","METRIC_GROUP" ,"METRIC_NAME")], by = c(#"Service" = "SERVICE",
                                                                   "Metric_Group" = "METRIC_GROUP",
                                                                   "Metric_Name" = "METRIC_NAME"
                                                                   ))
mdf <- mdf %>% filter(!(METRIC_NAME_SUBMITTED %in% budget_metrics))
#mdf <- bind_rows(mdf, budget_complete)



mdf <- mdf %>% mutate(Premier_Reporting_Period = ifelse(Metric_Group == "Productivity", Premier_Reporting_Period, format(Reporting_Month_Ref, "%b %Y"))) %>% 
  select(-Month, -Reporting_Month, -Metric_Name) %>% rename(Reporting_Month = Reporting_Month_Ref, value = value_rounded) %>%
  mutate(METRIC_NAME_SUBMITTED = ifelse(METRIC_NAME_SUBMITTED == 'Non-IsolationAverage TAT' &
                                            Service == 'Environmental Services', 'Non-Isolation Average TAT', METRIC_NAME_SUBMITTED)) %>%
  mutate(METRIC_NAME_SUBMITTED = ifelse(METRIC_NAME_SUBMITTED == 'Prime Time Room Utilization ESTIMATE All Rooms (%)' &
                                        Service == 'Imaging', 'Prime Time Room Utilization ESTIMATE All Rooms (%) 8:30A-5:00P, M-F', METRIC_NAME_SUBMITTED)
         )




time_updated <- time_updated %>% group_by(Service) %>% filter(Updated == max(Updated))
str(time_updated)

mdf_upt <- left_join(mdf, time_updated) %>% filter(Service == "Environmental Services")
mdf_upt <- mdf_upt %>% filter(!is.na(METRIC_NAME_SUBMITTED))


mdf_na <- mdf %>% filter(is.na(METRIC_NAME_SUBMITTED)) %>% mutate(METRIC_NAME_SUBMITTED = ifelse(Metric_Group == "% of Turns by Category", "% Isolation Turns",  "Isolation % > 90 mins"),
                                                                  Service = "Environmental Services")

#mdf_na <- mdf_na %>% select(-SERVICE, -Metric_Group)

mdf_test <- bind_rows(mdf_upt, mdf_na)






# mdf_upt <- mdf_upt %>% group_by(Service, Site, Reporting_Month, METRIC_NAME_SUBMITTED) %>% mutate(id = row_number()) %>% ungroup() %>% filter(id == 1) %>% select(-id)
# mdf_upt <- mdf_upt %>% filter(!is.na(METRIC_NAME_SUBMITTED))
mdf_upt <- mdf_upt %>% distinct()

write.csv(mdf_upt, "mdf_updated_3_3.csv")
