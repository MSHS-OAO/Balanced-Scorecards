# Script to agregate Disruptions and Issues Daily data to Monthly summary ---

bmedi_data  <- read_excel(bmedi_table_path)


monthly.di.data <- bmedi_data %>%
  group_by(Service,Site,Month) %>%
  summarise(`Total Disruptions/Issues` = sum(`Total Disruptions/Issues`,na.rm=TRUE)) %>%
  ungroup()

write_xlsx(monthly.di.data,paste0(home_path,"/Summary Repos/DisruptionsAndIssuesMonthly.xlsx"))


di_data <- metrics_final_df %>%
  filter(Metric_Group == "Total Disruptions or Equipment Issues") 

di_data <- di_data %>%
  mutate(Site = ifelse(Site=="MSSL","MSM",Site))


di_data <-  di_data %>% 
              select(-Target,-Status) %>%
              group_by(Site,Service,Metric_Group,Metric_Name,Premier_Reporting_Period,Reporting_Month,Reporting_Month_Ref) %>% 
              summarise(value_rounded = sum(value_rounded)) %>%
              mutate(Target = NA,
                     Status = NA)

di_data <- di_data[,c("Service",
                      "Site",
                      "Metric_Group",
                      "Metric_Name",
                      "Premier_Reporting_Period",
                      "Reporting_Month",
                      "value_rounded",
                      "Target",
                      "Status",
                      "Reporting_Month_Ref")]

metrics_final_df <- metrics_final_df %>%
  filter(!Metric_Group == "Total Disruptions or Equipment Issues") 
