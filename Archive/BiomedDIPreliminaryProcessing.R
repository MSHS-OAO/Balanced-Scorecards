# Script to agregate Disruptions and Issues Daily data to Monthly summary ---

bmedi_data  <- read_excel(bmedi_table_path)
kpibme <- read_excel(bmekpi_table_path)


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

metrics_final_df_form <- kpibme %>% 
  rename(value_rounded= Number,
         Metric_Name_Submitted = Metric,
         Reporting_Month_Ref = Month) %>%
  mutate(Reporting_Month = format(Reporting_Month_Ref,"%m-%Y"),
         Premier_Reporting_Period = format(Reporting_Month_Ref,"%b %Y"),
         value_rounded = round(as.numeric(value_rounded),2))

summary_metric_filter_subset <- summary_metric_filter %>% select(Metric_Group,Metric_Name,Metric_Name_Submitted)

metrics_final_df_form <- left_join(metrics_final_df_form,
                                   summary_metric_filter_subset,
                                   by = c("Metric_Name_Submitted"))
metrics_final_df_form <- left_join(metrics_final_df_form,
                                   target_mapping, 
                                   by = c("Service","Site","Metric_Group", "Metric_Name","Metric_Name_Submitted"))

metrics_final_df_form <- metrics_final_df_form %>%
  mutate(Variance = between(value_rounded, Range_1, Range_2)) %>%
  filter(Variance %in% c(TRUE,NA))

metrics_final_df_form <- metrics_final_df_form[,c("Service",
                                                  "Site",
                                                  "Metric_Group",
                                                  "Metric_Name",
                                                  "Premier_Reporting_Period",
                                                  "Reporting_Month",
                                                  "value_rounded",
                                                  "Target",
                                                  "Status",
                                                  "Reporting_Month_Ref")]


