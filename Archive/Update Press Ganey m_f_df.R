# This code is used to update ED and Nursing sections of metrics_final_df with the appropriate targets


# # Remove "Emergency Department" from Service column ---------
# # First correct any rows of metrics_final_df that are named as "Emergency Department" instead of "ED"
# # Check for duplicates
# test_df <- metrics_final_df %>%
#   filter(Service %in% c("ED", "Emergency Department") &
#            Metric_Group %in% c("Press Ganey Score", "HCAHPS (60 day lag)")) %>%
#   group_by(Service, Site, Metric_Name, Reporting_Month_Ref) %>%
#   summarize(Count = n())
# 
# # Rename Emergency Department in metrics_final_df as ED
# metrics_final_df_ed_fix <- metrics_final_df %>%
#   mutate(Service = ifelse(Service %in% "Emergency Department", "ED", Service))
# 
# saveRDS(metrics_final_df_ed_fix, metrics_final_df_path)
# 
# # test_dupl <- metrics_final_df %>%
# #   filter(Service %in% c("ED", "Emergency Department"),
# #          Metric_Group %in% c("Press Ganey Score", "HCAHPS (60 day lag)")) %>%
# #   group_by(Site, Metric_Name, Reporting_Month_Ref) %>%
# #   summarize(Count = n())



# For some reason, there are currently duplicated entries for "Staff Worked Together" metrics under ED and Emergency Department
# Remove the "Emergency Department" entries
# 
# metrics_final_df_corrected <- metrics_final_df %>%
#   filter(Service != "Emergency Department")

# # Save updated metrics_final_df
# saveRDS(metrics_final_df_corrected, metrics_final_df_path)


# 
# pg_metrics_final_df <- metrics_final_df %>%
#   filter(Metric_Group %in% c("Press Ganey Score", "HCAHPS (60 day lag)"))
# 
# pg_summary <- pg_metrics_final_df %>%
#   mutate(MissingTarget = is.na(Target)) %>%
#   group_by(Service, Site, Metric_Name, MissingTarget) %>%
#   summarize(Count = n())
# 
pg_m_f_df <- metrics_final_df %>% filter(Metric_Group %in% c("Press Ganey Score", "HCAHPS (60 day lag)"))
 
pg_month_summary <- pg_m_f_df %>%
  group_by(Service, Reporting_Month_Ref) %>%
  summarize(Count = n())

pg_target_summary <- pg_m_f_df %>%
  mutate(MissingTarget = is.na(Target)) %>%
  group_by(Service, Site, Metric_Name, MissingTarget) %>%
  summarize(Count = n())


# Incorporate nursing targets into historical data - Corrected on 3/4/2022 ------------------
# Update nursing in metrics_final_df
pg_nursing_repo <- press_ganey_data %>%
  filter(Service %in% c("Nursing") &
           ReportingType %in% c("Monthly"))

nursing_metrics_final_df <- press_ganey_metrics_final_df(pg_nursing_repo)

metrics_final_df <- metrics_final_df %>%
  arrange(Service, Site, Metric_Group, Metric_Name,
          Premier_Reporting_Period, Reporting_Month, Reporting_Month_Ref)

nursing_metrics_final_df <- nursing_metrics_final_df %>%
  arrange(Service, Site, Metric_Group, Metric_Name,
          Premier_Reporting_Period, Reporting_Month, Reporting_Month_Ref)

all.equal(nursing_metrics_final_df, metrics_final_df)

# saveRDS(nursing_metrics_final_df, metrics_final_df_path)

# Incorporate ED targets --------------------------
# Update ED in metrics_final_df
pg_ed_repo <- press_ganey_data %>%
  filter(Service %in% c("ED") &
           ReportingType %in% c("Monthly"))

ed_metrics_final_df <- press_ganey_metrics_final_df(pg_ed_repo)

metrics_final_df <- metrics_final_df %>%
  arrange(Service, Site, Metric_Group, Metric_Name,
          Premier_Reporting_Period, Reporting_Month, Reporting_Month_Ref)

ed_metrics_final_df <- ed_metrics_final_df %>%
  arrange(Service, Site, Metric_Group, Metric_Name,
          Premier_Reporting_Period, Reporting_Month, Reporting_Month_Ref)

all.equal(ed_metrics_final_df, metrics_final_df)

# saveRDS(ed_metrics_final_df, metrics_final_df_path)
