# Tibble to map new KPI Names ----

mapping <- tibble(KPI=c("Acuity Null",
                        "Acuity 5",
                        "Acuity 4",
                        "Acuity 3",
                        "Acuity 2",
                        "Acuity 1",
                        "Total Boarder Hours",
                        "Admit to Depart (Median Boarder Hours)",
                        "Door to Admit (Median)",
                        "ED LOS Treat & Release (Median)",
                        "ED LOS Admit (Median)",
                        "LWBS",
                        "Visit Volume (Epic)",
                        "ED LOS Treat&Release Patients (90th Percentile Hours)",
                        "ED LOS Admitted Patients (90th Percentile Hours)",
                        "Admit to Depart (90th Percentile Boarder Hours)",
                        "Acuity 1 count AAAEM",
                        "Acuity 2 count AAAEM",
                        "Acuity 3 count AAAEM",
                        "Acuity 4 count AAAEM",
                        "Acuity 5 count AAAEM",
                        "Acuity Null count AAAEM",
                        "LWBS %"), 
                  KPINew=c("Acuity Null",
                        "Acuity 5",
                        "Acuity 4",
                        "Acuity 3",
                        "Acuity 2",
                        "Acuity 1",
                        "Total Boarder Hours",
                        "Admit to Depart Boarder Hours (Median)",
                        "Door to Admit (Median)",
                        "ED LOS T&R Patients (Median)",
                        "ED LOS Admitted Patients (Median)",
                        "LWBS",
                        "Visit Volume (Epic)",
                        "ED LOS T&R Patients (90th Percentile)",
                        "ED LOS Admitted Patients (90th Percentile)",
                        "Admit to Depart Boarder Hours (90th Percentile)",
                        "Acuity 1 count AAAEM",
                        "Acuity 2 count AAAEM",
                        "Acuity 3 count AAAEM",
                        "Acuity 4 count AAAEM",
                        "Acuity 5 count AAAEM",
                        "Acuity Null count AAAEM",
                        "LWBS %"))

# Get the ED Summary Repo in Server and merge ----
EDSummaryRepo <-left_join(ed_summary_repo,
                         mapping) %>%
  select(-KPI) %>%
  rename(KPI=KPINew) %>%
  select(Service,Site,Month,KPI,Metric)

write_xlsx(EDSummaryRepo, ed_path)

# ED metrics to remap ----
KPI=c("Acuity Null",
      "Acuity 5",
      "Acuity 4",
      "Acuity 3",
      "Acuity 2",
      "Acuity 1",
      "Total Boarder Hours",
      "Admit to Depart (Median Boarder Hours)",
      "Door to Admit (Median)",
      "ED LOS Treat & Release (Median)",
      "ED LOS Admit (Median)",
      "LWBS",
      "Visit Volume (Epic)",
      "ED LOS Treat&Release Patients (90th Percentile Hours)",
      "ED LOS Admitted Patients (90th Percentile Hours)",
      "Admit to Depart (90th Percentile Boarder Hours)",
      "Acuity 1 count AAAEM",
      "Acuity 2 count AAAEM",
      "Acuity 3 count AAAEM",
      "Acuity 4 count AAAEM",
      "Acuity 5 count AAAEM",
      "Acuity Null count AAAEM",
      "LWBS %")
metrics_final_df_from_server_ED <- metrics_final_df_from_server %>% 
  filter(Metric_Name %in% KPI)

metrics_final_df_from_server <- metrics_final_df_from_server %>% 
  filter(!Metric_Name %in% KPI)

# Code to change the metric names in metrics final df ----
metrics_final_df_from_server_ED <- merge(metrics_final_df_from_server_ED,
                                         mapping,
                                         by.x = "Metric_Name",
                                         by.y = "KPI") %>%
  select(-Metric_Name) %>%
  rename(Metric_Name = KPINew) %>%
  select(names(metrics_final_df_from_server))

metrics_final_df_from_server <- rbind(metrics_final_df_from_server,metrics_final_df_from_server_ED)
