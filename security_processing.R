# Source code for processing Security KPIs

# Import historical repositories -------------
# ops_metrics_sec_events <- read_excel(ops_metrics_sec_events_path)

security_incident_reports <- read_excel(security_incident_reports_path)

# Reformat "Month" column in Proficiency Testing data for merging
security_incident_reports <- security_incident_reports %>%
  mutate(Month = date(Month))


# Determine last month and next month for Security Incident Reports
sec_inc_rpts_last_month <- max(security_incident_reports$Month)
sec_next_month <- sec_inc_rpts_last_month + months(1)

# Reformat Security Incident Reports data into wider format for manual entries
sec_inc_rpts_manual_table <- security_incident_reports %>%
  select(-Service,
         -Original) %>%
  filter(Metric %in% c("Incident Reports") &
           Month >= sec_inc_rpts_last_month - months(7)) %>%
  arrange(Month,
          Site) %>%
  mutate(Month = format(Month, "%m-%Y")) %>%
  pivot_wider(names_from = Month,
              values_from = Occurences) %>%
  # Add a column with the next month for the user to enter data
  mutate('{format(sec_inc_rpts_last_month + months(1), "%m-%Y")}' := "")

