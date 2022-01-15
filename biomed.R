# Source code for processing Biomed/CE KPIs

# Disruptions and Issues Reports -------------
# Import historical summary
disruptions_issues_reports <- read_excel(bmedi_table_path)

# Reformat "Month" and "Date" column in Disruptions and Issues for merging
disruptions_issues_reports <- disruptions_issues_reports %>%
  mutate(Month = date(Month),
         Date = date(Date))

# Determine last month and next month for Security Incident Reports
biomedDI_last_month <- max(disruptions_issues_reports$Month)

# Transform data to UI form
disruptions_issues_reports_ui <- disruptions_issues_reports %>%
  select(-Service,-Date) %>%
  filter(Month >= biomedDI_last_month - months(7)) %>%
  group_by(Site,Month) %>%
  summarise(`Total Disruptions and Issues` = as.integer(sum(`Total Disruptions/Issues`,na.rm=TRUE))) %>%
  ungroup() %>%
  mutate(Month = format(Month, "%m-%Y")) %>%
  pivot_wider(names_from = "Month",values_from = `Total Disruptions and Issues`,values_fill=0) %>%
  mutate(Metric = "Total Disruptions and Issues") %>%
  relocate(Metric,.after=Site) %>%
  mutate('{format(biomedDI_last_month + months(1), "%m-%Y")}' := "")


# KPIs Biomed -------------
# Import historical summary
kpibme_reports <- read_excel(bmekpi_table_path)

# Reformat "Month" column in KPIs for merging
kpibme_reports <- kpibme_reports %>%
  mutate(Month = date(Month))

# Determine last month and next month for KPIs
kpibme_last_month <- max(kpibme_reports$Month)

# Transform data to UI form
kpibme_reports_ui <- kpibme_reports %>%
  filter(Month >= kpibme_last_month - months(7)) %>%
  mutate(Month = format(Month, "%m-%Y"),
         Number = round(as.numeric(Number),2),
         Number = if_else(Metric %in% c("Documented Status","PM Compliance - High Risk Equipment","PM Compliance - All Medical Equipment"),Number*100,Number)) %>%
  pivot_wider(names_from = "Month",values_from = Number,values_fill=0) %>%
  group_by(Site) %>%
  mutate('{format(kpibme_last_month + months(1), "%m-%Y")}' := "") %>%
  arrange(Site)
