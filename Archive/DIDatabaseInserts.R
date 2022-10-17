di_data <- read_excel("C:/Users/tommad01/Desktop/DisruptionsAndIssuesMonthly.xlsx")
di_data <- di_data %>%
  mutate(Month = date(Month))
di_data <- di_data %>%
  select(-Service) %>%
  mutate(Month = format(Month, "%b-%Y")) %>%
  pivot_wider(names_from = "Month",values_from = `Total Disruptions/Issues`,values_fill=NA_integer_) %>%
  mutate(Metric = "Total Disruptions and Issues") %>%
  relocate(Metric,.after=Site) #%>%
di_data<- process_manual_entry_to_summary_repo_format_biomed(di_data,"DI","NULL")
write_temporary_table_to_database_and_merge(di_data,
                                            "TEMP_DI_BIOMED")



nursing_data <- read_excel("C:/Users/tommad01/Desktop/Nursing.xlsx")
write_temporary_table_to_database_and_merge(nursing_data,
                                            "TEMP_DI_BIOMED")
