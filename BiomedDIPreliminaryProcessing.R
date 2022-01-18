# Script to agregate Disruptions and Issues Daily data to Monthly summary ---

bmedi_data  <- read_excel(bmedi_table_path)


monthly.di.data <- bmedi_data %>%
  select(-Date) %>%
  group_by(Service,Site,Month) %>%
  summarise(`Total Disruptions/Issues` = sum(`Total Disruptions/Issues`,na.rm=TRUE)) %>%
  ungroup()

write_xlsx(monthly.di.data,paste0(home_path,"/Summary Repos/DisruptionsAndIssuesMonthly.xlsx"))
