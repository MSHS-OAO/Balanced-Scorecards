mdf <- readRDS("mdf_pull_2_8.rds")

mdf <- mdf %>% mutate(Premier_Reporting_Period = ifelse(Metric_Group == "Premier", Premier_Reporting_Period, format(Reporting_Month_Ref, "%b %Y"))) %>% 
  select(-Metric_Group, -Month, -Reporting_Month) %>% rename(Reporting_Month = Reporting_Month_Ref, value = value_rounded)

time_updated <- read_excel("time_updated_2_8.xlsx")
time_updated <- time_updated %>% group_by(Service) %>% filter(Updated == max(Updated))
str(time_updated)

mdf_upt <- left_join(mdf, time_updated)
