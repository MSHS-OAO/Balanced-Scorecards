# Reformat Press Ganey Summary Repository for consistency with metric names, etc.
pg_summary_repo <- read_excel(press_ganey_table_path)

# Import mapping file
pg_mapping <- read_excel(target_mapping_path, sheet = "Press Ganey v2")

# Rename columns
reformat_pg_summary_repo <- pg_summary_repo %>%
  mutate(`Reporting Closed Month` = NULL,
         Month = as.Date(Month)) %>%
  rename(Site_Mean = `Site Mean`,
         Site_N = `Site N`,
         All_PG_Database_N = `All N`,
         All_PG_Database_Mean = `All Mean`,
         All_PG_Database_Rank = `All Rank`) %>%
  mutate_at(vars(contains("Site_"),
                 contains("All_PG_Database_")),
            function(x) {
              as.numeric(
                str_replace(x, "N/A", replacement = NA_character_)
              )
            }) %>%
  mutate_at(vars(contains("_Mean")),
            ~round(., digits = 2))


reformat_pg_summary_repo <- left_join(reformat_pg_summary_repo,
                             pg_mapping,
                             by = c("KPI" = "Questions"))

pg_summary_repo_long <- reformat_pg_summary_repo %>%
  rename(Service = Service.y) %>%
  mutate(Service.x = NULL,
         KPI = NULL) %>%
  relocate(Service) %>%
  relocate(Question_Clean, .after = Month) %>%
  pivot_longer(cols = c(contains("Site_"),
                        contains("All_PG_Database_")),
               names_to = "Metric") %>%
  mutate(Incl_Scorecard = (Metric %in% "Site_Mean") |
           (Metric %in% "Site_N" & Incl_N) |
           (Metric %in% "All_PG_Database_Rank" & Incl_AllHosp_Rank),
         Incl_N = NULL,
         Incl_AllHosp_Rank = NULL,
         Metric_Name = ifelse(
           Metric %in% "Site_Mean", paste(Question_Clean, "- Score"),
           ifelse(Metric %in% "Site_N", paste(Question_Clean, "- N"),
                  ifelse(Metric %in% "All_PG_Database_Rank" & Incl_Scorecard,
                         "Rank - All Hospitals",
                         paste(Question_Clean, "-", Metric))))) %>%
  relocate(Metric_Name, .after = Metric)

write_xlsx(pg_summary_repo_long, "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Summary Repos/Press Ganey New Format.xlsx")

pg_summary_repo <- pg_summary_repo %>%
  mutate(Service.y = NULL,
         KPI = NULL,
         Site_Mean = round(Site_Mean, digits = 2),
         All_PG_Database_Mean = round(All_PG_Database_Mean, digits = 2)) %>%
  rename(Service = Service.x) %>%
  relocate(Metric_Name, .before = Site_Mean)
  
metric_names <- pg_summary_repo_long %>%
  filter(Incl_Scorecard) %>%
  group_by(Service, Metric_Name) %>%
  select(Service, Metric_Name) %>%
  distinct()
