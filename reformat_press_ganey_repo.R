# Reformat Press Ganey Summary Repository for consistency with metric names, etc.

# Import mapping file
press_ganey_mapping <- read_excel(target_mapping_path, sheet = "Press Ganey v2")


# pg_summary_repo <- read_excel(press_ganey_table_path)
# Import FTI's compiled data
fti_pg_data_compiled <- paste0("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization",
                               "/Projects/System Operations/Balanced Scorecards Automation",
                               "/Data_Dashboard/Input Data Raw/Press Ganey",
                               "/Press Ganey Sheets from FTI 2022-01-12.xlsx")

sheet_names <- excel_sheets(fti_pg_data_compiled)

fti_pg_data_list <- sapply(sheet_names,
                           function(x) {read_excel(fti_pg_data_compiled,
                                                   sheet = x) },
                           USE.NAMES = TRUE)

transport_pg <- fti_pg_data_list[[
  which(str_detect(names(fti_pg_data_list), "Transport"))]]

food_pg <- fti_pg_data_list[[
  which(str_detect(names(fti_pg_data_list), "Food"))]]

evs_pg <- fti_pg_data_list[[
  which(str_detect(names(fti_pg_data_list), "EVS"))]]

nursing_pg <- fti_pg_data_list[[
  which(str_detect(names(fti_pg_data_list), "Nursing"))]]

ed_pg <- fti_pg_data_list[[
  which(str_detect(names(fti_pg_data_list), "ED"))]]


# Process Food Services data from FTI -----------------------
food_pg <- food_pg %>%
  mutate(ReportingType = ifelse(str_detect(Month, "\\-"), "YTD", "Monthly"))

food_pg_monthly <- food_pg %>%
  filter(ReportingType %in% "Monthly") %>%
  mutate(Reporting_Date_Start =
           as.Date(as.numeric(Month), origin = "1899-12-30"),
         Reporting_Date_End = Reporting_Date_Start + months(1) - 1)

food_pg_ytd <- food_pg %>%
  filter(ReportingType %in% "YTD") %>%
  mutate(Reporting_Date_Start =
           as.Date(str_extract(Month, ".+(?=\\-)"),
                   format = "%m/%d/%Y"),
         Reporting_Date_End = as.Date(str_extract(Month, "(?<=\\-).+"),
                                      format = "%m/%d/%Y"))

food_pg_repo <- rbind(food_pg_monthly, food_pg_ytd)

food_pg_repo <- left_join(food_pg_repo,
                          press_ganey_mapping[c("Service",
                                                "Questions",
                                                "Question_Clean")],
                          by = c("KPI" = "Questions"))

food_pg_repo <- food_pg_repo %>%
  mutate(Month = NULL,
         `Reporting Closed Month` = NULL,
         KPI = NULL) %>%
  mutate_at(vars(c(contains("Site "),
                   contains("All "))),
            str_replace, "N/A", NA_character_) %>%
  mutate_at(vars(c(contains("Site "),
                   contains("All "))),
                 as.numeric) %>%
  mutate_at(vars(contains("Mean")),
            round, digits = 2) %>%
  relocate(Service) %>%
  relocate(Question_Clean, .after = Site) %>%
  relocate(ReportingType, .after = Question_Clean) %>%
  relocate(Reporting_Date_Start, .after = ReportingType) %>%
  relocate(Reporting_Date_End, .after = Reporting_Date_Start) %>%
  rename(Site_Mean = `Site Mean`,
         Site_N = `Site N`,
         All_PG_Database_Mean = `All Mean`,
         All_PG_Database_N = `All N`,
         All_PG_Database_Rank = `All Rank`)%>%
  relocate(All_PG_Database_Mean, .after = Site_N)
  
# Process Transport data from FTI -----------------------
transport_pg <- transport_pg %>%
  mutate(ReportingType = ifelse(str_detect(Month, "\\-"), "YTD", "Monthly"))

transport_pg_monthly <- transport_pg %>%
  filter(ReportingType %in% "Monthly") %>%
  mutate(Reporting_Date_Start =
           as.Date(as.numeric(Month), origin = "1899-12-30"),
         Reporting_Date_End = Reporting_Date_Start + months(1) - 1)

transport_pg_ytd <- transport_pg %>%
  filter(ReportingType %in% "YTD") %>%
  mutate(Reporting_Date_Start =
           as.Date(str_extract(Month, ".+(?=\\-)"),
                   format = "%m/%d/%Y"),
         Reporting_Date_End = as.Date(str_extract(Month, "(?<=\\-).+"),
                                      format = "%m/%d/%Y"))

transport_pg_repo <- rbind(transport_pg_monthly, transport_pg_ytd)

# Rename KPI column as "Question_Clean" for consistency with summary repo.
# There is no need to do a left join on this since the KPI has already  been cleaned.
transport_pg_repo <- transport_pg_repo %>%
  rename(Question_Clean = KPI)

transport_pg_repo <- transport_pg_repo %>%
  mutate(Hospital = NULL,
         Month = NULL,
         `Reporting Closed Month` = NULL,
         Service = "Patient Transport") %>%
  mutate_at(vars(contains("Mean")),
            round, digits = 2) %>%
  relocate(Service) %>%
  relocate(Question_Clean, .after = Site) %>%
  relocate(ReportingType, .after = Question_Clean) %>%
  relocate(Reporting_Date_Start, .after = ReportingType) %>%
  relocate(Reporting_Date_End, .after = Reporting_Date_Start) %>%
  rename(Site_Mean = `Site Mean`,
         Site_N = `Site N`,
         All_PG_Database_Mean = `All Mean`,
         All_PG_Database_N = `All N`,
         All_PG_Database_Rank = `All Rank`) %>%
  relocate(All_PG_Database_Mean, .after = Site_N)

# Process EVS data from FTI ----------------------------
evs_pg <- evs_pg %>%
  select(-starts_with("...")) %>%
  mutate(ReportingType = ifelse(is.na(`Reporting Month`),
                                "YTD", "Monthly"))

evs_pg_monthly <- evs_pg %>%
  filter(ReportingType %in% "Monthly") %>%
  mutate(Month = as.Date(`Reporting Month`),
         Reporting_Date_Start =
           as.Date(paste0(month(Month), "/", day(Month), "/", year(Month)),
                   format("%m/%d/%Y")),
         Reporting_Date_End = Reporting_Date_Start + months(1) - 1,
         Month = NULL)

evs_pg_ytd <- evs_pg %>%
  filter(ReportingType %in% "YTD") %>%
  mutate(Reporting_Date_Start =
           as.Date(str_extract(`Month (discharges)`, ".+(?=\\ -)"),
          format = "%m/%d/%Y"),
         Reporting_Date_End =
           as.Date(str_extract(`Month (discharges)`, "(?<=\\- ).+"),
                   format = "%m/%d/%Y"))

evs_pg_repo <- rbind(evs_pg_monthly, evs_pg_ytd)

evs_pg_repo <- left_join(evs_pg_repo,
                         press_ganey_mapping[c("Service",
                                               "Questions",
                                               "Question_Clean")],
                         by = c("Metric Name" = "Questions"))

evs_pg_repo <- evs_pg_repo %>%
  mutate(Hospital = NULL,
         `Month (discharges)` = NULL,
         `Reporting Month` = NULL,
         `State Avg.` = NULL,
         `Metric Name` = NULL,
         Target = NULL,
         Percentile = NULL,
         All_PG_Database_Mean = as.numeric(NA),
         All_PG_Database_N = as.numeric(NA),
         All_PG_Database_Rank = as.numeric(NA)) %>%
  rename(Site_N = N,
         Site_Mean = `HCAHP Score`) %>%
  relocate(Service) %>%
  relocate(Question_Clean, .after = Site) %>%
  relocate(Site_Mean, .after= Reporting_Date_End) %>%
  relocate(Site_N, .after = Site_Mean) %>%
  mutate(Site_Mean = round(Site_Mean, digits = 2))

# Process Nursing data from FTI -------------------------
nursing_pg <- nursing_pg %>%
  select(-starts_with("...")) %>%
  mutate(ReportingType = ifelse(str_detect(Month,
                                           paste("3-Month",
                                                 "YTD",
                                                 "\\-",
                                                 sep = "|")),
                                "YTD", "Monthly"))

nursing_pg_monthly <- nursing_pg %>%
  filter(ReportingType %in% "Monthly") %>%
  mutate(`Reporting Month Close` = as.Date(`Reporting Month Close`),
         Reporting_Date_Start =
           as.Date(as.numeric(Month), origin = "1899-12-30"),
         Reporting_Date_End = Reporting_Date_Start + months(1) - 1,
         Test = `Reporting Month Close` - Reporting_Date_Start)

# # Rename columns
# reformat_pg_summary_repo <- pg_summary_repo %>%
#   mutate(`Reporting Closed Month` = NULL,
#          Month = as.Date(Month)) %>%
#   mutate_at(vars(contains("Site_"),
#                  contains("All_PG_Database_")),
#             function(x) {
#               as.numeric(
#                 str_replace(x, "N/A", replacement = NA_character_)
#               )
#             }) %>%
#   mutate_at(vars(contains("_Mean")),
#             ~round(., digits = 2))
# 
# 
# reformat_pg_summary_repo <- left_join(reformat_pg_summary_repo,
#                              pg_mapping[c("Questions", "Question_Clean")],
#                              by = c("KPI" = "Questions"))
# 
# reformat_pg_summary_repo <- reformat_pg_summary_repo %>%
#   mutate(KPI = NULL) %>%
#   relocate(Question_Clean, .after = Month) %>%
#   relocate(All_PG_Database_Mean, .before = All_PG_Database_N) %>%
#   arrange(Month,
#           Service,
#           Site)
# 
# write_xlsx(reformat_pg_summary_repo, press_ganey_table_path)
# 
# food_services_test <- reformat_pg_summary_repo %>%
#   filter(Service %in% "Food Services") %>%
#   select(-Site_N, -All_PG_Database_Mean, -All_PG_Database_N, -All_PG_Database_Rank) %>%
#   pivot_wider(names_from = Month,
#               values_from = Site_Mean)
# 
# food_services_entry <- reformat_pg_summary_repo %>%
#   # filter(Service %in% "Food Services") %>%
#   group_by(Service, Site, Month, Question_Clean) %>%
#   summarize(Count = n())
# 
# 
# 
#
# pg_summary_repo_long <- reformat_pg_summary_repo %>%
#   rename(Service = Service.y) %>%
#   mutate(Service.x = NULL,
#          KPI = NULL) %>%
#   relocate(Service) %>%
#   relocate(Question_Clean, .after = Month) %>%
#   pivot_longer(cols = c(contains("Site_"),
#                         contains("All_PG_Database_")),
#                names_to = "Metric") %>%
#   mutate(Incl_Scorecard = (Metric %in% "Site_Mean") |
#            (Metric %in% "Site_N" & Incl_N) |
#            (Metric %in% "All_PG_Database_Rank" & Incl_AllHosp_Rank),
#          Incl_N = NULL,
#          Incl_AllHosp_Rank = NULL,
#          Metric_Name = ifelse(
#            Metric %in% "Site_Mean", paste(Question_Clean, "- Score"),
#            ifelse(Metric %in% "Site_N", paste(Question_Clean, "- N"),
#                   ifelse(Metric %in% "All_PG_Database_Rank" & Incl_Scorecard,
#                          "Rank - All Hospitals",
#                          paste(Question_Clean, "-", Metric))))) %>%
#   relocate(Metric_Name, .after = Metric)
# 
# write_xlsx(pg_summary_repo_long, "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Summary Repos/Press Ganey New Format.xlsx")
# 
# pg_summary_repo <- pg_summary_repo %>%
#   mutate(Service.y = NULL,
#          KPI = NULL,
#          Site_Mean = round(Site_Mean, digits = 2),
#          All_PG_Database_Mean = round(All_PG_Database_Mean, digits = 2)) %>%
#   rename(Service = Service.x) %>%
#   relocate(Metric_Name, .before = Site_Mean)
#   
# metric_names <- pg_summary_repo_long %>%
#   filter(Incl_Scorecard) %>%
#   group_by(Service, Metric_Name) %>%
#   select(Service, Metric_Name) %>%
#   distinct()
