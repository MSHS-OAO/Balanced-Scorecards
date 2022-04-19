# Source code for Press Ganey

# Import historical summary
pt_exp_data <- read_excel(pt_exp_table_path)
pt_exp_data <- pt_exp_data %>%
  mutate(Reporting_Date_Start = as.Date(Reporting_Date_Start),
         Reporting_Date_End = as.Date(Reporting_Date_End)) %>%
  filter(!(Site %in% "All"))

# Import mapping file
pt_exp_mapping <- read_excel(target_mapping_path, sheet = "Patient Experience")

# # Import current data for testing
# # Import monthly data
# data_ed_monthly <- read_csv(paste0(home_path,
#                                    "Input Data Raw/Press Ganey/",
#                                    "ED 09-2021.csv"),
#                             show_col_types = FALSE)
# data_nursing_monthly <- read_csv(paste0(home_path,
#                                         "Input Data Raw/Press Ganey/",
#                                         "Nursing 08-2021.csv"),
#                                  show_col_types = FALSE)
# data_support_monthly <- read_csv(paste0(home_path,
#                                         "Input Data Raw/Press Ganey/",
#                                         "Support Services 08-2021.csv"),
#                                  show_col_types = FALSE)
# # 
# # Import YTD data
# data_ed_ytd <- read_csv(paste0(home_path,
#                                "Input Data Raw/Press Ganey/",
#                                "ED YTD 012021 to 092021.csv"),
#                         show_col_types = FALSE)
# data_nursing_ytd <- read_csv(paste0(home_path,
#                                     "Input Data Raw/Press Ganey/",
#                                     "Nursing YTD 012021 to 082021.csv"),
#                              show_col_types = FALSE)
# data_support_ytd <- read_csv(paste0(home_path,
#                                     "Input Data Raw/Press Ganey/",
#                                     "Support Services YTD 012021 to 082021.csv"),
#                              show_col_types = FALSE)

# Custom function for processing Press Ganey data
pt_exp_dept_summary <- function(data) {

  data_reporting_period <- data[(which(data$`REPORT TITLE` == "Service Date")+1), 2]
  colnames(data_reporting_period) <- "ReportingPeriod"

  data_reporting_period <- data_reporting_period$ReportingPeriod[1]

  reporting_start <- as.Date(str_extract(data_reporting_period, ".*(?=\\s\\-)"),
                                  format = "%m/%d/%Y")
  reporting_end <- as.Date(str_extract(data_reporting_period, "(?<=\\- ).*"),
                           format = "%m/%d/%Y")
  
  reporting_type <- ifelse(month(reporting_start) == month(reporting_end) &
                             year(reporting_start) == year(reporting_end),
                           "Monthly", "YTD")

  # Find the rows where Press Ganey data is located
  pt_exp_data_start <- which(data$`REPORT TITLE` %in% "Service")[1] + 1
  pt_exp_data_end <- ifelse(
    sum(data$`REPORT TITLE` %in% "DEMOGRAPHIC REPORT") > 0,
    which(data$`REPORT TITLE` %in% "DEMOGRAPHIC REPORT") - 1,
    nrow(data))

  # Subset raw data for rows of interest
  data_pt_exp <- data[pt_exp_data_start:pt_exp_data_end, ]

  new_col_names <- str_split(data[pt_exp_data_start - 1, 2], pattern = ",")[[1]]
  new_col_names <- new_col_names[1:(length(new_col_names) - 1)]
  pt_exp_data_split <- separate(data_pt_exp, col = 2,
                                into = new_col_names, sep = ",")

  # Set site names
  pt_exp_data_split <- pt_exp_data_split %>%
    mutate(Site = case_when(
      str_detect(`My Sites`, "(Beth Israel Medical Center)|(Mount Sinai Beth Israel)") ~ "MSBI",
      str_detect(`My Sites`, "Mount Sinai Brooklyn") ~ "MSB",
      str_detect(`My Sites`, "(Mount Sinai Queens)|(Mount Sinai Hospital of Queens)") ~ "MSQ",
      str_detect(`My Sites`, "(Mount Sinai Medical Center)|(The Mount Sinai Hospital)") ~ "MSH",
      str_detect(`My Sites`, "Mount Sinai South Nassau") ~ "MSSN",
      str_detect(`My Sites`, "Mount Sinai St. Luke") ~ "MSM",
      str_detect(`My Sites`, "West") ~ "MSW",
      str_detect(`My Sites`, "New York Eye & Ear Infirmary") ~ "NYEE",
      str_detect(`My Sites`, "Total") ~ "MSHS"),
      ReportingType = reporting_type,
      Reporting_Date_Start = reporting_start,
      Reporting_Date_End = reporting_end
    )

  pt_exp_data_split <- pt_exp_data_split %>%
    filter(!(Site %in% c("MSSN", "MSHS"))) %>%
    rename(Raw_Pt_Exp_Service = `REPORT TITLE`)

  # Merge with mapping to get metric names for dashboard
  pt_exp_data_split <- left_join(pt_exp_data_split,
                                 pt_exp_mapping[c("Raw_Pt_Exp_Service",
                                                  "Service",
                                                  "Questions",
                                                  "Question_Clean")],
                             by = c("Raw_Pt_Exp_Service" = "Raw_Pt_Exp_Service",
                                    "Questions" = "Questions"))

  # Filter out unused data
  pt_exp_data_split <- pt_exp_data_split %>%
    # Remove irrelevant sites and service line combinations
    filter(!(Site %in% c("MSQ", "NYEE") & Service %in% "Patient Transport") &
             # Filter out data for questions that aren't being tracked in the scorecard
             !is.na(Service))
  
  # Replace blanks with NA
  pt_exp_data_split[pt_exp_data_split == ""] <- NA

  # Create a column/rename column with score
  if ("Mean" %in% colnames(pt_exp_data_split) &
      "Top Box" %in% colnames(pt_exp_data_split)) {
    pt_exp_data_split <- pt_exp_data_split %>%
      mutate(Site_Mean = coalesce(Mean, `Top Box`))

  } else if ("Mean" %in% colnames(pt_exp_data_split)) {
    pt_exp_data_split <- pt_exp_data_split %>%
      rename(Site_Mean = Mean)
  } else {
    pt_exp_data_split <- pt_exp_data_split %>%
      rename(Site_Mean = `Top Box`)
  }
  
  pt_exp_summary <- pt_exp_data_split %>%
    rename(Site_N = n,
           All_PG_Database_Mean = `All PG Database Score`,
           All_PG_Database_Rank = `All PG Database Rank`,
           All_PG_Database_N = `All PG Database N`) %>%
    mutate_at(vars(c(contains("Site_"),
                     contains("All_PG_Database_"))),
              as.numeric) %>%
    mutate_at(vars(contains("_Mean")),
              round, digits = 2) %>%
    select(Service,
           Site,
           Question_Clean,
           ReportingType,
           Reporting_Date_Start,
           Reporting_Date_End,
           Site_Mean,
           Site_N,
           All_PG_Database_Mean,
           All_PG_Database_N,
           All_PG_Database_Rank) %>%
    arrange(Service,
            Site,
            ReportingType,
            Reporting_Date_End)

  return(pt_exp_summary)

}

# Custom function for formatting Press Ganey data for metrics_final_df ---------
pt_exp_metrics_final_df <- function(pt_exp_summary) {
  
  # Filter out YTD data
  pt_exp_metrics_final_df <- pt_exp_summary %>%
    filter(ReportingType %in% "Monthly" &
             !is.na(Site_Mean))
  
  # Update Press Ganey mapping file used to determine which metrics to include
  pt_exp_mapping_simple <- pt_exp_mapping %>%
    select(-Questions)
  
  # Crosswalk Press Ganey data with mapping file to determine which metrics to include
  pt_exp_metrics_final_df <- left_join(pt_exp_metrics_final_df,
                                       pt_exp_mapping_simple,
                                       by = c("Service" = "Service",
                                              "Question_Clean" = "Question_Clean"))
  
  pt_exp_metrics_final_df <- pt_exp_metrics_final_df %>%
    # Convert to longer format
    pivot_longer(cols = c(contains("Site_"),
                          contains("All_PG_Database_")),
                 names_to = "Metric") %>%
    # Update metric names
    mutate(Metric_Name_Submitted = ifelse(Metric %in% "Site_Mean",
                                          paste0(Question_Clean, " - Score"),
                                          ifelse(Metric %in% "Site_N" & Incl_N,
                                                 paste0(Question_Clean, " - N"),
                                                 ifelse(Metric %in% "All_PG_Database_Rank" &
                                                          Incl_AllHosp_Rank,
                                                        "Rank - All Hospitals",
                                                        NA)))) %>%
    # Remove metrics that are not included in reporting from metrics_final_df
    # (ie, sample size not reported for all metrics, not all service lines report hospital ranking)
    filter(!is.na(Metric_Name_Submitted)) %>%
    rename(value_rounded = value) %>%
    mutate(Premier_Reporting_Period = format(Reporting_Date_Start, "%b %Y"),
           Reporting_Month = format(Reporting_Date_Start, "%m-%Y"),
           Question_Clean = NULL,
           ReportingType = NULL,
           Reporting_Date_Start = NULL,
           Reporting_Date_End = NULL,
           Incl_N = NULL,
           Incl_AllHosp_Rank = NULL,
           Metric = NULL)
  
  # Use custom function for updating metrics_final_df using standard process
  metrics_final_df <- metrics_final_df_subset_and_merge(pt_exp_metrics_final_df)
  
  # # Crosswalk with metric grouping
  # pt_exp_metrics_final_df <- merge(pt_exp_metrics_final_df,
  #                                  metric_mapping_breakout[c("Metric_Group",
  #                                                            "Metric_Name",
  #                                                            "Metric_Name_Submitted")],
  #                                  # metric_group_mapping[c("Metric_Group",
  #                                  #                        "Metric_Name",
  #                                  #                        "Metric_Name_Submitted")],
  #                                  by = c("Metric_Name_Submitted"))
  # 
  # 
  # # Select relevant columns
  # pt_exp_metrics_final_df <- pt_exp_metrics_final_df[, processed_df_cols]  
  #   
  # # Add reporting month back in
  # pt_exp_metrics_final_df <- pt_exp_metrics_final_df %>%
  #   mutate(Reporting_Month_Ref = as.Date(paste("01",
  #                                              as.yearmon(Reporting_Month,
  #                                                         "%m-%Y")),
  #                                        format = "%d %b %Y"))
  # 
  # new_rows <- unique(pt_exp_metrics_final_df[, c("Metric_Name",
  #                                                "Reporting_Month",
  #                                                "Service",
  #                                                "Site")])
  # 
  # metrics_final_df <- anti_join(metrics_final_df,
  #                               new_rows)
  # 
  # metrics_final_df <- full_join(metrics_final_df,
  #                               pt_exp_metrics_final_df)
  # 
  # return(metrics_final_df)
  
}

# # Custom function to format Press Ganey YTD data for Summary Tab
# reformat_pg_fytd <- function(data) {
#   
#   press_ganey_ytd <- press_ganey_data %>%
#     # Add logic to include Jan data in YTD data
#     mutate(ReportingType = ifelse(month(Reporting_Date_Start) == 1 &
#                                     month(Reporting_Date_End) == 1,
#                                   "YTD", ReportingType)) %>%
#     # Filter on YTD data and selected service
#     filter(ReportingType %in% "YTD" &
#              Service %in% service_input) %>%
#     mutate(Reporting_Month_Ref = floor_date(Reporting_Date_End,
#                                             unit = "month")) %>%
#     filter(Reporting_Month_Ref <= current_period)
#   
#   # Press Ganey mapping for metrics
#   pg_mapping_simple <- press_ganey_mapping %>%
#     select(-Questions)
#   
#   # Crosswalk PG YTD data with mapping
#   press_ganey_ytd <- left_join(press_ganey_ytd,
#                                pg_mapping_simple,
#                                by = c("Service" = "Service",
#                                       "Question_Clean" = "Question_Clean"))
#   
#   # Begin reformatting Press Ganey YTD data
#   pg_ytd_reformat <- press_ganey_ytd %>%
#     # Convert to longer format
#     pivot_longer(cols = c(contains("Site_"),
#                           contains("All_PG_Database_")),
#                  names_to = "Metric") %>%
#     # Update metric names
#     mutate(Metric_Name_Submitted = ifelse(Metric %in% "Site_Mean",
#                                           paste0(Question_Clean, " - Score"),
#                                           ifelse(Metric %in% "Site_N" & Incl_N,
#                                                  paste0(Question_Clean, " - N"),
#                                                  ifelse(Metric %in% "All_PG_Database_Rank" &
#                                                           Incl_AllHosp_Rank,
#                                                         "Rank - All Hospitals",
#                                                         NA)))) %>%
#     # Remove unused metrics
#     filter(!is.na(Metric_Name_Submitted)) %>%
#     # Remove unused columns
#     select(-Question_Clean,
#            -ReportingType,
#            -Incl_N,
#            -Incl_AllHosp_Rank,
#            -Metric) %>%
#     # Rename value column for consistency
#     rename(value_rounded = value)
#   
#   # Identify Press Ganey metrics to include in Summary tab
#   pg_summary_tab_metrics <- summary_metric_filter %>%
#     filter(Service %in% service_input) %>%
#     select(Service,
#            Metric_Group,
#            Metric_Name,
#            Metric_Name_Submitted,
#            Summary_Metric_Name)
#   
#   # Crosswalk Press Ganey YTD data with Summary tab metrics
#   pg_ytd_reformat <- left_join(pg_ytd_reformat,
#                                pg_summary_tab_metrics,
#                                by = c("Service" = "Service",
#                                       "Metric_Name_Submitted" = "Metric_Name_Submitted"))
#   
#   pg_ytd_reformat <- pg_ytd_reformat %>%
#     # Filter on 
#     filter(!is.na(Summary_Metric_Name) &
#              Reporting_Month_Ref == max(Reporting_Month_Ref)) %>%
#     mutate(`Fiscal Year to Date` = ifelse(month(Reporting_Month_Ref) == 1 &
#                                             month(Reporting_Date_Start) == 1,
#                                           format(Reporting_Month_Ref, "%b %Y"),
#                                           paste0(
#                                             format(Reporting_Date_Start, "%b"),
#                                             " - ",
#                                             format(Reporting_Month_Ref, "%b %Y"),
#                                             " Average"))) %>%
#     select(-Service,
#            -Reporting_Date_Start,
#            -Reporting_Date_End,
#            -Reporting_Month_Ref,
#            -Metric_Name_Submitted) %>%
#     relocate(value_rounded, .after = `Fiscal Year to Date`)
#   
#   return(pg_ytd_reformat)
#   
# }
#
# pg_metrics_final_df <- press_ganey_data %>%
#   filter(ReportingType %in% "Monthly")
# 
# pg_mapping_clean <- press_ganey_mapping %>%
#   select(-Questions)
# 
# pg_metrics_final_df <- left_join(pg_metrics_final_df,
#                                  pg_mapping_clean,
#                                  by = c("Service" = "Service",
#                                         "Question_Clean" = "Question_Clean"))
# 
# pg_metrics_final_df2 <- pg_metrics_final_df %>%
#   pivot_longer(cols = c(contains("Site_"),
#                         contains("All_PG_Database_")),
#                names_to = "Metric") %>%
#   mutate(Metric_Name_Submitted = ifelse(Metric %in% "Site_Mean",
#                                 paste0(Question_Clean, " - Score"),
#                                 ifelse(Metric %in% "Site_N" & Incl_N,
#                                        paste0(Question_Clean, " - N"),
#                                        ifelse(Metric %in% "All_PG_Database_Rank" &
#                                                 Incl_AllHosp_Rank,
#                                               "Rank - All Hospitals", NA)))) %>%
#   filter(!is.na(Metric_Name_Submitted)) %>%
#   rename(value_rounded = value) %>%
#   mutate(Premier_Reporting_Period = format(Reporting_Date_Start, "%b %Y"),
#          Reporting_Month = format(Reporting_Date_Start, "%m-%Y"),
#          Question_Clean = NULL,
#          ReportingType = NULL,
#          Reporting_Date_Start = NULL,
#          Reporting_Date_End = NULL,
#          Incl_N = NULL,
#          Incl_AllHosp_Rank = NULL,
#          Metric = NULL)
# 
# # test <- unique(pg_metrics_final_df2[c("Service", "Metric_Name_Submitted")])
# # 
# # test$Check <- test$Metric_Name_Submitted %in% metric_grouping_filter$Metric_Name_Submitted
# 
# pg_metrics_final_df2 <- merge(pg_metrics_final_df2,
#                              metric_group_mapping[c("Metric_Group",
#                                                     "Metric_Name",
#                                                     "Metric_Name_Submitted")],
#                              by = c("Metric_Name_Submitted"))
# 
# pg_metrics_target_status <- merge(pg_metrics_final_df2[, c("Service",
#                                                             "Site",
#                                                             "Metric_Group",
#                                                             "Metric_Name",
#                                                             "Reporting_Month",
#                                                             "value_rounded")],
#                                    target_mapping,
#                                    by.x = c("Service",
#                                             "Site",
#                                             "Metric_Group",
#                                             "Metric_Name"),
#                                    by.y = c("Service",
#                                             "Site",
#                                             "Metric_Group",
#                                             "Metric_Name"),
#                                    all.x = TRUE)
# 
# pg_metrics_target_status <- pg_metrics_target_status %>%
#   mutate(Variance = between(value_rounded, Range_1, Range_2)) %>%
#   filter(!is.na(Reporting_Month) &
#            !(Variance %in% FALSE))
# 
# pg_metrics_df_merge <- merge(pg_metrics_final_df2,
#                              pg_metrics_target_status[, c("Service",
#                                                           "Site",
#                                                           "Metric_Group",
#                                                           "Metric_Name",
#                                                           "Reporting_Month",
#                                                           "Target",
#                                                           "Status")],
#                              all = FALSE)
# 
# # Add reporting month back in
# pg_metrics_df_merge <- pg_metrics_df_merge %>%
#   mutate(Reporting_Month_Ref = as.Date(paste("01",
#                                              as.yearmon(Reporting_Month,
#                                                         "%m-%Y")),
#                                        format = "%d %b %Y"))
# 
# pg_metrics_df_merge <- pg_metrics_df_merge[, processed_df_cols]
# 
# new_rows <- unique(pg_metrics_df_merge[, c("Metric_Name",
#                                            "Reporting_Month",
#                                            "Service",
#                                            "Site")])
# 
# metrics_final_df <- anti_join(metrics_final_df,
#                               new_rows)
# 
# metrics_final_df <- full_join(metrics_final_df,
#                               pg_metrics_df_merge)
# 
# # saveRDS(metrics_final_df, metrics_final_df_path)
# 
# test_ed_monthly <- press_ganey_dept_summary(data = data_ed_monthly)
# test_ed_ytd <- press_ganey_dept_summary(data = data_ed_ytd)
# 
# test_nursing_monthly <- press_ganey_dept_summary(data = data_nursing_monthly)
# test_nursing_ytd <- press_ganey_dept_summary(data = data_nursing_ytd)
# 
# test_support_monthly <- press_ganey_dept_summary(data = data_support_monthly)
# test_support_ytd <- press_ganey_dept_summary(data = data_support_ytd)
#
# press_ganey_ed <- function(data){
#   # row_cutoff <- min(which(data$`REPORT TITLE` == "Emergency Department"))-1
#   # data <- data[row_cutoff:nrow(data),]
#   data <- data %>%
#     select(-`Survey Type`) %>%
#     select(-`Benchmarking Option`)
#   
#   data <- full_join(data, press_ganey_mapping)
#   
#   data <- data %>% filter(!is.na(Metric_Name)) %>%
#     filter(!is.na(`My Sites`)) %>%
#     filter(`My Sites` != "Total") 
#   
#   
#   data$Site <- ifelse(data$`My Sites` == "'Mount Sinai Brooklyn'", "MSB",
#                       ifelse(data$`My Sites` == "'Mount Sinai Beth Israel'", "MSBI",
#                              ifelse(data$`My Sites` == "'Mount Sinai Queens'", "MSQ",
#                                     ifelse(data$`My Sites` == "'Mount Sinai St. Luke's'", "MSM",
#                                            ifelse(data$`My Sites` == "'Mount Sinai West'", "MSW",
#                                                   ifelse(data$`My Sites` == "'The Mount Sinai Hospital'", "MSH", 
#                                                          ifelse(data$`My Sites` == "'New York Eye & Ear Infirmary'", "NYEE",
#                                                                 ifelse(data$`My Sites` == "'Mount Sinai Hospital of Queens'", "MSQ",
#                                                                        ifelse(data$`My Sites` == "'Mount Sinai Medical Center'", "MSH",NA)))))))))   
#   
#   
#   data <- data %>% filter(!is.na(Site)) %>%
#     select(-`My Sites`)
#   
#   data <- separate(data, col = `Benchmarking Period`, c("Month", "Reporting Closed Month"), sep = " - ")
#   
#   
#   data <- data %>%
#     select(-Metric_Name) %>%
#     rename(KPI = Questions,
#            `Site Mean` = Mean,
#            `Site N` = n,
#            `All N` = `All PG Database N`,
#            `All Mean` = `All PG Database Score`,
#            `All Rank` = `All PG Database Rank`)
#   
#   
# }
# 
# press_ganey_nursing <- function(data){
#   data <- data %>% filter(!is.na(Service)) %>%
#                   filter(Service != "DEMOGRAPHIC REPORT") %>%
#                   filter(Service != "Service") %>%
#                   select(-`Survey Type`) %>%
#                   select(-`Benchmarking Option`) %>%
#                   select(-Service)
#   
#   data <- full_join(data, press_ganey_mapping)
#   
#   data <- data %>% filter(!is.na(Metric_Name)) %>%
#                   filter(!is.na(`My Sites`)) %>%
#                   filter(`My Sites` != "Total") 
#   
#   data$Site <- ifelse(data$`My Sites` == "'Mount Sinai Brooklyn'", "MSB",
#                       ifelse(data$`My Sites` == "'Mount Sinai Beth Israel'", "MSBI",
#                              ifelse(data$`My Sites` == "'Mount Sinai Queens'", "MSQ",
#                                     ifelse(data$`My Sites` == "'Mount Sinai St. Luke's'", "MSM",
#                                            ifelse(data$`My Sites` == "'Mount Sinai West'", "MSW",
#                                                   ifelse(data$`My Sites` == "'The Mount Sinai Hospital'", "MSH", 
#                                                          ifelse(data$`My Sites` == "'New York Eye & Ear Infirmary'", "NYEE",NA)))))))   
#   
#   data <- data %>% filter(!is.na(Site)) %>%
#     select(-`My Sites`)
#   
#   data <- separate(data, col = `Benchmarking Period`, c("Month", "Reporting Closed Month"), sep = " - ")
#   
#   data <- data %>%
#     select(-Metric_Name) %>%
#     rename(KPI = Questions,
#            `Site Mean` = `Top Box`,
#            `Site N` = n,
#            `All N` = `All PG Database N`,
#            `All Mean` = `All PG Database Score`,
#            `All Rank` = `All PG Database Rank`)
# }
# 
# press_ganey_suppport_file <- function(data){
#   row_cutoff <- min(which(data$`REPORT TITLE` == "Inpatient"))-1
#   data <- data[row_cutoff:nrow(data),]
#   data <- data %>%
#     row_to_names(row_number = 1) %>%
#     select(-Service) %>%
#     select(-`Survey Type`) %>%
#     select(-`Benchmarking Option`)
#   
#   
#   data <- full_join(data,press_ganey_mapping)
#   data <- data %>%
#     filter(!is.na(Service))%>%
#     filter((Service != "Nursing")) %>%
#     filter(`My Sites` != "Total") %>%
#     filter(`My Sites` != "'Mount Sinai South Nassau'")
#   
#   
#   data$Site <- ifelse(data$`My Sites` == "'Mount Sinai Brooklyn'", "MSB",
#                       ifelse(data$`My Sites` == "'Mount Sinai Beth Israel'", "MSBI",
#                              ifelse(data$`My Sites` == "'Mount Sinai Queens'", "MSQ",
#                                     ifelse(data$`My Sites` == "'Mount Sinai St. Luke's'", "MSM",
#                                            ifelse(data$`My Sites` == "'Mount Sinai West'", "MSW",
#                                                   ifelse(data$`My Sites` == "'The Mount Sinai Hospital'", "MSH", 
#                                                          ifelse(data$`My Sites` == "'New York Eye & Ear Infirmary'", "NYEE",NA)))))))          
#   
#   data <- separate(data, col = `Benchmarking Period`, c("Month", "Reporting Closed Month"), sep = " - ")
#   
#   data <- data %>%
#     select(-`My Sites`) %>%
#     select(-Metric_Name) %>%
#     rename(KPI = Questions,
#            `Site Mean` = Mean,
#            `Site N` = n,
#            `All N` = `All PG Database N`,
#            `All Mean` = `All PG Database Score`,
#            `All Rank` = `All PG Database Rank`)
#   
#   data <- data %>% mutate(`Site Mean` = coalesce(`Site Mean`,`Top Box`)) %>%
#     select(-`Top Box`)
# }
# 
# press_ganey_processing <- function(data) {
#   data <- full_join(data,press_ganey_mapping, by = c("KPI" = "Questions", "Service"))
#   data <- data %>% filter(!is.na(Month))
#   data <- distinct(data)
#   
#   data$value_rounded <- ifelse(data$Metric_Name == "Score", data$`Site Mean`,
#                                ifelse(data$Metric_Name == "Number of Respondents", data$`Site N`,
#                                       ifelse(data$Metric_Name == "Meals Overall - Score", data$`Site Mean`,
#                                              ifelse(data$Metric_Name == "Meals Overall - N", data$`Site N`,
#                                                     ifelse(data$Metric_Name == "Temp of Food", data$`Site Mean`,
#                                                            ifelse(data$Metric_Name == "Quality of Food", data$`Site Mean`,
#                                                                   ifelse(data$Metric_Name == "Courtesy of person served food", data$`Site Mean`,
#                                                                          ifelse(data$Metric_Name == "Staff transported you around hosp- Score", data$`Site Mean`,
#                                                                                 ifelse(data$Metric_Name == "Rank - All Hospitals", data$`All Rank`,
#                                                                                        ifelse(data$Metric_Name == "Nurse Communication - Score", data$`Site Mean`,
#                                                                                               ifelse(data$Metric_Name == "Nurse Communication Responses - N", data$`Site N`,
#                                                                                                      ifelse(data$`Metric_Name` == "Response of Staff - Score", data$`Site Mean`,
#                                                                                                             ifelse(data$`Metric_Name` == "Response of Staff - N", data$`Site N`,
#                                                                                                                    ifelse(data$Metric_Name == "Score - Staff Cared", data$`Site Mean`,
#                                                                                                                           ifelse(data$Metric_Name == "# of Respondents - Staff Cared", data$`Site N`,
#                                                                                                                                  ifelse(data$Metric_Name == "Score - Overall Care",data$`Site Mean`, 
#                                                                                                                                         ifelse(data$Metric_Name == "# of Respondents - Overall Care", data$`Site N`,
#                                                                                                                                                ifelse(data$Metric_Name == "Score - Staff Worked Together", data$`Site Mean`,
#                                                                                                                                                       ifelse(data$Metric_Name == "# of Respondents - Staff Worked Together", data$`Site N`,NA)))))))))))))))))))
#   data <- data %>%
#     select(-KPI, -`Site Mean`, -`Site N`,-`All Mean`, -`All Rank`, -`All N`)
#   
#   data_df <- merge(data, metric_group_mapping[c("Metric_Group","Metric_Name")],
#                    by = c("Metric_Name"))
#   
#   
#   data_df <- data_df %>%
#     mutate(Reporting_Month = format(as.Date(`Reporting Closed Month`, "%m/%d/%Y") %m+% months(2), "%m-%Y")) %>%
#     mutate(Reporting_Month_Ref = format(as.Date(paste0(Reporting_Month, "-01"), "%m-%Y-%d"), "%Y-%m-%d"))  %>%
#     mutate(Premier_Reporting_Period = format(as.Date(Reporting_Month_Ref), "%h %Y"))
#   
#   data_df_target <- merge(data_df[, c("Service","Site","Metric_Group", "Metric_Name","Reporting_Month","value_rounded")],
#                           target_mapping, 
#                           by.x = c("Service","Site","Metric_Group", "Metric_Name"),
#                           by.y = c("Service","Site","Metric_Group", "Metric_Name"),
#                           all = TRUE)
#   
#   data_df_target <- data_df_target %>%
#     mutate(Variance = between(value_rounded, Range_1, Range_2)) %>% # Target mapping
#     filter(Variance == TRUE)
#   
#   
#   data_df_final <- full_join(data_df,data_df_target) %>%
#     select(-Metric_Name_Submitted, -Month, -`Reporting Closed Month`, -Range_1, -Range_2, -Variance)
#   
#   
#   # Subset processed data for merge 
#   data_df_final <- data_df_final[,processed_df_cols]
#   
#   data_df_final$Reporting_Month_Ref <- as.Date(paste('01', as.yearmon(data_df_final$Reporting_Month, "%m-%Y")), format='%d %b %Y')
#   
#   
#   updated_rows <- unique(data_df_final[c("Metric_Name","Reporting_Month","Service", "Site")])
#   metrics_final_df <- anti_join(metrics_final_df, updated_rows)
#   
#   data_df_final$value_rounded <- as.numeric(data_df_final$value_rounded)
#   
#   metrics_final_df <- full_join(metrics_final_df,data_df_final)
#   
# }
# 
# press_ganey_data <- press_ganey_data %>%
#   filter(!((Service %in% c("Nursing",
#                            "Food Services",
#                            "Patient Transport",
#                            "Environmental Services") &
#               year(Reporting_Date_End) == 2021 & month(Reporting_Date_End) == 8) |
#              (Service %in% c("Emergency Department") &
#                 year(Reporting_Date_End) == 2021 & month(Reporting_Date_End) == 9)))
# 
# metrics_final_df <- metrics_final_df %>%
#   filter(!((Service %in% c("Nursing",
#                            "Food Services",
#                            "Patient Transport",
#                            "Environmental Services") &
#               Reporting_Month_Ref == as.Date("8/1/21", format = "%m/%d/%y")) |
#              (Service %in% c("Emergency Department") &
#                 Reporting_Month_Ref == as.Date("9/1/21", format = "%m/%d/%y"))))
# 
# write_xlsx(press_ganey_data, press_ganey_table_path)
# 
# saveRDS(metrics_final_df, metrics_final_df_path)
# 
# metrics_final_df <- press_ganey_metrics_final_df(press_ganey_data)
