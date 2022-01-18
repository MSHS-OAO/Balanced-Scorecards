# Source code for Press Ganey

# Import historical summary
press_ganey_data <- read_excel(press_ganey_table_path)

# Import mapping file
press_ganey_mapping <- read_excel(target_mapping_path, sheet = "Press Ganey v2")

# Import current data for testing
data_ed_monthly <- read_csv(paste0(home_path,"Input Data Raw/Press Ganey/ED 09-2021.csv"))
data_nursing_monthly <- read_csv(paste0(home_path, "Input Data Raw/Press Ganey/Nursing 08-2021.csv"))
data_support_monthly <- read_csv(paste0(home_path,"Input Data Raw/Press Ganey/Support Services 08-2021.csv"))
# 
data <- raw_data_support

#
# test <- press_ganey_ed(data_ed)
# test <- press_ganey_processing(test)
#
# data <- press_ganey_suppport_file(data_support)


# Custom function for processing Press Ganey data
press_ganey_dept_summary <- function(data) {

  data_reporting_month <- data[(which(data$`REPORT TITLE` == "Service Date")+1), 2]
  colnames(data_reporting_month) <- "ReportingMo"

  data_reporting_month <- data_reporting_month$ReportingMo[1]

  data_reporting_month <- as.Date(str_extract(data_reporting_month, ".*(?=\\s\\-)"),
                                  format = "%m/%d/%Y")

  # Find the rows where Press Ganey data is located
  pg_data_start <- which(data$`REPORT TITLE` %in% "Service")[1] + 1
  pg_data_end <- ifelse(sum(data$`REPORT TITLE` %in% "DEMOGRAPHIC REPORT") > 0,
                        which(data$`REPORT TITLE` %in% "DEMOGRAPHIC REPORT") - 1,
                        nrow(data))

  # Subset raw data for rows of interest
  data_pg <- data[pg_data_start:pg_data_end, ]

  new_col_names <- str_split(data[pg_data_start - 1, 2], pattern = ",")[[1]]
  new_col_names <- new_col_names[1:(length(new_col_names) - 1)]
  pg_data_split <- separate(data_pg, col = 2, into = new_col_names, sep = ",")

  # Set site names
  pg_data_split <- pg_data_split %>%
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
      Month = data_reporting_month
    )

  pg_data_split <- pg_data_split %>%
    filter(!(Site %in% c("MSSN", "MSHS")))

  # Merge with mapping to get metric names for dashboard
  pg_data_split <- left_join(pg_data_split,
                             press_ganey_mapping[c("Service",
                                                 "Questions",
                                                 "Question_Clean")],
                             by = c("Questions" = "Questions"))

  # Filter out unused data
  pg_data_split <- pg_data_split %>%
    # Remove irrelevant sites and service line combinations
    filter(!(Site %in% c("MSQ", "NYEE") & Service %in% "Patient Transport") &
             # Filter out data for questions that aren't being tracked in the scorecard
             !is.na(Service))
  
  # Replace blanks with NA
  pg_data_split[pg_data_split == ""] <- NA

  # Create a column/rename column with score
  if ("Mean" %in% colnames(pg_data_split) &
      "Top Box" %in% colnames(pg_data_split)) {
    pg_data_split <- pg_data_split %>%
      mutate(Site_Mean = coalesce(Mean, `Top Box`))

  } else if ("Mean" %in% colnames(pg_data_split)) {
    pg_data_split <- pg_data_split %>%
      rename(Site_Mean = Mean)
  } else {
    pg_data_split <- pg_data_split %>%
      rename(Site_Mean = `Top Box`)
  }
  
  press_ganey_summary <- pg_data_split %>%
    rename(Site_N = n,
           All_PG_Database_Mean = `All PG Database Score`,
           All_PG_Database_Rank = `All PG Database Rank`,
           All_PG_Database_N = `All PG Database N`) %>%
    select(Service,
           Site,
           Month,
           Question_Clean,
           Site_Mean,
           Site_N,
           All_PG_Database_Mean,
           All_PG_Database_N,
           All_PG_Database_Rank) %>%
    arrange(Month,
            Service,
            Site)




  return(press_ganey_summary)

}

test_function_support <- press_ganey_dept_summary(data = data_support)
test_function_nursing <- press_ganey_dept_summary(data = data_nursing)
test_function_ed <- press_ganey_dept_summary(data = data_ed)

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



