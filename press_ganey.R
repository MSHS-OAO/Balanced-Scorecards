# Source code for Press Ganey

# Import historical summary
press_ganey_mapping <- read_excel(target_mapping_path, sheet = "Press Ganey")

# Import current data for testing
data_ed <- read_csv(paste0(home_path,"Input Data Raw/Press Ganey/ED 09-2021.csv"))
data_nursing <- read_csv(paste0(home_path, "Input Data Raw/Press Ganey/Nursing 08-2021.csv"))
data_support <- read_csv(paste0(home_path,"Input Data Raw/Press Ganey/Support Services 08-2021.csv"))

data <- data_ed

# 
# test <- press_ganey_ed(data_ed)
# test <- press_ganey_processing(test)
# 
# data <- press_ganey_suppport_file(data_support)

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

press_ganey_ed <- function(data){
  # row_cutoff <- min(which(data$`REPORT TITLE` == "Emergency Department"))-1
  # data <- data[row_cutoff:nrow(data),]
  data <- data %>%
    select(-`Survey Type`) %>%
    select(-`Benchmarking Option`)
  
  data <- full_join(data, press_ganey_mapping)
  
  data <- data %>% filter(!is.na(Metric_Name)) %>%
    filter(!is.na(`My Sites`)) %>%
    filter(`My Sites` != "Total") 
  
  
  data$Site <- ifelse(data$`My Sites` == "'Mount Sinai Brooklyn'", "MSB",
                      ifelse(data$`My Sites` == "'Mount Sinai Beth Israel'", "MSBI",
                             ifelse(data$`My Sites` == "'Mount Sinai Queens'", "MSQ",
                                    ifelse(data$`My Sites` == "'Mount Sinai St. Luke's'", "MSM",
                                           ifelse(data$`My Sites` == "'Mount Sinai West'", "MSW",
                                                  ifelse(data$`My Sites` == "'The Mount Sinai Hospital'", "MSH", 
                                                         ifelse(data$`My Sites` == "'New York Eye & Ear Infirmary'", "NYEE",
                                                                ifelse(data$`My Sites` == "'Mount Sinai Hospital of Queens'", "MSQ",
                                                                       ifelse(data$`My Sites` == "'Mount Sinai Medical Center'", "MSH",NA)))))))))   
  
  
  data <- data %>% filter(!is.na(Site)) %>%
    select(-`My Sites`)
  
  data <- separate(data, col = `Benchmarking Period`, c("Month", "Reporting Closed Month"), sep = " - ")
  
  
  data <- data %>%
    select(-Metric_Name) %>%
    rename(KPI = Questions,
           `Site Mean` = Mean,
           `Site N` = n,
           `All N` = `All PG Database N`,
           `All Mean` = `All PG Database Score`,
           `All Rank` = `All PG Database Rank`)
  
  
}

press_ganey_nursing <- function(data){
  data <- data %>% filter(!is.na(Service)) %>%
                  filter(Service != "DEMOGRAPHIC REPORT") %>%
                  filter(Service != "Service") %>%
                  select(-`Survey Type`) %>%
                  select(-`Benchmarking Option`) %>%
                  select(-Service)
  
  data <- full_join(data, press_ganey_mapping)
  
  data <- data %>% filter(!is.na(Metric_Name)) %>%
                  filter(!is.na(`My Sites`)) %>%
                  filter(`My Sites` != "Total") 
  
  data$Site <- ifelse(data$`My Sites` == "'Mount Sinai Brooklyn'", "MSB",
                      ifelse(data$`My Sites` == "'Mount Sinai Beth Israel'", "MSBI",
                             ifelse(data$`My Sites` == "'Mount Sinai Queens'", "MSQ",
                                    ifelse(data$`My Sites` == "'Mount Sinai St. Luke's'", "MSM",
                                           ifelse(data$`My Sites` == "'Mount Sinai West'", "MSW",
                                                  ifelse(data$`My Sites` == "'The Mount Sinai Hospital'", "MSH", 
                                                         ifelse(data$`My Sites` == "'New York Eye & Ear Infirmary'", "NYEE",NA)))))))   
  
  data <- data %>% filter(!is.na(Site)) %>%
    select(-`My Sites`)
  
  data <- separate(data, col = `Benchmarking Period`, c("Month", "Reporting Closed Month"), sep = " - ")
  
  data <- data %>%
    select(-Metric_Name) %>%
    rename(KPI = Questions,
           `Site Mean` = `Top Box`,
           `Site N` = n,
           `All N` = `All PG Database N`,
           `All Mean` = `All PG Database Score`,
           `All Rank` = `All PG Database Rank`)
}

press_ganey_suppport_file <- function(data){
  row_cutoff <- min(which(data$`REPORT TITLE` == "Inpatient"))-1
  data <- data[row_cutoff:nrow(data),]
  data <- data %>%
    row_to_names(row_number = 1) %>%
    select(-Service) %>%
    select(-`Survey Type`) %>%
    select(-`Benchmarking Option`)
  
  
  data <- full_join(data,press_ganey_mapping)
  data <- data %>%
    filter(!is.na(Service))%>%
    filter((Service != "Nursing")) %>%
    filter(`My Sites` != "Total") %>%
    filter(`My Sites` != "'Mount Sinai South Nassau'")
  
  
  data$Site <- ifelse(data$`My Sites` == "'Mount Sinai Brooklyn'", "MSB",
                      ifelse(data$`My Sites` == "'Mount Sinai Beth Israel'", "MSBI",
                             ifelse(data$`My Sites` == "'Mount Sinai Queens'", "MSQ",
                                    ifelse(data$`My Sites` == "'Mount Sinai St. Luke's'", "MSM",
                                           ifelse(data$`My Sites` == "'Mount Sinai West'", "MSW",
                                                  ifelse(data$`My Sites` == "'The Mount Sinai Hospital'", "MSH", 
                                                         ifelse(data$`My Sites` == "'New York Eye & Ear Infirmary'", "NYEE",NA)))))))          
  
  data <- separate(data, col = `Benchmarking Period`, c("Month", "Reporting Closed Month"), sep = " - ")
  
  data <- data %>%
    select(-`My Sites`) %>%
    select(-Metric_Name) %>%
    rename(KPI = Questions,
           `Site Mean` = Mean,
           `Site N` = n,
           `All N` = `All PG Database N`,
           `All Mean` = `All PG Database Score`,
           `All Rank` = `All PG Database Rank`)
  
  data <- data %>% mutate(`Site Mean` = coalesce(`Site Mean`,`Top Box`)) %>%
    select(-`Top Box`)
}

press_ganey_processing <- function(data) {
  data <- full_join(data,press_ganey_mapping, by = c("KPI" = "Questions", "Service"))
  data <- data %>% filter(!is.na(Month))
  data <- distinct(data)
  
  data$value_rounded <- ifelse(data$Metric_Name == "Score", data$`Site Mean`,
                               ifelse(data$Metric_Name == "Number of Respondents", data$`Site N`,
                                      ifelse(data$Metric_Name == "Meals Overall - Score", data$`Site Mean`,
                                             ifelse(data$Metric_Name == "Meals Overall - N", data$`Site N`,
                                                    ifelse(data$Metric_Name == "Temp of Food", data$`Site Mean`,
                                                           ifelse(data$Metric_Name == "Quality of Food", data$`Site Mean`,
                                                                  ifelse(data$Metric_Name == "Courtesy of person served food", data$`Site Mean`,
                                                                         ifelse(data$Metric_Name == "Staff transported you around hosp- Score", data$`Site Mean`,
                                                                                ifelse(data$Metric_Name == "Rank - All Hospitals", data$`All Rank`,
                                                                                       ifelse(data$Metric_Name == "Nurse Communication - Score", data$`Site Mean`,
                                                                                              ifelse(data$Metric_Name == "Nurse Communication Responses - N", data$`Site N`,
                                                                                                     ifelse(data$`Metric_Name` == "Response of Staff - Score", data$`Site Mean`,
                                                                                                            ifelse(data$`Metric_Name` == "Response of Staff - N", data$`Site N`,
                                                                                                                   ifelse(data$Metric_Name == "Score - Staff Cared", data$`Site Mean`,
                                                                                                                          ifelse(data$Metric_Name == "# of Respondents - Staff Cared", data$`Site N`,
                                                                                                                                 ifelse(data$Metric_Name == "Score - Overall Care",data$`Site Mean`, 
                                                                                                                                        ifelse(data$Metric_Name == "# of Respondents - Overall Care", data$`Site N`,
                                                                                                                                               ifelse(data$Metric_Name == "Score - Staff Worked Together", data$`Site Mean`,
                                                                                                                                                      ifelse(data$Metric_Name == "# of Respondents - Staff Worked Together", data$`Site N`,NA)))))))))))))))))))
  data <- data %>%
    select(-KPI, -`Site Mean`, -`Site N`,-`All Mean`, -`All Rank`, -`All N`)
  
  data_df <- merge(data, metric_group_mapping[c("Metric_Group","Metric_Name")],
                   by = c("Metric_Name"))
  
  
  data_df <- data_df %>%
    mutate(Reporting_Month = format(as.Date(`Reporting Closed Month`, "%m/%d/%Y") %m+% months(2), "%m-%Y")) %>%
    mutate(Reporting_Month_Ref = format(as.Date(paste0(Reporting_Month, "-01"), "%m-%Y-%d"), "%Y-%m-%d"))  %>%
    mutate(Premier_Reporting_Period = format(as.Date(Reporting_Month_Ref), "%h %Y"))
  
  data_df_target <- merge(data_df[, c("Service","Site","Metric_Group", "Metric_Name","Reporting_Month","value_rounded")],
                          target_mapping, 
                          by.x = c("Service","Site","Metric_Group", "Metric_Name"),
                          by.y = c("Service","Site","Metric_Group", "Metric_Name"),
                          all = TRUE)
  
  data_df_target <- data_df_target %>%
    mutate(Variance = between(value_rounded, Range_1, Range_2)) %>% # Target mapping
    filter(Variance == TRUE)
  
  
  data_df_final <- full_join(data_df,data_df_target) %>%
    select(-Metric_Name_Submitted, -Month, -`Reporting Closed Month`, -Range_1, -Range_2, -Variance)
  
  
  # Subset processed data for merge 
  data_df_final <- data_df_final[,processed_df_cols]
  
  data_df_final$Reporting_Month_Ref <- as.Date(paste('01', as.yearmon(data_df_final$Reporting_Month, "%m-%Y")), format='%d %b %Y')
  
  
  updated_rows <- unique(data_df_final[c("Metric_Name","Reporting_Month","Service", "Site")])
  metrics_final_df <- anti_join(metrics_final_df, updated_rows)
  
  data_df_final$value_rounded <- as.numeric(data_df_final$value_rounded)
  
  metrics_final_df <- full_join(metrics_final_df,data_df_final)
  
}



