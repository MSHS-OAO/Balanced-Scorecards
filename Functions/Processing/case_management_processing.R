#data <- read_excel("/SharedDrive//deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Group 1/Case Management/MSHS UM review for the score cards January-February 2023 with Admissions  draft 4-14-2023.xlsx", skip = 5)

case_management_function <- function(data, updated_user) {
  required_ur_review_index_end <- which(data$`Row Labels` %in% "# of Admissions with UR Review  LOS 2+")
  completed_ur_review_within_three_days_index_end <- which(data$`Row Labels` %in% "Reviews completed within 3 days from Admission")
  data_end_index <- nrow(data)
  
  required_ur_review <- data[1:required_ur_review_index_end-1,] ##truncate df for first table
  completed_ur_review <- data[required_ur_review_index_end:completed_ur_review_within_three_days_index_end-1,]
  three_day_completed_ur_review <- data[completed_ur_review_within_three_days_index_end:data_end_index,]
  
  ##Create df for all required review admisions
  required_ur_review <- required_ur_review %>% 
                        select(-`Grand Total`) %>% 
                        rename(Site = `Row Labels`) %>% 
                        pivot_longer(!Site, names_to = "Month", values_to = "Value") %>%
                        filter(Site != "Grand Total") %>% 
                        filter(!is.na(Value)) %>%
                        filter(!is.na(as.numeric(Value))) %>%
                        mutate(Metric = "Required_UR_Review",
                               Value = as.numeric(Value))
  
  ##Create df for all completed review admisions
  completed_ur_review <- completed_ur_review %>% 
    select(-`Grand Total`) %>% 
    rename(Site = `Row Labels`) %>% 
    pivot_longer(!Site, names_to = "Month", values_to = "Value") %>%
    filter(Site != "Grand Total") %>% 
    filter(!is.na(Value)) %>%
    mutate(Site = toupper(trimws(Site))) %>%
    filter(!(Site %in% c("LOS", "REVIEW TYPE", "COUNT OF ACCOUNT NUMBER", "ROW LABELS"))) %>%
    filter(!is.na(as.numeric(Value))) %>%
    mutate(Metric = "Completed_UR_Review",
           Value = as.numeric(Value))
  
  ##Create df for all completed review admisions within 3 days
  three_day_completed_ur_review <- three_day_completed_ur_review %>% 
    select(-`Grand Total`) %>% 
    rename(Site = `Row Labels`) %>% 
    pivot_longer(!Site, names_to = "Month", values_to = "Value") %>%
    filter(Site != "Grand Total") %>% 
    filter(!is.na(Value)) %>%
    mutate(Site = toupper(trimws(Site))) %>%
    filter(!(Site %in% c("LOS", "REVIEW TYPE", "COUNT OF ACCOUNT NUMBER", "ROW LABELS"))) %>%
    filter(!is.na(as.numeric(Value))) %>%
    mutate(Metric = "Three_Day_Completed_UR_Review",
           Value = as.numeric(Value))
  
  
  ##Bind 3 tables together to calculate metrics
  df <- bind_rows(required_ur_review, completed_ur_review)
  df <- bind_rows(df, three_day_completed_ur_review)
  
  ##Calulate operational metrics
  df <- df %>% pivot_wider(names_from = "Metric", values_from = "Value") 
  df <- df %>% mutate(`Reviews Never Complete` = 1-(Completed_UR_Review/Required_UR_Review),
                      `Time to First Review` = Three_Day_Completed_UR_Review/Required_UR_Review,
                      `Review Completed on Time` = Three_Day_Completed_UR_Review/Completed_UR_Review) %>%
        select(Site, Month, `Reviews Never Complete`, `Time to First Review`, `Review Completed on Time`)
  
  ##Format data frame to standard summer repo format
  df <- df %>% pivot_longer(!c("Site", "Month"), names_to = "METRIC_NAME_SUBMITTED", values_to = "VALUE") %>%
      rename(SITE = Site,
             REPORTING_MONTH = Month) %>%
      mutate(REPORTING_MONTH = as.Date(paste0(REPORTING_MONTH, " 01"), format = "%B %Y %d"),
             PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH , "%b %Y"),
             UPDATED_USER = updated_user,
             SERVICE = "Case Management / Social Work")
  
  
  
  
  
}


#LOS Processing

# https://stackoverflow.com/questions/4806823/how-to-detect-the-right-encoding-for-read-csv
# file_path_los <- "Tests/CMSWRev/LOS Data for Balanced Scorecard.csv"
# enc <- guess_encoding(file_path_los, n_max = 1000)
# raw_file_los <- file(file_path_los, open="r", encoding=as.list(enc[1, ])$encoding)
# raw_los_data <- read.table(raw_file_los, sep='\t', dec=',', header=TRUE,colClasses = "character")
# close(raw_file_los)
# updated_user <- "TEST"


case_management_los_processing <- function(raw_los_data,updated_user){
  
  raw_los_data  <- raw_los_data %>%
    mutate(
      `Month.of.Discharge.Date` = paste("01", sep =  " ", `Month.of.Discharge.Date`),
      `Month.of.Discharge.Date` = as.Date(`Month.of.Discharge.Date`,format = "%d %B %Y"),
      `Avg.LOS` = as.numeric(`Avg.LOS`),
      `Total.Cases` = as.numeric(gsub(",", "",`Total.Cases`)),
      `Weighted LOS` = `Avg.LOS`*`Total.Cases`,
       YEAR_REP = year(`Month.of.Discharge.Date`) )
  
  raw_los_data_ytd  <- raw_los_data %>%
    group_by(YEAR_REP,Facility,`Month.of.Discharge.Date`) %>%
    arrange(Facility, `Month.of.Discharge.Date`) %>%
    summarise(SUM_WEIGHTED_LOS = sum(`Weighted LOS`),
              CUM_CASES = sum(`Total.Cases`))%>%
    mutate(SUM_WEIGHTED_LOS = cumsum(SUM_WEIGHTED_LOS),
           CUM_CASES = cumsum(CUM_CASES),
           "Average LOS (YTD)" = SUM_WEIGHTED_LOS/CUM_CASES)%>%
    select(Facility,  `Month.of.Discharge.Date`, "Average LOS (YTD)")
  
  los_data <- join(raw_los_data,raw_los_data_ytd,type ="left",by=c("Facility", "Month.of.Discharge.Date")) %>%
    select(-YEAR_REP) %>%
    rename(SITE = Facility,
           REPORTING_MONTH = `Month.of.Discharge.Date`,
           "Average LOS" = `Avg.LOS`) %>%
    mutate(PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH , "%b %Y"),
           SERVICE = "Case Management / Social Work",
           UPDATED_USER = updated_user) %>%
    select(SERVICE, SITE,REPORTING_MONTH,PREMIER_REPORTING_PERIOD,"Average LOS (YTD)" ,"Average LOS",UPDATED_USER) %>%
    pivot_longer(cols=c("Average LOS (YTD)" ,"Average LOS"),
                 names_to = "METRIC_NAME_SUBMITTED",
                 values_to = "VALUE")
  
}

#processed_los_data <- case_management_los_processing(raw_los_data, updated_user)

# Re-admission processing
# raw_readm_data <- read_excel("Tests/CMSWRev/Readmissions data (3).xlsx")

# raw_readm_data <- read_excel("Tests/Readmission Data for Balanced Scorecard.xlsx")


case_management_readmission_processing <- function(raw_readm_data, updated_user){
  raw_readm_data  <- raw_readm_data %>%
    mutate(
      `Month of DSCH_DT_SRC` = as.Date(paste0("01 ",`Month of DSCH_DT_SRC`),format = "%d %B %Y"),
      #`Month of DSCH_DT_SRC` = as.Date(`Month of DSCH_DT_SRC`,format = "%Y-%m-%d"),
      Readmissions = as.numeric(Readmissions),
      `Total Cases` = as.numeric(sub(",", "", `Total Cases`, fixed = TRUE)),
      "Readmission Rate" = Readmissions/`Total Cases`,
      YEAR_REP = year(`Month of DSCH_DT_SRC`))
  
  
  raw_readm_data_ytd  <- raw_readm_data %>%
    group_by(YEAR_REP,Facility,`Month of DSCH_DT_SRC`) %>%
    arrange(Facility, `Month of DSCH_DT_SRC`) %>%
    summarise(Readmissions = sum(Readmissions),
              `Total Cases` = sum(`Total Cases`))%>%
    mutate(Readmissions = cumsum(Readmissions),
           `Total Cases` = cumsum(`Total Cases`),
           "Readmission Rate (YTD)" = Readmissions/`Total Cases`)%>%
    select(Facility,  `Month of DSCH_DT_SRC`, "Readmission Rate (YTD)")
  
  
  readm_data <- join(raw_readm_data,raw_readm_data_ytd,type ="left") %>%
    select(-YEAR_REP) %>%
    rename(SITE = Facility,
           REPORTING_MONTH = `Month of DSCH_DT_SRC`) %>%
    mutate(PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH , "%b %Y"),
           SERVICE = "Case Management / Social Work",
           UPDATED_USER = updated_user) %>%
    select(SERVICE, SITE,REPORTING_MONTH,PREMIER_REPORTING_PERIOD,"Readmission Rate" ,"Readmission Rate (YTD)",UPDATED_USER) %>%
    pivot_longer(cols=c("Readmission Rate" ,"Readmission Rate (YTD)"),
                 names_to = "METRIC_NAME_SUBMITTED",
                 values_to = "VALUE")
  
  readm_data <- readm_data %>%
    mutate(SITE = case_when(SITE == "Mount Sinai Beth Israel" ~ "MSBI",
                            SITE == "Mount Sinai Brooklyn" ~ "MSB",
                            SITE == "Mount Sinai Hospital" ~ "MSH",
                            SITE == "Mount Sinai Queens" ~ "MSQ",
                            SITE == "Mount Sinai Morningside" ~ "MSM",
                            SITE == "Mount Sinai South Nassau" ~ "MSSN",
                            SITE == "Mount Sinai West" ~ "MSW",
                            TRUE ~ SITE))
  readm_data %>% drop_na()
  
  
}

# processed_readmission_data <- case_management_readmission_processing(raw_readm_data,"Test")

