# start <- "J:" #Comment when publishing to RConnect
# # start <- "/SharedDrive"  #Uncomment when publishing to RConnect
# home_path <- paste0(start,"/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/")
# raw_TAT_EVS_df <- read_xlsx(paste0(home_path, "File Examples/EVS/MSHS Normal Clean vs Iso Clean TAT Dec.xlsx"))
# month <- excel_sheets(paste0(home_path, "File Examples/EVS/MSHS Normal Clean vs Iso Clean TAT Dec.xlsx"))[1]


# summary_repos_environmental <- read_excel(operational_metrics_environmental_path) %>%
#   mutate_if(is.logical, as.character)


evs_file_process <- function(data, month, updated_user) {
  data <- na.omit(data, na.action = "omit")
  
  data <- data %>%
    mutate(Hrs = ifelse(
      # First check to see if the letter h followed by a space is detected
      grepl("h\\s", TAT...4),
      # If hours are detected in the TAT, extract the characters before "h"
      # and convert to an integer
      as.integer(str_extract(TAT...4, "[0-9]*(?=h\\s)")),
      # Otherwise, set hours to 0
      0), 
      Mins = # Extract the numbers preceeding "m" and convert to integer
        as.integer(
          str_extract(TAT...4, "[0-9]+(?=m)")
        ),
      TATMin = Hrs*60 + Mins) %>%
    select(-Hrs, -Mins, -TAT...4) %>%
    rename(`Non-Isolation Average TAT` = TATMin)
  
  data <- data %>%
    mutate(Hrs = ifelse(
      # First check to see if the letter h followed by a space is detected
      grepl("h\\s", TAT...7),
      # If hours are detected in the TAT, extract the characters before "h"
      # and convert to an integer
      as.integer(str_extract(TAT...7, "[0-9]*(?=h\\s)")),
      # Otherwise, set hours to 0
      0), 
      Mins = # Extract the numbers preceeding "m" and convert to integer
        as.integer(
          str_extract(TAT...7, "[0-9]+(?=m)")
        ),
      TATMin = Hrs*60 + Mins) %>%
    select(-Hrs, -Mins, -TAT...7) %>%
    rename(`Isolation Average TAT` = TATMin)
  
  
  
  data <- data %>%
    rename(`Non-Isolation Requests` = `Normal Requests`,
           `Non-Isolation  % > 90 mins` = `% > 90 mins...3`,
           `Isolation % > 90 mins` = `% > 90 mins...6`)
  
  data$Month <- format(as.Date(paste(month, "01"), "%b %Y %d"), "%m/%d/%Y")
  data$Site <- ifelse(data$Hospital == "Mount Sinai Bi Brooklyn", "MSB",
                      ifelse(data$Hospital == "Mount Sinai Bi Petrie", "MSBI",
                             ifelse(data$Hospital == "Mount Sinai Queens Hospital", "MSQ",
                                    ifelse(data$Hospital == "Mount Sinai St. Luke's", "MSM",
                                           ifelse(data$Hospital == "Mount Sinai Morningside", "MSM",
                                              ifelse(data$Hospital == "Mount Sinai West", "MSW",
                                                  ifelse(data$Hospital == "The Mount Sinai Hospital", "MSH", NA)))))))
  data$Service <- "Environmental Services"
  
  data <- data %>% 
    relocate(Service, .before = Hospital) %>%
    relocate(Site, .after = Hospital) %>%
    relocate(Month, .after = Site) %>%  
    select(-Hospital) %>%
    mutate(`% Isolation Turns` = `Isolation Requests` / (`Isolation Requests` + 
                                                           `Non-Isolation Requests`),
           `% Non-Isolation Turns` = `Non-Isolation Requests` / (`Isolation Requests` + 
                                                                   `Non-Isolation Requests`)
    ) %>%
    pivot_longer(cols = c(-Service,-Site,-Month),
                 names_to = "METRIC_NAME_SUBMITTED",
                 values_to = "VALUE") %>%
    rename(SERVICE = Service,
           SITE = Site,
           REPORTING_MONTH = Month) %>%
    mutate(PREMIER_REPORTING_PERIOD = format(as.Date(REPORTING_MONTH,"%m/%d/%Y"),"%b %Y"),
           REPORTING_MONTH = format(as.Date(REPORTING_MONTH,"%m/%d/%Y"),"%Y-%m-%d"),
           UPDATED_USER = updated_user,
           REPORTING_MONTH = as.Date(REPORTING_MONTH))
  

  return(data)
}


# data <- evs_file_process(raw_TAT_EVS_df,month)
