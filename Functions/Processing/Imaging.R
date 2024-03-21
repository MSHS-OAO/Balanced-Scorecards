# data <- read_excel("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/File for Testing 012323/Imaging/IR/FTI-BalancedScorecard-2023-Jan1- 2022-Dec.xlsx")
# 
# imaging_repo <- read_excel(paste0(home_path, "Summary Repos/Imaging-IR.xlsx"))

imaging_dept_summary <- function(data, updated_user){
  
  data <- data %>% row_to_names(row_number = 1)
  data <- subset(data, select = -c(Measurement))

  #Figure out where MSW data begins
  site_index <- which(data$Category== "MSW Interventional Radiology Dashboard")
  
  ytd_index <- length(data)
  
  months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  year <- year(Sys.Date() %m-% months(1))
  
  months <- sprintf(paste0("%s-",year), months)
  
  ## Get the number of months in the data
  num_months <- ytd_index - 3
  colnames(data)[3:(ytd_index-1)] <- months[1:num_months]
  colnames(data)[ytd_index] <- paste0("YTD ", year)
  
  delete_rows <- which(data$Description == "Description")
  
  data <- data[-c(delete_rows),]
  
  delete_rows <- which(data$Description == "8:30A-5P, M,T,R,F and 9:30A-5P W")
  
  if(length(delete_rows != 0 )){
    data <- data[-c(delete_rows),]
  }
  
  data <- filter(data, rowSums(is.na(data)) != ncol(data))
  
  site_index <- which(data$Category== "MSW Interventional Radiology Dashboard")
  data$Site <- ""
  data[1:(site_index-1),length(data)] <- "MSH"
  data[site_index:nrow(data),length(data)] <- "MSW"
  
  data <- data[-c(site_index),]
  
  data <- data %>% fill(Category)
  
  data$Service <- "Imaging"
  
  data <- data %>% relocate(Site, .before = Category) %>%
                    relocate(Service, .before = (Site))
  
  data <- data[,-c(length(data))]
  
  data <- data %>%
            pivot_longer(cols = -c(Service, Site, Category, Description),
                         names_to = "Reporting_Month",
                         values_to = "value_rounded")
  
  data$Reporting_Month_Ref <- as.Date(paste0(data$Reporting_Month, "-01"), format = "%m-%Y-%d")
  
  data <- data %>% rename(Metric_Name_Submitted = Description)
  
  data$value_rounded <- round(data$value_rounded, digits = 4)
  
  budget_metrics <- c("Ambulatory Budgeted Revenue","Ambulatory Budgeted Volume")
  data_filter <- data %>% filter(!(Metric_Name_Submitted %in% budget_metrics)) %>% filter(!is.na(value_rounded))
  data_max_month <- max(data_filter$Reporting_Month_Ref)
  
  data <- data %>% filter(Reporting_Month_Ref <= data_max_month) %>%
    rename(SITE = Site,
           SERVICE = Service,
           METRIC_NAME_SUBMITTED = Metric_Name_Submitted,
           VALUE = value_rounded) %>%
    mutate(REPORTING_MONTH = as.Date(format(Reporting_Month_Ref,"%Y-%m-%d"), "%Y-%m-%d"),
           PREMIER_REPORTING_PERIOD = format(Reporting_Month_Ref,"%b %Y"),
           UPDATED_USER = updated_user,
           METRIC_NAME_SUBMITTED = ifelse(METRIC_NAME_SUBMITTED == 'Prime Time Room Utilization ESTIMATE All Rooms (%)',
                                          'Prime Time Room Utilization ESTIMATE All Rooms (%) 8:30A-5:00P, M-F',
                                          METRIC_NAME_SUBMITTED)) %>%
    select(SERVICE, 
           SITE, 
           REPORTING_MONTH,
           PREMIER_REPORTING_PERIOD, 
           METRIC_NAME_SUBMITTED,
           VALUE,
           UPDATED_USER)
  
  current_month <- as.Date(paste0(format(Sys.Date(), "%Y-%m"), "-01"), "%Y-%m-%d")

  data <- data %>% filter(REPORTING_MONTH < current_month) %>%
    mutate(VALUE = format(VALUE, scientific = FALSE)) %>% mutate(VALUE = trimws(VALUE)) %>% filter(VALUE != 'NA')
  data
}
