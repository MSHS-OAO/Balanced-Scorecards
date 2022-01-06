start <- "J:" #Comment when publishing to RConnect
# start <- "/SharedDrive"  #Uncomment when publishing to RConnect
home_path <- paste0(start,"/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/")

data_raw <- read_excel(paste0(home_path, "Input Data Raw/Food/Monthly Stats Summary for benchmarking 20211013.xlsx"))

census_days_file_process <- function(data){
  start_index <- which(colnames(data) == "Census Days") 
  end_index <- which(colnames(data) == "Nursery Days")-3 
  data <- data[,c(1,start_index:end_index)] #only get census days data
  data <- data %>%
            row_to_names(row_number = 1) 
  
  names(data)[1] <- "Date" #rename NA columns to Date 
  
  data <- data %>%
            filter(!is.na(Date))
  
  ###remove rows of all 0
  
  i <- c(2:length(data))
  
  data[ , i] <- apply(data[ , i], 2,            # Specify own function within apply
                      function(x) as.numeric(as.character(x)))
  data <- data[rowSums(data[,2:length(data)])>0,]
  
  data$Date <- gsub(",","",as.character(data$Date))

  data$Date <- as.Date(paste0(data$Date,"-01"), format = "%B %Y-%d")
  
  
  data <- data %>% filter(!is.na(Date))
  
  data <- data %>%
            filter(Date >= max(Date) %m-% months(3))
  
  data <- data %>% pivot_longer(2:length(.),
                       names_to = "Site",
                       values_to = "Census Days")
  
}



cost_and_revenue_file_process <- function(data){
  
  ## filter out na rows and delete the empty cells in the beginning of file
  data <- data %>% filter(!is.na(`Current Month`))
  site_index <- which(data$`Current Month` %in% c("MOUNT SINAI","MS MORNINGSIDE", "MS WEST", "MS BETH ISRAEL", "MS BROOKLYN", "MS QUEENS", "MS NYEE"))
  data <- data[site_index[1]:nrow(data),]
  
  
  data$Site <- data$`Current Month`
  ##Cnhange values other than site names to NA
  data$Site[which(!(data$Site %in% c("MOUNT SINAI","MS MORNINGSIDE", "MS WEST", "MS BETH ISRAEL", "MS BROOKLYN", "MS QUEENS", "MS NYEE")))] <- NA
  
  
  #foor loop to fill in the NA cells with the site name
  site_index <- which(data$`Site` %in% c("MOUNT SINAI","MS MORNINGSIDE", "MS WEST", "MS BETH ISRAEL", "MS BROOKLYN", "MS QUEENS", "MS NYEE"))
  for(i in site_index){
    data[i:(i+5),c("Site")] <- data[i,c("Site")]
  }
 
  data <- data %>% 
    row_to_names(row_number = 1) 
  
  ###Delete columns full of NA only
  data <- data[, colSums(is.na(data)) != nrow(data)]
  
  data <- data %>% rename(Metric = 1)
  data <- data %>% rename(Site = length(.)) 
  
  ##Delete rows that contain the site neams and months
  site_index <- which(data$`Metric` %in% c("MOUNT SINAI","MS MORNINGSIDE", "MS WEST", "MS BETH ISRAEL", "MS BROOKLYN", "MS QUEENS", "MS NYEE"))
  data <- data[-site_index,]
  
  
  ##Chnaage site to abbreviation 
  data$Site <- ifelse(data$Site == "MS BROOKLYN", "MSB",
                      ifelse(data$Site == "MS BETH ISRAEL", "MSBI",
                             ifelse(data$Site == "MS QUEENS", "MSQ",
                                    ifelse(data$Site == "MS MORNINGSIDE", "MSM",
                                                ifelse(data$Site == "MS WEST", "MSW",
                                                       ifelse(data$Site == "MS NYEE", "NYEE",
                                                        ifelse(data$Site == "MOUNT SINAI", "MSH", NA)))))))
  
  data <- data %>% relocate(Site, .before = Metric)
  
  data <- data[ , !(names(data) %in% c("YTD"))]
  
  ##Delete columns with all 0s
  data <- data[, colSums(data != 0) > 0]
  
  
  data <- data %>% pivot_longer(3:length(.),
                                names_to = "Month",
                                values_to = "Actual Revenue")
  
  ##Use previous month to get year for data
  data$Month <- as.Date(paste0(data$Month, "-01", "-",format(Sys.Date() %m-% months(1), "%Y")),"%b-%d-%Y")
  
  data$Service <- "Food Services"
  data <- data %>% relocate(Service, .before = Site)
  

}


##### Testing to read in Cost and Revenue from Summary Repos
census_days <- census_days_file_process(data_raw)
data_raw_cost <- read_excel(paste0(home_path, "Input Data Raw/Food/MSHS Workforce Data Request_Food_RecurringRequest 2021_Oct21.xlsx"), sheet = "Cost and Revenue")
cost_and_revenue_repo <- read_excel(paste0(home_path, "Summary Repos/Cost and Revenue.xlsx"))
