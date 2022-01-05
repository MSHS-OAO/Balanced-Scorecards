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
  
  
  data <- data %>%
            filter(format(Date, format = "%Y") >= "2021")
  
}


data_raw_cost <- read_excel(paste0(home_path, "Input Data Raw/Food/MSHS Workforce Data Request_Food_RecurringRequest 2021_Oct21.xlsx"), sheet = "Cost and Revenue")

cost_and_revenue_file_process <- function(data){
  data <- data %>% filter(!is.na(`Current Month`))
  site_index <- which(data$`Current Month` %in% c("MOUNT SINAI","MS MORNINGSIDE", "MS WEST", "MS BETH ISRAEL", "MS BROOKLYN", "MS QUEENS", "MS NYEE"))
  data <- data[site_index[1]:nrow(data),]
  
  data$Site <- data$`Current Month`
  
  data$Site[which(!(data$Site %in% c("MOUNT SINAI","MS MORNINGSIDE", "MS WEST", "MS BETH ISRAEL", "MS BROOKLYN", "MS QUEENS", "MS NYEE")))] <- NA
  
  site_index <- which(data$`Site` %in% c("MOUNT SINAI","MS MORNINGSIDE", "MS WEST", "MS BETH ISRAEL", "MS BROOKLYN", "MS QUEENS", "MS NYEE"))
  
  for(i in site_index){
    data[i:(i+5),c("Site")] <- data[i,c("Site")]
  }
 
  data <- data %>% 
    row_to_names(row_number = 1) 
  
  data <- data[, colSums(is.na(data)) != nrow(data)]
  
  data <- data %>% rename(Metric = 1)
  data <- data %>% rename(Site = length(.)) 
  
  site_index <- which(data$`Metric` %in% c("MOUNT SINAI","MS MORNINGSIDE", "MS WEST", "MS BETH ISRAEL", "MS BROOKLYN", "MS QUEENS", "MS NYEE"))
  data <- data[-site_index,]
}
##### Testing to read in Cost and Revenue from Summary Repos
census_days <- census_days_file_process(data_raw)
cost_and_revenue_repo <- read_excel(paste0(home_path, "Summary Repos/Cost and Revenue.xlsx"))
