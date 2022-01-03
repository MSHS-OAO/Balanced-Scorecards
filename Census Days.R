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
  
  data$Date <- as.Date(paste0(data$Date,"-01"), format = "%B %Y-%d")
  
  data$Date <- format(data$Date, "%Y-%m-%d")
  
  data
  
}


##### Testing to read in Cost and Revenue from Summary Repos
census_days <- census_days_file_process(data_raw)
cost_and_revenue_repo <- read_excel(paste0(home_path, "Summary Repos/Cost and Revenue.xlsx"))
