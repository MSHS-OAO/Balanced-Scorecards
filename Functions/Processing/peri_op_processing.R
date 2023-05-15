peri_op_processing <- function(datapath, updated_user) {
  
  sheets <- excel_sheets(datapath)
  monthly_data <- sheets[!grepl("Details", sheets)]
  counter <- 0
  for (i in 1:length(monthly_data)) {
    print(i)
    raw_data <- read_excel(datapath, sheet = monthly_data[i])
    ###Look for pattern and extarxt date from column title
    date_extract <- colnames(raw_data)[1]
    date <- sub(".*First Case On Time Starts - ", "", date_extract)
    date <- as.Date(paste0(date, "-01"), format = "%b %Y-%d")
    
    ###Look for string turnover to see where to split tables
    split_index <- grep("Turnover", colnames(raw_data))
    
    ##Split tables by index
    turnover_table_raw <- raw_data[,split_index:length(raw_data)]
    case_table_raw <- raw_data[,1:(split_index-2)]
    
    ##Process turnover table 
    row_to_column_name <- which(turnover_table_raw == "Site") ## Which row contains site to make column name 
    turnover_table_raw <- turnover_table_raw %>% row_to_names(row_number = row_to_column_name) %>% filter(!is.na(Site)) %>%
      rename(SITE = Site,
             VALUE = `Average Turnover (min)`) %>%
      mutate(METRIC_NAME_SUBMITTED = "Average Turnover (min)")
    
    ##Process Case Table
    row_to_column_name <- which(case_table_raw == "Site/ Service")
    case_table_raw <- case_table_raw %>% row_to_names(row_number = row_to_column_name)
    ##Figure out which rows contain the summarized metric
    case_table_raw <- case_table_raw %>% filter(`Site/ Service` %in% c("OR MSH", "OR MSQ", "OR MSM", "OR MSW", "OR MSBI", "MSB OR")) %>%
      select(`Site/ Service`, `On Time Start %`) %>%
      rename(SITE = `Site/ Service`,
             VALUE = `On Time Start %`) %>%
      mutate(METRIC_NAME_SUBMITTED = "On Time Start %")
    
    ##Merge both tables
    df <- rbind(turnover_table_raw, case_table_raw)
    
    df <- df %>% mutate(SITE = gsub("OR", "", SITE)) %>%
      mutate(across(where(is.character), str_trim)) %>%
      mutate(SERVICE = "Peri-Op",
             REPORTING_MONTH = date,
             PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
             UPDATED_USER = updated_user) %>%
      select(SERVICE, SITE, REPORTING_MONTH, METRIC_NAME_SUBMITTED, VALUE, UPDATED_USER, PREMIER_REPORTING_PERIOD)
    
    
    if (counter > 0) {
      df_final <- rbind(df, df_final)
    } else {
      df_final <- df
    }
    counter <- counter + 1 
  }
  
  df_final <- df_final %>% filter(SITE %in% c("MSB", "MSBI", "MSH", "MSM", "MSQ", "MSW", "NYEE"))
  
  
}

