peri_op_processing <- function(file_path, updated_user) {
  
  # sheets <- excel_sheets(datapath)
  # monthly_data <- sheets[!grepl("Details", sheets)]
  # counter <- 0
  # for (i in 1:length(monthly_data)) {
  #   print(i)
  #   raw_data <- read_excel(datapath, sheet = monthly_data[i])
  #   
  #   count_log_index <- which(raw_data[,1] == "Count of Log #")
  #   if(count_log_index != 2) {
  #     raw_data <- raw_data[-c(1:(count_log_index-2)),]
  #   }
  #   ###Look for pattern and extract date from column title
  #   date_extract <- colnames(raw_data)[1]
  #   date <- sub(".*First Case On Time Starts - ", "", date_extract)
  #   date <- as.Date(paste0(date, "-01"), format = "%b %Y-%d")
  #   
  #   ###Look for string turnover to see where to split tables
  #   split_index <- grep("Turnover", colnames(raw_data))
  #   
  #   ##Split tables by index
  #   turnover_table_raw <- raw_data[,split_index:length(raw_data)]
  #   case_table_raw <- raw_data[,1:(split_index-2)]
  #   
  #   ##Process turnover table 
  #   row_to_column_name <- which(turnover_table_raw == "Site") ## Which row contains site to make column name 
  #   turnover_table_raw <- turnover_table_raw %>% row_to_names(row_number = row_to_column_name) %>% filter(!is.na(Site)) %>%
  #     rename(SITE = Site,
  #            VALUE = `Average Turnover (min)`) %>%
  #     mutate(METRIC_NAME_SUBMITTED = "Average Turnover (min)")
  #   
  #   ##Process Case Table
  #   row_to_column_name <- which(case_table_raw == "Site / Service" | case_table_raw == "Site /Service" |
  #                                 case_table_raw == "Site/Service" | case_table_raw == "Site/ Service")
  # 
  #   case_table_raw <- case_table_raw %>% row_to_names(row_number = row_to_column_name)
  #   site_column_name <- colnames(case_table_raw)[1]
  #   ##Figure out which rows contain the summarized metric
  #   case_table_raw <- case_table_raw %>% filter(!!sym(site_column_name) %in% c("OR MSH", "OR MSQ", "OR MSM", "OR MSW", "OR MSBI", "MSB OR")) %>%
  #     select(!!sym(site_column_name), `On Time Start %`) %>%
  #     rename(SITE = !!sym(site_column_name),
  #            VALUE = `On Time Start %`) %>%
  #     mutate(METRIC_NAME_SUBMITTED = "On Time Start %")
  #   
  #   ##Merge both tables
  #   df <- rbind(turnover_table_raw, case_table_raw)
  #   
  #   df <- df %>% mutate(SITE = gsub("OR", "", SITE)) %>%
  #     mutate(across(where(is.character), str_trim)) %>%
  #     mutate(SERVICE = "Perioperative Services",
  #            REPORTING_MONTH = date,
  #            PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
  #            UPDATED_USER = updated_user,
  #            VALUE = as.numeric(VALUE)) %>%
  #     select(SERVICE, SITE, REPORTING_MONTH, METRIC_NAME_SUBMITTED, VALUE, UPDATED_USER, PREMIER_REPORTING_PERIOD)
  #   
  #   
  #   if (counter > 0) {
  #     df_final <- rbind(df, df_final)
  #   } else {
  #     df_final <- df
  #   }
  #   counter <- counter + 1 
  # }
  # 
  # df_final <- df_final %>% filter(SITE %in% c("MSB", "MSBI", "MSH", "MSM", "MSQ", "MSW", "NYEE"))
  
  
  data <- read_excel(file_path, col_types = c('text', 'text', 'text', 'numeric', 'numeric', 'numeric', 'numeric')) %>%
    mutate(REPORTING_MONTH = as.Date(paste0(Year, "-", Month, "-01"), format = '%Y-%B-%d'),
           PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
           SERVICE = "Perioperative Services") %>%
    select(-Month, -Year)
  
  
  site_mapping <- tibble(SITE=c("MOUNT SINAI HOSPITAL", 
                                "MS BETH ISREAL",
                                "MS BROOKLYN",
                                "MS MORNINGSIDE",
                                "MS QUEENS",
                                "MS WEST"), 
                         site_name_process=c("MSH","MSBI", "MSB", "MSM", "MSQ", "MSW"))
  
  data <- left_join(data, site_mapping) %>% filter(!is.na(site_name_process)) %>% select(-SITE) %>% 
    rename(SITE = site_name_process)
  data <- pivot_longer(data, cols = c(`TAT MTD`, `TAT YTD`, `FCOT MTD`, `FCOT YTD`), names_to = "raw_metric", values_to = "VALUE")
  
  metric_mapping <- tibble(raw_metric = c("TAT MTD", "TAT YTD", "FCOT MTD", "FCOT YTD"),
                           METRIC_NAME_SUBMITTED = c("Average Turnover (min)", "Average Turnover (min) (FYTD)", "On Time Start %", "On Time Start % (FYTD)"))
  
  data <- left_join(data, metric_mapping) %>% select(-raw_metric) %>% filter(!is.na(METRIC_NAME_SUBMITTED)) %>% mutate(UPDATED_USER = updated_user) %>%
    select(SERVICE, SITE, REPORTING_MONTH, METRIC_NAME_SUBMITTED, VALUE, UPDATED_USER, PREMIER_REPORTING_PERIOD)
  
  
}

