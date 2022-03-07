### Process Productivity Data 

productivity_process <- function(data, data_nursing_radiology){
  # Import productivity data for depts excluding Nursing and Radiology 
  raw_prod_df <- data
  #raw_prod_df <- read_excel("Data/Dept Report Builder.xlsx")
  raw_prod_df <- raw_prod_df %>% select(!`Entity Time Period Desc`)
  
  ## Productivity data pre-processing
  ### Aggregate reporting period columns
  prod_df <- raw_prod_df %>% 
    mutate(id = seq_len(n())) %>% 
    melt(id.var = c('Corporation Code','Corporation Name', 'Entity Code', 'Entity', 'Department Reporting Definition ID',
                    'Department Reporting Definition Name', 'Key Volume', 'Mapped Facility Cost Centers', 'id'), na.rm = F) %>% 
    select(-c('id'))
  
  
  ### Process Reporting Period to Reporting Month
  prod_df$variable <- gsub("\\..*","", prod_df$variable) # Remove underscores at end of reporting period
  names(prod_df)[names(prod_df) == 'variable'] <- 'Premier_Reporting_Period' # Rename Reporting Period column
  prod_df <- prod_df %>%
    mutate(Reporting_Month_Start = as.Date(dttm(gsub(" .*$", "", Premier_Reporting_Period))))
  
  prod_df_final <- prod_df %>% # Fill in Metric name in a column
    mutate(Metrics = as.numeric(value),
           Metrics = ifelse(is.na(Metrics), value, ""),
           Metrics = ifelse(Metrics == "", NA, Metrics)) %>%
    fill(Metrics) %>%
    filter(!is.na(`Corporation Code`)) %>%
    unique()
  
  
  # Import productivity data fo Nursing and Radiology 
  raw_prod_nursing_rad_df <- data_nursing_radiology
  #raw_prod_nursing_rad_df <- read_excel("Data/Dept Report Builder_nursing_radiology.xlsx")
  raw_prod_nursing_rad_df <- raw_prod_nursing_rad_df %>% select(!`Entity Time Period Desc`)
  
  
  ## Productivity data pre-processing
  ### Aggregate reporting period columns
  prod_nursing_rad_df <- raw_prod_nursing_rad_df %>% 
    mutate(id = seq_len(n())) %>% 
    melt(id.var = c('Corporation Code','Corporation Name', 'Entity Code', 'Entity', 'Department Reporting Definition ID',
                    'Department Reporting Definition Name', 'Key Volume', 'Mapped Facility Cost Centers', 'id'), na.rm = F) %>% 
    select(-c('id'))
  
  
  ### Process Reporting Period to Reporting Month
  prod_nursing_rad_df$variable <- gsub("\\..*","", prod_nursing_rad_df$variable) # Remove underscores at end of reporting period
  names(prod_nursing_rad_df)[names(prod_nursing_rad_df) == 'variable'] <- 'Premier_Reporting_Period' # Rename Reporting Period column
  prod_nursing_rad_df <- prod_nursing_rad_df %>%
    mutate(Reporting_Month_Start = as.Date(dttm(gsub(" .*$", "", Premier_Reporting_Period))))
  
  prod_nursing_rad_df_final <- prod_nursing_rad_df %>% # Fill in Metric name in a column
    mutate(Metrics = as.numeric(value),
           Metrics = ifelse(is.na(Metrics), value, ""),
           Metrics = ifelse(Metrics == "", NA, Metrics)) %>%
    fill(Metrics) %>%
    filter(!is.na(`Corporation Code`)) %>%
    unique()
  
  
  # Merge two data imports and exclude duplicates 
  prod_df_merged <- rbind(prod_df_final, prod_nursing_rad_df_final)
  prod_df_merged <- prod_df_merged %>%
    unique()
  
  
  ## Map Site and Service Group
  prod_df_all <- merge(prod_df_merged, site_mapping[,c("DEFINITION.CODE","KEY.VOLUME","SITE", "Balanced.Scorecards.Groupings")], 
                       by.x = c("Department Reporting Definition ID", "Key Volume"),
                       by.y = c("DEFINITION.CODE", "KEY.VOLUME"), all.x = TRUE)
  
  names(prod_df_all)[names(prod_df_all) == "Balanced.Scorecards.Groupings"] <- "Service"
  names(prod_df_all)[names(prod_df_all) == "SITE"] <- "Site"
  
  
  prod_df_all <- prod_df_all %>% # Process Metric Group column
    filter(!is.na(Service)) %>%
    mutate(Metric_Name = trim(gsub("-.*","", Metrics)),
           Metric_Group = ifelse(grepl("Overtime", Metric_Name), "Overtime Hours", "Productivity"),
           value_rounded = round(parse_number(value),2))
  
  ### Map Premier Reporting Period -> Dashboard Month 
  report_date_mapping <- report_date_mapping %>%
    mutate(`Report Data Updated until` = format(as.Date(`Report Data Updated until`, "%m/%d/%Y"), "%m/%d/%Y"),
           `Report Release Date` = format(as.Date(`Report Release Date`, "%m/%d/%Y"), "%m/%d/%Y"),
           `Dashboard Month` = format(as.Date(`Dashboard Month`, "%m/%d/%Y"), "%m/%d/%Y"))
  
  report_data <- as.data.frame(report_date_mapping$`Report Data Updated until`)
  report_data <- col_concat(report_data, sep = "|")
  report_data <- paste(report_data, collapse = "|")
  
  prod_df_all <- prod_df_all %>% filter(grepl(report_data, Premier_Reporting_Period))
  prod_df_all <- prod_df_all %>% separate(Premier_Reporting_Period, c("Report_Start","Report_End"), sep = " - ", remove = FALSE)
  prod_df_all$Reporting_Month_Ref <- report_date_mapping$`Dashboard Month`[match(prod_df_all$`Report_End`,report_date_mapping$`Report Data Updated until`)]
  prod_df_all$Reporting_Month_Ref <- as.Date(prod_df_all$Reporting_Month_Ref, format = "%m/%d/%Y")
  
  prod_df_all <- prod_df_all[, !(names(prod_df_all) %in% c("Report_Start","Report_End"))]
  prod_df_all <- prod_df_all %>% unique()
  
  
  ## Calculate aggregate Overtime % and Productivity Index for Nursing and Imaging
  prod_df_aggregate <- prod_df_all %>%
    group_by(Service, Site, Metric_Group, Metric_Name, Premier_Reporting_Period, Reporting_Month_Ref) %>%
    summarise(value_rounded = round(mean(value_rounded, na.rm = TRUE),2)) %>%
    mutate(Metric_Name = str_trim(Metric_Name)) %>%
    filter(Metric_Name != "Total Target Worked FTE") %>%
    filter(!(Service %in% c("Nursing","Imaging") & 
               Metric_Name %in% c("Overtime Percent of Paid Hours","Worked Hours Productivity Index")))
  
  
  nursing_rad_metric_calc <- prod_df_all %>% # Calculate Productivity and Overtime % separately 
    filter(Service %in% c("Nursing","Imaging")) %>%
    mutate(Metric_Name = str_trim(Metric_Name)) %>%
    filter(Metric_Name %in% c("Total Target Worked FTE","Actual Worked FTE",
                              "Overtime Hours", "Total Paid hours")) %>%
    group_by(Service, Site, Metric_Name, Premier_Reporting_Period, Reporting_Month_Ref) %>%
    summarise(value_rounded = round(sum(value_rounded, na.rm = TRUE),2)) %>%
    pivot_wider(names_from = Metric_Name,
                values_from = value_rounded) %>%
    mutate(`Worked Hours Productivity Index` = round(`Total Target Worked FTE`/`Actual Worked FTE`,2),
           `Overtime Percent of Paid Hours` = round(`Overtime Hours`/`Total Paid hours`,2)) %>%
    pivot_longer(5:length(.),
                 names_to = "Metric_Name",
                 values_to = "value_rounded") %>%
    filter(Metric_Name %in% c("Worked Hours Productivity Index","Overtime Percent of Paid Hours")) %>%
    mutate_at(vars(value_rounded), ~replace(., is.nan(.), 0)) %>%
    mutate(Metric_Group = ifelse(Metric_Name == "Worked Hours Productivity Index", "Productivity", "Overtime Hours"))
  
  
  prod_df_aggregate_all <- bind_rows(prod_df_aggregate, nursing_rad_metric_calc) # Merge newly calculated productivity index and overtime % for nursing and radiology 
  
  prod_df_aggregate_all$Metric_Name <- str_trim(prod_df_aggregate_all$Metric_Name)
  
  prod_df_aggregate_all$Metric_Name <- metric_group_mapping$Metric_Name[match(prod_df_aggregate_all$Metric_Name, 
                                                                              metric_group_mapping$Metric_Name_Submitted)] # Map final Metric_Name
  
  
  ### Create Target Variance Column
  prod_target_status <- merge(prod_df_aggregate_all[, c("Service","Site","Metric_Name","Reporting_Month_Ref","value_rounded")],
                              target_mapping, by = c("Service","Site","Metric_Name"))
  
  prod_target_status <- prod_target_status %>%
    mutate(Variance = between(value_rounded, Range_1, Range_2)) %>% # Target mapping
    filter(!(Variance %in% FALSE))
  
  prod_df_final <- merge(prod_df_aggregate_all, prod_target_status[,c("Service","Site","Metric_Name","Reporting_Month_Ref","Target","Status")],
                         all = TRUE)
  
  prod_df_final$Reporting_Month <- format(as.Date(prod_df_final$Reporting_Month_Ref, format = "%Y-%m-%d"),"%m-%Y")
  prod_df_final$Reporting_Month_Ref <- NULL
  
  prod_df_final$Premier_Reporting_Period <- sub(".*- ", "", prod_df_final$Premier_Reporting_Period)
  
  # Subset processed data for merge 
  prod_df_merge <- prod_df_final[,processed_df_cols] 
  
  prod_df_merge$Reporting_Month_Ref <- as.Date(paste('01', as.yearmon(prod_df_merge$Reporting_Month, "%m-%Y")), format='%d %b %Y')
  
  metrics_final_df <- full_join(metrics_final_df,prod_df_merge)
}
