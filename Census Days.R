# cost_and_revenue_repo <- read_excel(paste0(home_path, "Summary Repos/Food Services Cost and Revenue.xlsx"))

#data <- read_excel("C:/Users/villea04/Documents/MSHS Workforce Data Request_Food_RecurringRequest 2021.xlsx", sheet = "Rev Budget")


cost_budget_combine <- function(cost,rev){
  merge <- merge(cost,rev, by = c("Service", "Site", "Metric", "Month"), all.x = TRUE)
  
  merge
  
}

rev_budget_dept_summary <- function(data){
  data <- data %>% filter(!is.na(`REVENUE BUDGET`))
  site_index <- which(data$`REVENUE BUDGET` %in% c("MOUNT SINAI","MS MORNINGSIDE", "MS WEST", "MS BETH ISRAEL", "MS BROOKLYN", "MS QUEENS", "MS NYEE"))
  data <- data[site_index[1]:nrow(data),]
  
  
  data$Site <- data$`REVENUE BUDGET`
  ##Cnhange values other than site names to NA
  data$Site[which(!(data$Site %in% c("MOUNT SINAI","MS MORNINGSIDE", "MS WEST", "MS BETH ISRAEL", "MS BROOKLYN", "MS QUEENS", "MS NYEE")))] <- NA
  
  
  #for loop to fill in the NA cells with the site name
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
  
  
  data <- data[,1:14]
  
  
  data$Metric <- ifelse(data$Metric == "Retail Revenue", "Revenue from Retail", data$Metric)
  data$Metric <- ifelse(data$Metric == "Catering", "Revenue from Catering", data$Metric)
  data$Metric <- ifelse(data$Metric == "Total Revenue", "Revenue from R&C (Includes Foregone)", data$Metric)
  
  ##Delete columns with all 0s
  data <- data[rowSums(data[,3:14] != 0, na.rm = TRUE) > 0,]
  
  
  data <- data %>% pivot_longer(3:length(.),
                                names_to = "Month",
                                values_to = "Revenue Budget")
  
  ##Use previous month to get year for data
  data$Month <- as.Date(paste0(data$Month, "-01", "-",format(Sys.Date() %m-% months(1), "%Y")),"%b-%d-%Y")
  
  data$Service <- "Food Services"
  data <- data %>% relocate(Service, .before = Site)
  
  data <- data %>% filter(!is.na(`Revenue Budget`))
  
  data <- data %>% filter(Month <= as.Date(paste0(format(Sys.Date(),"%Y-%m"),"-01"), "%Y-%m-%d") %m-% months(1))
  
  data$Metric <- ifelse(data$Metric == "Catering", "Catering Revenue", data$Metric)
  
  data
}

census_days_dept_summary <- function(data){
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
                                values_to = "Census Days") %>%
    rename(Month = Date)
  data$Service <- "Food Services"
  data <- data %>% mutate_at(vars(c("Site")), ~ifelse(Site == "MSSL", "MSM", 
                                                      Site)
  )
  data
  
}



cost_and_revenue_dept_summary <- function(data){
  
  ## filter out na rows and delete the empty cells in the beginning of file
  data <- data %>% filter(!is.na(`Current Month`))
  site_index <- which(data$`Current Month` %in% c("MOUNT SINAI","MS MORNINGSIDE", "MS WEST", "MS BETH ISRAEL", "MS BROOKLYN", "MS QUEENS", "MS NYEE"))
  data <- data[site_index[1]:nrow(data),]
  
  
  data$Site <- data$`Current Month`
  ##Cnhange values other than site names to NA
  data$Site[which(!(data$Site %in% c("MOUNT SINAI","MS MORNINGSIDE", "MS WEST", "MS BETH ISRAEL", "MS BROOKLYN", "MS QUEENS", "MS NYEE")))] <- NA
  
  
  #for loop to fill in the NA cells with the site name
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
  
  data <- data[,1:14]
  ##Delete columns with all 0s
  data <- data[, colSums(data != 0, na.rm = TRUE) > 0]
  
  
  
  data <- data %>% pivot_longer(3:length(.),
                                names_to = "Month",
                                values_to = "Actual Revenue")
  
  ##Use previous month to get year for data
  data$Month <- as.Date(paste0(data$Month, "-01", "-",format(Sys.Date() %m-% months(1), "%Y")),"%b-%d-%Y")
  
  data$Service <- "Food Services"
  data <- data %>% relocate(Service, .before = Site)
  
  data <- data %>% filter(!is.na(`Actual Revenue`))
  
  data$Metric <- ifelse(data$Metric == "Non Labor Actual", "Non Labor Cost", data$Metric)
  data$Metric <- ifelse(data$Metric == "Salary Actual", "Labor Cost", data$Metric)
  
  cost <- data %>% filter(Metric == "Labor Cost" | Metric == "Non Labor Cost")
  data <- data %>% filter(!(Metric == "Labor Cost" | Metric == "Non Labor Cost"))
  
  cost$`Actual Revenue` <- as.numeric(cost$`Actual Revenue`)
  
  
  total <- cost %>% filter((Metric == "Non Labor Cost") | Metric == "Labor Cost") %>%
    group_by(Site,Month) %>%
    mutate(`Actual Revenue` = sum(`Actual Revenue`)) %>%
    mutate(Metric = "Total Cost") %>%
    distinct()
  
  combined <- rbind(data.frame(cost),data.frame(total),data.frame(data))
  
  
  combined$Metric <- ifelse(combined$Metric == "Retail Revenue", "Revenue from Retail", combined$Metric)
  combined$Metric <- ifelse(combined$Metric == "Catering Revenue", "Revenue from Catering", combined$Metric)
  
  
  r_c_revenue <- combined %>% filter(Metric == "Revenue from Retail" | Metric == "Revenue from Catering" | Metric == "Foregone Revenue")
  combined <- combined %>% filter(!(Metric == "Revenue from Retail" | Metric == "Revenue from Catering" | Metric == "Foregone Revenue"))
  
  r_c_revenue$`Actual.Revenue` <- as.numeric(r_c_revenue$`Actual.Revenue`)
  r_c_total <- r_c_revenue %>% filter((Metric == "Revenue from Retail" | Metric == "Revenue from Catering" | Metric == "Foregone Revenue")) %>%
    group_by(Site,Month) %>%
    mutate(`Actual.Revenue` = sum(`Actual.Revenue`)) %>%
    mutate(Metric = "Revenue from R&C (Includes Foregone)") %>%
    distinct()
  
  combined <- rbind(data.frame(r_c_revenue),data.frame(r_c_total),data.frame(combined))
  
  combined <- combined %>% rename(`Actual Revenue` = Actual.Revenue)
  
}

food_summary_repo_format <- function(data, updated_user) {
  flag <- 0
  raw_cost_rev_df <- data
  min_month <- min(raw_cost_rev_df$Month)
  max_month <- max(raw_cost_rev_df$Month)
  
  if(!("Census Days" %in% colnames(raw_cost_rev_df))){
    
    #summary_repo_data <- read_excel(paste0(home_path, "Summary Repos/Food Services Cost and Revenue.xlsx"))
    #summary_repo_data <- summary_repo_data %>% filter(Month >= min_month & Month <= max_month)
    #summary_repo_data <- summary_repo_data %>% select(-Metric, -Service, -`Actual Revenue`, -`Revenue Budget`)
    
    format <- "YYYY-MM-DD HH24:MI:SS"
    conn <- dbConnect(drv = odbc::odbc(),
                      dsn = dsn)
    summary_repo_data <- tbl(conn, "SUMMARY_REPO") %>% filter(SERVICE == "Food Services", REPORTING_MONTH >= TO_DATE(min_month, format), 
                                                                      REPORTING_MONTH <= TO_DATE(max_month, format)) %>% 
                                                    select(-SERVICE, -METRIC_NAME_SUBMITTED, -PREMIER_REPORTING_PERIOD, -UPDATED_USER, -VALUE, -UPDATED_TIME) %>%
                                                    rename(Site = SITE,
                                                           Month = REPORTING_MONTH) %>%
      collect()
    summary_repo_data <- summary_repo_data %>% distinct()
    
    if(nrow(summary_repo_data) != 0){
      raw_cost_rev_df <- left_join(raw_cost_rev_df,summary_repo_data)
    }else{
      raw_cost_rev_df$`Census Days` <- NA
    }
  } else {
    summary_repo_data <- read_excel(paste0(home_path, "Summary Repos/Food Services Cost and Revenue.xlsx"))
    summary_repo_data <- summary_repo_data %>% select(-`Census Days`)
    raw_cost_rev_df <- left_join(raw_cost_rev_df, summary_repo_data, by = c("Service" = "Service",
                                                                            "Month" = "Month",
                                                                            "Site" = "Site"))
    flag <- 1
    
  }
  
  raw_cost_rev_df$`Actual Revenue` <- as.numeric(raw_cost_rev_df$`Actual Revenue`)
  raw_cost_rev_df$`Revenue Budget` <- as.numeric(raw_cost_rev_df$`Revenue Budget`)
  #raw_cost_rev_df$`Census Days` <- as.numeric(raw_cost_rev_df$`Census Days`)
  
  # Cost and Revenue data pre-processing
  if (flag == 0) {
  cost_rev_df <- raw_cost_rev_df %>%
    mutate(
      `Actual Revenue` = as.numeric(`Actual Revenue`),
      #rev_per_census = ifelse(!is.na(`Census Days`), round(`Actual Revenue`/`Census Days`, 2), NA),
      budget_actual_var = as.numeric(ifelse(is.na(`Revenue Budget`), "", round(as.numeric(`Revenue Budget`) - as.numeric(`Actual Revenue`), 2)))) %>%
    #Target = ifelse(Metric == "Revenue from R&C (Includes Foregone)", round(budget_actual_var/`Revenue Budget`,2), ""),
    #Status = ifelse((is.na(Target) | Target == ""), "", ifelse(Target <= 0, "Green", ifelse(Target > 0.02, "Red", "Yellow")))) %>%
    pivot_longer(
      -c(Service, Site, Metric, Month),
      names_to = "Metric_Name_Submitted",
      values_to = "value") %>%
    mutate(
      Premier_Reporting_Period = format(as.Date(Month, format = "%Y-%m-%d"),"%b %Y"),
      Reporting_Month = format(as.Date(Month, format = "%Y-%m-%d"),"%m-%Y")) %>%
    rename(VALUE = value)
  } else {
    cost_rev_df <- raw_cost_rev_df %>%
      mutate(
        `Actual Revenue` = as.numeric(`Actual Revenue`),
        #rev_per_census = ifelse(!is.na(`Census Days`), round(`Actual Revenue`/`Census Days`, 2), NA),
        budget_actual_var = as.numeric(ifelse(is.na(`Revenue Budget`), "", round(as.numeric(`Revenue Budget`) - as.numeric(`Actual Revenue`), 2)))) %>%
      #Target = ifelse(Metric == "Revenue from R&C (Includes Foregone)", round(budget_actual_var/`Revenue Budget`,2), ""),
      #Status = ifelse((is.na(Target) | Target == ""), "", ifelse(Target <= 0, "Green", ifelse(Target > 0.02, "Red", "Yellow")))) %>%
      pivot_longer(
        6:9,
        names_to = "Metric_Name_Submitted",
        values_to = "value") %>%
      mutate(
        Premier_Reporting_Period = format(as.Date(Month, format = "%Y-%m-%d"),"%b %Y"),
        Reporting_Month = format(as.Date(Month, format = "%Y-%m-%d"),"%m-%Y")) %>%
      rename(value_rounded = value)
  }
  
  cost_rev_df_final <- left_join(cost_rev_df, cost_rev_mapping, 
                                 by = c("Metric", "Metric_Name_Submitted"))
  cost_rev_df_final <- cost_rev_df_final %>% filter(!(Service == "Food Services" & Metric_Group == "Cost per Census Day" & Site == "NYEE"))
  cost_rev_df_final <- cost_rev_df_final %>% filter(!(is.na(VALUE)))
  
  cost_rev_df_final <- cost_rev_df_final %>% select(-Metric_Group, -Metric_Name_Submitted, - Metric, -Reporting_Month) %>%
                         rename(METRIC_NAME_SUBMITTED = Metric_Name,
                                     REPORTING_MONTH = Month,
                                     SITE = Site,
                                     SERVICE = Service,
                                     PREMIER_REPORTING_PERIOD = Premier_Reporting_Period) %>% 
                            filter(!(is.na(METRIC_NAME_SUBMITTED))) %>% mutate(UPDATED_USER = updated_user) %>%
                            select(SERVICE, 
                                   SITE, 
                                   REPORTING_MONTH,
                                   PREMIER_REPORTING_PERIOD, 
                                   METRIC_NAME_SUBMITTED,
                                   VALUE,
                                   UPDATED_USER)

}
  