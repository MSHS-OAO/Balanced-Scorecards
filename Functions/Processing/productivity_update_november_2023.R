# datapath <- "tests/Scorecards Reporting Period Average.xlsx"
# datapath_old <- "tests/DeptReportBuilderRPAVG.xlsx"
# raw_data <- read_excel(datapath,skip = 2)
# raw_data_old <- read_excel(datapath_old)
# updated_user <- "Test_DNU"
productivity_processing <- function(raw_data, updated_user) {
  key_vol_mapping <- key_vol_mapping_oracle %>% mutate(Service = ifelse(grepl("Radiology", CORPORATE_SERVICE_LINE), "Radiology",
                                                                 ifelse(grepl("Biomed", CORPORATE_SERVICE_LINE), "Biomed / Clinical Engineering",
                                                                        ifelse(CORPORATE_SERVICE_LINE == "Support Services - Engineering", "Engineering",
                                                                               ifelse(CORPORATE_SERVICE_LINE == "Support Services - Environmental Services", "Environmental Services",
                                                                                      ifelse(CORPORATE_SERVICE_LINE == "Support Services - Food Services", "Food Services",
                                                                                             ifelse(grepl("Nursing", CORPORATE_SERVICE_LINE), "Nursing",
                                                                                                    ifelse(CORPORATE_SERVICE_LINE == "Support Services - Patient & Equipment Transport", "Patient & Equipment Transport",
                                                                                                           ifelse(CORPORATE_SERVICE_LINE == "Support Services - Security", "Security", 
                                                                                                                  ifelse(CORPORATE_SERVICE_LINE == "Perioperative Services", "Perioperative Services",
                                                                                                                         ifelse(CORPORATE_SERVICE_LINE == "Support Services - Clinical Nutrition", "Clinical Nutrition", NA
                                                                                                                         )
                                                                                                                  )
                                                                                                           )
                                                                                                    )
                                                                                             )
                                                                                      )
                                                                               )
                                                                        )
                                                                 )
  )
  ) %>%
    filter(!is.na(Service)) %>%
    filter(DEPARTMENT_BREAKDOWN == 1)
  
  raw_data <- raw_data %>%
    rename(`Key Volume` = `Corp Time Period Time Period End Date`,
           `Entity Code` = `Facility CODE`,
           `Entity` = `Facility DESC`,
           `Department Reporting Definition ID` = `Department CODE`,
           `Department Reporting Definition Name` = `Department DESC`) %>%
    mutate(`Corporation Name` = "Mount Sinai Health System")
  
  #raw_data <- raw_data %>% slice(-1)
  

  fytd_check <- raw_data[1,] %>% mutate(across(everything(), as.character)) %>% pivot_longer(everything())
  
  if(length(which(str_detect(fytd_check$value, 'FYTD Avg'))) > 0) {
    ytd_flag <- 1
  } else {
    ytd_flag <- 0
  }
  
  
  # Data Pre-processing -----------------------------------------------------
  ### Pivot data file longer
  prod_df <- raw_data %>%
    mutate(id = seq_len(n())) %>%
    reshape2::melt(id.var = c("Corporation Name", "Entity Code",
                              "Entity", "Department Reporting Definition ID",
                              "Department Reporting Definition Name",
                              "Key Volume", "id"),
                   na.rm = F) %>%
    select(-c("id"))
  
  ### Process Reporting Period to Reporting Month
  
  #Remove dots at end of reporting period
  prod_df$variable <- gsub("\\..*", "", prod_df$variable)
  prod_df <- prod_df %>%
    rename(Premier_Reporting_Period = variable)%>%
    mutate(Premier_Reporting_Period = format(as.Date(Premier_Reporting_Period,
                                                     format ="%m/%d/%Y"),
                                             "%m/%d/%Y"))
  
  prod_df_final <- prod_df %>% # Fill in Metric name in a column
    mutate(Metrics = as.numeric(value),
           Metrics = ifelse(is.na(Metrics), value, ""),
           Metrics = ifelse(Metrics == "", NA, Metrics)
    ) %>%
    fill(Metrics) %>%
    filter(!is.na(`Entity`)) %>%
    unique()
  
  ## Map Site and Service Group
  prod_df_all <- left_join(prod_df_final, key_vol_mapping[, c("DEFINITION_CODE",
                                                              "KEY_VOLUME", "SITE",
                                                              "Service")
  ],
  by = c("Department Reporting Definition ID" = "DEFINITION_CODE",
         "Key Volume" = "KEY_VOLUME")
  )
  
  #Remove unmapped Services and remove everything after "-"
  prod_df_all <- prod_df_all %>% filter(!is.na(Service)) %>%
    rename(Site = SITE) %>%
    mutate(Metric_Name = trim(gsub("-.*", "", Metrics)
    ),
    Metric_Group = ifelse(grepl("Overtime", 
                                Metric_Name
    ),
    "Overtime Hours", 
    "Productivity")
    )
  
  ### Map Premier Reporting Period -> Dashboard Month
  report_date_mapping <- report_date_mapping %>%
    mutate(`Report Data Updated until` = format(as.Date(`Report Data Updated until`,
                                                        format ="%m/%d/%Y"),
                                                "%m/%d/%Y"),
           `Report Release Date` = format(as.Date(`Report Release Date`,
                                                  format = "%m/%d/%Y"
           ),
           "%m/%d/%Y"
           ),
           `Dashboard Month` = format(as.Date(`Dashboard Month`,
                                              format = "%m/%d/%Y"
           ), "%m/%d/%Y")
    )
  
  
  #Create vector to filter out only reporting periods from mapping file
  # report_data <- as.data.frame(report_date_mapping$`Report Data Updated until`)
  # report_data <- col_concat(report_data, sep = "|") #return vector
  # report_data <- paste(report_data, collapse = "|")
  # 
  # 
  # prod_df_all <- prod_df_all %>%
  #   mutate(Premier_Reporting_Period = as.Date(Premier_Reporting_Period))
  # #filter out reporting periods
  # prod_df_all <- prod_df_all %>% filter(grepl(report_data, Premier_Reporting_Period)) 
  # prod_df_all <- prod_df_all %>% separate(Premier_Reporting_Period, 
  #                                         c("Report_Start","Report_End"), 
  #                                         sep = " - ", remove = FALSE
  # )
  
  #Create Reporting Month Ref column by matching Report_End with Report Data Updates Until
  prod_df_all <- left_join(prod_df_all, report_date_mapping[,c("Report Data Updated until", 
                                                               "Dashboard Month"
  )
  ],
  by = c("Premier_Reporting_Period" = "Report Data Updated until"
  )
  )
  
  prod_df_all <- prod_df_all %>%
    rename(Reporting_Month_Ref = `Dashboard Month`) %>%
    mutate(Reporting_Month_Ref = as.Date(Reporting_Month_Ref, 
                                         format = "%m/%d/%Y"
    )
    )
  
  
  prod_df_all <- prod_df_all %>% unique() %>%
    select(-`Corporation Name`, -`Entity Code`,
           -Entity, `Department Reporting Definition ID`,
           -`Department Reporting Definition Name`, -`Key Volume`,
           -Metrics
    ) %>%
    mutate(Metric_Name = str_trim(Metric_Name),
           value = as.numeric(value)
    ) %>% select(-Metric_Group, -`Department Reporting Definition ID`) 
  
  
  # GEt end dat for Premier Reporting Period
  # prod_df_all <- prod_df_all %>% separate(Premier_Reporting_Period, c("Report_Start","Report_End"), sep = " - ", remove = FALSE) %>%
  #   select(-Report_Start, -Premier_Reporting_Period) %>%
  #   rename(Premier_Reporting_Period = Report_End)
  
  # Create a dataframe to map old metrics to new names - Migration to 2.0
  metrics_mapper = list(Old_Metrics = c("Actual Worked FTE",
                                        "Agency Hours",
                                        "Overtime Hours",
                                        "Total Paid hours",
                                        "Total Worked Hours",
                                        "Volume",
                                        "Overtime Percent of Paid Hours",
                                        "Worked Hours Productivity Index",
                                        "Actual Worked Hours per Unit",
                                        "Total Target Worked FTE",
                                        "Total Paid hours"),
                        New_Metrics= c("Actual Worked FTE",
                                       "Agency Hours",
                                       "Actual Overtime Hrs",
                                       "Actual Paid Hrs",
                                       "Actual Worked Hrs",
                                       "Actual Measure Amount",
                                       "Actual Overtime % of Paid Hrs",
                                       "Worked Hours Productivity Index",
                                       "Actual Worked Hrs per Unit",
                                       "Total Target Wrked FTE",
                                       "Actual Paid Hours"))
  metrics_mapper_df <- as.data.frame(metrics_mapper)
  
  #Map the metrics to data
  prod_df_all <- left_join(prod_df_all, 
                           metrics_mapper_df,
  by = c("Metric_Name" = "New_Metrics")) %>%
    select(-Metric_Name) %>%
    rename("Metric_Name" = "Old_Metrics")
  
  ###group and sum all metrics except Overtime Percent of PAid Hours, WHPU, OT FTE, Agency FTE, Actual Worked Hours per Unit
  prod_df_all <- prod_df_all %>% filter(!(Metric_Name %in% c("Worked Hours Productivity Index", "Overtime Percent of Paid Hours", "Actual Worked Hours per Unit")
  )
  ) %>% group_by(Premier_Reporting_Period, Site, Service, Metric_Name, Reporting_Month_Ref) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    mutate(Metric_Name = ifelse(Metric_Name == "Total Paid hours", "Total Paid Hours", Metric_Name))
  
  whpi <- prod_df_all %>% filter(Metric_Name %in% c("Total Target Worked FTE", "Actual Worked FTE")) %>% 
    pivot_wider(names_from = Metric_Name,
                values_from = value
    ) %>% 
    mutate(`Worked Hours Productivity Index` = `Total Target Worked FTE`/`Actual Worked FTE`) %>%
    select(-`Actual Worked FTE`, -`Total Target Worked FTE`) %>%
    pivot_longer(-c(Service,Site,Reporting_Month_Ref, Premier_Reporting_Period),
                 names_to = "Metric_Name",
                 values_to = "value") %>%
    filter(Metric_Name %in% c("Worked Hours Productivity Index")) %>%
    mutate_at(vars(value), ~replace(., is.nan(.), 0))
  
  overtime_percent_worked_hours <- prod_df_all %>% filter(Metric_Name %in% c("Overtime Hours", "Total Worked Hours")) %>% 
    pivot_wider(names_from = Metric_Name,
                values_from = value
    ) %>%
    mutate(`Overtime Percent of Worked Hours` = `Overtime Hours`/`Total Worked Hours`) %>%
    select(-`Overtime Hours`, -`Total Worked Hours`) %>%
    pivot_longer(-c(Service,Site,Reporting_Month_Ref, Premier_Reporting_Period),
                 names_to = "Metric_Name",
                 values_to = "value") %>%
    filter(Metric_Name %in% c("Overtime Percent of Worked Hours")) %>%
    mutate_at(vars(value), ~replace(., is.nan(.), 0))
  
  ##Overtime percent of paid hours
  overtime_percent_paid_hours <- prod_df_all %>% filter(Metric_Name %in% c("Overtime Hours", "Total Paid Hours")) %>% 
    pivot_wider(names_from = Metric_Name,
                values_from = value
    ) %>%
    mutate(`Overtime Percent of Paid Hours` = `Overtime Hours`/`Total Paid Hours`) %>%
    select(-`Overtime Hours`, -`Total Paid Hours`) %>%
    pivot_longer(-c(Service,Site,Reporting_Month_Ref, Premier_Reporting_Period),
                 names_to = "Metric_Name",
                 values_to = "value") %>%
    filter(Metric_Name %in% c("Overtime Percent of Paid Hours")) %>%
    mutate_at(vars(value), ~replace(., is.nan(.), 0))
  
  # check for why volume mapper
  actual_worked_hours_per_unit <- prod_df_all %>% filter(Metric_Name %in% c("Total Worked Hours", "Volume")) %>% 
    pivot_wider(names_from = Metric_Name,
                values_from = value
    ) %>%
    mutate(`Actual Worked Hours per Unit` = `Total Worked Hours` / Volume) %>%
    select(-`Total Worked Hours`, Volume) %>%
    pivot_longer(-c(Service,Site,Reporting_Month_Ref, Premier_Reporting_Period),
                 names_to = "Metric_Name",
                 values_to = "value") %>%
    filter(Metric_Name %in% c("Actual Worked Hours per Unit")) %>%
    mutate_at(vars(value), ~replace(., is.nan(.), 0))
  
  
  ot_agency_fte <- prod_df_all %>% filter(Metric_Name %in% c("Overtime Hours", "Agency Hours")) %>%
    group_by(Service, Site, Metric_Name, Reporting_Month_Ref, Premier_Reporting_Period) %>%
    summarise(value = sum(value, na.rm = TRUE)/75)  %>%
    mutate(Metric_Name = ifelse(Metric_Name == "Overtime Hours", "OT FTE", Metric_Name),
           Metric_Name = ifelse(Metric_Name == "Agency Hours", "Agency FTE", Metric_Name)
    ) %>%
    ungroup()
  
  
  prod_df_aggregate <- rbind(prod_df_all, whpi, overtime_percent_worked_hours, actual_worked_hours_per_unit, ot_agency_fte, overtime_percent_paid_hours)
  prod_df_aggregate$Metric_Name <- str_trim(prod_df_aggregate$Metric_Name)
  
  
  prod_df_aggregate <- prod_df_aggregate %>% 
    mutate(Reporting_Month = format(Reporting_Month_Ref, "%m-%Y")) %>%
    rename(SERVICE = Service,
           SITE = Site,
           METRIC_NAME_SUBMITTED = Metric_Name,
           REPORTING_MONTH = Reporting_Month_Ref,
           PREMIER_REPORTING_PERIOD = Premier_Reporting_Period,
           VALUE = value) %>%
    select(-Reporting_Month) %>%
    mutate(UPDATED_USER = updated_user)
  
  prod_df_aggregate_cn <- prod_df_aggregate %>% filter(SERVICE == "Clinical Nutrition" & METRIC_NAME_SUBMITTED == "Overtime Percent of Worked Hours") %>% filter(SITE %in% c("MSB", "MSW"))
  prod_df_aggregate <- prod_df_aggregate %>% filter(SERVICE != "Clinical Nutrition" | METRIC_NAME_SUBMITTED != "Overtime Percent of Worked Hours")
  prod_df_aggregate <- rbind(prod_df_aggregate, prod_df_aggregate_cn)
  
  prod_df_aggregate_cn <- prod_df_aggregate %>% filter(SERVICE == "Clinical Nutrition" & METRIC_NAME_SUBMITTED == "Overtime Percent of Paid Hours") %>% filter(SITE %in% c("MSB", "MSW"))
  prod_df_aggregate <- prod_df_aggregate %>% filter(SERVICE != "Clinical Nutrition" | METRIC_NAME_SUBMITTED != "Overtime Percent of Paid Hours")
  prod_df_aggregate <- rbind(prod_df_aggregate, prod_df_aggregate_cn)
  
  
  # prod_df_aggregate_peri <- prod_df_aggregate %>% filter(SERVICE == "Perioperative Services" & METRIC_NAME_SUBMITTED == "Agency FTE") %>% filter(SITE %in% c("MSM"))
  # prod_df_aggregate <- prod_df_aggregate %>% filter(SERVICE != "Perioperative Services" | METRIC_NAME_SUBMITTED != "Agency FTE")
  # prod_df_aggregate <- rbind(prod_df_aggregate, prod_df_aggregate_peri)
  
  prod_df_aggregate_security <- prod_df_aggregate %>% filter(SERVICE == "Security" & METRIC_NAME_SUBMITTED == "Agency FTE") %>% filter(SITE %in% c("MSM", "MSW"))
  prod_df_aggregate <- prod_df_aggregate %>% filter(SERVICE != "Security" | METRIC_NAME_SUBMITTED != "Agency FTE")
  prod_df_aggregate <- rbind(prod_df_aggregate, prod_df_aggregate_security)
  
  
  if(ytd_flag == 1) {
    prod_df_aggregate <- prod_df_aggregate %>% mutate(METRIC_NAME_SUBMITTED = paste0(METRIC_NAME_SUBMITTED, " (FYTD)"))
  } 
  
  
  
  prod_df_aggregate <- prod_df_aggregate %>% 
    filter(VALUE != "NaN") %>%
    mutate(VALUE = round(VALUE,3)) %>%
    drop_na()
  
}


#Test ----

# processed_new_data <- productivity_processing(raw_data, updated_user)
