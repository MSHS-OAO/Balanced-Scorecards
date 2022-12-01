#raw_data <- read_excel("C:/Users/villea04/Documents/Productivity Update/System Reports.xlsx")

productivity_file_path <- paste0(home_path, "Summary Repos/Prodictivity.xlsx")
productivity_dept_summary <- function(raw_data, updated_user){
  key_vol_mapping <- key_vol_mapping %>% mutate(Service = ifelse(grepl("Radiology", CORPORATE.SERVICE.LINE), "Imaging",
                                                                 ifelse(grepl("Biomed", CORPORATE.SERVICE.LINE), "Biomed / Clinical Engineering",
                                                                        ifelse(CORPORATE.SERVICE.LINE == "Support Services - Engineering", "Engineering",
                                                                               ifelse(CORPORATE.SERVICE.LINE == "Support Services - Environmental Services", "Environmental Services",
                                                                                      ifelse(CORPORATE.SERVICE.LINE == "Support Services - Food Services", "Food Services",
                                                                                             ifelse(grepl("Nursing", CORPORATE.SERVICE.LINE), "Nursing",
                                                                                                    ifelse(CORPORATE.SERVICE.LINE == "Support Services - Patient Transport", "Patient Transport",
                                                                                                           ifelse(CORPORATE.SERVICE.LINE == "Support Services - Security", "Security", NA
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
    filter(FTE.TREND == 1)
  
  metric_group_mapping <- read_excel(target_mapping_path, 
                                     sheet = "Metric Group v2",  col_names = TRUE, na = c("", "NA")) # Metric group mapping
  metric_group_mapping <- metric_group_mapping %>% # Processing metric group mapping file
    pivot_longer(
      6:length(metric_group_mapping),
      names_to = "Service",
      values_to = "Inclusion"
    ) %>%
    filter(!is.na(Inclusion))
  
  
  raw_data <- raw_data %>% select(!`Entity Time Period Desc`)
  
  # Data Pre-processing -----------------------------------------------------
  ### Pivot data file longer
  prod_df <- raw_data %>%
    mutate(id = seq_len(n())) %>%
    reshape2::melt(id.var = c("Corporation Code", "Corporation Name", "Entity Code",
                              "Entity", "Department Reporting Definition ID",
                              "Department Reporting Definition Name",
                              "Key Volume", "Mapped Facility Cost Centers", "id"),
                   na.rm = F) %>%
    select(-c("id"))
  
  ### Process Reporting Period to Reporting Month
  
  #Remove dots at end of reporting period
  prod_df$variable <- gsub("\\..*", "", prod_df$variable)
  prod_df <- prod_df %>%
    rename(Premier_Reporting_Period = variable) %>%
    mutate(Reporting_Month_Start = as.Date(dttm(gsub(" .*$", "",
                                                     Premier_Reporting_Period)
    )
    )
    )
  
  prod_df_final <- prod_df %>% # Fill in Metric name in a column
    mutate(Metrics = as.numeric(value),
           Metrics = ifelse(is.na(Metrics), value, ""),
           Metrics = ifelse(Metrics == "", NA, Metrics)
    ) %>%
    fill(Metrics) %>%
    filter(!is.na(`Corporation Code`)) %>%
    unique()
  
  ## Map Site and Service Group
  prod_df_all <- left_join(prod_df_final, key_vol_mapping[, c("DEFINITION.CODE",
                                                              "KEY.VOLUME", "SITE",
                                                              "Service")
  ],
  by = c("Department Reporting Definition ID" = "DEFINITION.CODE",
         "Key Volume" = "KEY.VOLUME")
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
  report_data <- as.data.frame(report_date_mapping$`Report Data Updated until`)
  report_data <- col_concat(report_data, sep = "|") #return vector
  report_data <- paste(report_data, collapse = "|")
  
  #filter out reporting periods
  prod_df_all <- prod_df_all %>% filter(grepl(report_data, Premier_Reporting_Period)) 
  prod_df_all <- prod_df_all %>% separate(Premier_Reporting_Period, 
                                          c("Report_Start","Report_End"), 
                                          sep = " - ", remove = FALSE
  )
  
  #Create Reporting Month Ref column by matching Report_End with Report Data Updates Until
  prod_df_all <- left_join(prod_df_all, report_date_mapping[,c("Report Data Updated until", 
                                                               "Dashboard Month"
  )
  ],
  by = c("Report_End" = "Report Data Updated until"
  )
  )
  
  prod_df_all <- prod_df_all %>%
    rename(Reporting_Month_Ref = `Dashboard Month`) %>%
    mutate(Reporting_Month_Ref = as.Date(Reporting_Month_Ref, 
                                         format = "%m/%d/%Y"
    )
    )
  
  
  prod_df_all <- prod_df_all %>% unique() %>%
    select(-`Corporation Code`,-`Corporation Name`, -`Entity Code`,
           -Entity, `Department Reporting Definition ID`,
           -`Department Reporting Definition Name`, -`Key Volume`,
           -`Mapped Facility Cost Centers`, -Reporting_Month_Start,
           -Metrics, -Report_Start, -Report_End
    ) %>%
    mutate(Metric_Name = str_trim(Metric_Name),
           value = as.numeric(value)
    ) 
  
  
  ## Calculate average for all metrics and filter out Imaging and Nursing
  #metrics to calculate their metrics
  prod_df_aggregate <- prod_df_all %>%
    group_by(Service, Site, Metric_Group, Metric_Name, Reporting_Month_Ref, Premier_Reporting_Period) %>%
    summarise(value = mean(value, na.rm = TRUE)) %>%
    filter(Metric_Name != "Total Target Worked FTE") %>%
    filter(!(Service %in% c("Nursing","Imaging") & 
               Metric_Name %in% c("Overtime Percent of Paid Hours",
                                  "Worked Hours Productivity Index",
                                  "Actual Worked Hours per Unit"
               )
    )
    )%>%
    ungroup() %>%
    select(-Metric_Group)
  
  
  nursing_rad_metric_calc <- prod_df_all %>% # Calculate Productivity and Overtime % separately 
    filter(Service %in% c("Nursing","Imaging")) %>%
    filter(Metric_Name %in% c("Total Target Worked FTE","Actual Worked FTE",
                              "Overtime Hours", "Total Paid hours")) %>%
    group_by(Service, Site, Metric_Name, Reporting_Month_Ref, Premier_Reporting_Period) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    pivot_wider(names_from = Metric_Name,
                values_from = value
    ) %>%
    mutate(`Worked Hours Productivity Index` = `Total Target Worked FTE`/`Actual Worked FTE`,
           `Overtime Percent of Paid Hours` = `Overtime Hours`/`Total Paid hours`
    ) %>%
    pivot_longer(-c(Service,Site,Reporting_Month_Ref, Premier_Reporting_Period),
                 names_to = "Metric_Name",
                 values_to = "value") %>%
    filter(Metric_Name %in% c("Worked Hours Productivity Index","Overtime Percent of Paid Hours")) %>%
    mutate_at(vars(value), ~replace(., is.nan(.), 0)) %>%
    mutate(Metric_Group = ifelse(Metric_Name == "Worked Hours Productivity Index", "Productivity", "Overtime Hours"))%>%
    ungroup() %>%
    select(-Metric_Group)
  
  
  #Claulate WHPU
  nursing_rad_whpu <-  prod_df_all %>% 
    filter(Service %in% c("Nursing","Imaging")) %>%
    filter(Metric_Name %in% c("Total Worked Hours", "Volume")) %>%
    group_by(Service, Site, Metric_Group, Metric_Name, Reporting_Month_Ref, Premier_Reporting_Period) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    pivot_wider(names_from = Metric_Name,
                values_from = value
    ) %>%
    mutate(`Actual Worked Hours per Unit` = `Total Worked Hours` / Volume
    ) %>%
    pivot_longer(-c(Service, Site, Reporting_Month_Ref, Metric_Group, Premier_Reporting_Period),
                 names_to = "Metric_Name",
                 values_to = "value") %>%
    filter(Metric_Name == "Actual Worked Hours per Unit") %>%
    mutate_at(vars(value), ~replace(., is.nan(.), 0)) %>%
    mutate_at(vars(value), ~replace(., is.infinite(.), 0)) %>%
    mutate(Metric_Group = "Productivity") %>%
    ungroup() %>%
    select(-Metric_Group)
  
  
  
  
  prod_df_aggregate_all <- bind_rows(prod_df_aggregate, nursing_rad_metric_calc,nursing_rad_whpu) # Merge newly calculated productivity index and overtime % for nursing and radiology 
  
  prod_df_aggregate_all$Metric_Name <- str_trim(prod_df_aggregate_all$Metric_Name)
  
  
  prod_df_aggregate_all <- prod_df_aggregate_all %>% separate(Premier_Reporting_Period, c("Report_Start","Report_End"), sep = " - ", remove = FALSE) %>%
                            select(-Report_Start, -Premier_Reporting_Period) %>%
                            rename(Premier_Reporting_Period = Report_End)

  prod_df_aggregate_all <- prod_df_aggregate_all %>% 
                            mutate(Reporting_Month = format(Reporting_Month_Ref, "%m-%Y")) %>%
                            rename(Metric_Name_Submitted = Metric_Name,
                                   value_rounded = value) 
  
  
  
                            
  prod_df_aggregate_all <- prod_df_aggregate_all %>%
                            rename(SERVICE = Service,
                                   SITE = Site,
                                   METRIC_NAME_SUBMITTED = Metric_Name_Submitted,
                                   REPORTING_MONTH = Reporting_Month_Ref,
                                   PREMIER_REPORTING_PERIOD = Premier_Reporting_Period,
                                   VALUE = value_rounded) %>%
                                   select(-Reporting_Month) %>%
                                   mutate(UPDATED_USER = updated_user)
  

}

productivity_metrics_final_df <- function(data){
  mdf <- metrics_final_df_subset_and_merge(data)
}