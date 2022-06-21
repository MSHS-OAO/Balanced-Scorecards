productivity_dept_summary <- function(raw_data){
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
}