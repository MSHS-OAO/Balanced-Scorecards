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
}