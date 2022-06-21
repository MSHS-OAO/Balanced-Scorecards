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
  
}