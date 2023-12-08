
overtime_file_processs_new <- function(data, updated_user){
  
      key_vol_mapping <- key_vol_mapping %>% mutate(Service = ifelse(grepl("Radiology", CORPORATE.SERVICE.LINE), "Imaging",
                                                                     ifelse(grepl("Biomed", CORPORATE.SERVICE.LINE), "Biomed / Clinical Engineering",
                                                                            ifelse(CORPORATE.SERVICE.LINE == "Support Services - Engineering", "Engineering",
                                                                                   ifelse(CORPORATE.SERVICE.LINE == "Support Services - Environmental Services", "Environmental Services",
                                                                                          ifelse(CORPORATE.SERVICE.LINE == "Support Services - Food Services", "Food Services",
                                                                                                 ifelse(grepl("Nursing", CORPORATE.SERVICE.LINE), "Nursing",
                                                                                                        ifelse(CORPORATE.SERVICE.LINE == "Support Services - Patient Transport", "Patient Transport",
                                                                                                               ifelse(CORPORATE.SERVICE.LINE == "Support Services - Security", "Security",
                                                                                                                      ifelse(CORPORATE.SERVICE.LINE == "Support Services - Clinical Nutrition","Clinical Nutrition",
                                                                                                                             ifelse(CORPORATE.SERVICE.LINE == "Perioperative Services","Perioperative Services", NA))
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
        filter(FTE.TREND == 1) %>%
        filter(!Service %in% c("Nursing","Imaging"))
      
      key_vol_mapping <- key_vol_mapping %>% mutate(COST.CENTER = ifelse(nchar(COST.CENTER) == 7, paste0("0", COST.CENTER), COST.CENTER))
      
      key_vol_mapping <- key_vol_mapping[,c("CORPORATE.SERVICE.LINE", "COST.CENTER","SITE","Service")]
    
      
      conn <- dbConnect(drv = odbc::odbc(),
                        dsn = dsn)
      ot_mapping_db <- tbl(conn, "BSC_OVERTIME_MAPPING")
      ot_mapping_db <- ot_mapping_db %>%
        rename(Service = SERVICE,
                `Cost Center Group` = `COST_CENTER_GROUP`,
                `Site Abbr` = SITE_ABBR,
                Site = SITE,
                Metric_Name = METRIC_NAME) %>% 
        filter(Service %in% c("ED","Nursing","Imaging", "Lab")) %>%
        collect() %>%
        distinct() %>%
        mutate(`Cost Center Group` =  toupper(`Cost Center Group`),
               `Cost Center Group` = str_squish(`Cost Center Group`))
      
      dbDisconnect(conn)
      
      
      #data <- raw_data
      data <- data %>%
        mutate(`Cost Center Group` = toupper(`Cost Center Group`),
               `Cost Center Group` = str_squish(`Cost Center Group`))

      
      data <- left_join(data,ot_mapping_db)
        
      eni_data <- data%>%
        filter(Service %in% c("ED","Nursing","Imaging", "Lab"))
      
      #Procesing All Other Service Lines other than ED,Nursing, and Imaging
      
      # non_eni_data <- data%>%
      #   filter(is.na(Service))
      
      data <- data %>%
        select(-Service,-`Cost Center Group`,-`Site Abbr`,-Site,-Metric_Name)
      
      non_eni_data_cc_7 <- data %>% filter(nchar(CostCenter) == 7) %>% mutate(CostCenter = paste0("0", CostCenter))
      
      non_eni_data_cc_4 <- data %>% filter(nchar(CostCenter) == 4) %>% mutate(`Location Code` = ifelse(nchar(`Location Code`) == 1, paste0("0", `Location Code`), `Location Code`)) %>%
        mutate(CostCenter = paste0(COFT, `Location Code`, CostCenter))
      
      
      mapped_non_eni_data_cc_7 <- inner_join(non_eni_data_cc_7, key_vol_mapping, by = c("CostCenter" = "COST.CENTER")) #%>%
        #filter(!is.na(Service))
      
      mapped_non_eni_data_cc_4 <- inner_join(non_eni_data_cc_4, key_vol_mapping, by = c("CostCenter" = "COST.CENTER"))#%>%
        #filter(!is.na(Service))
      
      non_eni_data <- rbind(mapped_non_eni_data_cc_7,mapped_non_eni_data_cc_4)%>%
        filter(!(is.na(`Discharge Fisc Year-Period`))) %>%
        select(-VP,-`FLSA Category`, -`Uncontollable POT flag`,-`Pay Category`,-CostCenter,-COFT,-`Location Code`,-`Job Title`,-CORPORATE.SERVICE.LINE,-CostCenterDesc)  %>%
        rename(Site = SITE,
               `Associated Dashboard Month` = `Discharge Fisc Year-Period`) %>% 
        group_by(Site, Service, `Associated Dashboard Month`) %>%
        mutate(Value = (sum(`Actual Overtime Dollars`)/sum(`Actual Dollars`)))%>%
        distinct() %>%
        mutate(Metric_Name =  "Overtime Dollars - % (Finance)") %>%
        select(-`Actual Overtime Dollars`,-`Actual Dollars`)
      
      
      non_eni_data$`Associated Dashboard Month` <- as.Date(paste0(non_eni_data$`Associated Dashboard Month`, "-01"), format = "%Y-%m-%d")
      #data$Metric_Name <- "Overtime Dollars - % (Finance)"
      non_eni_data$Premier_Reporting_Period <- "FINANCE DATA"
      non_eni_data$Metric <- "FINANCE DATA"
      
      non_eni_data$Value[is.nan(non_eni_data$Value)] <- 0
      
      
      non_eni_data <- non_eni_data %>% rename(SERVICE = Service,
                                      SITE = Site,
                                      REPORTING_MONTH = `Associated Dashboard Month`,
                                      METRIC_NAME_SUBMITTED = Metric_Name,
                                      VALUE = Value) %>%
        mutate(PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH, "%b %Y"),
               UPDATED_USER = updated_user) %>%
        select(SERVICE, 
               SITE, 
               REPORTING_MONTH,
               PREMIER_REPORTING_PERIOD, 
               METRIC_NAME_SUBMITTED,
               VALUE,
               UPDATED_USER)

      
      eni_data <- eni_data %>% 
        filter(!(is.na(Service))) %>%
        filter(!(is.na(`Discharge Fisc Year-Period`))) %>%
        select(-Site,-VP,-`FLSA Category`, -`Uncontollable POT flag`)  %>%
        rename(Site = `Site Abbr`,
               `Associated Dashboard Month` = `Discharge Fisc Year-Period`)
      
      eni_data <- eni_data %>% group_by(Site, Service, `Associated Dashboard Month`, Metric_Name) %>%
        mutate(Value = (sum(`Actual Overtime Dollars`)/sum(`Actual Dollars`))) %>%
        select(-`Cost Center Group`,-`Pay Category`,-`Actual Overtime Dollars`, -`Actual Dollars`) %>%
        distinct()
      
      eni_data$`Associated Dashboard Month` <- as.Date(paste0(eni_data$`Associated Dashboard Month`, "-01"), format = "%Y-%m-%d")
      #data$Metric_Name <- "Overtime Dollars - % (Finance)"
      eni_data$Premier_Reporting_Period <- "FINANCE DATA"
      eni_data$Metric <- "FINANCE DATA"
      
      eni_data$Value[is.nan(eni_data$Value)] <- 0
      
      
      
      eni_data <- eni_data %>% rename(SERVICE = Service,
                              SITE = Site,
                              REPORTING_MONTH = `Associated Dashboard Month`,
                              METRIC_NAME_SUBMITTED = Metric_Name,
                              VALUE = Value) %>%
        mutate(PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH, "%b %Y"),
               UPDATED_USER = updated_user) %>%
        select(SERVICE, 
               SITE, 
               REPORTING_MONTH,
               PREMIER_REPORTING_PERIOD, 
               METRIC_NAME_SUBMITTED,
               VALUE,
               UPDATED_USER)
      
      processed_data <- rbind(eni_data,non_eni_data) %>%distinct()
      
      ## Below was added to not Include Nata for CN Sepcifcally for FInance OT for Sites MSBI, MSH, MSM, and MSQ
      processed_data_cn <- processed_data %>% filter(SERVICE == "Clinical Nutrition" & METRIC_NAME_SUBMITTED == "Overtime Dollars - % (Finance)") %>% filter(SITE %in% c("MSB", "MSW"))
      processed_data <- processed_data %>% filter(SERVICE != "Clinical Nutrition" | METRIC_NAME_SUBMITTED != "Overtime Dollars - % (Finance)")

      processed_data <- rbind(processed_data, processed_data_cn)
      processed_data
}

#tests
# raw_data <- read_excel("/home/tommad01/Copy of 2022 OT data v2.xlsx",col_types = c("guess", "guess", "guess", "guess","guess", "guess", "guess", "text","guess", "text", "text", "guess","guess","guess"))
# aw_data <- raw_data %>% filter(`Discharge Fisc Year-Period` == "2022-12")
# updated_user <- "TEST"
# processed_data <- overtime_file_processs_new(raw_data,updated_user)
