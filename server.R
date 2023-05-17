# Increase allowable file size (Sunquest monthly files are too large for default)
if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=100*1024^2)


  server <- function(input, output, session) {
    
    
  
    # 0. Observe Events for Filters ----------------------------------------------------------------
    
    
    
    # Code to update drop down selections based on selected service line -------------
    observeEvent(input$selectedService,{
      conn <- dbConnect(drv = odbc::odbc(), 
                        dsn = dsn)
      mdf_tbl <- tbl(conn, "BSC_METRICS_FINAL_DF")
      service_selected <- input$selectedService
      
      
      data <- mdf_tbl %>% filter(SERVICE %in% service_selected) %>% collect()
      dbDisconnect(conn)
      picker_choices <-  format(sort(unique(data$REPORTING_MONTH)), "%m-%Y")
      updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
    }, ignoreInit = T)
    
    observeEvent(input$selectedService2,{
      
      conn <- dbConnect(drv = odbc::odbc(), 
                        dsn = dsn)
      mdf_tbl <- tbl(conn, "BSC_METRICS_FINAL_DF")
      service_selected <- input$selectedService2
      
      
      
      data <- mdf_tbl %>% filter(SERVICE %in% service_selected) %>% collect()
      picker_choices <-  format(sort(unique(data$REPORTING_MONTH)), "%m-%Y")
      updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      
      campus_choices <- sort(unique(data$SITE))
      dbDisconnect(conn)
      updatePickerInput(session, "selectedCampus2", choices = campus_choices, selected = campus_choices)
      
    }, ignoreInit = T)
    
    
    observeEvent(input$selectedService3,{
      conn <- dbConnect(drv = odbc::odbc(), 
                        dsn = dsn)
      mdf_tbl <- tbl(conn, "BSC_METRICS_FINAL_DF")
      service_selected <- input$selectedService3
      
      
      
      data <- mdf_tbl %>% filter(SERVICE %in% service_selected) %>% collect()
      picker_choices <-  format(sort(unique(data$REPORTING_MONTH)), "%m-%Y")
      updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      
      campus_choices <- sort(unique(data$SITE))
      dbDisconnect(conn)
      updatePickerInput(session, "selectedCampus3", choices = campus_choices, selected = campus_choices)
    }, ignoreInit = T)
    
    observeEvent(input$selectedService4, {
      
      target_data <- target_mapping_reference %>%
        filter(Service %in% input$selectedService4)
      
      picker_choices_metric_group <- unique(target_data$Metric_Group)
      
      updatePickerInput(session, "selectedMetricGroup",
                        choices = picker_choices_metric_group,
                        selected = picker_choices_metric_group)
      
    }, ignoreInit = T)
    
    # observeEvent(input$selectedService2,{
    #   service <- input$selectedService2
    #   month <- input$selectedMonth2
    #   campus <- input$selectedCampus2
    #   
    #   month <- format(as.Date(paste0(month, "-01"), "%m-%Y-%d"), "%Y-%m-%d")
    #   format <- "YYYY-MM-DD HH24:MI:SS"
    #   
    #   conn <- dbConnect(drv = odbc::odbc(),
    #                     dsn = dsn)
    #   mdf_tbl <- tbl(conn, "BSC_METRICS_FINAL_DF") %>% filter(SERVICE == service, REPORTING_MONTH == TO_DATE(month, format),
    #                                                           SITE %in% campus) %>%
    #                                                 summarise(choices = unique(METRIC_NAME_SUBMITTED)) %>% collect()
    #   metric_choices <- unique(mdf_tbl$choices)
    #   
    #   #metric_choices <- unique(metrics_final_df[metrics_final_df$Service %in% input$selectedService2, "Metric_Name"])
    #   updatePickerInput(session,
    #                     inputId = "selectedMetric2",
    #                     choices = metric_choices,
    #                     selected = metric_choices
    #   )
    # },
    # ignoreInit = TRUE,
    # ignoreNULL = FALSE)
    # 
    # observeEvent(input$selectedService3,{
    #   service <- input$selectedService3
    #   month <- input$selectedMonth3
    #   campus <- input$selectedCampus3
    #   
    #   month <- format(as.Date(paste0(month, "-01"), "%m-%Y-%d"), "%Y-%m-%d")
    #   format <- "YYYY-MM-DD HH24:MI:SS"
    #   
    #   conn <- dbConnect(drv = odbc::odbc(),
    #                     dsn = dsn)
    #   mdf_tbl <- tbl(conn, "BSC_METRICS_FINAL_DF") %>% filter(SERVICE == service, REPORTING_MONTH == TO_DATE(month, format),
    #                                                           SITE %in% campus) %>%
    #     summarise(choices = unique(METRIC_NAME_SUBMITTED)) %>% collect()
    #   metric_choices <- unique(mdf_tbl$choices)
    #   
    #   updatePickerInput(session,
    #                     inputId = "selectedMetric3",
    #                     choices = metric_choices,
    #                     selected = metric_choices
    #   )
    # },
    # ignoreInit = TRUE,
    # ignoreNULL = FALSE)
    
    # 1. Summary Tab Output ---------------------------------------------------------------------------------
    output$siteSummary_title <- renderText({
      
      input$submit_prod
      input$submit_engineering
      input$submit_finance
      input$submit_food
      input$submit_evs
      input$submit_imaging
      input$submit_ytd_pt_exp
      input$submit_monthly_pt_exp
      input$submit_biomeddi
      input$submit_biomedkpis
      input$submit_imagingct
      input$submit_lab_pt
      input$submit_lab_tat
      input$submit_pt_tat
      input$submit_sec_inc_rpts
      input$submit_sec_events
      input$submit_ed
      input$submit_nursing
      input$submit_finance_census
      input$submit_peri_op
      input$submit_case_management
      input$submit_cn
      
      input_service <- input$selectedService
      
      conn <- dbConnect(odbc(), dsn)  
      time_df <- tbl(conn, "BSC_METRICS_FINAL_DF") %>% filter(SERVICE == input_service) %>% collect()
      dbDisconnect(conn)
      if(nrow(time_df) == 0){
        text = paste0("MSHS ",input_service, " Summary")
      }else{
        updated <- format(max(time_df$UPDATED_TIME, na.rm = TRUE), "%Y-%m-%d %I:%M %p")#, tz = "America/New_York")
        text = paste0("MSHS ",input_service, " Summary - Updated ",updated)
      }
      text
    })
    
    output$siteSummary_table <- function(){
      input$submit_prod
      input$submit_engineering
      input$submit_finance
      input$submit_food
      input$submit_evs
      input$submit_imaging
      input$submit_ytd_pt_exp
      input$submit_monthly_pt_exp
      input$submit_biomeddi
      input$submit_biomedkpis
      input$submit_imagingct
      input$submit_lab_pt
      input$submit_lab_tat
      input$submit_pt_tat
      input$submit_sec_inc_rpts
      input$submit_sec_events
      input$submit_ed
      input$submit_nursing
      input$submit_finance_ot
      input$submit_finance_census
      input$submit_peri_op
      input$submit_case_management
      input$submit_cn
      
      service_input <- input$selectedService
      month_input <- input$selectedMonth
      # service_input <- "Case Management / Social Work"
      # month_input <- "03-2023"

      metrics_final_df <- mdf_from_db(service_input, month_input) 
      


      # Code Starts ---------------------------------------------------------------------------------     
      summary_tab_metrics <- metric_mapping_summary_site %>%
        filter(Service == service_input) %>%
        select(-General_Group, -Display_Order)
      
      summary_metric_group_order <- unique(summary_tab_metrics$Metric_Group)
      
      summary_metric_name_order <- unique(summary_tab_metrics$Metric_Name_Summary)
      
      # # Target mappings using original structure
      status_section_metrics <- target_mapping_analysis %>%
        filter(Service %in% service_input) %>%
        select(Service, Metric_Group, Metric_Name) %>%
        distinct()
      
      # Subset target mapping to select Targets and Status Definitions for selected service line
      metric_targets_status <- target_mapping_analysis %>%
        filter(Service == service_input)
      
      # Variable setting
      current_period <- as.Date(fast_strptime(month_input, "%m-%Y"), "%Y-%m-%d")
      fiscal_year <- format(current_period,  "%Y")
      
      # # Filter data by service specific metrics
      data <- left_join(summary_tab_metrics, metrics_final_df,
                        by = c("Service",
                               "Metric_Group",
                               "Metric_Name"))
      
      # Crosswalk data with metric targets and status definitions
      data <- left_join(data,
                        metric_targets_status,
                        by = c("Service",
                               "Site",
                               "Metric_Group",
                               "Metric_Name",
                               "Metric_Name_Submitted"))
      data <- data %>%
        # Determine status based on status definitions
        mutate(Status = ifelse(is.na(Target), NA,
                               ifelse(between(value_rounded,
                                              Green_Start,
                                              Green_End),
                                      "Green",
                                      ifelse(between(value_rounded,
                                                     Yellow_Start,
                                                     Yellow_End),
                                             "Yellow",
                                             ifelse(between(value_rounded,
                                                            Red_Start,
                                                            Red_End),
                                                    "Red", NA))))) %>%
        # Filter on selected reporting period
        filter(Reporting_Month_Ref <= current_period)
      # Data Period Filtering
      period_filter <- data %>% 
        group_by(Metric_Group,
                 Metric_Name_Summary,
                 Metric_Name,
                 Reporting_Month_Ref,
                 Premier_Reporting_Period) %>% 
        #distinct() %>%
        summarise(total = n()) %>%                                                            #
        arrange(Metric_Group, Metric_Name_Summary,
                Metric_Name, desc(Reporting_Month_Ref)) %>%
        group_by(Metric_Group, Metric_Name_Summary, Metric_Name) %>%
        mutate(id = row_number())

      # Current Period Table -----------------
      current_summary_data <- left_join((period_filter %>% filter(id == 1)),
                                        data,
                                        by = c("Metric_Group",
                                               "Metric_Name_Summary",
                                               "Metric_Name",
                                               "Reporting_Month_Ref",
                                               "Premier_Reporting_Period"))%>% 
        mutate(across('Metric_Name_Submitted', str_replace, "\\(Monthly\\)", ''),
               Metric_Name_Submitted = str_trim(Metric_Name_Submitted))  #Take all most recent data (id = 1) and merge with all data 

      current_summary <- current_summary_data %>%
        mutate(`Current Period` = ifelse(str_detect(Premier_Reporting_Period, "/"), 
                                         paste0("Rep. Pd. Ending ",
                                                Premier_Reporting_Period),
                                         Premier_Reporting_Period))  ## Create Current Period column if it's premier say when it ends
      
      current_summary <- current_summary[, c("Metric_Group",
                                             "Metric_Name_Summary",
                                             "Current Period",
                                             "Site",
                                             "value_rounded",
                                             "Metric_Unit")]
      
      # Remove any duplicates
      # Should we fix the code so there are no duplicates anyway?
      current_summary <- unique(current_summary)
      
      # Think about renaming columns at the end
      current_summary <- current_summary %>%
        mutate(Section = "Metrics") %>%
        relocate(Section) %>%
        ungroup() %>%
        select(-Metric_Group) %>%
        pivot_wider(names_from = Site, values_from = value_rounded)

      # Identify any sites missing for the summary and add them with NA values
      missing_sites <- setdiff(sites_inc, names(current_summary))
      current_summary[missing_sites] <- NA
      
      min_month <- as.Date(paste0(month_input, "-01"), "%m-%Y-%d") %m-% months(12)
      
      format <- "YYYY-MM-DD HH24:MI:SS"
      
      summary_tab_metrics_budget <- summary_tab_metrics %>% 
        mutate(across('Metric_Name_Submitted', str_replace, "\\(Monthly\\)", ''),
               Metric_Name_Submitted = str_trim(Metric_Name_Submitted))
      
      budget_data_repo <- get_budget_data(service = service_input,month_input)
      
      month_selected_format <- as.Date(paste0(month_input, "-01"), format = "%m-%Y-%d")
      if (service_input %in% unique(budget_data_repo$Service) & month_selected_format >= as.Date("2022-01-01")) {
        month_in_repo <- unique(format(budget_data_repo$Month, "%Y-%m-%d"))
        month_selected <- as.Date(paste0(month_input, "-01"), format = "%m-%Y-%d")
        budget_metrics <- summary_tab_metrics %>% filter(Metric_Group == "Budget to Actual") %>%
          mutate(Metric_Name_Submitted = str_sub(Metric_Name_Submitted,end = -10),
                 Metric_Name_Submitted = str_trim(Metric_Name_Submitted))
        budget_metrics <- unique(budget_metrics$Metric_Name_Submitted)
        
        
        if (as.character(month_selected )%in% month_in_repo) {
          ytd_budget <- budget_data_repo %>% ungroup() %>% filter(Service %in% service_input, Month == month_selected, Metric_Name_Submitted %in% budget_metrics)
        } else {
          ytd_budget <- budget_data_repo %>% ungroup() %>% filter(Service %in% service_input, Month == max(Month), Metric_Name_Submitted %in% budget_metrics)
        }
        
        
        #ytd_budget <- budget_data_repo %>% ungroup() %>% filter(Service %in% service_input, Month == month_selected, Metric_Name_Submitted %in% budget_metrics)
        
        ytd_join <- left_join(ytd_budget, summary_tab_metrics_budget)
        ytd_join <- ytd_join %>% select(-Service, -Metric_Unit, -Value, - Metric_Name_Submitted) %>% rename(value_rounded = Value_ytd)
        # ytd_join <- ytd_join %>% select(-Metric_Unit, -Value, - Metric_Name_Submitted) %>% rename(value_rounded = Value_ytd)
        
        
        month_in_data <- unique(format(ytd_join$Month,"%b"))
        
        if(month_in_data != "Jan"){
          month_in_data <- unique(format(ytd_join$Month, "%b %Y"))
          fytd_name <- paste0("Jan - ", month_in_data, " Total")
        }else {
          fytd_name <- paste0(unique(format(ytd_join$Month, "%b %Y")), " Total")
        }
        
        ytd_join$Month <- fytd_name
        ytd_join <- ytd_join %>% rename(`Fiscal Year to Date` = Month)
        
      } else{
        ytd_join <- NULL
      }
      
      
      if(!is.null(ytd_join)){
        # FYTD Period Filter 
        fytd_period <- period_filter %>%        #Get all data from YTD
          # Remove monthly Patient Experience data from YTD sections since there is separate YTD data for this
          filter(!(Metric_Group %in% c("Patient Experience"))) %>%
          group_by(Metric_Group, Metric_Name_Summary, Metric_Name) %>%
          #filter(total == max(total)) %>%
          #filter(format(Reporting_Month_Ref, "%Y",) == fiscal_year) %>%
          filter(format(Reporting_Month_Ref, "%Y",) == max(format(Reporting_Month_Ref, "%Y"))) %>%
          group_by(Metric_Group, Metric_Name_Summary, Metric_Name) %>%
          mutate(`Fiscal Year to Date` = ifelse(str_detect(Premier_Reporting_Period, "/"), 
                                                paste0("FYTD Ending ", Premier_Reporting_Period[which.min(id)]),
                                                ifelse(which.max(id) == 1,
                                                       Premier_Reporting_Period[which.min(id)],
                                                       paste0(substr(Premier_Reporting_Period[which.max(id)], 1, 3), " - ", 
                                                              Premier_Reporting_Period[which.min(id)]))))
      } else{
        # FYTD Period Filter 
        fytd_period <- period_filter %>%        #Get all data from YTD
          # Remove monthly Patient Experience data from YTD sections since there is separate YTD data for this
          filter(!(Metric_Group %in% c("Patient Experience"))) %>%
          group_by(Metric_Group, Metric_Name_Summary, Metric_Name) %>%
          #filter(total == max(total)) %>%
          #filter(format(Reporting_Month_Ref, "%Y",) == fiscal_year) %>%
          filter(format(Reporting_Month_Ref, "%Y",) == max(format(Reporting_Month_Ref, "%Y"))) %>%
          group_by(Metric_Group, Metric_Name_Summary, Metric_Name) %>%
          mutate(`Fiscal Year to Date` = ifelse(str_detect(Premier_Reporting_Period, "/"), 
                                                paste0("FYTD Ending ", Premier_Reporting_Period[which.min(id)]),
                                                ifelse(which.max(id) == 1,
                                                       Premier_Reporting_Period[which.min(id)],
                                                       paste0(substr(Premier_Reporting_Period[which.max(id)], 1, 3), " - ", 
                                                              Premier_Reporting_Period[which.min(id)]))))
        
      }
      
      
      
      # FYTD Summary Table - for total
      fytd_summary_all <- left_join(fytd_period,
                                    data,
                                    by = c("Metric_Group",
                                           "Metric_Name_Summary",
                                           "Metric_Name",
                                           "Reporting_Month_Ref",
                                           "Premier_Reporting_Period"),
                                    all = TRUE)
      
      fytd_summary_total <- fytd_summary_all %>%
        # Metrics that need to be summarized by sum (total)
        filter(str_detect(Metric_Name_Summary,
                          "(Budget to Actual)|(Total Revenue to Budget Variance)")) %>%
        mutate(`Fiscal Year to Date` = paste(`Fiscal Year to Date`," Total")) %>%
        group_by(Site, Metric_Group, Metric_Name_Summary, Metric_Name, `Fiscal Year to Date`) %>%
        summarise(value_rounded = round(sum(value_rounded, na.rm = TRUE))) %>%
        ungroup()
      
      # FYTD Summary Table - for Patient Experience
      # fytd_press_ganey <- reformat_pg_fytd(press_ganey_data)
      #pt_exp_data <- metrics_final_df %>% filter(Metric_Group == "Patient Experience")
      conn <- dbConnect(odbc(), dsn)
      pt_exp_data <- tbl(conn, "BSC_PATIENT_EXPERIENCE_REPO") %>% 
        rename(Service = SERVICE,
               Site = SITE,
               Question_Clean = QUESTION_CLEAN,
               ReportingType = REPORTINGTYPE,
               Reporting_Date_Start = REPORTING_DATE_START,
               Reporting_Date_End = REPORTING_DATE_END,
               Site_Mean = SITE_MEAN,
               Site_N = SITE_N,
               All_PG_Database_Mean = ALL_PG_DATABASE_MEAN,
               All_PG_Database_N = ALL_PG_DATABASE_N,
               All_PG_Database_Rank = ALL_PG_DATABASE_RANK) %>%
        select(-UPDATED_USER, -UPDATED_TIME) %>%
        collect()
      pt_exp_data <- pt_exp_data %>%
        mutate(Reporting_Date_Start = as.Date(Reporting_Date_Start),
               Reporting_Date_End = as.Date(Reporting_Date_End)) %>%
        filter(!(Site %in% "All"))
      dbDisconnect(conn)
      
      if (service_input %in% unique(pt_exp_data$Service)) {
        
        pt_exp_ytd <- pt_exp_data %>%
          # Add logic to include Jan data in YTD data
          mutate(ReportingType = ifelse(month(Reporting_Date_Start) == 1 &
                                          month(Reporting_Date_End) == 1 &
                                          year(Reporting_Date_Start) == year(Reporting_Date_End),
                                        "YTD", ReportingType)) %>%
          # Filter on YTD data and selected service
          filter(ReportingType %in% "YTD" &
                   Service %in% service_input) %>%
          mutate(Reporting_Month_Ref = floor_date(Reporting_Date_End,
                                                  unit = "month")) %>%
          filter(Reporting_Month_Ref <= current_period)
        
        # Patient Experience mapping for metrics
        pt_exp_mapping_simple <- pt_exp_mapping %>%
          select(-Raw_Pt_Exp_Service, -Questions)
        
        # Crosswalk Patient Experience YTD data with mapping
        pt_exp_ytd <- left_join(pt_exp_ytd,
                                pt_exp_mapping_simple,
                                by = c("Service" = "Service",
                                       "Question_Clean" = "Question_Clean"))
        
        # Begin reformatting Patient Experience YTD data
        pt_exp_ytd_reformat <- pt_exp_ytd %>%
          # Convert to longer format
          pivot_longer(cols = c(contains("Site_"),
                                contains("All_PG_Database_")),
                       names_to = "Metric") %>%
          # Update metric names
          mutate(Metric_Name_Submitted = ifelse(Metric %in% "Site_Mean",
                                                paste0(Question_Clean, " - Score"),
                                                ifelse(Metric %in% "Site_N" & Incl_N,
                                                       paste0(Question_Clean, " - N"),
                                                       ifelse(Metric %in% "All_PG_Database_Rank" &
                                                                Incl_AllHosp_Rank,
                                                              "Rank - All Hospitals",
                                                              NA)))) %>%
          # Remove unused metrics
          filter(!is.na(Metric_Name_Submitted)) %>%
          # Remove unused columns
          select(-Question_Clean,
                 -ReportingType,
                 -Incl_N,
                 -Incl_AllHosp_Rank,
                 -Metric) %>%
          # Rename value column for consistency
          rename(value_rounded = value)
        
        # Identify Patient Experience metrics to include in Summary tab
        pt_exp_summary_tab_metrics <- metric_mapping_summary_site %>%
          filter(Service %in% service_input &
                   General_Group %in% "Patient Experience") %>%
          select(-General_Group, -Display_Order, -Metric_Unit,
                 Service, Metric_Group, Metric_Name_Summary,
                 Metric_Name, Metric_Name_Submitted)

        # Crosswalk Patient Experience YTD data with Summary tab metrics
        pt_exp_ytd_reformat <- left_join(pt_exp_ytd_reformat,
                                         pt_exp_summary_tab_metrics,
                                         by = c("Service" = "Service",
                                            "Metric_Name_Submitted" = "Metric_Name_Submitted"))
        
        pt_exp_ytd_reformat <- pt_exp_ytd_reformat %>%
          # Filter on 
          filter(!is.na(Metric_Name_Summary) &
                   Reporting_Month_Ref == max(Reporting_Month_Ref)) %>%
          mutate(`Fiscal Year to Date` = ifelse(month(Reporting_Month_Ref) == 1 &
                                                  month(Reporting_Date_Start) == 1,
                                                format(Reporting_Month_Ref, "%b %Y"),
                                                paste0(
                                                  format(Reporting_Date_Start, "%b"),
                                                  " - ",
                                                  format(Reporting_Month_Ref, "%b %Y"),
                                                  " Average"))) %>%
          select(-Service,
                 -Reporting_Date_Start,
                 -Reporting_Date_End,
                 -Reporting_Month_Ref,
                 -Metric_Name_Submitted) %>%
          relocate(value_rounded, .after = `Fiscal Year to Date`)

      } else {
        pt_exp_ytd_reformat <- NULL
      }
      
                    
      # FYTD Summary Table - for average 
      '%!in%' <<- function(x,y)!('%in%'(x,y))
      fytd_summary_avg <- fytd_summary_all %>%
        # For consistency, consider do a string detect here
        filter(Metric_Group %!in% c("Budget to Actual", "Total Revenue to Budget Variance", "Productivity")) %>% # Metrics that need to be summarized by sum (total)
        filter(Metric_Name != "Overtime Hours - % (Premier)") %>%
        mutate(`Fiscal Year to Date` = paste(`Fiscal Year to Date`," Average")) %>%
        group_by(Site,
                 Metric_Group,
                 Metric_Name_Summary,
                 Metric_Name,
                 `Fiscal Year to Date`) %>%
        summarise(value_rounded = mean(value_rounded, na.rm = TRUE)) %>%
        ungroup()
      
      fytd_metrics <- paste0(unique(fytd_summary_all$Metric_Name_Submitted), " (FYTD)")
      conn <- dbConnect(drv = odbc::odbc(),
                        dsn = dsn)
      
      summary_repo_tbl <- tbl(conn, "SUMMARY_REPO")
      fytd_prod <-  summary_repo_tbl %>% filter(service_input %in% SERVICE, METRIC_NAME_SUBMITTED %in% fytd_metrics) %>% 
        collect()
      dbDisconnect(conn)
      
      productivity_reporting_period <- current_summary[grep("Rep. Pd. Ending", current_summary$`Current Period`),]
      
      if(nrow(productivity_reporting_period) > 0) {
        productivity_reporting_period <- unique(productivity_reporting_period$`Current Period`)[1]
        premier_reporting_period <- gsub(".*Rep. Pd. Ending ", "",productivity_reporting_period)
        
        fytd_prod <- fytd_prod %>% filter(PREMIER_REPORTING_PERIOD == premier_reporting_period)
        
        fytd_prod <- fytd_prod %>% mutate(`Fiscal Year to Date` = paste0("FYTD Ending ", premier_reporting_period, 
                                                                         " Average")) %>%
                    select(-UPDATED_USER, -UPDATED_TIME, -REPORTING_MONTH, -PREMIER_REPORTING_PERIOD) %>%
                    rename(Site = SITE,
                           value_rounded = VALUE,
                           Service = SERVICE,
                           Metric_Name_Submitted = METRIC_NAME_SUBMITTED) %>%
                    mutate(Metric_Name_Submitted = gsub(" [(]FYTD[)].*", "", Metric_Name_Submitted))
        
        fytd_prod <- inner_join(fytd_prod, summary_tab_metrics, by = c("Service" = "Service",
                                                                       "Metric_Name_Submitted" = "Metric_Name_Submitted")) %>%
          select(Site, Metric_Group, Metric_Name_Summary, Metric_Name, `Fiscal Year to Date`, value_rounded)
        
        fytd_merged <- rbind(fytd_summary_avg, pt_exp_ytd_reformat, ytd_join, fytd_prod) %>% distinct()
      } else{
        # Merge for summary 
        # fytd_merged <- rbind(fytd_summary_total, fytd_summary_avg, pt_exp_ytd_reformat, ytd_join)
        fytd_merged <- rbind(fytd_summary_avg, pt_exp_ytd_reformat, ytd_join) %>% distinct()
      }
      fytd_summary <- fytd_merged
      # fytd_summary$Metric_Name <- NULL
      fytd_summary <- fytd_summary %>%
        select(-Metric_Group, -Metric_Name) %>%
        mutate(Section = "Metrics") %>%
        relocate(Section) %>%
        pivot_wider(names_from = Site, values_from = value_rounded)

      missing_sites <- setdiff(sites_inc, names(fytd_summary))
      fytd_summary[missing_sites] <- NA
      #fytd_summary$NYEE <- as.numeric(fytd_summary$NYEE)
      
      
      check <- target_mapping %>% filter(Service == service_input)
      if("Total Revenue to Budget Variance" %in% as.vector(check$Metric_Group)){
        fytd_data_budget <- fytd_summary_all %>% filter(Metric_Name %in% c("Variance to Budget", "Budget"))  %>%
          filter((Service == service_input) &
                   (Metric_Group == "Total Revenue to Budget Variance") &
                   (Metric_Name %in% c("Budget", "Variance to Budget")) & 
                   (Reporting_Month_Ref %in%
                      unique((fytd_period %>% 
                                filter(Metric_Name == "Variance to Budget"))$Reporting_Month_Ref))) %>%
          group_by(Service, Site, Metric_Group, Metric_Name) %>%
          summarise(value_rounded = sum(value_rounded)) %>%
          ungroup() %>%
          select(-Service, -Metric_Group, -Metric_Name) %>%
          mutate(Section = "Metrics", Metric_Name_Summary = "Total Revenue to Budget Variance") %>%
          pivot_wider(names_from = "Site", values_from = "value_rounded")
        
        fytd_summary_all_budget_variance <-fytd_summary_all %>% filter(Metric_Name == "Variance to Budget") 
        min_date <- min(fytd_summary_all_budget_variance$Reporting_Month_Ref)
        max_date <- max(fytd_summary_all_budget_variance$Reporting_Month_Ref)
        
        fytd_data_budget <- fytd_data_budget %>% mutate(`Fiscal Year to Date` = paste0(format(min_date, "%b"), " - ", format(max_date, "%b"), " ", year(max_date), " Total"))
        fytd_summary <- bind_rows(fytd_summary, fytd_data_budget)
      }
      current_summary_order <- c("Section", "Metric_Name_Summary", "Current Period", "Metric_Unit", "MSB", "MSBI", "MSH", "MSM", "MSQ", "MSW", "NYEE")
      current_summary <- current_summary[, current_summary_order]
      
      # Merge FYTD and Current Period Metrics Summary 
      metrics_summary <- merge(fytd_summary, current_summary,
                               by = c("Section","Metric_Name_Summary"), all = TRUE)
      
           
      # Format metrics based on units and remove Metric_Unit column
      metrics_summary <- metrics_summary %>%
        mutate(across(where(is.numeric),
                      .fns = function(x) {
                        ifelse(Metric_Unit %in% "Dollar",
                               dollar(round(x)),
                               ifelse(Metric_Unit %in% "Percent",
                                      percent(x, 0.1),
                                      prettyNum(round(x, digits = 1),
                                                big.mark = ",")))
                      }),
               Metric_Unit = NULL)
      
      # Simpler code for fixing text NAs
      # NOTE: Roll this into the above dplyr
      metrics_summary <- metrics_summary %>%
        mutate_all(function(x) str_replace(x,
                                           pattern = paste("NA",
                                                           "NaN",
                                                           "NA%",
                                                           "%NA",
                                                           "NaN%",
                                                           "$NaN",
                                                           sep = "|"),
                                           NA_character_))
      
      # Change this to dplyr logic using mutate_if or mutate_all
      metrics_summary[is.na(metrics_summary)] <- "-"
      
      # Reorder rows based on Summary Tab Metrics order
      metrics_summary <- metrics_summary %>%
        mutate(Metric_Name_Summary = factor(Metric_Name_Summary,
                                            levels = summary_metric_name_order,
                                            ordered = TRUE)) %>%
        arrange(Metric_Name_Summary) %>%
        mutate(Metric_Name_Summary = as.character(Metric_Name_Summary))
      
      # Code for comparing metrics to targets and displaying statuses ------------
      
      
      # Code for Status Section -----------------------------------
      # Crosswalk Current Period Summary with metrics to be displayed in Status section
      # current_status <- left_join(current_summary_data,
      #                             status_section_metrics,
      #                             by = c)
      # Not sure if we actually need to do this anymore since we are already
      # filtering out metrics based on whether or not they are included in the
      # summary and site tabs and then filtering on whether or not there is a status
      # NOTE: Revisit this after reviewing which services/metrics are shown in summary tab but not in the status section
      
      
      current_status <- current_summary_data %>%
        mutate(TestCol = paste(Metric_Group, Metric_Name),
               Incl = TestCol %in% unique(
                 paste(status_section_metrics$Metric_Group,
                       status_section_metrics$Metric_Name))) %>%
        filter(Incl) %>%
        select(-TestCol, -Incl)

      # Manually remove duplicates - this should be fixed in the repositories
      current_status <- unique(current_status)
      
      current_status <- current_status %>%
        ungroup() %>%
        filter(!is.na(Status)) %>%
        mutate(`Current Period` = ifelse(
          str_detect(Premier_Reporting_Period, "/"), 
          paste0("Rep. Pd. Ending ",
                 Premier_Reporting_Period),
          Premier_Reporting_Period)) %>%
        select(Metric_Group,
               Metric_Name_Summary,
               `Current Period`,
               Site,
               Status) %>%
        mutate(Section = "Status") %>%
        relocate(Section) %>%
        pivot_wider(names_from = Site,
                    values_from = Status)
      
      check <- target_mapping %>% filter(Service == service_input)
      if("Total Revenue to Budget Variance" %in% as.vector(check$Metric_Group)){
        
        current_month <- current_summary_data %>% filter(Metric_Name == "Variance to Budget")
        current_month <- unique(current_month$Reporting_Month_Ref)
        
        budget_to_variance <- metrics_final_df %>% filter(Metric_Name %in% c("Variance to Budget", "Budget")) %>% filter(Reporting_Month_Ref == current_month) %>%
          group_by(Service, Site, Metric_Group, Metric_Name) %>%
          summarise(value_rounded = sum(value_rounded)) %>%
          pivot_wider(names_from = "Metric_Name",
                      values_from = "value_rounded") %>%
          mutate(Target = round(`Variance to Budget`/ Budget,2),
                 Status = ifelse(Target <= 0, "Green", ifelse(Target > 0.02, "Red", "Yellow"))) %>%
          pivot_longer(4:5,
                       names_to = "Summary_Metric_Name",
                       values_to = "value_rounded") %>%
          filter(Summary_Metric_Name == "Variance to Budget") %>%
          ungroup()
        
        
        current_period_budget <- current_summary_data %>% filter(Metric_Name_Summary == "Total Revenue to Budget Variance")
        current_period_budget <- unique(current_period_budget$Premier_Reporting_Period)
        
        budget_to_variance <- budget_to_variance %>% select(-Service, -Summary_Metric_Name, -Target, -value_rounded) %>% mutate(Metric_Name_Summary = "Total Revenue to Budget Variance", 
                                                                                                                                Section = "Status",
                                                                                                                                `Current Period` = current_period_budget) %>% 
          pivot_wider(names_from = "Site", values_from = "Status")
        
        current_status <- bind_rows(current_status, budget_to_variance)
      }
      
      
      if("Budget to Actual" %in% current_summary_data$Metric_Group) {
        current_status_budget <- current_summary_data %>%
          filter(Metric_Group == "Budget to Actual")
        
        budget_target_current <- get_budget_data(service = service_input,month_input)
        
        if (as.character(month_selected )%in% month_in_repo) {
          budget_target_current <- budget_target_current %>% ungroup() %>% filter(Service %in% service_input, Month == month_selected, Metric_Name_Submitted == "Budget_Total") %>%
            select(-Service, -Month) %>% mutate(Metric_Name_Submitted = "Budget to Actual Variance - Total")
        } else {
          budget_target_current <- budget_target_current %>% ungroup() %>% filter(Service %in% service_input, Month == max(Month), Metric_Name_Submitted == "Budget_Total") %>%
            select(-Service, -Month) %>% mutate(Metric_Name_Submitted = "Budget to Actual Variance - Total")
        }
        
        budget_target_current <- left_join(current_status_budget, budget_target_current) 
        
        budget_target_current <- budget_target_current %>% select(-Target) %>%
          rename(Target = Value) %>%
          mutate(Target = value_rounded/Target,
                 Status = ifelse(Target >= 0, "Green", ifelse(Target < -0.02, "Red", "Yellow"))) %>%
          filter(!is.na(Target)) %>%
          mutate(Section = "Status",
                 `Current Period` = ifelse(
                   str_detect(Premier_Reporting_Period, "/"), 
                   paste0("Rep. Pd. Ending ",
                          Premier_Reporting_Period),
                   Premier_Reporting_Period)) %>%
          ungroup() %>%
          select(Metric_Group, Section, Metric_Name_Summary, `Current Period`, Site, Status) %>%
          relocate(Section) %>%
          pivot_wider(names_from = Site,
                      values_from = Status)
        
        current_status <- full_join(current_status, budget_target_current)
        
      }
      
      # Find and add any missing sites
      missing_sites <- setdiff(sites_inc, names(current_status))
      current_status[missing_sites] <- NA
        

      # FYTD Summary with status indicators using new structure     
      fytd_status <- left_join(fytd_merged,
                                metric_targets_status,
                                by = c("Site",
                                       "Metric_Group",
                                       "Metric_Name"))
      
      # Determine status definitions for FYTD metrics
      fytd_status <- fytd_status %>%
        # Determine status based on status definitions
        mutate(Status = ifelse(is.na(Target), NA,
                               ifelse(between(value_rounded,
                                              Green_Start,
                                              Green_End),
                                      "Green",
                                      ifelse(between(value_rounded,
                                                     Yellow_Start,
                                                     Yellow_End),
                                             "Yellow",
                                             ifelse(between(value_rounded,
                                                            Red_Start,
                                                            Red_End),
                                                    "Red", NA))))) %>%
        filter(!is.na(Target)) %>%
        mutate(Section = "Status") %>%
        select(Section, Metric_Name_Summary, `Fiscal Year to Date`, Site, Status)
      
      
      budget_target_check <- target_mapping %>%
        select(-contains("Status")) %>%
        filter(Target %in% c("Budget")) %>%
        filter(Service %in% service_input) %>%
        select(Service, Metric_Group, Metric_Name) %>%
        distinct()
            
      # NOTE: Should we use a str_detect instead here so we can capture Labor and Non Labor as well?
      if("Budget to Actual" %in% current_summary_data$Metric_Group){
        
        fytd_status_budget <- left_join(fytd_merged,
                                 metric_targets_status,
                                 by = c("Site",
                                        "Metric_Group",
                                        "Metric_Name"))
        
        fytd_status_budget <- fytd_status_budget %>%
                              filter(Metric_Group == "Budget to Actual") %>%
                              mutate(Service = service_input,
                                     Metric_Name_Submitted = "Budget to Actual Variance - Total") %>%
                              select(-value_rounded)
        
        
        budget_actual <- get_budget_data(service = service_input,month_input)
        
        if (as.character(month_selected )%in% month_in_repo) {
          budget_actual <- budget_actual %>% ungroup() %>% filter(Service %in% service_input, Month == month_selected, Metric_Name_Submitted == "Budget to Actual Variance - Total") %>%
            select(-Service, -Month, -Value_ytd) %>% rename(value_rounded = Value)
        } else {
          budget_actual <- budget_actual %>% ungroup() %>% filter(Service %in% service_input, Month == max(Month), Metric_Name_Submitted == "Budget to Actual Variance - Total") %>%
            select(-Service, -Month, -Value_ytd) %>% rename(value_rounded = Value)
        }
        fytd_status_budget <- left_join(fytd_status_budget, budget_actual)
        
        
        budget_target <- get_budget_data(service = service_input,month_input)
        
        if (as.character(month_selected )%in% month_in_repo) {
          budget_target <- budget_target %>% ungroup() %>% filter(Service %in% service_input, Month == month_selected, Metric_Name_Submitted == "Budget_Total") %>%
            select(-Service, -Month, -Value) %>% mutate(Metric_Name_Submitted = "Budget to Actual Variance - Total")
        } else {
          budget_target <- budget_target %>% ungroup() %>% filter(Service %in% service_input, Month == max(Month), Metric_Name_Submitted == "Budget_Total") %>%
            select(-Service, -Month, -Value) %>% mutate(Metric_Name_Submitted = "Budget to Actual Variance - Total")
        }
        
        budget_to_actual_target <- left_join(fytd_status_budget, budget_target)
        budget_to_actual_target <- budget_to_actual_target %>% select(-Target) %>%
                                    rename(Target = Value_ytd) %>%
          mutate(Target = value_rounded/Target,
                 Status = ifelse(Target >= 0, "Green", ifelse(Target < -0.02, "Red", "Yellow"))) %>%
          filter(!is.na(Target)) %>%
          mutate(Section = "Status") %>%
          select(Section, Metric_Name_Summary, `Fiscal Year to Date`, Site, Status)
        
        
        #Calculate FYTD Budget to Actual Targets
        # budget_to_actual_target <- metrics_final_df %>% 
        #   filter((Service == service_input) &
        #            (Metric_Name %in% c("Budget_Total", "Budget to Actual")) & 
        #            (Reporting_Month_Ref %in%
        #               unique((fytd_period %>% 
        #                         filter(Metric_Name == "Budget to Actual"))$Reporting_Month_Ref))) %>%
        #   group_by(Service, Site, Metric_Group, Metric_Name) %>%
        #   summarise(value_rounded = sum(value_rounded)) %>%
        #   pivot_wider(names_from = "Metric_Name",
        #               values_from = "value_rounded") %>%
        #   mutate(Target = round(`Budget to Actual`/ Budget_Total, 2),
        #          Status = ifelse(Target >= 0, "Green", ifelse(Target < -0.02, "Red", "Yellow"))) %>%
        #   pivot_longer(4:5,
        #                names_to = "Summary_Metric_Name",
        #                values_to = "value_rounded") %>%
        #   filter(Summary_Metric_Name == "Budget to Actual") %>%
        #   mutate(Section = "Status")
        # 
        # budget_to_actual_target <- budget_to_actual_target[,c("Section", "Summary_Metric_Name", "Site", "Status")]
        # budget_to_actual_target$`Fiscal Year to Date` <-
        #   fytd_status$`Fiscal Year to Date`[match(budget_to_actual_target$Summary_Metric_Name, fytd_status$Metric_Name_Summary)]
      }else{
        budget_to_actual_target <- NULL
      }
      check <- target_mapping %>% filter(Service == service_input)
      if("Total Revenue to Budget Variance" %in% as.vector(check$Metric_Group)){
        #Calculate FYTD Budget to Actual Targets
        variance_to_budget_target <- metrics_final_df %>% 
          filter((Service == service_input) &
                   (Metric_Group == "Total Revenue to Budget Variance") &
                   (Metric_Name %in% c("Budget", "Variance to Budget")) & 
                   (Reporting_Month_Ref %in%
                      unique((fytd_period %>% 
                                filter(Metric_Name == "Variance to Budget"))$Reporting_Month_Ref))) %>%
          group_by(Service, Site, Metric_Group, Metric_Name) %>%
          summarise(value_rounded = sum(value_rounded)) %>%
          pivot_wider(names_from = "Metric_Name",
                      values_from = "value_rounded") %>%
          mutate(Target = round(`Variance to Budget`/ Budget,2),
                 Status = ifelse(Target <= 0, "Green", ifelse(Target > 0.02, "Red", "Yellow"))) %>%
          pivot_longer(4:5,
                       names_to = "Summary_Metric_Name",
                       values_to = "value_rounded") %>%
          filter(Summary_Metric_Name == "Variance to Budget") %>%
          mutate(Section = "Status")
        
        
        
        #Calculate FYTD Budget to Actual Targets
        variance_to_budget_target_fytd <- metrics_final_df %>% 
          filter((Service == service_input) &
                   (Metric_Group == "Total Revenue to Budget Variance") &
                   (Metric_Name %in% c("Budget", "Variance to Budget")) & 
                   (Reporting_Month_Ref %in%
                      unique((fytd_period %>% 
                                filter(Metric_Name == "Variance to Budget"))$Reporting_Month_Ref)))
        
        
        min_date <- min(variance_to_budget_target_fytd$Reporting_Month_Ref)
        max_date <- max(variance_to_budget_target_fytd$Reporting_Month_Ref)
        
        variance_to_budget_target <- variance_to_budget_target[,c("Section", "Summary_Metric_Name", "Site", "Status")]
        variance_to_budget_target <- variance_to_budget_target %>% mutate(`Fiscal Year to Date` = paste0(format(min_date, "%b"), " - ", format(max_date, "%b"), " ", year(max_date), " Total"))
        variance_to_budget_target <- variance_to_budget_target %>% rename(Metric_Name_Summary = Summary_Metric_Name)
        variance_to_budget_target <- variance_to_budget_target %>% mutate(Metric_Name_Summary = ifelse(Metric_Name_Summary == "Variance to Budget", "Total Revenue to Budget Variance", Metric_Name_Summary))
        #variance_to_budget_target$`Fiscal Year to Date` <- fytd_status$`Fiscal Year to Date`[match(variance_to_budget_target$Metric_Name_Summary, fytd_status$Metric_Name_Summary)]
      } else{
        variance_to_budget_target <- NULL
      }
      
      
      # # Merge with rest of the metrics 
      # Merge FYTD metrics with budget metrics
      fytd_status <- bind_rows(fytd_status,
                               budget_to_actual_target,
                               variance_to_budget_target)

      # Pivot wider for dashboard format
      fytd_status <- fytd_status %>%
        pivot_wider(names_from = Site, values_from = Status)        
      
      # Identify missing sites and populate columns
      missing_sites <- setdiff(sites_inc, names(fytd_status))
      fytd_status[missing_sites] <- NA

      
      # Merge FYTD and Current Period Targets
      targets_summary <- left_join(fytd_status,
                                   current_status,
                                   by = c("Section", "Metric_Name_Summary"))

      # Remove Metric_Group column
      targets_summary <- targets_summary %>%
        select(-Metric_Group)
      
      # Use summary_metrics_name_order vector from early in this section to reorder rows
      targets_summary <- targets_summary[
        order(factor(targets_summary$Metric_Name_Summary,
                     levels = summary_metric_name_order,
                     ordered = TRUE)), ]
      
      targets_summary <- as.data.frame(targets_summary)

      # Create traffic lights for the metric statuses
      targets_summary <- targets_summary %>%
        mutate(across(.cols = everything(),
                      .fns = function(x) {
                        ifelse(x %in% "Red", paste0('<div style="text-align:center">',
                                                    '<span style="color:',
                                                    c("red"),'">',
                                                    fa('fas fa-circle'),
                                                    '</span>',
                                                    '</div>'),
                               ifelse(x %in% "Yellow", 
                                      paste0('<div style="text-align:center">',
                                             '<span style="color:',
                                             c("yellow"),'">',
                                             fa('fas fa-circle'),
                                             '</span>',
                                             '</div>'),
                                      ifelse(x %in% "Green",
                                             paste0('<div style="text-align:center">',
                                                    '<span style="color:',
                                                    c("green"),'">',
                                                    fa('fas fa-circle'),
                                                    '</span>',
                                                    '</div>'),
                                             ifelse(is.na(x),
                                                    paste0('<div style="text-align:center">',
                                                           '-',
                                                           '</div>'),
                                                    x))))
                      }
        ))
      #metrics_summary <- metrics_summary %>% filter(!(Metric_Name_Summary == "Total Revenue to Budget Variance"))
      
      summary_tab_tb <- rbind(metrics_summary, targets_summary) # Don't think we need to specify which columns anymore#2[,1:18])
      # Why do we need this? There is no NYEE, there is NYEE.x and NYEE.y
      #summary_tab_tb$NYEE <- NULL
      
      # Rename Metric_Name_Summary as Metric_Name
      summary_tab_tb <- summary_tab_tb %>%
        rename(Metric_Name = Metric_Name_Summary)
      
      colnames(summary_tab_tb) <- gsub("\\..*", " ", colnames(summary_tab_tb))
      colnames(summary_tab_tb) <- gsub("_", " ", colnames(summary_tab_tb), fixed = TRUE)
      
      ## Create table output headers
      header_above <- c(" " = 2, "ytd_header" = 7, " " = 1, "current_header" = 7)
      names(header_above) <- c(" ", "Year to Date", " ", "Current Period")
      
      kable_col_names <- colnames(summary_tab_tb)[2:length(summary_tab_tb)]
     
      
      if(service_input == "Imaging"){
          ir_start <- which(summary_tab_tb$`Metric Name` == "Outpatient Cancellations (All)")[1]
          dr_start <- which(summary_tab_tb$`Metric Name` == "ED Head CT Without Contrast (Exam Code CTNHEAD0) - Ordered to Scan Completed, % <= 60m")[1]
        
          kable(summary_tab_tb[,2:length(summary_tab_tb)], escape = FALSE,
                col.names = kable_col_names) %>%
            pack_rows(index = table(summary_tab_tb$Section), label_row_css = "background-color: #212070; color: white;") %>%
            kable_styling(bootstrap_options = c("hover","bordered","striped"), full_width = FALSE,
                          position = "center", row_label_position = "c", font_size = 16) %>%
            # add_header_above(header_above,
            #                  font_size = 16, bold = TRUE, color = "white", background = c("white", "#d80b8c", "white", "#00AEEF")) %>%
            row_spec(0,  background = "#212070", color = "white") %>%
            column_spec(1, bold = TRUE) %>%
            column_spec(c(2, 10), italic = TRUE) %>%
            column_spec(3:9, background = "#fee7f5") %>%
            column_spec(11:17, background = "#E6F8FF") %>%
            group_rows(group_label = "Interventional Radiology", indent = FALSE,
                       start_row = ir_start,
                       end_row = dr_start-1,
                       label_row_css = "background-color: #212070; color: white;") %>%
            group_rows(group_label = "Diagnostic Radiology", indent = FALSE,
                       start_row = dr_start,
                       end_row = (dr_start + 1),
                       label_row_css = "background-color: #212070; color: white;")
      }else{
        kable(summary_tab_tb[,2:length(summary_tab_tb)], escape = FALSE,
              col.names = kable_col_names) %>%
          pack_rows(index = table(summary_tab_tb$Section), label_row_css = "background-color: #212070; color: white;") %>%
          kable_styling(bootstrap_options = c("hover","bordered","striped"), full_width = FALSE,
                        position = "center", row_label_position = "c", font_size = 16) %>%
          # add_header_above(header_above,
          #                  font_size = 16, bold = TRUE, color = "white", background = c("white", "#d80b8c", "white", "#00AEEF")) %>%
          row_spec(0,  background = "#212070", color = "white") %>%
          column_spec(1, bold = TRUE) %>%
          column_spec(c(2, 10), italic = TRUE) %>%
          column_spec(3:9, background = "#fee7f5") %>%
          column_spec(11:17, background = "#E6F8FF") 
      }
      
      
    }
    
    
    # 2. Site Comparison Tab Output -------------------------------------------------------------------------------
    output$siteComp_title <- renderText({

      input$submit_prod
      input$submit_engineering
      input$submit_finance
      input$submit_food
      input$submit_evs
      input$submit_imaging
      input$submit_ytd_pt_exp
      input$submit_monthly_pt_exp
      input$submit_biomeddi
      input$submit_biomedkpis
      input$submit_imagingct
      input$submit_lab_pt
      input$submit_lab_tat
      input$submit_pt_tat
      input$submit_sec_inc_rpts
      input$submit_sec_events
      input$submit_ed
      input$submit_nursing
      input$submit_finance_census
      input$submit_peri_op
      input$submit_case_management
      input$submit_cn
      
      input_service <- input$selectedService2
      conn <- dbConnect(odbc(), dsn)  
      time_df <- tbl(conn, "BSC_METRICS_FINAL_DF") %>% filter(SERVICE == input_service) %>% collect()
      dbDisconnect(conn)
      if(nrow(time_df) == 0){
        text = paste0("MSHS ",input_service, " Key Metric Rollup")
      }else{
        updated <- format(max(time_df$UPDATED_TIME, na.rm = TRUE), "%Y-%m-%d %I:%M %p")#, tz = "America/New_York")
        text = paste0("MSHS ",input_service, " Key Metric Rollup - Updated ",updated)
      }
      text
    })
    
    output$siteComp_table <- function(){
      input$submit_prod
      input$submit_engineering
      input$submit_finance
      input$submit_food
      input$submit_evs
      input$submit_imaging
      input$submit_ytd_pt_exp
      input$submit_monthly_pt_exp
      input$submit_biomeddi
      input$submit_biomedkpis
      input$submit_imagingct
      input$submit_lab_pt
      input$submit_lab_tat
      input$submit_pt_tat
      input$submit_sec_inc_rpts
      input$submit_sec_events
      input$submit_ed
      input$submit_nursing
      input$submit_finance_ot
      input$submit_finance_census
      input$submit_peri_op
      input$submit_case_management
      input$submit_cn
      
      
      service_input <- input$selectedService2
      month_input <- input$selectedMonth2
      site_input <- input$selectedCampus2
      
      metrics_final_df <- mdf_from_db(service_input, month_input)
# 
#       service_input <- "Case Management / Social Work"
#       month_input <- "03-2023"
#       site_input <- "MSH"

      
      validate(
        need(!(is.na(site_input)), "Please select at least one site")
      )

      # Code Starts ---------------------------------------------------------------------------------     
      summary_tab_metrics <- metric_mapping_summary_site %>%
        filter(Service == service_input) %>%
        select(-General_Group, -Display_Order)
      
      summary_metric_group_order <- unique(summary_tab_metrics$Metric_Group)
      
      summary_metric_name_order <- unique(summary_tab_metrics$Metric_Name_Summary)
      
      # Target mappings ---------------------
      # Subset target mapping to select Metric_Group and  Metric_Names to be 
      # displayed in status indicator section based on the selected service line
      status_section_metrics <- target_mapping_analysis %>%
        filter(Service %in% service_input) %>%
        select(Service, Metric_Group, Metric_Name) %>%
        distinct()
      
      # Subset target mapping to select Targets and Status Definitions for selected service line
      metric_targets_status <- target_mapping_analysis %>%
        filter(Service == service_input)
      
      current_period <- as.Date(fast_strptime(month_input, "%m-%Y"), "%Y-%m-%d")
      fiscal_year <- format(current_period,  "%Y")
      
      # Subset data for selected servicel line based on metrics to be included in Site tab
      data <- left_join(summary_tab_metrics,
                          metrics_final_df,
                          by = c("Service",
                                 "Metric_Group",
                                 "Metric_Name"))
      
      # Filter based on selected time period
      data <- data %>%
        filter(Site %in% site_input &
                 Reporting_Month_Ref <= current_period &
                 Reporting_Month_Ref >= current_period - months(11)) %>%
        distinct() %>%
        arrange(Site, Metric_Group, Metric_Name_Summary, Metric_Name, 
                desc(Reporting_Month_Ref)) %>%
        group_by(Site, Metric_Group, Metric_Name_Summary, Metric_Name) %>%
        mutate(id = row_number())
      
      # Crosswalk with metric targets and status definitions
      data <- left_join(data,
                        metric_targets_status,
                        by = c("Service",
                               "Site",
                               "Metric_Group",
                               "Metric_Name",
                               "Metric_Name_Submitted"))
      

      # Determine status based on status definitions
      data <- data %>%
        mutate(Status = ifelse(is.na(Target), NA,
                               ifelse(between(value_rounded,
                                              Green_Start, Green_End),
                                      "Green",
                                      ifelse(between(value_rounded,
                                                     Yellow_Start, Yellow_End),
                                             "Yellow",
                                             ifelse(between(value_rounded,
                                                            Red_Start, Red_End),
                                                    "Red", NA))))) %>%
        select(-contains(c("_Start", "_End")), -Metric_Name_Submitted,
               -Service)
      
      
      
      # Selected Month/Year Metric
      # Current Period Table
      current_breakdown <- data %>%
        ungroup() %>%
        filter(Reporting_Month_Ref == current_period) %>%
        mutate(`Current Period` = ifelse(str_detect(Premier_Reporting_Period,
                                                    "/"), 
                                         paste0("Rep. Pd. Ending ",
                                                Premier_Reporting_Period),
                                         Premier_Reporting_Period)) %>%
        select(Metric_Group, Metric_Name_Summary, Site,
               value_rounded, Status, Target, Metric_Unit)
      
      # # Not sure if this code is needed anymore
      # current_breakdown <- as.data.frame(current_breakdown)
      
      ## Create target traffic lights for current month metrics
      current_breakdown <- current_breakdown %>%
        ungroup() %>%
        mutate(Status = ifelse(Status %in% c("Red", "Yellow", "Green"),
                               paste0('<div style="text-align:center">',
                                      '<span style="color:',
                                      Status,'">',
                                      fa('fas fa-circle'),
                                      '</span>',
                                      '</div>'),
                               paste0('<div style="text-align:center">',
                                      '-',
                                      '</div>')))

      # Previous Months Summary
      ## Past 12 months of Summary 
      past_avg <- data %>%
        filter(id >= 2) %>%
        group_by(Metric_Group, Metric_Name_Summary, Site) %>%
        summarise(`Avg. of Past Months Shown` = round(
          mean(value_rounded, na.rm = TRUE), 2))

      ## Past 12 months of Breakout
      past_breakdown <- data %>%
        filter(id >= 2) %>%
        group_by(Metric_Group, Metric_Name_Summary,
                 Site, Reporting_Month_Ref) %>%
        summarise(value_rounded = round(
          mean(value_rounded, na.rm = TRUE), 2)) %>%
        arrange(Reporting_Month_Ref) %>%
        mutate(Reporting_Month_Ref = format(
          as.Date(Reporting_Month_Ref, format = "%Y-%m-%d"),"%b-%Y")) %>%
        pivot_wider(names_from = Reporting_Month_Ref,
                    values_from = value_rounded)
      
      # Merge Current and Previous Months Breakdown
      breakdown_all <- merge(current_breakdown, past_avg,
                             by = c("Metric_Group",
                                    "Metric_Name_Summary",
                                    "Site"))
      breakdown_all <- merge(breakdown_all, past_breakdown,
                             by = c("Metric_Group",
                                    "Metric_Name_Summary",
                                    "Site"))

      # Rename column "value rounded" column with current period selected
      names(breakdown_all)[names(breakdown_all) == 'value_rounded'] <-
        format(as.Date(current_period, format = "%Y-%m-%d"),"%b-%Y")

      breakdown_all <- breakdown_all %>%
        mutate(across(where(is.numeric),
                      .fns = function(x) {
                        ifelse(Metric_Unit %in% "Dollar",
                               dollar(round(x)),
                               ifelse(Metric_Unit %in% "Percent",
                                      percent(x, 0.1),
                                      prettyNum(round(x, digits = 1),
                                                big.mark = ",")))
                      }),
               Metric_Unit = NULL)
      
      
      # Create and Format Comparison Table
      # NOTE: Roll this into single dplyr with above
      breakdown_all <- breakdown_all %>%
        mutate_all(function(x) str_replace(x,
                                           pattern = paste("NA",
                                                           "NaN",
                                                           "NA%",
                                                           "%NA",
                                                           "NaN%",
                                                           "$NaN",
                                                           sep = "|"),
                                           NA_character_)) %>%
        replace(is.na(.), "-")
      
     
      
      breakdown_all <- breakdown_all %>%
        mutate(
          # Format budget related metrics
          Target = ifelse(Metric_Name_Summary %in% c("Total Revenue to Budget Variance"),
                          ">= Budget",
                          ifelse(Metric_Name_Summary %in% c("Budget to Actual"),
                                 "<= Budget", Target)),
          # Set columns as factors and rearrange dataframe
          Metric_Name_Summary = factor(
            Metric_Name_Summary,
            levels = summary_metric_name_order,
            ordered = TRUE)) %>%
        arrange(#Metric_Group,
                Metric_Name_Summary,
                Site) %>%
        mutate(#Metric_Group = as.character(Metric_Group),
               Metric_Name_Summary = as.character(Metric_Name_Summary))
      
      factor_ordering <- table(breakdown_all$Metric_Name_Summary)
      factor_ordering <- factor_ordering[order(factor(names(factor_ordering),
                                                      levels = summary_metric_name_order))]
      
      # NOTE: MAY BE WORTH REVIEWING THE LOGIC IN THESE LINES
      ## Get the months in the df
      month_included <- breakdown_all %>%
        select(-Metric_Name_Summary,
               -Metric_Group,
               -Site,
               -Status,
               -Target,
               -`Avg. of Past Months Shown`)
    
      original_columns <- as.Date(sprintf("%s-01",colnames(month_included)), format= "%b-%Y-%d")
      
      #Subtract 12 months from the latest month drop the day and add the first of the month back in
      latest_month_shown <- as.Date(
        paste0(format(original_columns[1] %m-% months(11), "%Y-%m"), "-01"),
        format = "%Y-%m-%d")
      
      columns_being_removed <- which(original_columns < latest_month_shown)
      columns_being_removed <- original_columns[columns_being_removed]
      columns_being_removed <- format(columns_being_removed, "%b-%Y")
      
      
      breakdown_all <- breakdown_all %>%
        select(-all_of(columns_being_removed))
      
      ### Add missing months
      months_breakdown <- breakdown_all %>%
        select(-Metric_Name_Summary,
               -Metric_Group,
               -Site,
               -Status,
               -Target,
               -`Avg. of Past Months Shown`)
      
      months_breakdown <- as.Date(sprintf("%s-01",colnames(months_breakdown)),
                                  "%b-%Y-%d")
      
      complete_months <- seq.Date(min(months_breakdown), max(months_breakdown),
                                  by= 'month')
      
      missing_months <- which(!(complete_months %in% months_breakdown))
      missing_months <- as.character(format(complete_months[missing_months],
                                            "%b-%Y"))
      
      breakdown_all[,missing_months] <- NA
      
      # Do we need any code to account for the order of the dates?
      
      #breakdown_all <- breakdown_all %>% relocate(`Aug-2021`, .before = `Mar-2021`) ##to test ordering
      
      subset_data <- breakdown_all[,8:ncol(breakdown_all)]
      
      date_names <- sprintf("%s-01",colnames(subset_data))
      colnames(subset_data) <- date_names
      
      dates_order <- as.Date(names(subset_data), format = "%b-%Y-%d")
      subset_data <- subset_data[order(dates_order)]
      
      
      breakdown_all <- breakdown_all[,1:7]
      breakdown_all <- bind_cols(breakdown_all,subset_data)
      
      breakdown_all_cols <- colnames(breakdown_all)[8:ncol(breakdown_all)]
      breakdown_all_cols <- format(as.Date(breakdown_all_cols, "%b-%Y-%d"), "%b-%Y")
      colnames(breakdown_all)[8:ncol(breakdown_all)] <- breakdown_all_cols
      
      options(knitr.kable.NA = '-')
      
      breakdown_all[,3:length(breakdown_all)] %>%
        kable(align = "l", escape = FALSE) %>%
         #pack_rows(index = table(breakdown_all$Metric_Group)[metric_group_order], label_row_css = "background-color: #212070; color: white;") %>%
        #pack_rows(index = table(breakdown_all$Summary_Metric_Name)[metric_name_order], label_row_css = "background-color: #212070; color: white;") %>%
        pack_rows(index = factor_ordering,
                  label_row_css = "background-color: #212070; color: white;") %>%
        kable_styling(bootstrap_options = c("hover","bordered","striped"),
                      full_width = FALSE,
                      position = "center",
                      row_label_position = "c",
                      font_size = 16) %>%
        add_header_above(c(" " = 1,
                           "Selected Month-Year" = 2,
                           " " = 2,
                           "Monthly Breakout (Shows Previous Periods)" = length(breakdown_all)-7),
                         font_size = 16,
                         bold = TRUE,
                         color = "white",
                         background = c("white", "#d80b8c", "white", "#00AEEF")) %>% 
        row_spec(0,  background = "#212070", color = "white") %>%
        column_spec(1, bold = TRUE) %>%
        column_spec(2:3, background = "#fee7f5", bold = TRUE) %>%
        column_spec(6:(length(breakdown_all)-2), 
                    background = "#E6F8FF")

    }
    
    
    # 3. Breakout Tab Output ----------------------------------------------------------------------------------
    output$siteBreakout_title <- renderText({
      
      input$submit_prod
      input$submit_engineering
      input$submit_finance
      input$submit_food
      input$submit_evs
      input$submit_imaging
      input$submit_ytd_pt_exp
      input$submit_monthly_pt_exp
      input$submit_biomeddi
      input$submit_biomedkpis
      input$submit_imagingct
      input$submit_lab_pt
      input$submit_lab_tat
      input$submit_pt_tat
      input$submit_sec_inc_rpts
      input$submit_sec_events
      input$submit_ed
      input$submit_nursing
      input$submit_finance_census
      input$submit_peri_op
      input$submit_case_management
      input$submit_cn
      
      input_service <- input$selectedService3
      conn <- dbConnect(odbc(), dsn)  
      time_df <- tbl(conn, "BSC_METRICS_FINAL_DF") %>% filter(SERVICE == input_service) %>% collect()
      dbDisconnect(conn)
      if(nrow(time_df) == 0){
        text = paste0(input$selectedCampus3, " ",input_service, " Breakout")
      }else{
        updated <- format(max(time_df$UPDATED_TIME, na.rm = TRUE), "%Y-%m-%d %I:%M %p")#, tz = "America/New_York")
        text = paste0(input$selectedCampus3," ",input_service, " Breakout - Updated ",updated)
      }
      text
    })
    
    output$siteBreakout_table <- function(){
      
      input$submit_prod
      input$submit_engineering
      input$submit_finance
      input$submit_food
      input$submit_evs
      input$submit_imaging
      input$submit_ytd_pt_exp
      input$submit_monthly_pt_exp
      input$submit_biomeddi
      input$submit_biomedkpis
      input$submit_imagingct
      input$submit_lab_pt
      input$submit_lab_tat
      input$submit_pt_tat
      input$submit_sec_inc_rpts
      input$submit_sec_events
      input$submit_ed
      input$submit_nursing
      input$submit_finance_ot
      input$submit_finance_census
      input$submit_peri_op
      input$submit_case_management
      input$submit_cn
            
      service_input <- input$selectedService3
      month_input <- input$selectedMonth3
      site_input <- input$selectedCampus3
      
      metrics_final_df <- mdf_from_db(service_input, month_input)

      # service_input <- "Engineering"
      # month_input <- "12-2021"
      # site_input <- "MSB"


      # Code Starts ---------------------------------------------------------------------------------     
      breakout_tab_metrics <- metric_mapping_breakout %>%
        filter(Service %in% service_input) %>%
        select(-General_Group, -Metric_Name_Summary, -Display_Order)
      
      # Create a data frame with units for each metric.
      # Crosswalk this after current period, past average, and past breakout are merged to ensure all
      # metrics with units are accurately represented
      breakout_tab_metrics_units <- breakout_tab_metrics %>%
        select(Metric_Group, Metric_Name, Metric_Unit)
      
      metric_group_order <- unique(breakout_tab_metrics$Metric_Group)
      
      metric_name_order <- unique(breakout_tab_metrics$Metric_Name)
      
      # Subset target mapping to select Targets and Status Definitions for selected service line
      metric_targets_status <- target_mapping_analysis %>%
        filter(Service %in% service_input)
      
      current_period <- as.Date(fast_strptime(month_input, "%m-%Y"), "%Y-%m-%d")
      fiscal_year <- format(current_period,  "%Y")
      
      # Try to do this the same was we do it in the Summary and Site tabs for consistency
      # First crosswalk metrics to include and metrics_final_df
      data <- left_join(breakout_tab_metrics,
                        metrics_final_df,
                        by = c("Service",
                               "Metric_Group",
                               "Metric_Name"))
      
      data <- data %>%
        filter(Service %in% service_input,
               Site %in% site_input,
               Reporting_Month_Ref <= current_period,
               Reporting_Month_Ref >= current_period - months(11)) %>%
        arrange(Site, Metric_Group, Metric_Name,
                desc(Reporting_Month_Ref)) %>%
        distinct()
      
      # Do we need this?
      # This will be needed until duplicate monthly entries are corrected in metrics_final_df
      months <- metrics_final_df %>% 
        filter(Service == service_input) %>% # input$selectedService
        filter(Site == site_input) %>%
        filter(Reporting_Month_Ref <= current_period) %>%
        distinct(Reporting_Month_Ref) %>%
        arrange(desc(Reporting_Month_Ref)) %>%
        mutate(id = row_number()) %>%
        filter(Reporting_Month_Ref >= current_period - months(11))
      
      data <- left_join(data, months,
                            by = "Reporting_Month_Ref")
      
      # Crosswalk with metric targets and determine status
      data <- left_join(data,
                        metric_targets_status,
                        by = c("Service",
                               "Site",
                               "Metric_Group",
                               "Metric_Name",
                               "Metric_Name_Submitted"))
      
      # Determine status based on status definitions
      data <- data %>%
        mutate(Status = ifelse(is.na(Target), NA,
                               ifelse(between(value_rounded,
                                              Green_Start, Green_End),
                                      "Green",
                                      ifelse(between(value_rounded,
                                                     Yellow_Start, Yellow_End),
                                             "Yellow",
                                             ifelse(between(value_rounded,
                                                            Red_Start, Red_End),
                                                    "Red", NA))))) %>%
        select(-contains(c("_Start", "_End")), -Metric_Name_Submitted)
      
      # # Selected Month/Year Metric
      # # Current Period Table
      current_site_breakdown_new <- data %>%
        filter(Reporting_Month_Ref == current_period) %>%
        select(Metric_Group, Metric_Name, Metric_Unit,
               value_rounded, Status, Target)
      
      # Convert to data frame for color formatting
      current_site_breakdown_new <- as.data.frame(current_site_breakdown_new)
      
      validate(
        need(nrow(current_site_breakdown_new) != 0, "Please choose a different site, the currently selected site does not have data asscoiated with it")
      )
      
      # ## Create target traffic lights for current month metrics
      # # Create traffic lights for the targets
      current_site_breakdown_new <- current_site_breakdown_new %>%
        mutate(Status = ifelse(Status %in% c("Red", "Yellow", "Green"),
                               paste0('<div style="text-align:center">',
                                      '<span style="color:',
                                      Status,'">',
                                      fa('fas fa-circle'),
                                      '</span>',
                                      '</div>'),
                               paste0('<div style="text-align:center">',
                                      '-',
                                      '</div>')))
      
      # Previous 11 Months Summary      
      past_avg_site_new <- data %>%
        filter(id >= 2) %>%
        group_by(Metric_Group, Metric_Name, Metric_Unit) %>%
        summarise(`Avg. of Past Months Shown` = mean(value_rounded,
                                                     na.rm = TRUE))
      
      
      # ## Breakdown of prior 11 months     
      past_site_breakdown_new <- data %>%
        filter(id >= 2) %>%
        group_by(Metric_Group, Metric_Name, Metric_Unit,
                 Reporting_Month_Ref) %>%
        summarise(value_rounded = mean(value_rounded, na.rm = TRUE)) %>%
        arrange(Reporting_Month_Ref) %>%
        mutate(Reporting_Month_Ref = format(
          as.Date(Reporting_Month_Ref, format = "%Y-%m-%d"),"%b-%Y")) %>%
        pivot_wider(names_from = Reporting_Month_Ref,
                    values_from = value_rounded)

      # Merge Current and Previous Months Breakdown
      # Combine current reporting period with average from prior 11 months
      breakdown_all_site_new <- merge(current_site_breakdown_new,
                                      past_avg_site_new,
                                      by = c("Metric_Group",
                                             "Metric_Name",
                                             "Metric_Unit"),
                                      all = TRUE)
      # # Combine with monthly breakdown from prior 11 months
      breakdown_all_site_new <- merge(breakdown_all_site_new,
                                      past_site_breakdown_new,
                                      by = c("Metric_Group",
                                             "Metric_Name",
                                             "Metric_Unit"),
                                      all = TRUE)
      
            
      # Rename value_rounded column with selected reporting period
      names(breakdown_all_site_new)[names(breakdown_all_site_new) == 'value_rounded'] <-
        format(as.Date(current_period, format = "%Y-%m-%d"),"%b-%Y")
      
      # Format units
      # Check this because we've renamed "Metric_Name_Submitted" as "Metric_Name". If these are not the same, no units are imported
      # breakdown_all_site <- merge(breakdown_all_site, metric_unit_filter,
      #                        by.x = c("Metric_Group","Metric_Name"),
      #                        by.y = c("Metric_Group","Metric_Name"),
      #                        all.x = TRUE)
      
      # Crosswalk with units and format appropriately
      # breakdown_all_site <- left_join(breakdown_all_site,
      #                                 metric_unit_filter_new,
      #                                 by = c("Metric_Group" = "Metric_Group",
      #                                        "Metric_Name" = "Metric_Name_Submitted"))
      
      # breakdown_all_site <- breakdown_all_site %>%
      #   mutate_if(is.numeric, funs(ifelse(is.na(Metric_Unit),
      #                                     prettyNum(round(.,1), big.mark = ','),
      #                                     ifelse(Metric_Unit == "Dollar",
      #                                            dollar(round(.)),
      #                                            percent(.,1))))) %>%
      #   select(-Metric_Unit)
      
      # 
      # breakdown_all_site$Metric_Unit <- NULL
      
      
      
      # Create and Format Comparison Table
      breakdown_all_site_new <- breakdown_all_site_new %>%
        mutate(
          # Format numbers based on metric unit
          across(where(is.numeric),
                 .fns = function(x) {
                   ifelse(Metric_Unit %in% "Dollar",
                          dollar(round(x)),
                          ifelse(Metric_Unit %in% "Percent",
                                 percent(x, 0.1),
                                 prettyNum(round(x, digits = 1),
                                           big.mark = ",")))
                      }),
          # Remove metric_unit column
          Metric_Unit = NULL,
          # Replace NAs
          across(.cols = everything(),
                 .fns = function(x) {
                   str_replace(x,
                               pattern = paste("NA",
                                               "NaN",
                                               "NA%",
                                               "%NA",
                                               "NaN%",
                                               "$NaN",
                                               sep = "|"),
                               NA_character_)
                   })) %>%
        replace(is.na(.), "-") %>%
        # Reorder rows based on Metric_Group and Metric_Name
        mutate(Metric_Group = factor(Metric_Group,
                                     levels = metric_group_order,
                                     ordered = TRUE),
               Metric_Name = factor(Metric_Name,
                                    levels = metric_name_order,
                                    ordered = TRUE)) %>%
        arrange(Metric_Group, Metric_Name) %>%
        mutate(Metric_Group = as.character(Metric_Group),
               Metric_Name = as.character(Metric_Name))

      
      # Manually update Targets for budget related metrics
      breakdown_all_site_new <- breakdown_all_site_new %>%
        mutate(Target = ifelse(Metric_Name %in% c("Variance to Budget"),
                               ">= Budget",
                               ifelse(str_detect(Metric_Name,
                                                 "(Budget to Actual)"),
                                      "<= Budget", Target)))
      
      # Determine order metrics should appear
      factor_ordering_new <- table(breakdown_all_site_new$Metric_Group)
      factor_ordering_new <- factor_ordering_new[order(
        factor(names(factor_ordering_new),
               levels = metric_group_order))]
      

      
      ## Get the months in the df
      month_included <- breakdown_all_site_new %>%
        select(-Metric_Name, -Metric_Group,
               -Status, -Target, -`Avg. of Past Months Shown`)

      original_columns <- as.Date(sprintf("%s-01",colnames(month_included)), format= "%b-%Y-%d")

      #Subtract 12 months from the latest month drop the day and add the first of the month back in
      # NOTE: IS THIS LATEST MONTH OR EARLIEST MONTH?
      latest_month_shown <- as.Date(paste0(
        format(original_columns[1] %m-% months(11), "%Y-%m"), "-01"),
        format = "%Y-%m-%d")

      columns_being_removed <- which(original_columns < latest_month_shown)
      columns_being_removed <- original_columns[columns_being_removed]
      columns_being_removed <- format(columns_being_removed, "%b-%Y")


      breakdown_all_site_new <- breakdown_all_site_new %>%
        select(-all_of(columns_being_removed))

      ### Add missing months
      months_breakdown <-  breakdown_all_site_new %>%
        select(-Metric_Name, -Metric_Group,
               -Status, -Target, -`Avg. of Past Months Shown`)
      
      months_breakdown <- as.Date(sprintf("%s-01", colnames(months_breakdown)),
                                  "%b-%Y-%d")

      complete_months <- seq.Date(min(months_breakdown), max(months_breakdown), by= 'month')

      missing_months <- which(!(complete_months %in% months_breakdown))
      missing_months <- as.character(format(complete_months[missing_months], "%b-%Y"))
      
      # Do we need to do anything to format the order this occurs in?
      breakdown_all_site_new[, missing_months] <- NA

      #breakdown_all <- breakdown_all %>% relocate(`Aug-2021`, .before = `Mar-2021`) ##to test ordering
      
      # QUESTION: Can any of this be simplified?
      subset_data <- breakdown_all_site_new[, 8:ncol(breakdown_all_site_new)]

      date_names <- sprintf("%s-01",colnames(subset_data))
      colnames(subset_data) <- date_names

      dates_order <- as.Date(names(subset_data), format = "%b-%Y-%d")
      subset_data <- subset_data[order(dates_order)]


      breakdown_all_site_new <- breakdown_all_site_new[, 1:7]
      breakdown_all_site_new <- bind_cols(breakdown_all_site_new, subset_data)

      breakdown_all_cols <- colnames(breakdown_all_site_new)[8:ncol(breakdown_all_site_new)]
      breakdown_all_cols <- format(as.Date(breakdown_all_cols, "%b-%Y-%d"), "%b-%Y")
      colnames(breakdown_all_site_new)[8:ncol(breakdown_all_site_new)] <- breakdown_all_cols
      
      breakdown_all_site_new <- breakdown_all_site_new %>%
        rename(Metric = Metric_Name)
      
      breakdown_all_site_new[, 2:length(breakdown_all_site_new)] %>%
        kable(align = "l", escape = FALSE) %>%
        # pack_rows(index = table(breakdown_all_site$Metric_Group)[metric_group_order], label_row_css = "background-color: #212070; color: white;") %>%
        #pack_rows(index = table(breakdown_all_site$Metric_Group)[metric_group_order], label_row_css = "background-color: #212070; color: white;") %>%
        pack_rows(index = factor_ordering_new,
                  label_row_css = "background-color: #212070; color: white;") %>%
        kable_styling(bootstrap_options = c("hover","bordered","striped"),
                      full_width = FALSE,
                      position = "center",
                      row_label_position = "c",
                      font_size = 16) %>%
        add_header_above(c(" " = 1,
                           "Selected Month-Year" = 2,
                           " " = 2,
                           "Monthly Breakout (Shows Previous Periods)" = length(breakdown_all_site_new)-6),
                         font_size = 16,
                         bold = TRUE,
                         color = "white",
                         background = c("white", "#d80b8c", "white", "#00AEEF")) %>% 
        row_spec(0, background = "#212070", color = "white") %>%
        column_spec(1, bold = TRUE) %>%
        column_spec(2:3, background = "#fee7f5", bold = TRUE) %>%
        column_spec(6:(length(breakdown_all_site_new) - 1), 
                    background = "#E6F8FF")
      
    }
    
    
    # 4. Data Tab Output ---------------------------------------------------------------------------------
    observeEvent(input$submit_finance,{
      button_name <- "submit_finance"
      shinyjs::disable(button_name)
      inFile_budget <- input$finance_budget
      flag <- 0
      
      if(input$name_finance == ""){
        showModal(modalDialog(
          title = "Error",
          paste0("Please fill in the required fields"),
          easyClose = TRUE,
          footer = NULL
        ))
      }else{
        updated_user <- input$name_finance
        file_path <- inFile_budget$datapath
        tryCatch({data <- read_excel(file_path, sheet = "5-BSC Cost Center Detail", skip = 3,
                                     col_types = c("text", "text", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "text"))
        flag <- 1
        },
        error = function(err){  showModal(modalDialog(
          title = "Error",
          paste0("There seems to be an issue with the budget file."),
          easyClose = TRUE,
          footer = NULL
        ))
          shinyjs::enable(button_name)
        })
      }
      
      if(flag == 1){
        # Process the data into standar Summary Repo format
        tryCatch({budget_process <- budget_raw_file_process(data, updated_user)
        flag <- 2
        
        },
        error = function(err){  showModal(modalDialog(
          title = "Error",
          paste0("There seems to be an issue with the budget file."),
          easyClose = TRUE,
          footer = NULL
        ))
          shinyjs::enable(button_name)
        })
      }
      
      
      if(flag == 2){
        ##Compare submitted results to what is in the Summary Repo in db and return only updated rows
        budget_data <- file_return_updated_rows(budget_process)
        
        #wirte the updated data to the Summary Repo in the server
        write_temporary_table_to_database_and_merge(budget_data,
                                                    "TEMP_BUDGET", button_name)
        
        update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                  input$selectedService3)
      }
      shinyjs::enable(button_name)
      
    })
    
    ## Read in Patient Experience data -----------------------------------
    # ED Monthly Data Observe Event -------------------
    observeEvent(input$submit_monthly_pt_exp, {
      # Name ED monthly data
      ed_monthly <- input$pt_exp_ed_monthly
      button <- input$submit_monthly_pt_exp
      name <- input$name_monthly_patient_experience
      pt_exp_server_function(button, ed_monthly, "ED", name, "Monthly")
      
      update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                input$selectedService3)
      
    })
    
    # Nursing Patient Experience Monthly Data Observe Event -------------------
    observeEvent(input$submit_monthly_pt_exp, {
      # Name Nursing monthly data
      nursing_monthly <- input$pt_exp_nursing_monthly
      button <- input$submit_monthly_pt_exp
      name <- input$name_monthly_patient_experience
      
      pt_exp_server_function(button, nursing_monthly, "Nursing", name, "Monthly")
      update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                input$selectedService3)
        
  
    })
    
    # Support Services Patient Experience Monthly Data Observe Event -------------------
    observeEvent(input$submit_monthly_pt_exp, {
      # Name Support Services monthly data
      support_monthly <- input$pt_exp_support_monthly
      button <- input$submit_monthly_pt_exp
      name <- input$name_monthly_patient_experience
      
      pt_exp_server_function(button, support_monthly, "Support Services", name, "Monthly")
      update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                input$selectedService3)
    })

    # ED YTD Data Observe Event -------------------
    observeEvent(input$submit_ytd_pt_exp, {
      button <- "submit_ytd_pt_exp"
      # Name ED YTD data
      ed_ytd <- input$pt_exp_ed_ytd
      name <- input$name_ytd_patient_experience
      
      pt_exp_server_function(button, ed_ytd, "ED", name, "YTD")
      update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                input$selectedService3)
     
      
    })
    
    # Nursing YTD Data Observe Event -------------------
    observeEvent(input$submit_ytd_pt_exp, {
      button <- "submit_ytd_pt_exp"
      # Name Nursing YTD data
      nursing_ytd <- input$pt_exp_nursing_ytd
      name <- input$name_ytd_patient_experience
      
      pt_exp_server_function(button, nursing_ytd, "Nursing", name, "YTD")
      update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                input$selectedService3)
      
      
    })
    
    # Support Services YTD Data Observe Event -------------------
    observeEvent(input$submit_ytd_pt_exp, {
      button <- "submit_ytd_pt_exp"
      # Name Support Services YTD data
      support_ytd <- input$pt_exp_support_ytd
      name <- input$name_ytd_patient_experience
      
      pt_exp_server_function(button, support_ytd, "Support Services", name, "YTD")
      update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                input$selectedService3)
      

    })
    
    
    ## Productivity data --------------------------------
    ## Read in productivity data and process
    observeEvent(input$submit_prod,{
      button_name <- "submit_prod"
      shinyjs::disable(button_name)
      inFile <- input$productiviy_data
      flag <- 0
      #data <- read_excel(inFile$datapath)


      if(input$name_productivity == ""){
        showModal(modalDialog(
          title = "Error",
          paste0("Please fill in the required fields"),
          easyClose = TRUE,
          footer = NULL
        ))
        shinyjs::enable(button_name)
        
      }
      else{
        updated_user <- input$name_productivity
        productivity_filepath <- inFile$datapath
        #imaging_filepath <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/Imaging/FTI-BalancedScorecard-2021-Jan1-Nov30 (1).xlsx"
        tryCatch({data <- read_excel(productivity_filepath)
        flag <- 1
        }, error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with the Productivity file."),
            easyClose = TRUE,
            footer = NULL
          ))
          shinyjs::enable(button_name)
          
          })
      }

      if(flag == 1){
        # Process Imaging data
        tryCatch({prod_summary <- productivity_dept_summary(data, updated_user)
        flag <- 2
        }, error = function(err){  showModal(modalDialog(
          title = "Error",
          paste0("There seems to be an issue with the Productivity file."),
          easyClose = TRUE,
          footer = NULL
        ))
          shinyjs::enable(button_name)
          
          })
      }

      if(flag == 2){
        prod_summary <- prod_summary %>% filter(REPORTING_MONTH >= max(REPORTING_MONTH) %m-% months(1))
        ##Compare submitted results to what is in the Summary Repo in db and return only updated rows
        productivity_new_data <- file_return_updated_rows(prod_summary)

        #wirte the updated data to the Summary Repo in the server
        write_temporary_table_to_database_and_merge(productivity_new_data,
                                                    "TEMP_PRODUCTIVITY", button_name)

        update_picker_choices_sql(session, input$selectedService, input$selectedService2,
                                  input$selectedService3)
        
        shinyjs::enable(button_name)

      }
      
    })
    
    
    # Submit Food Services -----
    observeEvent(input$submit_food,{
      button_name <- "submit_food"
      shinyjs::disable(button_name)
      food_file <- input$food_cost_and_revenue
      flag <- 0
      
      if(input$name_food == ""){
        showModal(modalDialog(
          title = "Error",
          paste0("Please fill in the required fields"),
          easyClose = TRUE,
          footer = NULL
        ))
      }else{
        food_file_path <- food_file$datapath
        updated_user <- input$name_food
        #food_file_path <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/Food/MSHS Workforce Data Request_Food_RecurringRequest 2021_Oct21.xlsx"
        tryCatch({food_data <- read_excel(food_file_path, sheet = "Cost and Revenue")
        food_data_rev <- read_excel(food_file_path, sheet = "Rev Budget")
        flag <- 1
        }, error = function(err){  showModal(modalDialog(
          title = "Error",
          paste0("There seems to be an issue with one of the files"),
          easyClose = TRUE,
          footer = NULL
        ))
          shinyjs::enable(button_name)
          })
      }
      
      if (flag == 1){
        # Process Cost and Revenue data
        tryCatch({food_summary_data <- cost_and_revenue_dept_summary(food_data)
        food_summary_budget <- rev_budget_dept_summary(food_data_rev)
        
        food_summary_data <- cost_budget_combine(food_summary_data,food_summary_budget)
        food_summary_data <- food_summary_repo_format(food_summary_data, updated_user)
        flag <- 2
        }, error = function(err){  showModal(modalDialog(
          title = "Error",
          paste0("There seems to be an issue with one of the files"),
          easyClose = TRUE,
          footer = NULL
        ))
          shinyjs::enable(button_name)
          })
      }
      
      if (flag == 2){
        ##Compare submitted results to what is in the Summary Repo in db and return only updated rows
        food_summary_data <- file_return_updated_rows(food_summary_data)
        
        #wirte the updated data to the Summary Repo in the server
        write_temporary_table_to_database_and_merge(food_summary_data,
                                                    "TEMP_FOOD", button_name)
        
        update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                  input$selectedService3)
        shinyjs::enable(button_name)
        
      }
      
    })
    
    # Submit Interventional Radiology -----
    observeEvent(input$submit_imaging, {
      button_name <- "submit_imaging"
      shinyjs::disable(button_name)
      imaging_file <- input$imaging_IR
      flag <- 0
      
      if(input$imaging_ir_username == ""){
        showModal(modalDialog(
          title = "Error",
          paste0("Please fill in the required fields"),
          easyClose = TRUE,
          footer = NULL
        ))
      }
      else{
        updated_user <- input$imaging_ir_username
        imaging_filepath <- imaging_file$datapath
        #imaging_filepath <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/Imaging/FTI-BalancedScorecard-2021-Jan1-Nov30 (1).xlsx"
        tryCatch({imaging_data <- read_excel(imaging_filepath)
            flag <- 1
        }, error = function(err){  
          showModal(modalDialog(
          title = "Error",
          paste0("There seems to be an issue with the Imaging file."),
          easyClose = TRUE,
          footer = NULL
        ))
          shinyjs::enable(button_name)
          })
      }

      if(flag == 1){
        # Process Imaging data
        tryCatch({imaging_summary_data <- imaging_dept_summary(imaging_data, updated_user)
                    flag <- 2
        }, error = function(err){  showModal(modalDialog(
          title = "Error",
          paste0("There seems to be an issue with the Imaging file."),
          easyClose = TRUE,
          footer = NULL
        ))
          shinyjs::enable(button_name)
          })
      }
      
      if(flag == 2){
        
        ##Compare submitted results to what is in the Summary Repo in db and return only updated rows
        imaging_new_data <- file_return_updated_rows(imaging_summary_data)
        
        #wirte the updated data to the Summary Repo in the server
        write_temporary_table_to_database_and_merge(imaging_new_data,
                                                    "TEMP_IMAGING", button_name)
        
        update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                  input$selectedService3)
        
        shinyjs::enable(button_name)
      
        
      }
      
    })
    
    # Submit Engineering -----
    observeEvent(input$submit_engineering,{
      button_name <- "submit_engineering"
      shinyjs::disable(button_name)
      flag <- 0
      if(input$name_engineering_kpi == ""){
        showModal(modalDialog(
          title = "Error",
          paste0("Please fill in the required fields"),
          easyClose = TRUE,
          footer = NULL
        ))
      }else {
        updated_user <- input$name_engineering_kpi
        tryCatch({
          # Convert rhandsontable to R object
          engineering_manual_updates <- hot_to_r(input$engineering_kpi)
          # Identify columns with no data in them and remove before further processing
          engineering_manual_updates <- remove_empty_manual_columns(engineering_manual_updates)  
          flag <- 1
        },
        error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with the Engineering data entered"),
            easyClose = TRUE,
            footer = NULL
          ))
          shinyjs::enable(button_name)
        })
        
        if (flag == 1) {
          
          #removing the metrics that can have numbers greater than 1
          engineering_manual_updates_check <- engineering_manual_updates %>% 
                                              filter(!(Metric %in% c("Total Critical PMs", 
                                                                     "Number of Work Orders Created with a Life Safety Priority", 
                                                                     "EOC/Patient Care Work Orders Received"
                                                                     )
                                                       )
                                                     ) 
          #check if metrics have not been entered as decimals 
          user_format_error <- manual_format_check(engineering_manual_updates_check)
          
          if (user_format_error) {
            
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with the data entered. Data should be entered as a decimal between 0 and 1."),
              easyClose = TRUE,
              footer = NULL
              )
            )
            
          } else {
            ## Updated rows returns flag and the processed updated rows by comparing what is currently in the summary repo
            updated_rows <- manual_process_and_return_updates(engineering_manual_updates, 
                                                              "Engineering", 
                                                              "cm_kpi", 
                                                              updated_user, button_name)


            if(updated_rows$flag == 2) {
              ##Updated the data on the databse
              write_temporary_table_to_database_and_merge(updated_rows$updated_rows,
                                                          "TEMP_ENGINEERING", button_name)              
              update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                        input$selectedService3)
              
            }
            
          }
          
        }
        
      }
      shinyjs::enable(button_name)
      

    
    })
    # Engineering Reactive dataset -----
    data_engineering_kpi <- reactive({
      
      data <- sql_manual_table_output("Engineering", "cm_kpi")
      # Arrange by sites in alphabetical order
      data <- data %>%
        arrange(Site)      
      
      data <- manual_table_month_order(data)
      
      
      
      
    })
    
    # Engineering Rhandsontable ------ 
    output$engineering_kpi <- renderRHandsontable({
      #data <- data
      data <- data_engineering_kpi()
      
      
      
      unique_sites <- unique(data$Site)
      site_1 <- which(data$Site == unique_sites[1])
      site_2 <- which(data$Site == unique_sites[2])
      site_3 <- which(data$Site == unique_sites[3])
      site_4 <- which(data$Site == unique_sites[4])
      site_5 <- which(data$Site == unique_sites[5])
      site_6 <- which(data$Site == unique_sites[6])
      site_7 <- which(data$Site == unique_sites[7])
      
      
      rendederer_string <- "
    function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.NumericRenderer.apply(this, arguments);

      if (instance.params) {
            hcols = instance.params.col_highlight;
            hcols = hcols instanceof Array ? hcols : [hcols];
          }

      if (instance.params && hcols.includes(col)) {
        td.style.background = '#EEEDE7';
      }
  }"
      
      
      col_highlight <- ncol(data) - 1
      
      
      rhandsontable(data, overflow= 'visible', col_highlight = col_highlight, rowHeaders = FALSE, readOnly = FALSE) %>%
        hot_table(mergeCells = list(
          list(row = min(site_1)-1, col = 0, rowspan = length(site_1), colspan = 1),
          list(row = min(site_2)-1, col = 0, rowspan = length(site_2), colspan = 1),
          list(row = min(site_3)-1, col = 0, rowspan = length(site_3), colspan = 1),
          list(row = min(site_4)-1, col = 0, rowspan = length(site_4), colspan = 1),
          list(row = min(site_5)-1, col = 0, rowspan = length(site_5), colspan = 1),
          list(row = min(site_6)-1, col = 0, rowspan = length(site_6), colspan = 1),
          list(row = min(site_7)-1, col = 0, rowspan = length(site_7), colspan = 1)
        )) %>%
        hot_cols(renderer = rendederer_string)  %>%
        hot_col(1:3, readOnly = T)
    })
    
    #Submit Environemental Services -----
    observeEvent(input$submit_evs,{
      button_name <- "submit_evs"
      shinyjs::disable(button_name)
      flag <- 0
      evs_file <- input$evs_data
      
      if(input$name_evs == ""){
        showModal(modalDialog(
          title = "Error",
          paste0("Please fill in the required fields"),
          easyClose = TRUE,
          footer = NULL
        ))
      }else{
        updated_user <- input$name_evs
        file_path <- evs_file$datapath
        #file_path <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/EVS/MSHS Normal Clean vs Iso Clean TAT July 2022.xlsx"
        tryCatch({evs_data <- read_excel(file_path)
                  month <- excel_sheets(file_path)[1]
                  flag <- 1
        },
        error = function(err){  showModal(modalDialog(
          title = "Error",
          paste0("There seems to be an issue with the enviromental services file."),
          easyClose = TRUE,
          footer = NULL
        ))
          shinyjs::enable(button_name)
          })
      }
      
      if(flag == 1){
        # Process the data into standar Summary Repo format
      tryCatch({evs_data <- evs_file_process(evs_data, month, updated_user)
                  flag <- 2
        
      },
        error = function(err){  showModal(modalDialog(
          title = "Error",
          paste0("There seems to be an issue with the enviromental services file."),
          easyClose = TRUE,
          footer = NULL
        ))
          shinyjs::enable(button_name)
          })
      }
      

      if(flag == 2){
        ##Compare submitted results to what is in the Summary Repo in db and return only updated rows
        evs_data <- file_return_updated_rows(evs_data)
    
        #wirte the updated data to the Summary Repo in the server
        write_temporary_table_to_database_and_merge(evs_data,
                                                    "TEMP_EVS", button_name)
        
        update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                  input$selectedService3)
      }
      shinyjs::enable(button_name)
      
    })
    

   

   
    # Lab KPI - Turnaround Time ------------
    # SCC Data submission -----------------
    observeEvent(input$submit_lab_tat,{
      button_name <- "submit_lab_tat"
      shinyjs::disable(button_name)
      
      flag <- 0

      # Name SCC file
      scc_file <- input$lab_scc
      
      if (is.null(scc_file)) {
        return(NULL)
      }else{
        
        if(input$lab_tat_username == "") {
          showModal(modalDialog(
            title = "Error",
            "Please fill in the required fields",
            easyClose = TRUE,
            footer = NULL
          ))
        } else {
          
          updated_user <- input$lab_tat_username
          
          scc_file_path <- scc_file$datapath
          # scc_file_path <- paste0("J:/deans/Presidents/HSPI-PM",
          #                         "/Operations Analytics and Optimization",
          #                         "/Projects/System Operations",
          #                         "/Balanced Scorecards Automation",
          #                         "/Data_Dashboard/Input Data Raw",
          #                         "/Lab & Blood Bank/SCC",
          #                         "/SCC HGB Report Mar 2022.xlsx")

          # Try catch statement to ensure file type is correct
          tryCatch({
            # Read in SCC file
            scc_data <- read_excel(scc_file_path)
            
            flag <- 1
            
          },
          
          error = function(err){
            showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with this SCC file."),
            easyClose = TRUE,
            footer = NULL
          ))
            shinyjs::enable(button_name)
          }
          )
        }
      }
      
      # Process data if the right file format was submitted
      if(flag == 1) {
        tryCatch({
          # Process SCC data
          scc_summary_data <- lab_scc_tat_dept_summary(scc_data, updated_user)
          
          flag <- 2
          
          # showModal(modalDialog(
          #   title = "Success",
          #   paste0("This SCC data has been imported successfully."),
          #   easyClose = TRUE,
          #   footer = NULL
          # ))
        },
        error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with this SCC file."),
            easyClose = TRUE,
            footer = NULL
          ))
          shinyjs::enable(button_name)
          
        })
      }
      
      if(flag == 2){
        
        write_temporary_table_to_database_and_merge(scc_summary_data,
                                                    "TEMP_SCC_TAT", button_name)
        
        update_picker_choices_sql(session, input$selectedService, input$selectedService2, input$selectedService3)
        
      }
      shinyjs::enable(button_name)
      
    }
    )
    
    # Sunquest data submission -------------------
    observeEvent(input$submit_lab_tat,{
      button_name <- "submit_lab_tat"
      shinyjs::disable(button_name)
      
      flag <- 0
      
      # Name Sunquest file
      sun_file <- input$lab_sun

      if (is.null(sun_file)) {
        return(NULL)
      }else{
        
        if(input$lab_tat_username == "") {
          showModal(modalDialog(
            title = "Error",
            "Please fill in the required fields",
            easyClose = TRUE,
            footer = NULL
          ))
        } else {
          
          updated_user <- input$lab_tat_username
          
          sun_file_path <- sun_file$datapath
          # sun_file_path <- paste0("/SharedDrive//deans/Presidents/HSPI-PM",
          #                     "/Operations Analytics and Optimization",
          #                     "/Projects/System Operations",
          #                     "/Balanced Scorecards Automation/Data_Dashboard",
          #                     "/Input Data Raw/Lab & Blood Bank/SUNQUEST",
          #                     "/SQ Monthly TROP-HGB-June 2022withTROPHS-Kate.xlsx")
          
          # Try catch statement to ensure file type is correct
          tryCatch({
            # Read in Sunquest file
            sun_data <- read_excel(sun_file_path)
            
            flag <- 1
          },
          error = function(err){
            showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with this Sunquest file."),
            easyClose = TRUE,
            footer = NULL
            ))
            shinyjs::enable(button_name)
            
          }
        )
        }
      }
      
      # Process data if the right file format was submitted
      if(flag == 1) {
        tryCatch({
          # Process Sunquest data
          sun_summary_data <- lab_sun_tat_dept_summary(sun_data, updated_user)
          
          flag <- 2
          
          # showModal(modalDialog(
          #   title = "Success",
          #   paste0("This Sunquest data has been imported successfully"),
          #   easyClose = TRUE,
          #   footer = NULL
          # ))
        },
        error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with this Sunquest file."),
            easyClose = TRUE,
            footer = NULL
          ))
          shinyjs::enable(button_name)
          
        })
      }
      
      if(flag == 2) {
        
        write_temporary_table_to_database_and_merge(sun_summary_data,
                                                    "TEMP_SUN_TAT", button_name)
        
        update_picker_choices_sql(session, input$selectedService, input$selectedService2, input$selectedService3)
        
        shinyjs::enable(button_name)

      }
    })
    
    # Lab Metrics - Proficiency Testing (Manual Entry) -----------------------
    # Create reactive data table for manual entry
    data_lab_prof_test <- reactive({
      data <- sql_manual_table_output("Lab", "proficiency_testing")
      # Arrange by sites in alphabetical order
      data <- data %>%
        arrange(Site)


      data <- manual_table_month_order(data)

    }
    )
    
    output$lab_prof_test <- renderRHandsontable({
      

      unique_sites <- unique(data_lab_prof_test()$Site)
      
      site_1 <- which(data_lab_prof_test()$Site == unique_sites[1])
      site_2 <- which(data_lab_prof_test()$Site == unique_sites[2])
      site_3 <- which(data_lab_prof_test()$Site == unique_sites[3])
      site_4 <- which(data_lab_prof_test()$Site == unique_sites[4])
      site_5 <- which(data_lab_prof_test()$Site == unique_sites[5])
      site_6 <- which(data_lab_prof_test()$Site == unique_sites[6])
      site_7 <- which(data_lab_prof_test()$Site == unique_sites[7])
      
      # # Code for testing manual entry table without reactive data
      # data_lab_prof_test <- data
      # 
      # unique_sites <- unique(data_lab_prof_test$Site)
      # 
      # site_1 <- which(data_lab_prof_test$Site == unique_sites[1])
      # site_2 <- which(data_lab_prof_test$Site == unique_sites[2])
      # site_3 <- which(data_lab_prof_test$Site == unique_sites[3])
      # site_4 <- which(data_lab_prof_test$Site == unique_sites[4])
      # site_5 <- which(data_lab_prof_test$Site == unique_sites[5])
      # site_6 <- which(data_lab_prof_test$Site == unique_sites[6])
      # site_7 <- which(data_lab_prof_test$Site == unique_sites[7])
      # 
      # col_highlight <- ncol(data_lab_prof_test) - 1
      
      renderer_string <- "
    function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.NumericRenderer.apply(this, arguments);

      if (instance.params) {
            hcols = instance.params.col_highlight;
            hcols = hcols instanceof Array ? hcols : [hcols];
          }

      if (instance.params && hcols.includes(col)) {
        td.style.background = '#EEEDE7';
      }
  }"
      
      col_highlight <- ncol(data_lab_prof_test()) - 1
      
      rhandsontable(data_lab_prof_test(),
                    # # Dataframe for non-reactive testing
                    # data_lab_prof_test,
                    overflow = 'visible',
                    col_highlight = col_highlight,
                    rowHeaders = FALSE,
                    readOnly = FALSE) %>%
        hot_cols(renderer = renderer_string) %>%
        hot_col(1:2, readOnly = T)
      
    })
    
    
    # Create observe event actions for manual data submission-----
    observeEvent(input$submit_lab_pt, {
      button_name <- "submit_lab_pt"
      shinyjs::disable(button_name)
      
      flag <- 0
      
      if(input$lab_pt_username == "") {
        showModal(modalDialog(
          title = "Error",
          "Please fill in the required fields",
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        
        updated_user <- input$lab_pt_username
        
        tryCatch({
          # Convert rhandsontable to R object
          prof_test_manual_updates <- hot_to_r(input$lab_prof_test)
          
          # Identify columns with no data in them and remove before further processing
          prof_test_manual_updates <- remove_empty_manual_columns(prof_test_manual_updates)
          
          flag <- 1
        },
        error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with the Proficiency Test data entered"),
            easyClose = TRUE,
            footer = NULL
          ))
          shinyjs::enable(button_name)
          
        })
        
        
        if (flag == 1) {
          
          # Check Proficiency Test data to make sure user entered data in correct format
          # ie, number between 0 and 1, no spaces, percentage signs, etc.
          # user_format_error <- user_format_error(prof_test_manual_updates)
          # Check Proficiency Test data to make sure user entered data in correct format
          # ie, number between 0 and 1, no spaces, percentage signs, etc.
          user_format_error <- manual_format_check(prof_test_manual_updates)
          
          if (user_format_error) {
            
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with the data entered. Data should be entered as a decimal between 0 and 1."),
              easyClose = TRUE,
              footer = NULL
            ))
            
          } else {
            
            updated_rows <- manual_process_and_return_updates(
              prof_test_manual_updates,
              "Lab",
              "proficiency_testing",
              updated_user,
              button_name
            )

            # 
            # # Check that data can be reformatted for department summary repo
            # tryCatch({
            # 
            #   # Reformat data from manual input table into department summary format
            #   prof_test_summary_data <-
            #     # lab_prof_test_dept_summary(prof_test_manual_table)
            #     lab_prof_test_dept_summary(prof_test_manual_updates,
            #                                updated_user)
            # 
            # 
            #   prof_test_summary_data <- return_updated_manual_data("Lab", "proficiency_testing", prof_test_summary_data)
            # 
            # 
            # 
            #   flag <- 2
            # 
            #   showModal(modalDialog(
            #     title = "Success",
            #     paste0("This Proficiency Test data has been submitted successfully."),
            #     easyClose = TRUE,
            #     footer = NULL
            #   ))
            # },
            # error = function(err){
            #   showModal(modalDialog(
            #     title = "Error",
            #     paste0("There seems to be an issue with the Proficiency Test data entered."),
            #     easyClose = TRUE,
            #     footer = NULL
            #   ))
            # })
            
            if(updated_rows$flag == 2) {
              
              write_temporary_table_to_database_and_merge(
                updated_rows$updated_rows,
                "TEMP_PROF_TEST", button_name)
                
              update_picker_choices_sql(session,
                                        input$selectedService,
                                        input$selectedService2,
                                        input$selectedService3)
            
            }
            
          }
          
        }
        
      }
      shinyjs::enable(button_name)
      

    })

      
      # Security Metrics - Incident Reports (Manual Entry) -----------------------
      # Create reactive data table for manual entry
      data_sec_inc_rpts <- reactive({
        
        data <- sql_manual_table_output("Security", "incident_reports")
        
        data <- data %>%
          arrange(Site)
        
        data <- manual_table_month_order(data)
        
      }
      )
      
      output$sec_inc_rpts <- renderRHandsontable({
        
        
        
        unique_sites <- unique(data_sec_inc_rpts()$Site)
        
        site_1 <- which(data_sec_inc_rpts()$Site == unique_sites[1])
        site_2 <- which(data_sec_inc_rpts()$Site == unique_sites[2])
        site_3 <- which(data_sec_inc_rpts()$Site == unique_sites[3])
        site_4 <- which(data_sec_inc_rpts()$Site == unique_sites[4])
        site_5 <- which(data_sec_inc_rpts()$Site == unique_sites[5])
        site_6 <- which(data_sec_inc_rpts()$Site == unique_sites[6])
        site_7 <- which(data_sec_inc_rpts()$Site == unique_sites[7])
        
        col_highlight <- ncol(data_sec_inc_rpts()) - 1
        
        # # Code for testing manual entry table without reactive data
        # data_sec_inc_rpts <- data
        # 
        # unique_sites <- unique(data_sec_inc_rpts$Site)
        # 
        # site_1 <- which(data_sec_inc_rpts$Site == unique_sites[1])
        # site_2 <- which(data_sec_inc_rpts$Site == unique_sites[2])
        # site_3 <- which(data_sec_inc_rpts$Site == unique_sites[3])
        # site_4 <- which(data_sec_inc_rpts$Site == unique_sites[4])
        # site_5 <- which(data_sec_inc_rpts$Site == unique_sites[5])
        # site_6 <- which(data_sec_inc_rpts$Site == unique_sites[6])
        # site_7 <- which(data_sec_inc_rpts$Site == unique_sites[7])
        # 
        # col_highlight <- ncol(data_sec_inc_rpts) - 1
        
        renderer_string <- "
    function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.NumericRenderer.apply(this, arguments);

      if (instance.params) {
            hcols = instance.params.col_highlight;
            hcols = hcols instanceof Array ? hcols : [hcols];
          }

      if (instance.params && hcols.includes(col)) {
        td.style.background = '#EEEDE7';
      }
  }"
        
        # col_highlight <- ncol(data) - 1

        
        rhandsontable(data_sec_inc_rpts(),
                      # # Dataframe for non-reactive testing
                      # data_sec_inc_rpts,
                      overflow = 'visible',
                      col_highlight = col_highlight,
                      rowHeaders = FALSE,
                      readOnly = FALSE) %>%
          hot_table(mergeCells = list(
            list(row = min(site_1)-1, col = 0, rowspan = length(site_1), colspan = 1),
            list(row = min(site_2)-1, col = 0, rowspan = length(site_2), colspan = 1),
            list(row = min(site_3)-1, col = 0, rowspan = length(site_3), colspan = 1),
            list(row = min(site_4)-1, col = 0, rowspan = length(site_4), colspan = 1),
            list(row = min(site_5)-1, col = 0, rowspan = length(site_5), colspan = 1),
            list(row = min(site_6)-1, col = 0, rowspan = length(site_6), colspan = 1),
            list(row = min(site_7)-1, col = 0, rowspan = length(site_7), colspan = 1)
          )) %>%
          hot_cols(renderer = renderer_string) %>%
          hot_col(1:2, readOnly = TRUE)
        
      })
      
      # Create observer event actions for manual data submission
      observeEvent(input$submit_sec_inc_rpts, {
        button_name <- "submit_sec_inc_rpts"
        shinyjs::disable(button_name)
        
        flag <- 0
        
        if(input$sec_inc_rpts_username == "") {
          showModal(modalDialog(
            title = "Error",
            "Please fill in the required fields.",
            easyClose = TRUE,
            footer = NULL
          ))
        } else {
          
          updated_user <- input$sec_inc_rpts_username
          
          tryCatch({
            
            # Convert rhandsontable to R object
            sec_inc_rpts_manual_updates <- hot_to_r(input$sec_inc_rpts)
            updated_user <- input$sec_inc_rpts_username

            # Identify columns with no data in them and remove before further processing
            sec_inc_rpts_manual_updates <- remove_empty_manual_columns(
              sec_inc_rpts_manual_updates)

            flag <- 1
            
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with the Security Incident Reports data entered."),
              easyClose = TRUE,
              footer = NULL
            ))
            shinyjs::enable(button_name)
          })
          
          if(flag == 1) {
            
            updated_rows <- manual_process_and_return_updates(
              sec_inc_rpts_manual_updates,
              "Security",
              "incident_reports",
              updated_user,
              button_name
            )
            
          }
          
          if(updated_rows$flag == 2) {
            
            # Update data on database
            write_temporary_table_to_database_and_merge(
              updated_rows$updated_rows,
              "TEMP_SEC_INC_RPTS", button_name)
            
            update_picker_choices_sql(session,
                                      input$selectedService,
                                      input$selectedService2,
                                      input$selectedService3)
            
            
          }
          
        }
        shinyjs::enable(button_name)
        
        
      })
      
      # Security Metrics - Security Events (Manual Entry) -------------------
      # Create reactive data table for manual entry
      data_sec_events <- reactive({

        data <- sql_manual_table_output("Security", "security_events")
        
        data <- data %>%
          arrange(Site)
        
        data <- manual_table_month_order(data)
        

      }
      )
      
      output$sec_events <- renderRHandsontable({
        
        unique_sites <- unique(data_sec_events()$Site)
        
        site_1 <- which(data_sec_events()$Site == unique_sites[1])
        site_2 <- which(data_sec_events()$Site == unique_sites[2])
        site_3 <- which(data_sec_events()$Site == unique_sites[3])
        site_4 <- which(data_sec_events()$Site == unique_sites[4])
        site_5 <- which(data_sec_events()$Site == unique_sites[5])
        site_6 <- which(data_sec_events()$Site == unique_sites[6])
        site_7 <- which(data_sec_events()$Site == unique_sites[7])
        
        col_highlight <- ncol(data_sec_events()) - 1
        
        # # Code for testing manual entry table without reactive data
        # data_sec_events <- data
        # 
        # unique_sites <- unique(data_sec_events$Site)
        # 
        # site_1 <- which(data_sec_events$Site == unique_sites[1])
        # site_2 <- which(data_sec_events$Site == unique_sites[2])
        # site_3 <- which(data_sec_events$Site == unique_sites[3])
        # site_4 <- which(data_sec_events$Site == unique_sites[4])
        # site_5 <- which(data_sec_events$Site == unique_sites[5])
        # site_6 <- which(data_sec_events$Site == unique_sites[6])
        # site_7 <- which(data_sec_events$Site == unique_sites[7])
        # 
        # col_highlight <- ncol(data_sec_events) - 1
        
        renderer_string <- "
        function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.NumericRenderer.apply(this, arguments);
        
        if (instance.params) {
        hcols = instance.params.col_highlight;
        hcols = hcols instanceof Array ? hcols : [hcols];
        }
        
        if (instance.params && hcols.includes(col)) {
        td.style.background = '#EEEDE7';
        }
        }"
        
        rhandsontable(data_sec_events(),
                      # # Dataframe for non-reactive testing
                      # data_sec_events,
                      overflow = 'visible',
                      col_highlight = col_highlight,
                      rowHeaders = FALSE,
                      readOnly = FALSE) %>%
          hot_cols(renderer = renderer_string) %>%
          hot_col(1:2, readOnly = TRUE)
        
      }
      )
      
      # Create observe event actions for manual data submission
      observeEvent(input$submit_sec_events, {
        button_name <- "submit_sec_events"
        shinyjs::disable(button_name)
        
        flag <- 0
        
        if(input$sec_events_username == "") {
          showModal(modalDialog(
            title = "Error",
            "Please fill in the required fields.",
            easyClose = TRUE,
            footer = NULL
          )
          )
        } else {
          
          updated_user <- input$sec_events_username
          
          tryCatch({
            
            # Convert rhandsontable to R object
            sec_events_manual_updates <- hot_to_r(input$sec_events)
            sec_events_manual_updates_test <<- sec_events_manual_updates
            
            # Identify columns with no data in them and remove before further processing
            sec_events_manual_updates <- remove_empty_manual_columns(
              sec_events_manual_updates
            )
            
            flag <- 1
            
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with the Security Events data entered."),
              easyClose = TRUE,
              footer = NULL
            ))
            shinyjs::enable(button_name)
            
          })
          
          if(flag == 1) {
            
            updated_rows <- manual_process_and_return_updates(
              sec_events_manual_updates,
              "Security",
              "security_events",
              updated_user,
              button_name
            )
            

          }
          
          if(updated_rows$flag == 2) {
            
            # Update summary repo data on database
            write_temporary_table_to_database_and_merge(
              updated_rows$updated_rows,
              "TEMP_SEC_EVENTS", button_name
            )
            
            update_picker_choices_sql(session,
                                      input$selectedService,
                                      input$selectedService2,
                                      input$selectedService3)

          }
          
        }
        shinyjs::enable(button_name)
        
        
      })

    # 5. Overtime - Data Input ---------------------------------------------------------------------------------
    observeEvent(input$submit_finance, {
      button_name <- "submit_finance"
      shinyjs::disable(button_name)
      census_file <- input$finance_census
      flag <- 0
      
      if(is.null(census_file)){
        return(NULL)
      }else{
        census_filepath <- census_file$datapath
        #census_filepath <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/Food/Monthly Stats Summary for benchmarking 20211013.xlsx"
        #Read in census file
        tryCatch({census_data <- read_excel(census_filepath)
        flag <- 1
        showModal(modalDialog(
          title = "Success",
          paste0("The census data has been imported succesfully"),
          easyClose = TRUE,
          footer = NULL
        ))
        }, error = function(err){  showModal(modalDialog(
          title = "Error",
          paste0("There seems to be an issue with the census file"),
          easyClose = TRUE,
          footer = NULL
        ))
          shinyjs::enable(button_name)
          
          })
      }
      

      ## Process Census Data
      tryCatch({census_summary_data <- census_days_dept_summary(census_data)
                  flag <- 2
      
      showModal(modalDialog(
        title = "Success",
        paste0("The census data has been imported succesfully"),
        easyClose = TRUE,
        footer = NULL
      ))
      }, error = function(err){  showModal(modalDialog(
        title = "Error",
        paste0("There seems to be an issue the census file"),
        easyClose = TRUE,
        footer = NULL
      ))
        shinyjs::enable(button_name)
        })
      
      if (flag == 2){

        # Save prior version of COst and Revenue Summary data
        write_xlsx(cost_and_revenue_repo,
                   paste0(hist_archive_path,
                          "Cost and Revenue ",
                          format(Sys.time(), "%Y%m%d_%H%M%S"),
                          ".xlsx"))
        
        # Append Lab TAT summary with new data
        # First, identify the sites, months, and metrics in the new data
        census_new_data <- unique(
          census_summary_data[  c("Service", "Site", "Month")]
        )
        
        # Second, remove these sites, months, and metrics from the historical data, if they exist there.
        # This allows us to ensure no duplicate entries for the same site, metric, and time period
        cost_and_revenue_repo <<- anti_join(cost_and_revenue_repo,
                                            census_new_data,
                                            by = c("Service" = "Service",
                                                   "Site" = "Site",
                                                   "Month" = "Month"))
        
        
        # Third, combine the updated historical data with the new data
        cost_and_revenue_repo <<- full_join(cost_and_revenue_repo,
                                          census_summary_data)
        
        # Lastly, save the updated summary data
        write_xlsx(cost_and_revenue_repo, paste0(home_path, "Summary Repos/Cost and Revenue.xlsx"))
        
        # Update metrics_final_df with latest SCC data using custom function
        metrics_final_df <<- census_days_metrics_final_df(census_summary_data)
        
        # Save updated metrics_final_df
        saveRDS(metrics_final_df, metrics_final_df_path)
        
        # Update "Reporting Month" drop down in each tab
        picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
        updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      }
      shinyjs::enable(button_name)
      
      
    })
    
      
    observeEvent(input$submit_finance_ot, {
      button_name <- "submit_finance_ot"
      shinyjs::disable(button_name)
      
      flag <- 0
      overtime_file <- input$finance_overtime
      
      if(input$name_finance == ""){
        showModal(modalDialog(
          title = "Error",
          paste0("Please fill in the required fields"),
          easyClose = TRUE,
          footer = NULL
        ))
      }else{
        updated_user <- input$name_finance
        overtime_file_path <- overtime_file$datapath
        tryCatch({#overtime_file_path <- paste0(home_path,"Input Data Raw/Finance/Overtime Hours/OT_extract_sample_2021_09.xlsx")
                   overtime_data <- read_excel(overtime_file_path)
        flag <- 1
        },
        error = function(err){  showModal(modalDialog(
          title = "Error",
          paste0("There seems to be an issue with the Overtime file."),
          easyClose = TRUE,
          footer = NULL
        ))
          shinyjs::enable(button_name)
        })
      }
      
      if(flag == 1){
        # Process the data into standar Summary Repo format
        tryCatch({overtime_summary_data <- overtime_file_processs(overtime_data, updated_user)
        flag <- 2
        
        },
        error = function(err){  showModal(modalDialog(
          title = "Error",
          paste0("There seems to be an issue with the Overtime file."),
          easyClose = TRUE,
          footer = NULL
        ))
          shinyjs::enable(button_name)
        })
      }
      
      
      if(flag == 2){
        ##Compare submitted results to what is in the Summary Repo in db and return only updated rows
        overtime_summary_data <- file_return_updated_rows(overtime_summary_data)
        
        #wirte the updated data to the Summary Repo in the server
        write_temporary_table_to_database_and_merge(overtime_summary_data,
                                                    "TEMP_OVERTIME", button_name)
        
        update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                  input$selectedService3)
      }
      shinyjs::enable(button_name)
      
        
    })
    
    
    # })
    
    # Transport Metrics - Non Patient Data  -----------------------
    
      observeEvent(input$submit_npt_tat,{
        button_name <- "submit_npt_tat"
        shinyjs::disable(button_name)
        
        npt_file <- input$non_patient_transport
        flag <- 0 
        
        if (is.null(npt_file)) {
          return(NULL)
        }else{
          #file_path <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/EVS/MSHS Normal Clean vs Iso Clean TAT Sept 2021.xlsx"
          #pt_data <- read_excel(file_path)
          if(input$name_transport_npt == ""){
            showModal(modalDialog(
              title = "Error",
              paste0("Please fill in the required fields"),
              easyClose = TRUE,
              footer = NULL
            ))
          }
          
          tryCatch({
            # Read in SCC file
            file_path <- npt_file$datapath
            updated_user <- input$name_transport_npt
            
            
            npt_data<- read_excel(file_path)
            
            flag <- 1
            
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with this Patient Transport Data file."),
              easyClose = TRUE,
              footer = NULL
            ))
            shinyjs::enable(button_name)
            
          }
          )
          
        }
        
        
        if(flag==1){
          
          tryCatch({
            # Process Input Data
            npt_summary_repo <- process_NPT_raw_data(npt_data,updated_user)
            flag <- 2
            
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with the Support Services file."),
              easyClose = TRUE,
              footer = NULL
            ))
            shinyjs::enable(button_name)
            
          }
          )
          
        }
        if(flag==2){
          
          ##Compare submitted results to what is in the Summary Repo in db and return only updated rows
          npt_summary_repo <- file_return_updated_rows(npt_summary_repo)
          write_temporary_table_to_database_and_merge(npt_summary_repo,
                                                      "TEMP_NPT", button_name)
          
          update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                    input$selectedService3)
          
          
        }
        shinyjs::enable(button_name)
        
        
      })
    # Transport Metrics - Patient Data  -----------------------
    
      
      observeEvent(input$submit_pt_tat,{
        button_name <- "submit_pt_tat"
        shinyjs::disable(button_name)
        
        pt_file <- input$patient_transport
        flag <- 0 
        
        if (is.null(pt_file)) {
          return(NULL)
        }else{
          #file_path <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/EVS/MSHS Normal Clean vs Iso Clean TAT Sept 2021.xlsx"
          #pt_data <- read_excel(file_path)
            if(input$name_transport_pt == ""){
              showModal(modalDialog(
              title = "Error",
              paste0("Please fill in the required fields"),
              easyClose = TRUE,
              footer = NULL
            ))
           }
          
            tryCatch({
              # Read in SCC file
              file_path <- pt_file$datapath
              updated_user <- input$name_transport_pt
              
              
              pt_data_raw <- read_excel(file_path, sheet = "PTET")
              
              
              flag <- 1
              
            },
            error = function(err){
              showModal(modalDialog(
                title = "Error",
                paste0("There seems to be an issue with this Patient Transport Data file."),
                easyClose = TRUE,
                footer = NULL
              ))
              shinyjs::enable(button_name)
            }
            )

        }
        
        
        if(flag==1){

          tryCatch({
            # Process Input Data
            pt_summary_repo <- process_PT_data(pt_data_raw,updated_user)
            flag <- 2
            
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with the Support Services file."),
              easyClose = TRUE,
              footer = NULL
            ))
            shinyjs::enable(button_name)
            
          }
          )
          
        }
        if(flag==2){
          
          ##Compare submitted results to what is in the Summary Repo in db and return only updated rows
          pt_summary_repo <- file_return_updated_rows(pt_summary_repo)
          write_temporary_table_to_database_and_merge(pt_summary_repo,
                                                      "TEMP_PT", button_name)
          
          update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                  input$selectedService3)

          
        }
        shinyjs::enable(button_name)
        
        
      })
      
      # KPI Biomed Output Table -------
      
      data_bimoed_kpi <- reactive({
        #data  <- kpibme_reports_ui %>% ungroup()
        data <- sql_manual_table_output("Biomed / Clinical Engineering", "KPIs")
        data <- data %>%
          arrange(Site)
        result <- manual_table_month_order(data)
        
      })
      
      
      output$biomed_kpi <- renderRHandsontable({
        #data <- data
        data <- data_bimoed_kpi()
        
        
        
        unique_sites <- unique(data$Site)
        site_1 <- which(data$Site == unique_sites[1])
        site_2 <- which(data$Site == unique_sites[2])
        site_3 <- which(data$Site == unique_sites[3])
        site_4 <- which(data$Site == unique_sites[4])
        site_5 <- which(data$Site == unique_sites[5])
        site_6 <- which(data$Site == unique_sites[6])
        site_7 <- which(data$Site == unique_sites[7])
        
        
        rendederer_string <- "
    function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.NumericRenderer.apply(this, arguments);

      if (instance.params) {
            hcols = instance.params.col_highlight;
            hcols = hcols instanceof Array ? hcols : [hcols];
          }

      if (instance.params && hcols.includes(col)) {
        td.style.background = '#EEEDE7';
      }
  }"
        
        
        col_highlight <- ncol(data) - 1
        
        
        rhandsontable(data, overflow= 'visible', col_highlight = col_highlight, rowHeaders = FALSE, readOnly = FALSE) %>%
          hot_table(mergeCells = list(
            list(row = min(site_1)-1, col = 0, rowspan = length(site_1), colspan = 1),
            list(row = min(site_2)-1, col = 0, rowspan = length(site_2), colspan = 1),
            list(row = min(site_3)-1, col = 0, rowspan = length(site_3), colspan = 1),
            list(row = min(site_4)-1, col = 0, rowspan = length(site_4), colspan = 1),
            list(row = min(site_5)-1, col = 0, rowspan = length(site_5), colspan = 1),
            list(row = min(site_6)-1, col = 0, rowspan = length(site_6), colspan = 1),
            list(row = min(site_7)-1, col = 0, rowspan = length(site_7), colspan = 1)
          )) %>%
          hot_cols(renderer = rendederer_string)  %>%
          hot_col(1:2, readOnly = T)
      })
      #KPIs Biomed Observe Event----- 
      observeEvent(input$submit_biomedkpis, {
        button_name <- "submit_biomedkpis"
        shinyjs::disable(button_name)
        updated_user <- input$name_biomed_kpi
        if(input$name_biomed_kpi == "") {
          showModal(modalDialog(
            title = "Error",
            "Please fill in the required fields.",
            easyClose = TRUE,
            footer = NULL
          ))
        }
        else{
          tryCatch({
            
            # Convert rhandsontable to R object
            bme_kpi_manual_updates <- hot_to_r(input$biomed_kpi)          
            
            # Identify columns with no data in them and remove before further processing
            # This ensures months with no data do not get added to the department summary

            bme_kpi_manual_updates <- remove_empty_manual_columns(bme_kpi_manual_updates)
            flag <- 1
          },
          error =function(err){
            
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with the Biomedical KPIs data entered."),
              easyClose = TRUE,
              footer = NULL
            ))
            shinyjs::enable(button_name)
            
            
          })
          
          if(flag==1){
            user_format_error <- manual_format_check(bme_kpi_manual_updates%>%
                                                        filter(Metric %in% c("PM Compliance - High Risk Equipment",
                                                                             "PM Compliance - All Medical Equipment",
                                                                             "Documented Status")))
            
            if (user_format_error) {
              
              showModal(modalDialog(
                title = "Error",
                paste0("There seems to be an issue with the data entered. Data should be entered as a decimal between 0 and 1."),
                easyClose = TRUE,
                footer = NULL
              ))
              
            } 
            else{
              ## Updated rows returns flag and the processed updated rows by comparing what is currently in the summary repo
              updated_rows <- manual_process_and_return_updates(bme_kpi_manual_updates, 
                                                                "Biomed / Clinical Engineering", 
                                                                "KPIs", 
                                                                updated_user,
                                                                button_name)
              
              if(updated_rows$flag == 2) {
                ##Updated the data on the databse
                write_temporary_table_to_database_and_merge(updated_rows$updated_rows,
                                                            "TEMP_BIOMEDKPIs", button_name)
                
                update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                          input$selectedService3)
              
              }
        }
      }
    }})
      
      
      #D&I Biomed Output Table -------
      
      data_bimoed_di <- reactive({
        #data  <- kpibme_reports_ui %>% ungroup()
        data <- sql_manual_table_output("Biomed / Clinical Engineering", "disruptions_and_issues")
        data <- data %>%
          arrange(Site)
        result <- manual_table_month_order(data)
        
      })
      
      
      output$bimoed_di <- renderRHandsontable({
        data <- data_bimoed_di()
        
        
        
        unique_sites <- unique(data$Site)
        site_1 <- which(data$Site == unique_sites[1])
        site_2 <- which(data$Site == unique_sites[2])
        site_3 <- which(data$Site == unique_sites[3])
        site_4 <- which(data$Site == unique_sites[4])
        site_5 <- which(data$Site == unique_sites[5])
        site_6 <- which(data$Site == unique_sites[6])
        site_7 <- which(data$Site == unique_sites[7])
        
        
        rendederer_string <- "
    function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.NumericRenderer.apply(this, arguments);

      if (instance.params) {
            hcols = instance.params.col_highlight;
            hcols = hcols instanceof Array ? hcols : [hcols];
          }

      if (instance.params && hcols.includes(col)) {
        td.style.background = '#EEEDE7';
      }
  }"
        
        
        col_highlight <- ncol(data) - 1
        
        
        rhandsontable(data, overflow= 'visible', col_highlight = col_highlight, rowHeaders = FALSE, readOnly = FALSE) %>%
          hot_table(mergeCells = list(
            list(row = min(site_1)-1, col = 0, rowspan = length(site_1), colspan = 1),
            list(row = min(site_2)-1, col = 0, rowspan = length(site_2), colspan = 1),
            list(row = min(site_3)-1, col = 0, rowspan = length(site_3), colspan = 1),
            list(row = min(site_4)-1, col = 0, rowspan = length(site_4), colspan = 1),
            list(row = min(site_5)-1, col = 0, rowspan = length(site_5), colspan = 1),
            list(row = min(site_6)-1, col = 0, rowspan = length(site_6), colspan = 1),
            list(row = min(site_7)-1, col = 0, rowspan = length(site_7), colspan = 1)
          )) %>%
          hot_cols(renderer = rendederer_string)  %>%
          hot_col(1:2, readOnly = T)
      })
      
      # D&I Biomed Observe Event----- 
      observeEvent(input$submit_biomeddi, {
        button_name <- "submit_biomeddi"
        shinyjs::disable(button_name)
        if(input$name_biomed_distruptions == "") {
          showModal(modalDialog(
            title = "Error",
            "Please fill in the required fields.",
            easyClose = TRUE,
            footer = NULL
          ))
        }else{
          tryCatch({
            # Convert rhandsontable to R object
            bme_di_manual_updates <- hot_to_r(input$bimoed_di)
            updated_user <- input$name_biomed_distruptions
            # Identify columns with no data in them and remove before further processing
            # This ensures months with no data do not get added to the department summary
            bme_di_manual_updates <- remove_empty_manual_columns(bme_di_manual_updates)  
            flag <- 1
        
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with the Disruptions and Issues data entered."),
              easyClose = TRUE,
              footer = NULL
            ))
            shinyjs::enable(button_name)
            
          })
          if(flag ==1){
            
            updated_rows <- manual_process_and_return_updates(bme_di_manual_updates, 
                                                              "Biomed / Clinical Engineering",
                                                              "disruptions_and_issues", 
                                                              updated_user,
                                                              button_name)
            
           
          }
          
          if(updated_rows$flag == 2){
            
            write_temporary_table_to_database_and_merge(updated_rows$updated_rows,
                                                        "TEMP_DI_BIOMED", button_name)
            
        
            update_picker_choices_sql(session, input$selectedService, input$selectedService2, input$selectedService3)
            #record_timestamp("Biomed / Clinical Engineering")
            
            
          } 
        }
        shinyjs::enable(button_name)
        
      })
      

      # Imaging DR X-RAY data submission ----- 
      observeEvent(input$submit_imagingxray, {
        button_name <- "submit_imagingxray"
        shinyjs::disable(button_name)
        flag <- 0
        xray_file <- input$imaging_DR_XRay
        if(input$imaging_xray_username == "") {
          showModal(modalDialog(
            title = "Error",
            "Please fill in the required fields.",
            easyClose = TRUE,
            footer = NULL
          ))
        }else{
          updated_user <- input$imaging_xray_username
          file_path <- xray_file$datapath

          tryCatch({
            xray_data <- read.xlsx(xlsxFile = file_path, fillMergedCells = TRUE,colNames = TRUE)
            
            flag <- 1
            
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with this X-Ray file."),
              easyClose = TRUE,
              footer = NULL
            ))
            shinyjs::enable(button_name)
            
          }
          )
        }
        
        # Process data if the right file format was submitted
        if(flag == 1) {
          tryCatch({
            # Reformat data from manual input table into summary repo format
            xray_summary_data <-
              process_xray_data(xray_data, updated_user)
            
            flag <- 2
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with this data file."),
              easyClose = TRUE,
              footer = NULL
            ))
            shinyjs::enable(button_name)
            
          })
        }
        
        
        if(flag == 2){
          
          
          ##Compare submitted results to what is in the Summary Repo in db and return only updated rows
          xray_summary_data <- file_return_updated_rows(xray_summary_data)
          #wirte the updated data to the Summary Repo in the server
          write_temporary_table_to_database_and_merge(xray_summary_data,
                                                      "TEMP_IMAGING_XRAY", button_name)
          
          update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                    input$selectedService3)
            
        }
        shinyjs::enable(button_name)
        
      })
      
      # Imaging DR Chest CT data submission ----- 
      observeEvent(input$submit_imagingct, {
        button_name <- "submit_imagingct"
        shinyjs::disable(button_name)
        flag <- 0
        ct_file <- input$imaging_DR_ct

        if(input$imaging_ct_username == "") {
          showModal(modalDialog(
            title = "Error",
            "Please fill in the required fields.",
            easyClose = TRUE,
            footer = NULL
          ))
        }else{
          updated_user <- input$imaging_ct_username
          file_path <- ct_file$datapath
          
          
          tryCatch({
            
            ct_data <- read.xlsx(xlsxFile = file_path, fillMergedCells = TRUE,colNames = TRUE)
            
            flag <- 1
            
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with this data file."),
              easyClose = TRUE,
              footer = NULL
            ))
            shinyjs::enable(button_name)
            
          }
          )
          
        }
        
        # Process data if the right file format was submitted
        if(flag == 1) {
          tryCatch({
            # Reformat data from manual input table into summary repo format
            ct_summary_data <-
              process_ctdata_data(ct_data, updated_user)
            
            flag <- 2
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with this data file."),
              easyClose = TRUE,
              footer = NULL
            ))
            shinyjs::enable(button_name)
            
          })
        }
        
        
        
        if(flag == 2){
          
          ##Compare submitted results to what is in the Summary Repo in db and return only updated rows
          ct_summary_data <- file_return_updated_rows(ct_summary_data)
          
          #wirte the updated data to the Summary Repo in the server
          write_temporary_table_to_database_and_merge(ct_summary_data,
                                                      "TEMP_IMAGING_CT", button_name)
          
          update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                    input$selectedService3)
            
        }
        shinyjs::enable(button_name)
        
      })
      
      # Nursing observer event actions for data submission ----- 
      observeEvent(input$submit_nursing, {
        button_name <- "submit_nursing"
        shinyjs::disable(button_name)
        
        flag <- 0
        nursing_file <- input$nursing
        if(input$name_nursing == ""){
          showModal(modalDialog(
            title = "Error",
            paste0("Please fill in the required fields"),
            easyClose = TRUE,
            footer = NULL))
          }else{
            
            updated_user <- input$name_nursing
            file_path <- nursing_file$datapath
            
            tryCatch({
              nursing_data <- read_excel(file_path)
              flag <- 1
            },
              error = function(err){
                showModal(modalDialog(
                  title = "Error",
                  paste0("There seems to be an issue with this data file."),
                  easyClose = TRUE,
                  footer = NULL))
              })
            shinyjs::enable(button_name)
            
          }
        # Process data if the right file format was submitted
        if(flag == 1) {
          tryCatch({
            # Reformat data from manual input table into summary repo format
            nursing_summary_data <-
              process_nursing_data(nursing_data,updated_user)
            
            flag <- 2
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with this data file."),
              easyClose = TRUE,
              footer = NULL
            ))
            shinyjs::enable(button_name)
            
          })
        }
        
        if(flag == 2){
          
          
          ##Compare submitted results to what is in the Summary Repo in db and return only updated rows
          nursing_summary_data <- file_return_updated_rows(nursing_summary_data)
          
          #wirte the updated data to the Summary Repo in the server
          write_temporary_table_to_database_and_merge(nursing_summary_data,
                                                      "TEMP_NURSING", button_name)
          
          update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                    input$selectedService3)
        }
        shinyjs::enable(button_name)
        
      })
      
      
      # ED observer event actions for data submission ----- 
      observeEvent(input$submit_ed, {
        button_name <- "submit_ed"
        shinyjs::disable(button_name)
        flag <- 0
        ed_file <- input$ed
        
        if(input$name_ed == ""){
          showModal(modalDialog(
            title = "Error",
            paste0("Please fill in the required fields"),
            easyClose = TRUE,
            footer = NULL
          ))
        }else{
          updated_user <- input$name_ed
          file_path <- ed_file$datapath
          tryCatch({
            ed_data_ts <- read.xlsx(file_path,sheet = "Sheet2",fillMergedCells=TRUE,colNames = FALSE,startRow = 2)
            ed_data_percentiles <- read.xlsx(file_path,sheet = "Sheet1",fillMergedCells=TRUE,colNames = FALSE,startRow = 2)
            data <- ed_data_preprocess(ed_data_ts,ed_data_percentiles)
            
            ed_data_ts <- data[[1]]
            ed_data_percentiles <- data[[2]]
            
            flag <- 1
            
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with this data file."),
              easyClose = TRUE,
              footer = NULL
            ))
            shinyjs::enable(button_name)
            
            })
          
        }
        
        # Process data if the right file format was submitted
        if(flag == 1) {
          tryCatch({
            # Reformat data from manual input table into summary repo format
            ed_summary_data <-
              ed_dept_summary(ed_data_ts,ed_data_percentiles,updated_user)
            
            flag <- 2
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with this data file."),
              easyClose = TRUE,
              footer = NULL
            ))
            shinyjs::enable(button_name)
            
          })
        }
        
        if(flag == 2){
          ##Compare submitted results to what is in the Summary Repo in db and return only updated rows
          ed_summary_data <- file_return_updated_rows(ed_summary_data)
          
          #wirte the updated data to the Summary Repo in the server
          write_temporary_table_to_database_and_merge(ed_summary_data,
                                                      "TEMP_ED", button_name)
          
          update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                    input$selectedService3)
          
        }
        shinyjs::enable(button_name)
        
      })
      # Clinical Nutrition observer event actions for data submission ----- 
      observeEvent(input$submit_cn, {
        button_name <- "submit_cn"
        shinyjs::disable(button_name)
        flag <- 0
        cn_file <- input$cn_file
        
        if(input$name_cn == ""){
          showModal(modalDialog(
            title = "Error",
            paste0("Please fill in the required fields"),
            easyClose = TRUE,
            footer = NULL
          ))
        }else{
          updated_user <- input$name_cn
          file_path <- cn_file$datapath
          tryCatch({
            print("flag2")
            cn_data_raw <- lapply(excel_sheets(file_path), read_xlsx, path = file_path)
            processed_data <- list()
            i <- 1
            for(yearly_data in cn_data_raw){
             cn_summary_data_yearly <- cn_dept_summary(yearly_data,updated_user)
             processed_data[[i]]<-cn_summary_data_yearly
             i <- i+1
            }
            cn_summary_data <- bind_rows(processed_data)
            flag <- 1
            
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with this data file."),
              easyClose = TRUE,
              footer = NULL
            ))
            shinyjs::enable(button_name)
            
          })
          
        }
        
        # Process data if the right file format was submitted
        if(flag == 1) {
          tryCatch({
            print("flag1")
            ##Compare submitted results to what is in the Summary Repo in db and return only updated rows
            cn_summary_data <- file_return_updated_rows(cn_summary_data)
            #wirte the updated data to the Summary Repo in the server
            write_temporary_table_to_database_and_merge(cn_summary_data,
                                                        "TEMP_CN", button_name)
            
            flag <- 2
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with this data file."),
              easyClose = TRUE,
              footer = NULL
            ))
            shinyjs::enable(button_name)
            
          })
        }
        
        if(flag == 2){
          
          update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                    input$selectedService3)
          
        }
        shinyjs::enable(button_name)
        
      })
      
      
      # 5. Target and Status Definitions tab ----------------------------------
      
      # Create title based on user selected service line
      output$targetSummary_title <- renderText({
        
        text = paste0("MSHS ", input$selectedService4, " Metric Targets & Status Definitions")
        
        text
        
      })

      # Create table based on selected service and metric groups
      output$targetSummary_table <- renderDataTable(datatable({
        
        service_input <- input$selectedService4
        metric_group_input <- input$selectedMetricGroup
        
        # service_input <- "Patient Transport"
        # metric_group_input <- metric_group_choices
        
        targets_metric_group_order <- unique(metric_mapping_breakout$Metric_Group)
        
        targets_table <- target_mapping_reference %>%
          filter(Service %in% service_input &
                   Metric_Group %in% metric_group_input) %>%
          select(-Service)
        
        validate(
          need(nrow(targets_table) != 0, "Please select at least one metric group.")
        )
        
        # Determine the number of unique targets and status definitions for each metric
        unique_targets_status <- targets_table %>%
          group_by(Metric_Group, Metric_Name) %>%
          distinct(Metric_Group, Metric_Name,
                   Target,
                   Green_Status, Yellow_Status, Red_Status) %>%
          mutate(Num_Definitions = n()) %>%
          # select(-Target,
          #        -contains(": ")) %>%
          distinct()
        
        targets_table <- left_join(targets_table,
                                   unique_targets_status,
                                   by = c("Metric_Name" = "Metric_Name",
                                          "Metric_Group" = "Metric_Group",
                                          "Target" = "Target",
                                          "Green_Status" = "Green_Status",
                                          "Yellow_Status" = "Yellow_Status",
                                          "Red_Status" = "Red_Status"))
        
        targets_table_summary <- targets_table %>%
          relocate(Site, .after = Metric_Name) %>%
          mutate(Site = ifelse(Num_Definitions == 1, "All Sites", Site)) %>%
          select(-Num_Definitions) %>%
          distinct() %>%
          arrange(Metric_Group, Metric_Name, Site)
        
        # Crosswalk with units and format
        target_metrics_units <- metric_mapping_breakout %>%
          filter(Service %in% service_input) %>%
          select(Metric_Group, Metric_Name, Metric_Name_Submitted,
                 Metric_Unit)
        
        targets_table_summary <- left_join(targets_table_summary,
                                           target_metrics_units,
                                           by = c("Metric_Group",
                                                  "Metric_Name",
                                                  "Metric_Name_Submitted"))
        # 
        # Update format of metrics as percent
        targets_table_summary$Target[which(
          targets_table_summary$Metric_Unit == "Percent")] <-
          percent(
            as.numeric(
              targets_table_summary$Target[which(
                targets_table_summary$Metric_Unit == "Percent")]),
                  accuracy = 1)
        
        # Update format of metrics as numbers with 2 decimals
        targets_table_summary$Target[which(
          is.na(targets_table_summary$Metric_Unit))] <-
          as.character(
            as.numeric(
              targets_table_summary$Target[which(
                is.na(targets_table_summary$Metric_Unit))]))
        
        # Remove Metric_Unit column
        targets_table_summary$Metric_Unit <- NULL
        
        # Arrange metrics based on metric group
        targets_table_summary <- targets_table_summary %>%
          mutate(Metric_Group = factor(
            Metric_Group,
            levels = targets_metric_group_order[targets_metric_group_order %in% .$Metric_Group],
            ordered = TRUE)) %>%
          arrange(Metric_Group, Metric_Name, Site) %>%
          mutate(Metric_Group = as.character(Metric_Group)) %>%
          # select(-Metric_Group) %>%
          select(-Metric_Name_Submitted)
        
        colnames(targets_table_summary)[
          which(str_detect(colnames(targets_table_summary), "Green"))] <-
          paste0('<span style="color:',
                 c("green"),'">',
                 "Green Status ",
                 fa('fas fa-circle'),
                 '</span>')
        
        colnames(targets_table_summary)[
          which(str_detect(colnames(targets_table_summary), "Yellow"))] <-
          paste0('<span style="color:',
                 c("#FFD700"),'">',
                 "Yellow Status ",
                  fa('fas fa-circle'),
                 '</span>')
        
        colnames(targets_table_summary)[
          which(str_detect(colnames(targets_table_summary), "Red"))] <-
          paste0('<span style="color:',
                 c("red"),'">',
                 "Red Status ",
                 fa('fas fa-circle'),
                 '</span>')

        targets_table_summary
        },
        escape = FALSE,
        rownames = FALSE,
        colnames = str_replace(colnames(targets_table_summary),
                               pattern = "\\_",
                               replacement = "\\ "),
        options = list(
          paging = FALSE
        )

      ))
            
      
      # Perioperative Metrics - Operational Data  -----------------------
      
      
      observeEvent(input$submit_peri_op,{
        button_name <- "submit_peri_op"
        shinyjs::disable(button_name)
        
        peri_op_file <- input$peri_op_file
        flag <- 0 
        
        if (is.null(peri_op_file)) {
          return(NULL)
          shinyjs::enable(button_name)
          print("null")
        }else{
          #file_path <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/EVS/MSHS Normal Clean vs Iso Clean TAT Sept 2021.xlsx"
          #pt_data <- read_excel(file_path)
          if(input$name_peri_op == ""){
            showModal(modalDialog(
              title = "Error",
              paste0("Please fill in the required fields"),
              easyClose = TRUE,
              footer = NULL
            ))
          }
          
          tryCatch({
            file_path <- peri_op_file$datapath
            updated_user <- input$name_peri_op
            
            flag <- 1
            
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with the Perioperative Services data file."),
              easyClose = TRUE,
              footer = NULL
            ))
            shinyjs::enable(button_name)
          }
          )
          
        }
        
        
        if(flag==1){
          
          tryCatch({
            # Process Input Data
            peri_op_summary_repo <- peri_op_processing(file_path, updated_user)
            flag <- 2
            
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with the Perioperative Services data file."),
              easyClose = TRUE,
              footer = NULL
            ))
            shinyjs::enable(button_name)
            
          }
          )
          
        }
        if(flag==2){
          
          ##Compare submitted results to what is in the Summary Repo in db and return only updated rows
          peri_op_summary_repo <- file_return_updated_rows(peri_op_summary_repo)
          write_temporary_table_to_database_and_merge(peri_op_summary_repo,
                                                      "TEMP_PT", button_name)
          
          update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                    input$selectedService3)
          
          
        }
        shinyjs::enable(button_name)
        
        
      })
      #Submit Case Management/Social Work Services -----
      observeEvent(input$submit_case_management,{
        button_name <- "submit_case_management"
        shinyjs::disable(button_name)
        flag <- 0
        case_management_file <- input$case_management
        
        if(input$name_case_management == ""){
          showModal(modalDialog(
            title = "Error",
            paste0("Please fill in the required fields"),
            easyClose = TRUE,
            footer = NULL
          ))
        }else{
          updated_user <- input$name_case_management
          file_path <- case_management_file$datapath
          #file_path <- "/SharedDrive//deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Group 1/Case Management/MSHS IP admissions for Monthly Score Cards March 2023 draft 5-8-2023.xlsx"
          tryCatch({case_management_data <- read_excel(file_path, skip = 5)
          flag <- 1
          },
          error = function(err){  showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with the case management/social work file."),
            easyClose = TRUE,
            footer = NULL
          ))
            shinyjs::enable(button_name)
          })
        }
        
        if(flag == 1){
          # Process the data into standar Summary Repo format
          print(updated_user)
          print(case_management_data)
          tryCatch({case_management_data <- case_management_function(case_management_data, updated_user)
          flag <- 2
          
          },
          error = function(err){  showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with the case management/social work file."),
            easyClose = TRUE,
            footer = NULL
          ))
            shinyjs::enable(button_name)
          })
        }
        
        
        if(flag == 2){
          ##Compare submitted results to what is in the Summary Repo in db and return only updated rows
          case_management_data <- file_return_updated_rows(case_management_data)
          
          #wirte the updated data to the Summary Repo in the server
          write_temporary_table_to_database_and_merge(case_management_data,
                                                      "TEMP_CASE_MANAGEMENT", button_name)
          
          update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
                                    input$selectedService3)
        }
        shinyjs::enable(button_name)
        
      })
      

} # Close Server



