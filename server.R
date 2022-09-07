# Increase allowable file size (Sunquest monthly files are too large for default)
if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=100*1024^2)


  server <- function(input, output, session) {
    
    
  
    # 0. Observe Events for Filters ----------------------------------------------------------------
    observeEvent(input$selectedService2,{
      metric_choices <- unique(metrics_final_df[metrics_final_df$Service %in% input$selectedService2, "Metric_Name"])
      updatePickerInput(session,
                        inputId = "selectedMetric2",
                        choices = metric_choices,
                        selected = metric_choices
      )
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE)
    
    observeEvent(input$selectedService3,{
      metric_choices <- unique(metrics_final_df[metrics_final_df$Service %in% input$selectedService3, "Metric_Name"])
      updatePickerInput(session,
                        inputId = "selectedMetric3",
                        choices = metric_choices,
                        selected = metric_choices
      )
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE)
    
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
      
      time_df <- read_excel(paste0(home_path, "time_updated.xlsx"))
      time_df <- time_df %>% filter(Service == input$selectedService)
      if(nrow(time_df) == 0){
        text = paste0("MSHS ",input$selectedService, " Summary")
      }else{
        updated <- format(max(time_df$Updated), "%Y-%m-%d %I:%M %p", tz = "America/New_York")
        text = paste0("MSHS ",input$selectedService, " Summary - Updated ",updated)
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
      
      service_input <- input$selectedService
      month_input <- input$selectedMonth
      
      min_month <- as.Date(paste0(month_input, "-01"), "%m-%Y-%d") %m-% months(12)

      # service_input <- "Lab"
      # month_input <- "05-2022"
      format <- "YYYY-MM-DD HH24:MI:SS"
      conn <- dbConnect(drv = odbc::odbc(), 
                        dsn = "OAO Cloud DB")
      mdf_tbl <- tbl(conn, "BSC_METRICS_FINAL_DF")
      metrics_final_df <- mdf_tbl %>% filter(SERVICE %in% service_input, 
                                                TO_DATE(min_month, format) <= REPORTING_MONTH) %>%
                                        select(-UPDATED_TIME, -UPDATED_USER, -METRIC_NAME_SUBMITTED) %>% collect() %>%
                                        rename(Service = SERVICE,
                                               Site = SITE,
                                               Metric_Group = METRIC_GROUP,
                                               Metric_Name = METRIC_NAME,
                                               Premier_Reporting_Period = PREMIER_REPORTING_PERIOD,
                                               value_rounded = VALUE,
                                               Reporting_Month_Ref = REPORTING_MONTH
                                        ) %>%
                                      mutate(Reporting_Month = format(Reporting_Month_Ref, "%m-%Y"))
      dbDisconnect(conn)
      



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
                                               "Premier_Reporting_Period"))  #Take all most recent data (id = 1) and merge with all data 

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
        filter(Metric_Group %!in% c("Budget to Actual", "Total Revenue to Budget Variance")) %>% # Metrics that need to be summarized by sum (total)
        mutate(`Fiscal Year to Date` = paste(`Fiscal Year to Date`," Average")) %>%
        group_by(Site,
                 Metric_Group,
                 Metric_Name_Summary,
                 Metric_Name,
                 `Fiscal Year to Date`) %>%
        summarise(value_rounded = mean(value_rounded, na.rm = TRUE)) %>%
        ungroup()
    
      # Merge for summary 
      fytd_merged <- rbind(fytd_summary_total, fytd_summary_avg, pt_exp_ytd_reformat)
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
            
      # NOTE: Should we use a str_detect instead here so we can capture Labor and Non Labor as well?
      if("Budget to Actual" %in% as.vector(status_section_metrics$Metric_Name)){
        
        #Calculate FYTD Budget to Actual Targets
        budget_to_actual_target <- metrics_final_df %>% 
          filter((Service == service_input) &
                   (Metric_Name %in% c("Budget_Total", "Budget to Actual")) & 
                   (Reporting_Month_Ref %in%
                      unique((fytd_period %>% 
                                filter(Metric_Name == "Budget to Actual"))$Reporting_Month_Ref))) %>%
          group_by(Service, Site, Metric_Group, Metric_Name) %>%
          summarise(value_rounded = sum(value_rounded)) %>%
          pivot_wider(names_from = "Metric_Name",
                      values_from = "value_rounded") %>%
          mutate(Target = round(`Budget to Actual`/ Budget_Total, 2),
                 Status = ifelse(Target >= 0, "Green", ifelse(Target < -0.02, "Red", "Yellow"))) %>%
          pivot_longer(4:5,
                       names_to = "Summary_Metric_Name",
                       values_to = "value_rounded") %>%
          filter(Summary_Metric_Name == "Budget to Actual") %>%
          mutate(Section = "Status")
        
        budget_to_actual_target <- budget_to_actual_target[,c("Section", "Summary_Metric_Name", "Site", "Status")]
        budget_to_actual_target$`Fiscal Year to Date` <-
          fytd_status$`Fiscal Year to Date`[match(budget_to_actual_target$Summary_Metric_Name, fytd_status$Metric_Name)]
      }else{
        budget_to_actual_target <- NULL
      }
      
      if("Total Revenue to Budget Variance" %in% as.vector(status_section_metrics$Metric_Name)){
        #Calculate FYTD Budget to Actual Targets
        variance_to_budget_target <- metrics_final_df %>% 
          filter((Service == service_input) &
                   (Metric_Group == "Total Revenue to Budget Variance") &
                   (Metric_Name %in% c("Budget", "Total Revenue to Budget Variance")) & 
                   (Reporting_Month_Ref %in%
                      unique((fytd_period %>% 
                                filter(Metric_Name == "Total Revenue to Budget Variance"))$Reporting_Month_Ref))) %>%
          group_by(Service, Site, Metric_Group, Metric_Name) %>%
          summarise(value_rounded = sum(value_rounded)) %>%
          pivot_wider(names_from = "Metric_Name",
                      values_from = "value_rounded") %>%
          mutate(Target = round(`Variance to Budget`/ Budget,2),
                 Status = ifelse(Target <= 0, "Green", ifelse(Target > 0.02, "Red", "Yellow"))) %>%
          pivot_longer(4:5,
                       names_to = "Summary_Metric_Name",
                       values_to = "value_rounded") %>%
          filter(Summary_Metric_Name == "Total Revenue to Budget Variance") %>%
          mutate(Section = "Status")
        
        variance_to_budget_target <- variance_to_budget_target[,c("Section", "Summary_Metric_Name", "Site", "Status")]
        variance_to_budget_target$`Fiscal Year to Date` <- fytd_status$`Fiscal Year to Date`[match(variance_to_budget_target$Summary_Metric_Name, fytd_status$Metric_Name)]
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
            group_rows(group_label = "Interventional Radiology",
                       start_row = ir_start,
                       end_row = dr_start-1,
                       label_row_css = "background-color: #212070; color: white;") %>%
            group_rows(group_label = "Diagnostic Radiology",
                       start_row = dr_start,
                       end_row = (dr_start + 2),
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
      
      time_df <- read_excel(paste0(home_path, "time_updated.xlsx"))
      time_df <- time_df %>% filter(Service == input$selectedService2)
      if(nrow(time_df) == 0){
        text = paste0("MSHS ",input$selectedService2, " Key Metric Rollup")
      }else{
        updated <- format(max(time_df$Updated), "%Y-%m-%d %I:%M %p", tz = "America/New_York")
        text = paste0("MSHS ",input$selectedService2, " Key Metric Rollup - Updated ",updated)
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
      
      
      service_input <- input$selectedService2
      month_input <- input$selectedMonth2
      site_input <- input$selectedCampus2
# 
#       service_input <- "ED"
#       month_input <- "03-2021"
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
      
      time_df <- read_excel(paste0(home_path, "time_updated.xlsx"))
      time_df <- time_df %>% filter(Service == input$selectedService3)
      if(nrow(time_df) == 0){
        text = paste0(input$selectedCampus3, " ",input$selectedService3, " Breakout")
      }else{
        updated <- format(max(time_df$Updated), "%Y-%m-%d %I:%M %p", tz = "America/New_York")
        text = paste0(input$selectedCampus3," ",input$selectedService3, " Breakout - Updated ",updated)
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
      
      
      service_input <- input$selectedService3
      month_input <- input$selectedMonth3
      site_input <- input$selectedCampus3

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
      inFile_budget <- input$finance_budget
      flag <- 0
      
      
      if (is.null(inFile_budget)) {
        return(NULL)
      }else{
        tryCatch({
          data <- read_excel(inFile_budget$datapath,  sheet = "1-Pivot Summary by Site", skip = 5)
           budget_process <- budget_raw_file_process(data)
           showModal(modalDialog(
             title = "Success",
             paste0("The data has been imported succesfully"),
             easyClose = TRUE,
             footer = NULL
           ))
        }, error = function(err){  showModal(modalDialog(
          title = "Error",
          paste0("There seems to be an issue with one of the files"),
          easyClose = TRUE,
          footer = NULL
        ))})
      }
      
      
      metrics_final_df <<- budget_to_actual_metrics_final_df(budget_process)
      saveRDS(metrics_final_df, metrics_final_df_path)
      
      
      budget_to_actual_current_summary_repo <- read_excel(budget_to_actual_path_new)
      updated_rows <- unique(budget_process[c("Service", "Site", "Metric_Name_Submitted", "Month")])
      
      budget_to_actual_current_summary_repo <- anti_join(budget_to_actual_current_summary_repo, updated_rows)
      budget_to_actual_current_summary_repo <- full_join(budget_to_actual_current_summary_repo,budget_process)
      
      write_xlsx(budget_to_actual_current_summary_repo, budget_to_actual_path_new)

      
    })
    
    # MSH, MSM, and MSQ Submit Census Days -----
    observeEvent(input$submit_finance,{
      
      inFile_census <- input$finance_census

      
      
      if (is.null(inFile_census)) {
        return(NULL)
      }else{
        data_census <- read_excel(inFile_census$datapath)
      }

    })

    
    
    ## Read in Patient Experience data -----------------------------------
    # ED Monthly Data Observe Event -------------------
    observeEvent(input$submit_monthly_pt_exp, {
      flag <- 0
      # Name ED monthly data
      ed_monthly <- input$pt_exp_ed_monthly
      
      if (is.null(ed_monthly)) {
        return(NULL)
      } else {
        ed_monthly_filepath <- ed_monthly$datapath
        
        # ed_monthly_filepath <- paste0(home_path,
        #                               "Input Data Raw/Press Ganey/",
        #                               "ED 09-2021.csv")
        
        # Try catch statement to ensure file type is correct
        tryCatch({
          data_ed_monthly <- read_csv(ed_monthly_filepath,
                                      show_col_types = FALSE)
          
          flag <- 1
        },
        error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with this Patient Experience ED file."),
            easyClose = TRUE,
            footer = NULL
          ))
        }
        )
      }
      
      # Process data if the right file format was submitted
      if(flag == 1) {
        
        tryCatch({
          # Process ED monthly data
          pt_exp_ed_monthly_summary_data <- pt_exp_dept_summary(data_ed_monthly)
          
          flag <- 2
          
          showModal(modalDialog(
            title = "Success",
            paste0("This Patient Experience ED data has been imported successfully"),
            easyClose = TRUE,
            footer = NULL
          ))
        },
        error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with this Patient Experience ED file."),
            easyClose = TRUE,
            footer = NULL
          ))
        })
      }
      
      if(flag == 2) {
        
        # Save prior version of Patient Experience Dept Summary data
        write_xlsx(pt_exp_data,
                   paste0(hist_archive_path,
                          "Pt Exp Pre-ED Monthly ",
                          format(Sys.time(), "%Y%m%d_%H%M%S"),
                          ".xlsx"))
        
        # Append Patient Experience summary with new data
        # First, identify the sites, months, and metrics in the new data
        pt_exp_new_data <- unique(
          pt_exp_ed_monthly_summary_data[c("Service",
                                           "Site",
                                           "ReportingType",
                                           "Reporting_Date_Start",
                                           "Reporting_Date_End",
                                           "Question_Clean")]
        )
        
        # Second, remove these sites, months, and metrics from the historical data, if they exist there.
        # This allows us to ensure no duplicate entries for the same site, metric, and time period
        pt_exp_data <<- anti_join(pt_exp_data,
                                  pt_exp_new_data,
                                  by = c("Service" = "Service",
                                         "Site" = "Site",
                                         "Question_Clean" = "Question_Clean",
                                         "ReportingType" = "ReportingType",
                                         "Reporting_Date_Start" = "Reporting_Date_Start",
                                         "Reporting_Date_End" = "Reporting_Date_End")
        )
        
        # Third, combine the updated historical data with the new data
        pt_exp_data <<- full_join(pt_exp_data,
                                  pt_exp_ed_monthly_summary_data)
        
        # Next, arrange the department summary by month, metric name, and site
        pt_exp_data <<- pt_exp_data %>%
          arrange(Service,
                  Site,
                  ReportingType,
                  Reporting_Date_End)
        
        # Lastly, save the updated summary data
        write_xlsx(pt_exp_data, pt_exp_table_path)
        
        # Update metrics_final_df with latest ED Patient Experience data using custom function
        metrics_final_df <<- pt_exp_metrics_final_df(pt_exp_ed_monthly_summary_data)
        
        # Save updated metrics_final_df
        saveRDS(metrics_final_df, metrics_final_df_path)
        
        # # Update "Reporting Month" drop down in each tab
        # picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
        # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        
        update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
        record_timestamp("ED Monthly Patient Experience")
        
      }
    })
    
    # Nursing Patient Experience Monthly Data Observe Event -------------------
    observeEvent(input$submit_monthly_pt_exp, {
      flag <- 0
      # Name Nursing monthly data
      nursing_monthly <- input$pt_exp_nursing_monthly
      
      if (is.null(nursing_monthly)) {
        return(NULL)
      } else {
        nursing_monthly_filepath <- nursing_monthly$datapath
        
        # nursing_monthly_filepath <- paste0(home_path,
        #                               "Input Data Raw/Press Ganey/",
        #                               "Nursing 08-2021.csv")
        
        # Try catch statement to ensure file type is correct
        tryCatch({
          
          data_nursing_monthly <- read_csv(nursing_monthly_filepath,
                                           show_col_types = FALSE)
          
          flag <- 1
        },
        error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with this Patient Experience Nursing file."),
            easyClose = TRUE,
            footer = NULL
          ))
        })
      }
      
      # Process data if the right file format was submitted
      if(flag == 1) {
        
        tryCatch({
          
          # Process Nursing monthly data
          pt_exp_nursing_monthly_summary_data <- pt_exp_dept_summary(data_nursing_monthly)
          
          flag <- 2
          
          showModal(modalDialog(
            title = "Success",
            paste0("This Patient Experience Nursing data has been imported successfully"),
            easyClose = TRUE,
            footer = NULL
          ))
        },
        error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with this Patient Experience Nursing file."),
            easyClose = TRUE,
            footer = NULL
          ))
        })
      }
      
      if(flag == 2) {
        
        # Save prior version of Patient Experience Dept Summary data
        write_xlsx(pt_exp_data,
                   paste0(hist_archive_path,
                          "Pt Exp Pre-RN Monthly ",
                          format(Sys.time(), "%Y%m%d_%H%M%S"),
                          ".xlsx"))
        
        # Append Patient Experience summary with new data
        # First, identify the sites, months, and metrics in the new data
        pt_exp_new_data <- unique(
          pt_exp_nursing_monthly_summary_data[c("Service",
                                                "Site",
                                                "ReportingType",
                                                "Reporting_Date_Start",
                                                "Reporting_Date_End",
                                                "Question_Clean")]
        )
        
        # Second, remove these sites, months, and metrics from the historical data, if they exist there.
        # This allows us to ensure no duplicate entries for the same site, metric, and time period
        pt_exp_data <<- anti_join(pt_exp_data,
                                  pt_exp_new_data,
                                  by = c("Service" = "Service",
                                         "Site" = "Site",
                                         "Question_Clean" = "Question_Clean",
                                         "ReportingType" = "ReportingType",
                                         "Reporting_Date_Start" = "Reporting_Date_Start",
                                         "Reporting_Date_End" = "Reporting_Date_End")
        )
        
        # Third, combine the updated historical data with the new data
        pt_exp_data <<- full_join(pt_exp_data,
                                  pt_exp_nursing_monthly_summary_data)
        
        # Next, arrange the department summary by month, metric name, and site
        pt_exp_data <<- pt_exp_data %>%
          arrange(Service,
                  Site,
                  ReportingType,
                  Reporting_Date_End)
        
        # Lastly, save the updated summary data
        write_xlsx(pt_exp_data, pt_exp_table_path)
        
        # Update metrics_final_df with latest Nursing Patient Experience data using custom function
        metrics_final_df <<- pt_exp_metrics_final_df(pt_exp_nursing_monthly_summary_data)
        
        # Save updated metrics_final_df
        saveRDS(metrics_final_df, metrics_final_df_path)
        
        # # Update "Reporting Month" drop down in each tab
        # picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
        # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        
        
        update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
        record_timestamp("Nursing Monthly Patient Experience")
        
      }

    })
    
    # Support Services Patient Experience Monthly Data Observe Event -------------------
    observeEvent(input$submit_monthly_pt_exp, {
      flag <- 0
      # Name Support Services monthly data
      support_monthly <- input$pt_exp_support_monthly
      
      if (is.null(support_monthly)) {
        return(NULL)
      } else {
        support_monthly_filepath <- support_monthly$datapath
        
        # support_monthly_filepath <- paste0(home_path,
        #                                    "Input Data Raw/Press Ganey/",
        #                                    "Support Services 08-2021.csv")
        
        # Try catch statement to ensure file type is correct
        tryCatch({
          
          data_support_monthly <- read_csv(support_monthly_filepath,
                                           show_col_types = FALSE)
          
          flag <- 1
          
        },
        error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with this Patient Experience Support Services file."),
            easyClose = TRUE,
            footer = NULL
          ))
        })
      }
      
      # Process data if the right file format was submitted
      if(flag == 1) {
        
        tryCatch({
          
          # Process Support Services monthly data
          pt_exp_support_monthly_summary_data <- pt_exp_dept_summary(data_support_monthly)
          
          flag <- 2
          
          showModal(modalDialog(
            title = "Sucess",
            paste0("This Patient Experience Support Services data has been imported successfully."),
            easyClose = TRUE,
            footer = NULL
          ))
        },
        error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with this Patient Experience Support Services file."),
            easyClose = TRUE,
            footer = NULL
          ))
        })
      }
      
      if(flag == 2) {
        
        # Save prior version of Patient Experience Dept Summary data
        write_xlsx(pt_exp_data,
                   paste0(hist_archive_path,
                          "Pt Exp Pre-Support Monthly ",
                          format(Sys.time(), "%Y%m%d_%H%M%S"),
                          ".xlsx"))
        
        
        
        # Append Patient Experience summary with new data
        # First, identify the sites, months, and metrics in the new data
        pt_exp_new_data <- unique(
          pt_exp_support_monthly_summary_data[c("Service",
                                                "Site",
                                                "ReportingType",
                                                "Reporting_Date_Start",
                                                "Reporting_Date_End",
                                                "Question_Clean")]
        )
        
        # Second, remove these sites, months, and metrics from the historical data, if they exist there.
        # This allows us to ensure no duplicate entries for the same site, metric, and time period
        pt_exp_data <<- anti_join(pt_exp_data,
                                  pt_exp_new_data,
                                  by = c("Service" = "Service",
                                         "Site" = "Site",
                                         "Question_Clean" = "Question_Clean",
                                         "ReportingType" = "ReportingType",
                                         "Reporting_Date_Start" = "Reporting_Date_Start",
                                         "Reporting_Date_End" = "Reporting_Date_End")
        )
        
        # Third, combine the updated historical data with the new data
        pt_exp_data <<- full_join(pt_exp_data,
                                  pt_exp_support_monthly_summary_data)
        
        # Next, arrange the department summary by month, metric name, and site
        pt_exp_data <<- pt_exp_data %>%
          arrange(Service,
                  Site,
                  ReportingType,
                  Reporting_Date_End)
        
        # Lastly, save the updated summary data
        write_xlsx(pt_exp_data, pt_exp_table_path)
        
        # Update metrics_final_df with latest Support Services Patient Experience data using custom function
        metrics_final_df <<- pt_exp_metrics_final_df(pt_exp_support_monthly_summary_data)
        
        # Save updated metrics_final_df
        saveRDS(metrics_final_df, metrics_final_df_path)
        
        # # Update "Reporting Month" drop down in each tab
        # picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
        # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        
        update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
        record_timestamp("Support Services Monthly Patient Experience")
        
        
      }
    })

    # ED YTD Data Observe Event -------------------
    observeEvent(input$submit_ytd_pt_exp, {
      flag <- 0
      # Name ED YTD data
      ed_ytd <- input$pt_exp_ed_ytd
      
      if (is.null(ed_ytd)) {
        return(NULL)
      } else {
        ed_ytd_filepath <- ed_ytd$datapath
        
        # ed_ytd_filepath <- paste0(home_path,
        #                               "Input Data Raw/Press Ganey/",
        #                               "ED YTD 012021 to 092021.csv")
        
        # TryCatch statement to ensure file type if correct
        tryCatch({
          
          data_ed_ytd <- read_csv(ed_ytd_filepath,
                                  show_col_types = FALSE)
          
          flag <- 1
        },
        error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with this Patient Experience ED file."),
            easyClose = TRUE,
            footer = NULL
          ))
        })
      }
      
      # Process data if the right file format was submitted
      if(flag == 1) {
        
        tryCatch({
          
          # Process ED YTD data
          pt_exp_ed_ytd_summary_data <- pt_exp_dept_summary(data_ed_ytd)
          
          flag <- 2
          
          showModal(modalDialog(
            title = "Success",
            paste0("This Patient Experience ED data has been imported successfully"),
            easyClose = TRUE,
            footer = NULL
          ))
        },
        error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with this Patient Experience ED file."),
            easyClose = TRUE,
            footer = NULL
          ))
        })
      }
      
      if(flag == 2) {
        
        # Save prior version of Patient Experience Dept Summary data
        write_xlsx(pt_exp_data,
                   paste0(hist_archive_path,
                          "Pt Exp Pre-ED YTD ",
                          format(Sys.time(), "%Y%m%d_%H%M%S"),
                          ".xlsx"))
        
        
        
        # Append Patient Experience summary with new data
        # First, identify the sites, months, and metrics in the new data
        pt_exp_new_data <- unique(
          pt_exp_ed_ytd_summary_data[c("Service",
                                       "Site",
                                       "ReportingType",
                                       "Reporting_Date_Start",
                                       "Reporting_Date_End",
                                       "Question_Clean")]
        )
        
        # Second, remove these sites, months, and metrics from the historical data, if they exist there.
        # This allows us to ensure no duplicate entries for the same site, metric, and time period
        pt_exp_data <<- anti_join(pt_exp_data,
                                  pt_exp_new_data,
                                  by = c("Service" = "Service",
                                         "Site" = "Site",
                                         "Question_Clean" = "Question_Clean",
                                         "ReportingType" = "ReportingType",
                                         "Reporting_Date_Start" = "Reporting_Date_Start",
                                         "Reporting_Date_End" = "Reporting_Date_End")
        )
        
        # Third, combine the updated historical data with the new data
        pt_exp_data <<- full_join(pt_exp_data,
                                       pt_exp_ed_ytd_summary_data)
        
        # Next, arrange the department summary by month, metric name, and site
        pt_exp_data <<- pt_exp_data %>%
          arrange(Service,
                  Site,
                  ReportingType,
                  Reporting_Date_End)
        
        # Lastly, save the updated summary data
        write_xlsx(pt_exp_data, pt_exp_table_path)
        
        # # Update "Reporting Month" drop down in each tab
        # picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
        # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
       
        update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
        record_timestamp("ED YTD Patient Experience")
        
        
      }
      
    })
    
    # Nursing YTD Data Observe Event -------------------
    observeEvent(input$submit_ytd_pt_exp, {
      flag <- 0
      # Name Nursing YTD data
      nursing_ytd <- input$pt_exp_nursing_ytd
      
      if (is.null(nursing_ytd)) {
        return(NULL)
      } else {
        nursing_ytd_filepath <- nursing_ytd$datapath
        
        # nursing_ytd_filepath <- paste0(home_path,
        #                               "Input Data Raw/Press Ganey/",
        #                               "Nursing YTD 012021 to 082021.csv")
        
        # TryCatch statement to ensure file type is correct
        tryCatch({
          
          data_nursing_ytd <- read_csv(nursing_ytd_filepath,
                                       show_col_types = FALSE)
          
          flag <- 1
        },
        error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with this Patient Experience Nursing file."),
            easyClose = TRUE,
            footer = NULL
          ))
        })
      }
      
      # Process data if the right file format was submitted
      if(flag == 1) {
        
        tryCatch({
          # Process Nursing YTD data
          pt_exp_nursing_ytd_summary_data <- pt_exp_dept_summary(data_nursing_ytd)
          
          flag <- 2
          
          showModal(modalDialog(
            title = "Success",
            paste0("This Patient Experience Nursing data has been imported successfully"),
            easyClose = TRUE,
            footer = NULL
          ))
        },
        error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with this Patient Experience Nursing file."),
            easyClose = TRUE,
            footer = NULL
          ))
        })
      }
      
      if(flag == 2) {
        
        # Save prior version of Patient Experience Dept Summary data
        write_xlsx(pt_exp_data,
                   paste0(hist_archive_path,
                          "Pt Exp Pre-RN YTD ",
                          format(Sys.time(), "%Y%m%d_%H%M%S"),
                          ".xlsx"))
        
        
        
        # Append Patient Experience summary with new data
        # First, identify the sites, months, and metrics in the new data
        pt_exp_new_data <- unique(
          pt_exp_nursing_ytd_summary_data[c("Service",
                                            "Site",
                                            "ReportingType",
                                            "Reporting_Date_Start",
                                            "Reporting_Date_End",
                                            "Question_Clean")]
        )
        
        # Second, remove these sites, months, and metrics from the historical data, if they exist there.
        # This allows us to ensure no duplicate entries for the same site, metric, and time period
        pt_exp_data <<- anti_join(pt_exp_data,
                                  pt_exp_new_data,
                                  by = c("Service" = "Service",
                                         "Site" = "Site",
                                         "Question_Clean" = "Question_Clean",
                                         "ReportingType" = "ReportingType",
                                         "Reporting_Date_Start" = "Reporting_Date_Start",
                                         "Reporting_Date_End" = "Reporting_Date_End")
        )
        
        # Third, combine the updated historical data with the new data
        pt_exp_data <<- full_join(pt_exp_data,
                                  pt_exp_nursing_ytd_summary_data)
        
        # Next, arrange the department summary by month, metric name, and site
        pt_exp_data <<- pt_exp_data %>%
          arrange(Service,
                  Site,
                  ReportingType,
                  Reporting_Date_End)
        
        # Lastly, save the updated summary data
        write_xlsx(pt_exp_data, pt_exp_table_path)
        
        # # Update "Reporting Month" drop down in each tab
        # picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
        # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        
        update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
        record_timestamp("Nursing YTD Patient Experience")
        
        
      }
      
    })
    
    # Support Services YTD Data Observe Event -------------------
    observeEvent(input$submit_ytd_pt_exp, {
      flag <- 0
      # Name Support Services YTD data
      support_ytd <- input$pt_exp_support_ytd
      
      if (is.null(support_ytd)) {
        return(NULL)
      } else {
        support_ytd_filepath <- support_ytd$datapath
        
        # support_ytd_filepath <- paste0(home_path,
        #                               "Input Data Raw/Press Ganey/",
        #                               "Support Services YTD 012021 to 082021.csv")
        
        # TryCatch statement to ensure file type is correct
        tryCatch({
          
          data_support_ytd <- read_csv(support_ytd_filepath,
                                       show_col_types = FALSE)
          
          flag <- 1
        },
        error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with this Patient Experience Support Services file."),
            easyClose = TRUE,
            footer = NULL
          ))
        })
      }
      
      # Process data if the right file format was submitted
      if(flag == 1) {
        
        tryCatch({
          
          # Process Support Services YTD data
          pt_exp_support_ytd_summary_data <- pt_exp_dept_summary(data_support_ytd)
          
          flag <- 2
          
          showModal(modalDialog(
            title = "Success",
            paste0("This Patient Experience Support Services data has been imported successfully"),
            easyClose = TRUE,
            footer = NULL
          ))
        },
        error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with this Patient Experience Support Services file."),
            easyClose = TRUE,
            footer = NULL
          ))
        })
      }
      
      if(flag == 2) {
        
        # Save prior version of Patient Experience Dept Summary data
        write_xlsx(pt_exp_data,
                   paste0(hist_archive_path,
                          "Pt Exp Pre-Support YTD ",
                          format(Sys.time(), "%Y%m%d_%H%M%S"),
                          ".xlsx"))
        
        
        
        # Append Patient Experience summary with new data
        # First, identify the sites, months, and metrics in the new data
        pt_exp_new_data <- unique(
          pt_exp_support_ytd_summary_data[c("Service",
                                            "Site",
                                            "ReportingType",
                                            "Reporting_Date_Start",
                                            "Reporting_Date_End",
                                            "Question_Clean")]
        )
        
        # Second, remove these sites, months, and metrics from the historical data, if they exist there.
        # This allows us to ensure no duplicate entries for the same site, metric, and time period
        pt_exp_data <<- anti_join(pt_exp_data,
                                  pt_exp_new_data,
                                  by = c("Service" = "Service",
                                         "Site" = "Site",
                                         "Question_Clean" = "Question_Clean",
                                         "ReportingType" = "ReportingType",
                                         "Reporting_Date_Start" = "Reporting_Date_Start",
                                         "Reporting_Date_End" = "Reporting_Date_End")
        )
        
        # Third, combine the updated historical data with the new data
        pt_exp_data <<- full_join(pt_exp_data,
                                       pt_exp_support_ytd_summary_data)
        
        # Next, arrange the department summary by month, metric name, and site
        pt_exp_data <<- pt_exp_data %>%
          arrange(Service,
                  Site,
                  ReportingType,
                  Reporting_Date_End)
        
        # Lastly, save the updated summary data
        write_xlsx(pt_exp_data, pt_exp_table_path)
        
        # # Update "Reporting Month" drop down in each tab
        # picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
        # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        
        update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
        record_timestamp("Support Services YTD Patient Transport")
        
        
      }

    })
    
    
    ## Productivity data --------------------------------
    ## Read in productivity data and process
    observeEvent(input$submit_prod,{
      inFile <- input$productiviy_data
      data <- read_excel(inFile$datapath)


      tryCatch({prod_summary <- productivity_dept_summary(data)
        metrics_final_df <<- productivity_metrics_final_df(prod_summary)
        saveRDS(metrics_final_df, metrics_final_df_path)
        write_xlsx(prod_summary, productivity_file_path)
        
      showModal(modalDialog(
        title = "Success",
        paste0("The data has been imported succesfully"),
        easyClose = TRUE,
        footer = NULL
      ))
      }, error = function(err){  showModal(modalDialog(
        title = "Error",
        paste0("There seems to be an issue with one of the files"),
        easyClose = TRUE,
        footer = NULL
      ))})
      record_timestamp("Productivity")
      
    })
    
    
    # Submit Food Services -----
    observeEvent(input$submit_food,{
      food_file <- input$food_cost_and_revenue
      flag <- 0
      
      if(is.null(food_file)){
        return(NULL)
      }else{
       food_file_path <- food_file$datapath
       #food_file_path <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/Food/MSHS Workforce Data Request_Food_RecurringRequest 2021_Oct21.xlsx"
       tryCatch({food_data <- read_excel(food_file_path, sheet = "Cost and Revenue")
       food_data_rev <- read_excel(food_file_path, sheet = "Rev Budget")
       flag <- 1
       showModal(modalDialog(
         title = "Success",
         paste0("The data has been imported succesfully"),
         easyClose = TRUE,
         footer = NULL
       ))
       }, error = function(err){  showModal(modalDialog(
         title = "Error",
         paste0("There seems to be an issue with one of the files"),
         easyClose = TRUE,
         footer = NULL
       ))})
      }

      #Save prior version of Food Dept Summary data
      write_xlsx(cost_and_revenue_repo,
                 paste0(hist_archive_path,
                        "Food Services Updates ",
                        format(Sys.time(), "%Y%m%d_%H%M%S"),
                        ".xlsx"))
      
      if (flag == 1){
      # Process Cost and Revenue data
        tryCatch({food_summary_data <- cost_and_revenue_dept_summary(food_data)
                  food_summary_budget <- rev_budget_dept_summary(food_data_rev)
        flag <- 2
        showModal(modalDialog(
          title = "Success",
          paste0("The data has been imported succesfully"),
          easyClose = TRUE,
          footer = NULL
        ))
        }, error = function(err){  showModal(modalDialog(
          title = "Error",
          paste0("There seems to be an issue with one of the files"),
          easyClose = TRUE,
          footer = NULL
        ))})
      }
      
      if (flag == 2){
        food_summary_data <- cost_budget_combine(food_summary_data,food_summary_budget)
        # Append Food summary with new data
        # First, identify the sites, months, and metrics in the new data
        food_new_data <- unique(
          food_summary_data[  c("Service", "Site", "Month", "Metric")]
        )
        
        
        # Second, remove these sites, months, and metrics from the historical data, if they exist there.
        # This allows us to ensure no duplicate entries for the same site, metric, and time period
        cost_and_revenue_repo <<- anti_join(cost_and_revenue_repo,
                                            food_new_data,
                                          by = c("Service" = "Service",
                                                 "Site" = "Site",
                                                 "Month" = "Month",
                                                 "Metric" = "Metric")
        )
        
        food_summary_data$`Revenue Budget` <- as.double(food_summary_data$`Revenue Budget`)
        # Third, combine the updated historical data with the new data
        cost_and_revenue_repo <<- full_join(cost_and_revenue_repo,
                                            food_summary_data)
        # Lastly, save the updated summary data
        write_xlsx(cost_and_revenue_repo, paste0(home_path, "Summary Repos/Food Services Cost and Revenue.xlsx"))
        
        cost_and_revenue_repo$Month <- as.Date(cost_and_revenue_repo$Month)
        
        # Update metrics_final_df with latest SCC data using custom function
        metrics_final_df <<- census_days_metrics_final_df(food_summary_data)
        saveRDS(metrics_final_df, metrics_final_df_path)
        # Save updated metrics_final_df
        #saveRDS(metrics_final_df, paste0(home_path, "Summary Repos/Food Services Cost and Revenue.xlsx"))
  
        # # Update "Reporting Month" drop down in each tab
        # picker_choices <- format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
        # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        
        update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
        
        record_timestamp("Food Services")
        # time_df <- read_excel(paste0(home_path, "time_updated.xlsx"))
        # date_time <- data.frame(Updated = as.POSIXct(Sys.time()))
        # date_time$Service = "Food Services"
        # date_time <- rbind(time_df, date_time)
        # write_xlsx(date_time, paste0(home_path, "time_updated.xlsx"))
        
        
      }
      
    })
    
    # Submit Interventional Radiology -----
    observeEvent(input$submit_imaging, {
      imaging_file <- input$imaging_IR
      flag <- 0
      
      if(is.null(imaging_file)){
        return(NULL)
      }else{
        imaging_filepath <- imgaing_file <- imaging_file$datapath
        #imaging_filepath <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/Imaging/FTI-BalancedScorecard-2021-Jan1-Nov30 (1).xlsx"
        tryCatch({imaging_data <- read_excel(imaging_filepath)
            flag <- 1
        }, error = function(err){  showModal(modalDialog(
          title = "Error",
          paste0("There seems to be an issue with one of the files"),
          easyClose = TRUE,
          footer = NULL
        ))})
      }
      
      # Save prior version of Lab TAT Dept Summary data
      write_xlsx(imaging_repo,
                 paste0(hist_archive_path,
                        "Imaging-IR ",
                        format(Sys.time(), "%Y%m%d_%H%M%S"),
                        ".xlsx"))
      
      if(flag == 1){
        # Process Imaging data
        tryCatch({imaging_summary_data <- imaging_dept_summary(imaging_data)
                    flag <- 2
                    showModal(modalDialog(
                      title = "Success",
                      paste0("The data has been imported succesfully"),
                      easyClose = TRUE,
                      footer = NULL
                    ))
        }, error = function(err){  showModal(modalDialog(
          title = "Error",
          paste0("There seems to be an issue with one of the files"),
          easyClose = TRUE,
          footer = NULL
        ))})
      }
      
      if(flag == 2){
        
        # Append Imaging with new data
        # First, identify the sites, months, and metrics in the new data
        imaging_new_data <- unique(
          imaging_summary_data[  c("Service", "Site", "Reporting_Month", "Metric_Name_Submitted")]
        )
        
        # Second, remove these sites, months, and metrics from the historical data, if they exist there.
        # This allows us to ensure no duplicate entries for the same site, metric, and time period
        imaging_repo <<- anti_join(imaging_repo,
                                   imaging_new_data,
                                          by = c("Service" = "Service",
                                                 "Site" = "Site",
                                                 "Reporting_Month" = "Reporting_Month",
                                                 "Metric_Name_Submitted" = "Metric_Name_Submitted")
        )
        
        # Third, combine the updated historical data with the new data
        imaging_repo <<- full_join(imaging_repo,
                                   imaging_summary_data)
        
  
        
        # Lastly, save the updated summary data
        write_xlsx(imaging_repo, paste0(home_path, "Summary Repos/Imaging-IR.xlsx"))
        
        # Update metrics_final_df with latest SCC data using custom function
        metrics_final_df <<- imaging_metrics_final_df(imaging_summary_data)
        
        # Save updated metrics_final_df
        saveRDS(metrics_final_df, metrics_final_df_path)
        
        # Update "Reporting Month" drop down in each tab
        # picker_choices <- format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
        # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        
        update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
        
        
        record_timestamp("Imaging")
        
        # time_df <- read_excel(paste0(home_path, "time_updated.xlsx"))
        # date_time <- data.frame(Updated = as.POSIXct(Sys.time()))
        # date_time$Service = "Imaging"
        # date_time <- rbind(time_df, date_time)
        # write_xlsx(date_time, paste0(home_path, "time_updated.xlsx"))
      
        
      }
      
    })
    
    # Submit Engineering -----
    observeEvent(input$submit_engineering,{
      flag <- 0
      if(input$name_engineering_kpi == ""){
        showModal(modalDialog(
          title = "Error",
          paste0("Please fill in the required fields"),
          easyClose = TRUE,
          footer = NULL
        ))
      }else {
        
        tryCatch({
          # Convert rhandsontable to R object
          engineering_manual_updates <<- hot_to_r(input$engineering_kpi)
          engineering_manual_updates <<- engineering_manual_updates %>% filter(!(Metric %in% c("Total Critical PMs", "Number of Work Orders Created with a Life Safety Priority", "EOC/Patient Care Work Orders Received")))          
          engineering_manual_updates[engineering_manual_updates == "N/A"] <- NA
          engineering_manual_updates <<- remove_empty_manual_columns(engineering_manual_updates)  
          flag <- 1
        },
        error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with the Engineering data entered. 1"),
            easyClose = TRUE,
            footer = NULL
          ))
        })
        
        
        if (flag == 1) {
          
          user_format_error <<- manual_format_check(engineering_manual_updates)
          
          if (user_format_error) {
            
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with the data entered. Data should be entered as a decimal between 0 and 1."),
              easyClose = TRUE,
              footer = NULL
            ))
            
          } else {
            
            # Check that data can be reformatted for department summary repo
            tryCatch({
              
              engineering_manual_updates_table <<- hot_to_r(input$engineering_kpi)
              
              empty_columns <- colSums(is.na(engineering_manual_updates_table) | engineering_manual_updates_table == "") == nrow(engineering_manual_updates_table)
              engineering_manual_updates_table <- engineering_manual_updates_table[, !empty_columns]
              # Reformat data from manual input table into department summary format
              engineering_summary_data <<-
                # lab_prof_test_dept_summary(prof_test_manual_table)
                engineering_summary_repos(engineering_manual_updates_table)

              flag <- 2
              
              showModal(modalDialog(
                title = "Success",
                paste0("The Engineering data has been submitted successfully."),
                easyClose = TRUE,
                footer = NULL
              ))
            },
            error = function(err){
              showModal(modalDialog(
                title = "Error",
                paste0("There seems to be an issue with the Engineering data entered."),
                easyClose = TRUE,
                footer = NULL
              ))
            })
            
            if(flag == 2) {
              
              engineering_summary_data$Month <- as.Date(paste0(engineering_summary_data$Month, "-01"), "%m-%Y-%d")
              engineering_summary_data$`Number of Work Orders Created with a Life Safety Priority` <- as.double(engineering_summary_data$`Number of Work Orders Created with a Life Safety Priority`)
              
              
              
              engineering_columns <- c("Site", "Month", "% of Critical PM's Completed on Time", 
                "Total Critical PMs","Work Order Completion Rate",
                "Number of Work Orders Created with a Life Safety Priority",
                "EOC/Patient Care Work Orders Received",
                "EOC/Patient Care Work Order Completion Rate")
              
              save_append_join_updates(engineering_summary_repos_data, 
                                       engineering_summary_data, 
                                       "Engineerng Metrics Pre Updates", 
                                       engineering_columns, 
                                       operational_metrics_engineering_path, cm_kpi)
              
              # # Save prior version of Lab Proficiency Testing Dept Summary data
              # write_xlsx(engineering_summary_repos_data,
              #            paste0(hist_archive_path,
              #                   "Engineerng Metrics Pre Updates ",
              #                   format(Sys.time(), "%Y%m%d_%H%M%S"),
              #                   ".xlsx"))
              # 
              # # Append Lab Proficiency Testing summary with new data
              # # First, identify the sites, months, and metrics in the new data
              # engineering_new_data <- unique(
              #   engineering_summary_data[, c("Site", "Month", "% of Critical PM's Completed on Time", "Total Critical PMs","Work Order Completion Rate","Number of Work Orders Created with a Life Safety Priority","EOC/Patient Care Work Orders Received","EOC/Patient Care Work Order Completion Rate")]
              # )
              # 
              # # Second, remove these sites, months, and metrics from the historical data, if they exist there
              # # This allows us to ensure no duplicate entries for the same site, metric, and time period
              # engineering_summary_repos_data  <<- anti_join(engineering_summary_repos_data,
              #                                  engineering_new_data)#,
              #                                  # by = c("Site" = "Site",
              #                                  #        "Month" = "Month",
              #                                  #        "% of Critical PM's Completed on Time" = "% of Critical PM's Completed on Time",
              #                                  #        "Total Critical PMs","Work Order Completion Rate" = "Total Critical PMs","Work Order Completion Rate",
              #                                  #        "Number of Work Orders Created with a Life Safety Priority" = "Number of Work Orders Created with a Life Safety Priority",
              #                                  #        "EOC/Patient Care Work Orders Received" ="EOC/Patient Care Work Orders Received",
              #                                  #        "EOC/Patient Care Work Order Completion Rate" = "EOC/Patient Care Work Order Completion Rate"))
              # 
              # # Third, combine the updated historical data with the new data
              # engineering_summary_repos_data  <<- full_join(engineering_summary_repos_data,
              #                                  engineering_summary_data)
              # 
              # # Lastly, save the updated summary data
              # engineering_summary_repos_data$Month <- as.Date(as.character(engineering_summary_repos_data$Month))
              # #write_xlsx(engineering_summary_repos_data, operational_metrics_engineering_path)
              # 
              # # Update metrics_final_df with latest Proficiency Testing data using custom function
              # metrics_final_df <<- cm_kpi(engineering_summary_data)
              # 
              # # Save updated metrics_final_df
              # saveRDS(metrics_final_df, metrics_final_df_path)
              # 
              
              update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
              record_timestamp("Engineering")
              
              
            }
            
          }
          
        }
        
      }

    
    })
    # Engineering Reactive dataset -----
    data_engineering_kpi <- reactive({
      
      #input$submit_engineering
      #operational_metrics_engineering <- engineering_repo_pull()
      data  <- operational_metrics_engineering

      colnames(data)[grepl("-",colnames(data))] <- format(as.Date(colnames(data)[grepl("-",colnames(data))]), "%m-%Y")
      
      result <- manual_table_month_order(data)

      
      result <- result %>% 
        mutate_if(is.logical, as.character) %>%
        mutate_if(is.double, as.character)
      
      
      
      
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
    
    # Submit Environemental Services -----
    observeEvent(input$submit_evs,{
      
      evs_file <- input$evs_data
      flag <- 0
      
      if (is.null(evs_file)) {
        return(NULL)
      }else{
        file_path <- evs_file$datapath
        #file_path <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/EVS/MSHS Normal Clean vs Iso Clean TAT Sept 2021.xlsx"
        tryCatch({evs_data <- read_excel(file_path)
                  month <- excel_sheets(file_path)[1]
                  flag <- 1
        },
        error = function(err){  showModal(modalDialog(
          title = "Error",
          paste0("There seems to be an issue with the enviromental services file"),
          easyClose = TRUE,
          footer = NULL
        ))})
      }
      
      if(flag == 1){
      tryCatch({evs_data <- evs_file_process(evs_data,month)
                  flag <- 2
        
        showModal(modalDialog(
          title = "Success",
          paste0("The environmental services data has been imported succesfully"),
          easyClose = TRUE,
          footer = NULL
        ))
        },
        error = function(err){  showModal(modalDialog(
          title = "Error",
          paste0("There seems to be an issue with the enviromental services file"),
          easyClose = TRUE,
          footer = NULL
        ))})
      }
      

      if(flag == 2){
        #Save prior version of Lab TAT Dept Summary data
        write_xlsx(summary_repos_environmental,
                   paste0(hist_archive_path,
                          "EVS historical ",
                          format(Sys.time(), "%Y%m%d_%H%M%S"),
                          ".xlsx"))

        metrics_final_df <<- evs__metrics_final_df_process(evs_data)
        
        saveRDS(metrics_final_df, metrics_final_df_path)
        
        evs_summary_repo <- read_excel(evs_table_path)
        updated_rows <- unique(evs_data[c("Service","Site", "Month")])
        updated_rows$Month <- as.Date(updated_rows$Month, "%m/%d/%Y")
        
        evs_summary_repo <- anti_join(evs_summary_repo, updated_rows)
        evs_summary_repo <- evs_summary_repo %>% filter(!is.na(Month))
        
        evs_data$Month <- as.Date(evs_data$Month, "%m/%d/%Y")
        evs_data$`Non-Isolation Requests` <- as.character(evs_data$`Non-Isolation Requests`)
        evs_data$`Non-Isolation  % > 90 mins` <- as.character(evs_data$`Non-Isolation  % > 90 mins`)
        evs_data$`Non-IsolationAverage TAT` <- as.character(evs_data$`Non-IsolationAverage TAT`)
        evs_data$`Isolation Requests` <- as.character(evs_data$`Isolation Requests`)
        evs_data$`Isolation % > 90 mins` <- as.character(evs_data$`Isolation % > 90 mins`)
        evs_data$`Isolation Average TAT` <- as.character(evs_data$`Isolation Average TAT`)
        
        
        
        evs_summary_repo <- full_join(evs_summary_repo, evs_data)
        evs_summary_repo <- as.data.frame(evs_summary_repo)
        write_xlsx(evs_summary_repo, evs_table_path)
        
        # picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
        # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        
        update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
        # time_df <- read_excel(paste0(home_path, "time_updated.xlsx"))
        # date_time <- data.frame(Updated = as.POSIXct(Sys.time()))
        # date_time$Service = "Environmental Services"
        # date_time <- rbind(time_df, date_time)
        # write_xlsx(date_time, paste0(home_path, "time_updated.xlsx"))
        record_timestamp("Environmental Services")
      }
      
    })
    

   

   
    # Lab KPI - Turnaround Time ------------
    # SCC Data submission -----------------
    observeEvent(input$submit_lab_tat,{
      
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
          
          showModal(modalDialog(
            title = "Success",
            paste0("This SCC data has been imported successfully."),
            easyClose = TRUE,
            footer = NULL
          ))
        },
        error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with this SCC file."),
            easyClose = TRUE,
            footer = NULL
          ))
        })
      }
      
      if(flag == 2){
        
        write_temporary_table_to_database_and_merge(scc_summary_data,
                                                    "TEMP_SCC_TAT")
        
        update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
        
      }
    }
    )
    
    # Sunquest data submission -------------------
    observeEvent(input$submit_lab_tat,{
      
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
          # sun_file_path <- paste0("J:/deans/Presidents/HSPI-PM",
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
          
          showModal(modalDialog(
            title = "Success",
            paste0("This Sunquest data has been imported successfully"),
            easyClose = TRUE,
            footer = NULL
          ))
        },
        error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with this Sunquest file."),
            easyClose = TRUE,
            footer = NULL
          ))
        })
      }
      
      if(flag == 2) {
        
        write_temporary_table_to_database_and_merge(sun_summary_data,
                                                    "TEMP_SUN_TAT")
        
        update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)

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
          prof_test_manual_updates <<- hot_to_r(input$lab_prof_test)
          
          # Identify columns with no data in them and remove before further processing
          # This ensures months with no data do not get added to the department summary
          # repo and metrics_final_df repository
          non_empty_cols <- !(apply(prof_test_manual_updates,
                                    MARGIN = 2,
                                    function(x) 
                                      all(is.na(x))))
          
          prof_test_manual_updates <<- prof_test_manual_updates[, non_empty_cols]
          
          flag <- 1
        },
        error = function(err){
          showModal(modalDialog(
            title = "Error",
            paste0("There seems to be an issue with the Proficiency Test data entered."),
            easyClose = TRUE,
            footer = NULL
          ))
        })
        
        
        if (flag == 1) {
          
          # Check Proficiency Test data to make sure user entered data in correct format
          # ie, number between 0 and 1, no spaces, percentage signs, etc.
          user_format_error <<- any(
            apply(X = prof_test_manual_updates[, 3:ncol(prof_test_manual_updates)],
                  MARGIN = 2,
                  function(x)
                    # Determine if there are issues converting any user entries to numeric values
                    # ie, if the user enters "%" or text, the entry will be converted to NA
                    is.na(
                      suppressWarnings(
                        as.numeric(
                          str_replace_na(x, replacement = "0")
                        )
                      )
                    )
            )
          ) |
            any(
              apply(X = prof_test_manual_updates[, 3:ncol(prof_test_manual_updates)],
                    MARGIN = 2,
                    function(x)
                      # Determine if numeric value is greater than 1
                      max(
                        suppressWarnings(
                          as.numeric(
                            str_replace_na(x, replacement = "0")
                          )
                        ), na.rm = TRUE
                      ) > 1
              )
            )
          
          if (user_format_error) {
            
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with the data entered. Data should be entered as a decimal between 0 and 1."),
              easyClose = TRUE,
              footer = NULL
            ))
            
          } else {
            
            # Check that data can be reformatted for department summary repo
            tryCatch({
              
              existing_data <- sql_manual_table_output("Lab",
                                                       "proficiency_testing")
              # Arrange by sites in alphabetical order
              existing_data <- existing_data %>%
                arrange(Site)
              
              
              existing_data <- manual_table_month_order(existing_data)
              
              existing_data <- existing_data %>%
                pivot_longer(cols = -contains(c("Site", "Metric")),
                             names_to = "Month",
                             values_to = "Value") %>%
                mutate(Value = as.numeric(Value),
                       Month = as.Date(paste0(Month, "-01"),
                                       format = "%m-%Y-%d"))
              
              # Reformat data from manual input table into department summary format
              prof_test_summary_data <-
                # lab_prof_test_dept_summary(prof_test_manual_table)
                lab_prof_test_dept_summary(prof_test_manual_updates,
                                           updated_user)
              
              prof_test_summary_data <- anti_join(prof_test_summary_data,
                                                   existing_data,
                                                   by = c(
                                                     "SITE" = "Site",
                                                     "METRIC_NAME_SUBMITTED" =
                                                       "Metric",
                                                     "REPORTING_MONTH" =
                                                       "Month",
                                                     "VALUE" = "Value"))
              
              flag <- 2
              
              showModal(modalDialog(
                title = "Success",
                paste0("This Proficiency Test data has been submitted successfully."),
                easyClose = TRUE,
                footer = NULL
              ))
            },
            error = function(err){
              showModal(modalDialog(
                title = "Error",
                paste0("There seems to be an issue with the Proficiency Test data entered."),
                easyClose = TRUE,
                footer = NULL
              ))
            })
            
            if(flag == 2) {
              
              write_temporary_table_to_database_and_merge(prof_test_summary_data,
                                                          "TEMP_PROF_TEST")

              update_picker_choices_sql(session, input$selectedService, input$selectedService2, input$selectedService3)
            
            }
            
          }
          
        }
        
      }

    })

      
      # Security Metrics - Incident Reports (Manual Entry) -----------------------
      # Create reactive data table for manual entry
      data_sec_inc_rpts <- reactive({
        
        tbl <- manual_table_month_order(sec_inc_rpts_manual_table)
        
        
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
        if(input$sec_inc_rpts_username == "") {
          showModal(modalDialog(
            title = "Error",
            "Please fill in the required fields.",
            easyClose = TRUE,
            footer = NULL
          ))
        } else {
          
          tryCatch({
            
            # Convert rhandsontable to R object
            sec_inc_rpts_manual_updates <<- hot_to_r(input$sec_inc_rpts)
            
            # Identify columns with no data in them and remove before further processing
            # This ensures months with no data do not get added to the department summary
            # repo and metrics_final_df repository
            non_empty_cols <- !(apply(sec_inc_rpts_manual_updates,
                                  MARGIN = 2,
                                  function(x) 
                                    all(is.na(x))))
            
            # non_empty_cols <- !(apply(data_sec_inc_rpts,
            #                           MARGIN = 2,
            #                           function(x) 
            #                             all(is.na(x))))
            
            sec_inc_rpts_manual_updates <<- sec_inc_rpts_manual_updates[, non_empty_cols]
            
            # sec_inc_rpts_manual_updates <<- data_sec_inc_rpts[, non_empty_cols]
            
            flag <- 1
            
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with the Security Incident Reports data entered."),
              easyClose = TRUE,
              footer = NULL
            ))
          })
          
          if(flag == 1) {
            
            tryCatch({
              
              # Reformat data from manual input table into department summary format
              sec_inc_rpts_summary_data <<-
                sec_inc_rpts_dept_summary(sec_inc_rpts_manual_updates)

              flag <- 2
              
              showModal(modalDialog(
                title = "Success",
                paste0("This Security Incident Reports data has been submitted successfully."),
                easyClose = TRUE,
                footer = NULL
              ))
            },
            error = function(err) {
              showModal(modalDialog(
                title = "Error",
                paste0("There seems to be an issue with the Security Incident Reports data entered."),
                easyClose = TRUE,
                footer = NULL
              ))
            })
          }
          
          if(flag == 2) {
            
            # Save prior version of Security Incident Reports Dept Summary data
            write_xlsx(security_incident_reports,
                       paste0(hist_archive_path,
                              "Security Incident Reports Pre Updates ",
                              format(Sys.time(), "%Y%m%d_%H%M%S"),
                              ".xlsx"))



            # Append Security Incident Reports summary with new data
            # First, identify the sites, months, and metrics in the new data
            sec_inc_rpts_new_data <- unique(
              sec_inc_rpts_summary_data[, c("Service", "Site", "Month", "Metric")]
            )

            # Second, remove these sites, months, and metrics from the historical data,
            # if they exist there. This allows us to ensure no duplicate entries for
            # the same site, metric, and time period.
            security_incident_reports <<- anti_join(security_incident_reports,
                                                    sec_inc_rpts_new_data,
                                                    by = c("Service" = "Service",
                                                           "Site" = "Site",
                                                           "Month" = "Month",
                                                           "Metric" = "Metric"))

            # Third, combine the updated historical data with the new data
            security_incident_reports <<- full_join(security_incident_reports,
                                                    sec_inc_rpts_summary_data)

            # Next, arrange the incident reports summary data by month, metric, and site
            security_incident_reports <<- security_incident_reports %>%
              arrange(Month,
                      desc(Metric),
                      Site)

            # Lastly, save the updated summary data
            write_xlsx(security_incident_reports, security_incident_reports_path)

            # Update metrics_final_df with latest data using custom function
            metrics_final_df <<- sec_inc_rpts_metrics_final_df(sec_inc_rpts_summary_data)

            # # Code for running entire department summary into metrics_final_df
            # metrics_final_df <<- sec_inc_rpts_metrics_final_df(security_incident_reports)

            # Save updates metrics_final_df
            saveRDS(metrics_final_df, metrics_final_df_path)

            # picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
            # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # 
            # time_df <- read_excel(paste0(home_path, "time_updated.xlsx"))
            # date_time <- data.frame(Updated = as.POSIXct(Sys.time()))
            # date_time$Service = "Security"
            # date_time <- rbind(time_df, date_time)
            # write_xlsx(date_time, paste0(home_path, "time_updated.xlsx"))
            update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
            record_timestamp("Security")
            
            
          }
          
        }
        
      })
      
      # Security Metrics - Security Events (Manual Entry) -------------------
      # Create reactive data table for manual entry
      data_sec_events <- reactive({
        

        tbl <- manual_table_month_order(sec_events_manual_table)
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
        if(input$sec_events_username == "") {
          showModal(modalDialog(
            title = "Error",
            "Please fill in the required fields.",
            easyClose = TRUE,
            footer = NULL
          )
          )
        } else {
          
          tryCatch({
            
            # Convert rhandsontable to R object
            sec_events_manual_updates <<- hot_to_r(input$sec_events)
            
            # Identify columns with no data in them and remove before further processing
            # This ensures months with no data do not get added to the department summary
            # repo and metrics_final_df repository
            non_empty_cols <- !(apply(sec_events_manual_updates,
                                      MARGIN = 2,
                                      function(x) 
                                        all(is.na(x))))
            
            # non_empty_cols <- !(apply(data_sec_events,
            #                           MARGIN = 2,
            #                           function(x) 
            #                             all(is.na(x))))
            
            sec_events_manual_updates <<- sec_events_manual_updates[, non_empty_cols]
            
            # sec_events_manual_updates <<- data_sec_events[, non_empty_cols]
            
            flag <- 1
            
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with the Security Events data entered."),
              easyClose = TRUE,
              footer = NULL
            ))
          })
          
          if(flag == 1) {
            
            tryCatch({
              
              # Reformat data from manual input table into department summary format
              sec_events_summary_data <-
                sec_events_dept_summary(sec_events_manual_updates)
              
              flag <- 2
              
              showModal(modalDialog(
                title = "Success",
                paste0("This Security Events data has been submitted successfully."),
                easyClose = TRUE,
                footer = NULL
              ))
              
            },
            error = function(err) {
              showModal(modalDialog(
                title = "Error",
                paste0("There seems to be an issue with the Security Events data entered."),
                easyClose = TRUE,
                footer = NULL
              ))
            })
          }
          
          if(flag == 2) {
            
            # Save prior version of Monthly Security Events Dept Summary data
            write_xlsx(security_events,
                       paste0(hist_archive_path,
                              "Security Events Monthly Pre Updates ",
                              format(Sys.time(), "%Y%m%d_%H%M%S"),
                              ".xlsx"))
            
            
            
            # Append Security Events Monthly summary with new data
            # First, identify the sites, months, and metrics in the new data
            sec_events_new_data <- unique(
              sec_events_summary_data[, c("Service", "Site", "Month", "Metric")]
            )
            
            # Second, remove these sites, months, and metrics from the historical data,
            # if they exist there. This allows us to ensure no duplicate entries for
            # the same site, metric, or time period.
            security_events <<- anti_join(security_events,
                                          sec_events_new_data,
                                          by = c("Service" = "Service",
                                                 "Site" = "Site",
                                                 "Month" = "Month",
                                                 "Metric" = "Metric"))
            
            # Third, combine the updated historical data with the new data
            security_events <<- full_join(security_events,
                                          sec_events_summary_data)
            
            # Next, arrance the security events summary data by month, metric, and site
            security_events <<- security_events %>%
              arrange(Month,
                      desc(Metric),
                      Site)
            
            # Lastly, save the updated summary data
            write_xlsx(security_events, security_events_path)
            
            # Update metrics_final_df with the latest data using custom function
            metrics_final_df <<- sec_events_metrics_final_df(sec_events_summary_data)
            
            # # Code for running entire department summary history into metrics_final_df
            # metrics_final_df <<- sec_events_metrics_final_df(security_events)
            
            # Save updated metrics_final_df
            saveRDS(metrics_final_df, metrics_final_df_path)
            
            # picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
            # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # 
            # time_df <- read_excel(paste0(home_path, "time_updated.xlsx"))
            # date_time <- data.frame(Updated = as.POSIXct(Sys.time()))
            # date_time$Service = "Security"
            # date_time <- rbind(time_df, date_time)
            # write_xlsx(date_time, paste0(home_path, "time_updated.xlsx"))
            
            update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
            record_timestamp("Security")

            
          }
          
        }
        
      })

    # 5. Overtime - Data Input ---------------------------------------------------------------------------------
    observeEvent(input$submit_finance, {
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
        ))})
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
      ))})
      
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
      
    })
    
      
    observeEvent(input$submit_finance_ot, {
      
      if(input$name_finance == "") {
        showModal(modalDialog(
          title = "Error",
          "Please fill in the required fields.",
          easyClose = TRUE,
          footer = NULL
        )
        )
      }
      
      overtime_file <- input$finance_overtime
      
      
      flag <- 0
      
      if(is.null(overtime_file)){
        return(NULL)
      } else{
        overtime_file_path <- overtime_file$datapath
        #overtime_file_path <- paste0(home_path,"Input Data Raw/Finance/Overtime Hours/OT_extract_sample_2021_09.xlsx")
        overtime_data <- read_excel(overtime_file_path)
        
      }
      
      # Save prior version of Lab TAT Dept Summary data
      write_xlsx(summary_repos_overtime,
                 paste0(hist_archive_path,
                        "OVertime Historical ",
                        format(Sys.time(), "%Y%m%d_%H%M%S"),
                        ".xlsx"))
      
      
      # Process Overtime data
      tryCatch({overtime_summary_data <- overtime_file_processs(overtime_data)
          flag <- 1
      showModal(modalDialog(
        title = "Success",
        paste0("The data has been imported succesfully"),
        easyClose = TRUE,
        footer = NULL
      ))
      }, error = function(err){  showModal(modalDialog(
        title = "Error",
        paste0("There seems to be an issue with one of the files"),
        easyClose = TRUE,
        footer = NULL
      ))})
      
      
      if (flag == 1){
        # Append Overtime summary with new data
        # First, identify the sites, months, and metrics in the new data
        overtime_new_data <- unique(
          overtime_summary_data[  c("Service", "Site", "Associated Dashboard Month", "Metric_Name")]
        )
        
        # Second, remove these sites, months, and metrics from the historical data, if they exist there.
        # This allows us to ensure no duplicate entries for the same site, metric, and time period
        summary_repos_overtime <- anti_join(summary_repos_overtime,
                                            overtime_new_data,
                                         by = c("Service" = "Service",
                                                "Site" = "Site",
                                                "Associated Dashboard Month" = "Associated Dashboard Month",
                                                "Metric_Name" = "Metric_Name")
        )
        summary_repos_overtime$Value <- as.numeric(summary_repos_overtime$Value)
        # Third, combine the updated historical data with the new data
        summary_repos_overtime <- full_join(summary_repos_overtime,
                                            overtime_summary_data)
        
        # Lastly, save the updated summary data
        write_xlsx(summary_repos_overtime, summary_repos_overtime_path)
        
        metrics_final_df <<- overtime_metrics_final_df_process(overtime_summary_data)
        saveRDS(metrics_final_df, metrics_final_df_path)
        
        # picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
        # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
        record_timestamp("Overtime")
        
      }
        
    })
    
    
      
      
      
      
    # })
    
    # Transport Metrics - Non Patient Data  -----------------------
    
      observeEvent(input$submit_npt_tat,{
        
        npt_file <- input$non_patient_transport
        
        if (is.null(npt_file)) {
          return(NULL)
        }else{
          file_path <- npt_file$datapath
          #file_path <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/EVS/MSHS Normal Clean vs Iso Clean TAT Sept 2021.xlsx"
          
          
          tryCatch({
            npt_data <- read_excel(file_path)
            
            flag <- 1
            
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with this Non Patient Transport data file."),
              easyClose = TRUE,
              footer = NULL
            ))
          }
          )
        }
        
        
        # Process data if the right file format was submitted
        if(flag == 1) {
          tryCatch({
            data <- process_NPT_raw_data(npt_data)
            
            npt_data <- data[[1]]
            
            summary_repo_format <- data[[2]]
            
            flag <- 2
            
            showModal(modalDialog(
              title = "Success",
              paste0("This Non Patient Transport data has been imported successfully."),
              easyClose = TRUE,
              footer = NULL
            ))
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with this Non Patient Transport data file."),
              easyClose = TRUE,
              footer = NULL
            ))
          })
        }
        
        if(flag==2){
        
            metrics_final_df <<- transport__metrics_final_df_process(npt_data)
            
            saveRDS(metrics_final_df, metrics_final_df_path)
            
            transport_summary_repo <- read_excel(transport_table_path)
            
            write_xlsx(transport_summary_repo,
                       paste0(hist_archive_path,
                              "TAT Transport",
                              format(Sys.time(), "%Y%m%d_%H%M%S"),
                              ".xlsx"))
            
            
            transport_summary_repo$Month <- as.Date(transport_summary_repo$Month)
            
            updated_rows <- unique(summary_repo_format[c("Site","Date","Month","Transport Type")])
            updated_rows$Month <- as.Date(updated_rows$Month, "%m/%d/%Y")
            
            transport_summary_repo <- anti_join(transport_summary_repo, updated_rows)
            transport_summary_repo <- transport_summary_repo %>% filter(!is.na(Month))
            transport_summary_repo <- full_join(transport_summary_repo, summary_repo_format)
            transport_summary_repo <- as.data.frame(transport_summary_repo)
            write_xlsx(transport_summary_repo, transport_table_path)
            
            # picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
            # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # 
            # time_df <- read_excel(paste0(home_path, "time_updated.xlsx"))
            # date_time <- data.frame(Updated = as.POSIXct(Sys.time()))
            # date_time$Service = "Patient Transport"
            # date_time <- rbind(time_df, date_time)
            # write_xlsx(date_time, paste0(home_path, "time_updated.xlsx"))
            
            update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
            record_timestamp("Patient Transport")
            
            
        }
      })
    # Transport Metrics - Patient Data  -----------------------
    
      
      observeEvent(input$submit_pt_tat,{
        
        pt_file <- input$patient_transport
        flag <- 0 
        
        if (is.null(pt_file)) {
          return(NULL)
        }else{
          file_path <- pt_file$datapath
          #file_path <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/EVS/MSHS Normal Clean vs Iso Clean TAT Sept 2021.xlsx"
          #pt_data <- read_excel(file_path)
          
          
            tryCatch({
              # Read in SCC file
              data <- process_PT_data(file_path)
              
              pt_data <- data[[1]]
              
              summary_repo_format <- data[[2]]
              
              flag <- 1
              
              showModal(modalDialog(
                title = "Success",
                paste0("This Patient Transport data has been imported and processed successfully."),
                easyClose = TRUE,
                footer = NULL
              ))
              
              
            },
            error = function(err){
              showModal(modalDialog(
                title = "Error",
                paste0("There seems to be an issue with this Patient Transport Data file."),
                easyClose = TRUE,
                footer = NULL
              ))
            }
            )

        }
        
        
        if(flag==1){
        
          metrics_final_df <<- transport__metrics_final_df_process(pt_data)
          
          saveRDS(metrics_final_df, metrics_final_df_path)
          
          transport_summary_repo <- read_excel(transport_table_path)
          transport_summary_repo$Date <- as.Date(transport_summary_repo$Date)
          transport_summary_repo$Month <- as.Date(transport_summary_repo$Month)
          updated_rows <- unique(summary_repo_format[c("Site","Date","Month","Transport Type")])
          
          transport_summary_repo <- anti_join(transport_summary_repo, updated_rows)
          transport_summary_repo <- transport_summary_repo %>% filter(!is.na(Month))
          transport_summary_repo <- full_join(transport_summary_repo, summary_repo_format)
          transport_summary_repo <- as.data.frame(transport_summary_repo)
          write_xlsx(transport_summary_repo, transport_table_path)
          
          # picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
          # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
          # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
          # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
          # 
          # time_df <- read_excel(paste0(home_path, "time_updated.xlsx"))
          # date_time <- data.frame(Updated = as.POSIXct(Sys.time()))
          # date_time$Service = "Patient Transport"
          # date_time <- rbind(time_df, date_time)
          # write_xlsx(date_time, paste0(home_path, "time_updated.xlsx"))
          update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
          record_timestamp("Patient Transport")
          
          
        }
        
      })
      
      # KPI Biomed Output Table -------
      
      data_bimoed_kpi <- reactive({
        data  <- kpibme_reports_ui %>% ungroup()
        
        months_only <- data %>% select(-Site,-Metric)
        months <- format(as.Date(paste0(colnames(months_only),"-01"), "%b-%Y-%d"), "%m-%Y")
        
        colnames(data)[3:length(data)] <- months
        
        data <- data %>% 
          mutate(across(!Site & !Metric,as.character))
        
        
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
            bme_kpi_manual_updates <<- hot_to_r(input$biomed_kpi)          
            
            # Identify columns with no data in them and remove before further processing
            # This ensures months with no data do not get added to the department summary
            # repo and metrics_final_df repository
            non_empty_cols <- !(apply(bme_kpi_manual_updates,
                                      MARGIN = 2,
                                      function(x) 
                                        all(is.na(x))))
            
            bme_kpi_manual_updates <<- bme_kpi_manual_updates[, non_empty_cols]
            
            bme_kpi_manual_updates <<- bme_kpi_manual_updates %>% 
              mutate(across(!Site & !Metric,as.numeric))
            
            flag <- 1
          },
          error =function(err){
            
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with the Biomedical KPIs data entered."),
              easyClose = TRUE,
              footer = NULL
            ))
            
          })
          
          if(flag==1){
            user_format_error <<- manual_format_check(bme_kpi_manual_updates%>%
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
              tryCatch({
                # Reformat data from manual input table into summary repo format
                bme_kpi_summary_data <<-
                  process_manual_entry_to_summary_repo_format_biomed(bme_kpi_manual_updates,"KPI")
                
                flag <- 2
                
                
                showModal(modalDialog(
                  title = "Success",
                  paste0("This Biomedical KPIs data has been submitted successfully."),
                  easyClose = TRUE,
                  footer = NULL
                ))
            },
            
            error = function(err) {
              showModal(modalDialog(
                title = "Error",
                paste0("There seems to be an issue with the Biomedical KPIs data entered."),
                easyClose = TRUE,
                footer = NULL
              ))
            })
          
          
          if(flag==2){
            
            # Save prior version of KPIs Dept Summary data
            write_xlsx(kpibme_reports,
                       paste0(hist_archive_path,
                              "KPIs Biomed and Clinical Engineering ",
                              format(Sys.time(), "%Y%m%d_%H%M%S"),
                              ".xlsx"))
            
            
            # First, identify the sites, months, and metrics in the new data
            bme_kpi_new_data <- unique(
              bme_kpi_summary_data[, c("Service", "Site", "Month", "Metric")]
            )
            
            # Second, remove these sites, months, and metrics from the historical data,
            # if they exist there. This allows us to ensure no duplicate entries for
            # the same site, metric, and time period.
            kpi_bme <<- anti_join(kpibme_reports,
                                  bme_kpi_new_data,
                                  by = c("Service" = "Service",
                                         "Site" = "Site",
                                         "Month" = "Month",
                                         "Metric" = "Metric"))
            
            # Third, combine the updated historical data with the new data
            kpibme_reports <<- full_join(kpi_bme,
                                         bme_kpi_summary_data)
            
            glimpse(kpibme_reports)
            # Next, arrange the incident reports summary data by month, metric, and site
            kpibme_reports <<- kpibme_reports %>%
              arrange(Month,
                      desc(Metric),
                      Site)
            
            # Lastly, save the updated summary data
            write_xlsx(kpibme_reports, bmekpi_table_path)
            
            # Update metrics_final_df with latest data using custom function
            metrics_final_df <<- biomed__metrics_final_df_process(kpibme_reports,"KPIs")
            
            # Save updates metrics_final_df
            saveRDS(metrics_final_df, metrics_final_df_path)
            
            # picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
            # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # 
            # time_df <- read_excel(paste0(home_path, "time_updated.xlsx"))
            # date_time <- data.frame(Updated = as.POSIXct(Sys.time()))
            # date_time$Service = "Biomed / Clinical Engineering"
            # date_time <- rbind(time_df, date_time)
            # write_xlsx(date_time, paste0(home_path, "time_updated.xlsx"))
            
            update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
            record_timestamp("Biomed / Clinical Engineering")
            
          }
          }
        }
      }
    })
      
      
      #D&I Biomed Output Table -------
      
      data_bimoed_di <- reactive({
        data  <- disruptions_issues_reports_ui %>% ungroup()
        
        months_only <- data %>% select(-Site,-Metric)
        months <- format(as.Date(paste0(colnames(months_only),"-01"), "%b-%Y-%d"), "%m-%Y")
        
        colnames(data)[3:length(data)] <- months
        
        data <- data %>% 
          mutate(across(!Site & !Metric,as.character))
        
        result <- manual_table_month_order(data)
        
        # ##### Code that adds months missing months to the rhandsontable
        # months_only <- data %>% select(-Site,-Metric)
        # months <- format(as.Date(paste0(colnames(months_only), "-01"), "%m-%Y-%d"), "%m-%Y")
        # 
        # max_month <- as.Date(paste0(format(Sys.Date() %m-% months(1), "%m-%Y"), "-01"), "%m-%Y-%d")
        # 
        # months <- as.Date(sprintf("%s-01", months), format = "%m-%Y-%d")
        # 
        # months_to_drop <- which(months < max_month %m-% months(6))
        # months_to_drop <- format(months[months_to_drop], "%m-%Y")
        # 
        # complete_months <- seq.Date(months[1], max_month, by= 'month')
        # 
        # missing_months <- which(!(complete_months %in% months))
        # missing_months <- as.character(format(complete_months[missing_months], "%m-%Y"))
        # 
        # data[,missing_months] <- NA_character_
        # 
        # months_df <- data[,!(names(data) %in% c("Metric", "Site"))]
        # months <- order(as.yearmon(colnames(months_df), "%m-%Y"))
        # order_months <- months_df[months]
        # 
        # 
        # index <- months+2
        # index <- c(1:2,index)
        # 
        # data <- data[index]
        # 
        # data <- data %>% select(-all_of(months_to_drop))
        
        
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
            bme_di_manual_updates <<- hot_to_r(input$bimoed_di)
            
            # Identify columns with no data in them and remove before further processing
            # This ensures months with no data do not get added to the department summary
            # repo and metrics_final_df repository
            non_empty_cols <- !(apply(bme_di_manual_updates,
                                      MARGIN = 2,
                                      function(x) 
                                        all(is.na(x))))
            
            bme_di_manual_updates <<- bme_di_manual_updates[, non_empty_cols]
            
            bme_di_manual_updates <<- bme_di_manual_updates %>% 
              mutate(across(!Site & !Metric,as.numeric))
            
            flag <- 1
        
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with the Disruptions and Issues data entered."),
              easyClose = TRUE,
              footer = NULL
            ))
          })
        
          if(flag ==1){
            
            tryCatch({
            # Reformat data from manual input table into summary repo format
            bme_di_summary_data <-
              process_manual_entry_to_summary_repo_format_biomed(bme_di_manual_updates,"DI")
            
            flag <- 2
            
            showModal(modalDialog(
              title = "Success",
              paste0("This Disruptions and Issues data has been submitted successfully."),
              easyClose = TRUE,
              footer = NULL
            ))
            
            },
            error = function(err) {
              showModal(modalDialog(
                title = "Error",
                paste0("There seems to be an issue with the Disruptions and Issues data entered."),
                easyClose = TRUE,
                footer = NULL
              ))
              
            })
          }
          
          if(flag==2){
            
            # Save prior version of DI Reports Dept Summary data
            write_xlsx(disruptions_issues_reports,
                       paste0(hist_archive_path,
                              "DI Biomed and Clinical Engineering ",
                              format(Sys.time(), "%Y%m%d_%H%M%S"),
                              ".xlsx"))
            
        
            # First, identify the sites, months, and metrics in the new data
            bme_di_new_data <- unique(
              bme_di_summary_data[, c("Service", "Site", "Month")]
            )
            
            # Second, remove these sites, months, and metrics from the historical data,
            # if they exist there. This allows us to ensure no duplicate entries for
            # the same site, metric, and time period.
            di_bme <<- anti_join(disruptions_issues_reports,
                                 bme_di_new_data,
                                  by = c("Service" = "Service",
                                         "Site" = "Site",
                                         "Month" = "Month"))
            
            # Third, combine the updated historical data with the new data
            disruptions_issues_reports <<- full_join(di_bme,
                                                     bme_di_summary_data)
            
            # Next, arrange the DI reports summary data by month, metric, and site
            disruptions_issues_reports <<- disruptions_issues_reports %>%
              arrange(Month,
                      Site)
            # Lastly, save the updated summary data
            write_xlsx(disruptions_issues_reports, bmedi_table_path)
            
            # Update metrics_final_df with latest data using custom function
            metrics_final_df <<- biomed__metrics_final_df_process(disruptions_issues_reports,"DI")
            
            # Save updates metrics_final_df
            saveRDS(metrics_final_df, metrics_final_df_path)
            
            # picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
            # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # 
            # time_df <- read_excel(paste0(home_path, "time_updated.xlsx"))
            # date_time <- data.frame(Updated = as.POSIXct(Sys.time()))
            # date_time$Service = "Biomed / Clinical Engineering"
            # date_time <- rbind(time_df, date_time)
            # write_xlsx(date_time, paste0(home_path, "time_updated.xlsx"))
            update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
            record_timestamp("Biomed / Clinical Engineering")
            
            
              } 
        }
      })
      

      # Imaging DR X-RAY data submission ----- 
      observeEvent(input$submit_imagingxray, {
        if(input$imaging_xray_username == "") {
          showModal(modalDialog(
            title = "Error",
            "Please fill in the required fields.",
            easyClose = TRUE,
            footer = NULL
          ))
        }
        
        xray_file <- input$imaging_DR_XRay
        
        if (is.null(xray_file)) {
          return(NULL)
        }else{
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
          }
          )
        }
        
        # Process data if the right file format was submitted
        if(flag == 1) {
          tryCatch({
            # Reformat data from manual input table into summary repo format
            xray_summary_data <-
              process_xray_data(xray_data)
            
            flag <- 2
            
            showModal(modalDialog(
              title = "Success",
              paste0("This data file has been imported successfully."),
              easyClose = TRUE,
              footer = NULL
            ))
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with this data file."),
              easyClose = TRUE,
              footer = NULL
            ))
          })
        }
        
        
        if(flag == 2){
            # Save prior version of xray Reports Dept Summary data
            write_xlsx(ImagingSummaryRepo,
                       paste0(hist_archive_path,
                              "Imaging DR-Ops",
                              format(Sys.time(), "%Y%m%d_%H%M%S"),
                              ".xlsx"))
            
            
            
            # First, identify the sites, months, and metrics in the new data
            xray_new_data <- unique(
              xray_summary_data[, c("Service", "Site", "Month", "Metric_Name_Submitted")]
            )
            
            # Second, remove these sites, months, and metrics from the historical data,
            # if they exist there. This allows us to ensure no duplicate entries for
            # the same site, metric, and time period.
            ImagingSummaryRepo <<- anti_join(ImagingSummaryRepo,
                                 xray_new_data,
                                 by = c("Service" = "Service",
                                        "Site" = "Site",
                                        "Month" = "Month",
                                        "Metric_Name_Submitted" = "Metric_Name_Submitted"))
            
            # Third, combine the updated historical data with the new data
            ImagingSummaryRepo <<- full_join(ImagingSummaryRepo,
                                               xray_summary_data)
            
            # Next, arrange the incident reports summary data by month, metric, and site
            ImagingSummaryRepo <<- ImagingSummaryRepo %>%
              arrange(Month,
                      Site)
            
            # Lastly, save the updated summary data
            write_xlsx(ImagingSummaryRepo, imagingDR_path)
            
            # Update metrics_final_df with latest data using custom function
            metrics_final_df <<- imagingdrxray__metrics_final_df_process(xray_summary_data)
            
            # Save updates metrics_final_df
            saveRDS(metrics_final_df, metrics_final_df_path)
            
            # picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
            # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # 
            # 
            # time_df <- read_excel(paste0(home_path, "time_updated.xlsx"))
            # date_time <- data.frame(Updated = as.POSIXct(Sys.time()))
            # date_time$Service = "Imaging"
            # date_time <- rbind(time_df, date_time)
            # write_xlsx(date_time, paste0(home_path, "time_updated.xlsx"))
            
            update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
            record_timestamp("Imaging")
            
        }
      })
      
      # Imaging DR Chest CT data submission ----- 
      observeEvent(input$submit_imagingct, {
        if(input$imaging_ct_username == "") {
          showModal(modalDialog(
            title = "Error",
            "Please fill in the required fields.",
            easyClose = TRUE,
            footer = NULL
          ))
        }
        
        ct_file <- input$imaging_DR_ct
        
        if (is.null(ct_file)) {
          return(NULL)
        }else{
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
          }
          )
          
        }
        
        # Process data if the right file format was submitted
        if(flag == 1) {
          tryCatch({
            # Reformat data from manual input table into summary repo format
            ct_summary_data <-
              process_ctdata_data(ct_data)
            
            flag <- 2
            
            showModal(modalDialog(
              title = "Success",
              paste0("This data file has been imported successfully."),
              easyClose = TRUE,
              footer = NULL
            ))
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with this data file."),
              easyClose = TRUE,
              footer = NULL
            ))
          })
        }
        
        
        
        if(flag == 2){
          
            # Save prior version of Imaging Reports Dept Summary data
            write_xlsx(ImagingSummaryRepo,
                       paste0(hist_archive_path,
                              "Imaging DR-Ops",
                              format(Sys.time(), "%Y%m%d_%H%M%S"),
                              ".xlsx"))
            
            
    
            # First, identify the sites, months, and metrics in the new data
            ct_new_data <- unique(
              ct_summary_data[, c("Service", "Site", "Month", "Metric_Name_Submitted")]
            )
            
            # Second, remove these sites, months, and metrics from the historical data,
            # if they exist there. This allows us to ensure no duplicate entries for
            # the same site, metric, and time period.
            ImagingSummaryRepo <<- anti_join(ImagingSummaryRepo,
                                       ct_new_data,
                                       by = c("Service" = "Service",
                                              "Site" = "Site",
                                              "Month" = "Month",
                                              "Metric_Name_Submitted" = "Metric_Name_Submitted"))
            
            # Third, combine the updated historical data with the new data
            ImagingSummaryRepo <<- full_join(ImagingSummaryRepo,
                                             ct_summary_data)
            
            # Next, arrange the imaging reports summary data by month, metric, and site
            ImagingSummaryRepo <<- ImagingSummaryRepo %>%
              arrange(Month,
                      Site)
            
            # Lastly, save the updated summary data
            write_xlsx(ImagingSummaryRepo, imagingDR_path)
            
            # Update metrics_final_df with latest data using custom function
            metrics_final_df <<- imagingdrct__metrics_final_df_process(ct_summary_data)
            
            # Save updates metrics_final_df
            saveRDS(metrics_final_df, metrics_final_df_path)
            
            # picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
            # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
            # 
            # time_df <- read_excel(paste0(home_path, "time_updated.xlsx"))
            # date_time <- data.frame(Updated = as.POSIXct(Sys.time()))
            # date_time$Service = "Imaging"
            # date_time <- rbind(time_df, date_time)
            # write_xlsx(date_time, paste0(home_path, "time_updated.xlsx"))
            
            update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
            record_timestamp("Imaging")
            
        }
      })
      
      # Nursing observer event actions for data submission ----- 
      observeEvent(input$submit_nursing, {
       
        nursing_file <- input$nursing
        
        if (is.null(nursing_file)) {
          return(NULL)
        }else{
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
              footer = NULL
            ))
          }
          )
          
        }
        
        # Process data if the right file format was submitted
        if(flag == 1) {
          tryCatch({
            # Reformat data from manual input table into summary repo format
            nursing_summary_data <-
              process_nursing_data(nursing_data)
            
            flag <- 2
            
            showModal(modalDialog(
              title = "Success",
              paste0("This data file has been imported successfully."),
              easyClose = TRUE,
              footer = NULL
            ))
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with this data file."),
              easyClose = TRUE,
              footer = NULL
            ))
          })
        }
        
        
        
        if(flag == 2){
          
          # Save prior version of Imaging Reports Dept Summary data
          write_xlsx(NursingSummaryRepo,
                     paste0(hist_archive_path,
                            "Nursing",
                            format(Sys.time(), "%Y%m%d_%H%M%S"),
                            ".xlsx"))
          
          
          
          # First, identify the sites, months, and metrics in the new data
          nursing_new_data <- unique(
            nursing_summary_data[, c("Service", "Site", "Month")]
          )
          
          # Second, remove these sites, months, and metrics from the historical data,
          # if they exist there. This allows us to ensure no duplicate entries for
          # the same site, metric, and time period.
          nursing_old_data <<- anti_join(NursingSummaryRepo,
                                   nursing_new_data,
                                   by = c("Service" = "Service",
                                          "Site" = "Site",
                                          "Month" = "Month"))
          
          # Third, combine the updated historical data with the new data
          nursing_reports <<- full_join(nursing_old_data,
                                           nursing_summary_data)
          
          # Next, arrange the imaging reports summary data by month, metric, and site
          nursing_reports <<- nursing_reports %>%
            arrange(Month,
                    Site)
          
          # Lastly, save the updated summary data
          write_xlsx(nursing_reports, nursing_path)
          
          # Update metrics_final_df with latest data using custom function
          metrics_final_df <<- nursing__metrics_final_df_process(nursing_summary_data)
          
          # Save updates metrics_final_df
          saveRDS(metrics_final_df, metrics_final_df_path)
          
          # picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
          # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
          # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
          # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
          
          update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
          record_timestamp("Nurisng")
          
        }
      })
      
      
      # ED observer event actions for data submission ----- 
      observeEvent(input$submit_ed, {
        flag <- 0
        ed_file <- input$ed
        
        if (is.null(ed_file)) {
          return(NULL)
        }else{
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
            print("Yo!")
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with this data file."),
              easyClose = TRUE,
              footer = NULL
            ))
          }
          )
          
        }
        
        # Process data if the right file format was submitted
        if(flag == 1) {
          tryCatch({
            # Reformat data from manual input table into summary repo format
            ed_summary_data <-
              ed_dept_summary(ed_data_ts,ed_data_percentiles)
            
            flag <- 2
            
            showModal(modalDialog(
              title = "Success",
              paste0("This data file has been imported successfully."),
              easyClose = TRUE,
              footer = NULL
            ))
          },
          error = function(err){
            showModal(modalDialog(
              title = "Error",
              paste0("There seems to be an issue with this data file."),
              easyClose = TRUE,
              footer = NULL
            ))
          })
        }
        
        
        
        if(flag == 2){
          
          # Save prior version of Imaging Reports Dept Summary data
          write_xlsx(ed_summary_repo,
                     paste0(hist_archive_path,
                            "ED",
                            format(Sys.time(), "%Y%m%d_%H%M%S"),
                            ".xlsx"))
          
          
          
          # First, identify the sites, months, and metrics in the new data
          ed_new_data <- unique(
            ed_summary_data[, c("Service", "Site", "Month", "KPI")]
          )
          
          # Second, remove these sites, months, and metrics from the historical data,
          # if they exist there. This allows us to ensure no duplicate entries for
          # the same site, metric, and time period.
          ed_old_data <<- anti_join(ed_summary_repo,
                                    ed_new_data,
                                         by = c("Service" = "Service",
                                                "Site" = "Site",
                                                "Month" = "Month",
                                                "KPI" = "KPI"))
          
          # Third, combine the updated historical data with the new data
          ed_reports <<- full_join(ed_old_data,
                                        ed_summary_data)
          
          # Next, arrange the imaging reports summary data by month, metric, and site
          ed_reports <<- ed_reports %>%
            arrange(Month,
                    Site)
          
          # Lastly, save the updated summary data
          write_xlsx(ed_reports, ed_path)
          
          # Update metrics_final_df with latest data using custom function
          metrics_final_df <<- ed__metrics_final_df_process(ed_summary_data)
          
          # Save updates metrics_final_df
          saveRDS(metrics_final_df, metrics_final_df_path)
          
          # picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
          # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
          # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
          # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
          
          update_picker_choices(session, input$selectedService, input$selectedService2, input$selectedService3)
          record_timestamp("ED")
          
        }
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
      
      
      # Code to update drop down selections based on selected service line -------------
      # observeEvent(input$selectedService,{
      #   
      #   data <- metrics_final_df %>% filter(Service == input$selectedService)
      #   picker_choices <-  format(sort(unique(data$Reporting_Month_Ref)), "%m-%Y")
      #   updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      # })
      
      observeEvent(input$selectedService2,{
        
        data <- metrics_final_df %>% filter(Service == input$selectedService2)
        picker_choices <-  format(sort(unique(data$Reporting_Month_Ref)), "%m-%Y")
        updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])

        campus_choices <- sort(unique(data$Site))
        updatePickerInput(session, "selectedCampus2", choices = campus_choices, selected = campus_choices)

      })
      
      
      observeEvent(input$selectedService3,{
        
        data <- metrics_final_df %>% filter(Service == input$selectedService3)
        picker_choices <-  format(sort(unique(data$Reporting_Month_Ref)), "%m-%Y")
        updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        
        
        campus_choices <- sort(unique(data$Site))
        updatePickerInput(session, "selectedCampus3", choices = campus_choices, selected = campus_choices)
      })
      
      observeEvent(input$selectedService4, {
        
        target_data <- target_mapping_reference %>%
          filter(Service %in% input$selectedService4)
        
        picker_choices_metric_group <- unique(target_data$Metric_Group)
        
        updatePickerInput(session, "selectedMetricGroup",
                          choices = picker_choices_metric_group,
                          selected = picker_choices_metric_group)
        
      })


      
} # Close Server



