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
      paste0("MSHS ",input$selectedService, " Summary")
    })
    
    output$siteSummary_table <- function(){
      input$submit_prod
      input$submit_engineering
      input$submit_finance
      input$submit_food
      input$submit_evs
      
      service_input <- input$selectedService
      month_input <- input$selectedMonth

      # service_input <- "Imaging"
      # month_input <- "09-2021"

      # Code Starts ---------------------------------------------------------------------------------
      summary_tab_metrics <- unique((summary_metric_filter %>% #summary_metric_filter is from summary_metrics tab reformatted 
                                       filter(Service == service_input))[,c("Service","Metric_Group","Metric_Name","Summary_Metric_Name")]) # Filter out summary tab metrics only
      
      target_section_metrics <- unique((target_mapping %>%  #target_mapping is read in from excel sheet in target mapping file
                                       filter(Service == service_input))[,c("Service","Metric_Group","Metric_Name")])
      
      metric_targets <- target_mapping %>% filter(Service == service_input)
      
      # Variable setting
      current_period <- as.Date(fast_strptime(month_input, "%m-%Y"), "%Y-%m-%d")
      fiscal_year <- format(current_period,  "%Y")
      
      # Filter data by service specific metrics
      data <- left_join(summary_tab_metrics, metrics_final_df, by = c("Service", "Metric_Group", "Metric_Name"))  ##extract selected service from metric_final_df
      data <- data  %>% 
        filter(Reporting_Month_Ref <= current_period) %>% #Ensure only selected service and all data before selected month is returned
        filter(Service == service_input)
      
      # Data Period Filtering
      period_filter <- data %>% 
        group_by(Metric_Group, Metric_Name, Reporting_Month_Ref, Premier_Reporting_Period) %>% 
        #distinct() %>%
        summarise(total = n()) %>%                                                            #
        arrange(Metric_Group, Metric_Name, desc(Reporting_Month_Ref)) %>%
        group_by(Metric_Group, Metric_Name) %>%
        mutate(id = row_number())
        
      
      # Current Period Table
      current_summary_data <- left_join((period_filter %>% filter(id == 1)), data, by = c("Metric_Group","Metric_Name","Reporting_Month_Ref","Premier_Reporting_Period"))  #Take all most recent data (id = 1) and merge with all data 

      current_summary <- current_summary_data %>%
        mutate(`Current Period` = ifelse(str_detect(Premier_Reporting_Period, "/"), 
                                         paste0("Rep. Pd. Ending ", Premier_Reporting_Period), Premier_Reporting_Period))  ## Create Current Period column if it's premier say when it ends
      current_summary <- current_summary[,c("Metric_Group","Summary_Metric_Name","Current Period","Site","value_rounded")]
      
      # Remove any duplicates
      current_summary <- unique(current_summary)
      
      current_summary <- current_summary %>%
        `colnames<-` (c("Section","Metric_Name","Current Period","Site","value_rounded")) %>%
        mutate(Section = "Metrics") %>%
        pivot_wider(names_from = Site, values_from = value_rounded)

      missing_sites <- setdiff(sites_inc, names(current_summary))
      current_summary[missing_sites] <- NA
      #current_summary$NYEE <- as.numeric(current_summary$NYEE)
      
      # FYTD Period Filter 
      fytd_period <- period_filter %>%        #Get all data from YTD
        # Remove monthly Press Ganey and HCAHPS data from YTD sections since there is separate YTD data for this
        filter(!(Metric_Group %in% c("Press Ganey Score", "HCAHPS (60 day lag)"))) %>%
        group_by(Metric_Group, Metric_Name) %>%
        #filter(total == max(total)) %>%
        filter(format(Reporting_Month_Ref, "%Y",) == fiscal_year) %>%
        group_by(Metric_Group, Metric_Name) %>%
        mutate(`Fiscal Year to Date` = ifelse(str_detect(Premier_Reporting_Period, "/"), 
                                              paste0("FYTD Ending ", Premier_Reporting_Period[which.min(id)]),
                                              ifelse(which.max(id) == 1,
                                                     Premier_Reporting_Period[which.min(id)],
                                                     paste0(substr(Premier_Reporting_Period[which.max(id)], 1, 3), " - ", 
                                                            Premier_Reporting_Period[which.min(id)]))))
      
      
      
      # FYTD Summary Table - for total
      fytd_summary_all <- left_join(fytd_period, data, by = c("Metric_Group","Metric_Name", "Reporting_Month_Ref","Premier_Reporting_Period"), all = TRUE)
      
      fytd_summary_total <- fytd_summary_all %>%
        filter(Metric_Name %in% c("Budget to Actual MOM", "Variance to Budget")) %>% # Metrics that need to be summarized by sum (total)
        mutate(`Fiscal Year to Date` = paste(`Fiscal Year to Date`," Total")) %>%
        group_by(Site, Metric_Group, Metric_Name, Summary_Metric_Name, `Fiscal Year to Date`) %>%
        summarise(value_rounded = round(sum(value_rounded, na.rm = TRUE))) %>%
        ungroup()
      
      # FYTD Summary Table - for Press Ganey
      # fytd_press_ganey <- reformat_pg_fytd(press_ganey_data)
      if (service_input %in% unique(press_ganey_data$Service)) {
        
        press_ganey_ytd <- press_ganey_data %>%
          # Add logic to include Jan data in YTD data
          mutate(ReportingType = ifelse(month(Reporting_Date_Start) == 1 &
                                          month(Reporting_Date_End) == 1,
                                        "YTD", ReportingType)) %>%
          # Filter on YTD data and selected service
          filter(ReportingType %in% "YTD" &
                   Service %in% service_input) %>%
          mutate(Reporting_Month_Ref = floor_date(Reporting_Date_End,
                                                  unit = "month")) %>%
          filter(Reporting_Month_Ref <= current_period)
        
        # Press Ganey mapping for metrics
        pg_mapping_simple <- press_ganey_mapping %>%
          select(-Questions)
        
        # Crosswalk PG YTD data with mapping
        press_ganey_ytd <- left_join(press_ganey_ytd,
                                     pg_mapping_simple,
                                     by = c("Service" = "Service",
                                            "Question_Clean" = "Question_Clean"))
        
        # Begin reformatting Press Ganey YTD data
        pg_ytd_reformat <- press_ganey_ytd %>%
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
        
        # Identify Press Ganey metrics to include in Summary tab
        pg_summary_tab_metrics <- summary_metric_filter %>%
          filter(Service %in% service_input) %>%
          select(Service,
                 Metric_Group,
                 Metric_Name,
                 Metric_Name_Submitted,
                 Summary_Metric_Name)

        # Crosswalk Press Ganey YTD data with Summary tab metrics
        pg_ytd_reformat <- left_join(pg_ytd_reformat,
                                     pg_summary_tab_metrics,
                                     by = c("Service" = "Service",
                                            "Metric_Name_Submitted" = "Metric_Name_Submitted"))
        
        pg_ytd_reformat <- pg_ytd_reformat %>%
          # Filter on 
          filter(!is.na(Summary_Metric_Name) &
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
        pg_ytd_reformat <- NULL
      }
      
                    
      # FYTD Summary Table - for average 
      '%!in%' <<- function(x,y)!('%in%'(x,y))
      fytd_summary_avg <- fytd_summary_all %>%
        filter(Metric_Name %!in% c("Budget to Actual MOM", "Variance to Budget")) %>% # Metrics that need to be summarized by sum (total)
        mutate(`Fiscal Year to Date` = paste(`Fiscal Year to Date`," Average")) %>%
        group_by(Site, Metric_Group, Metric_Name, Summary_Metric_Name, `Fiscal Year to Date`) %>%
        summarise(value_rounded = mean(value_rounded, na.rm = TRUE)) %>%
        mutate(value_rounded = ifelse(Summary_Metric_Name %in% metric_unit_perc, round(value_rounded, 2), round(value_rounded))) %>%
        ungroup()
    
      # Merge for summary 

      fytd_merged <- rbind(fytd_summary_total, fytd_summary_avg, pg_ytd_reformat)
      fytd_summary <- fytd_merged
      fytd_summary$Metric_Name <- NULL
      fytd_summary <- fytd_summary %>%
        `colnames<-` (c("Site","Section","Metric_Name", "Fiscal Year to Date","value_rounded")) %>%
        pivot_wider(names_from = Site, values_from = value_rounded) %>%
        mutate(Section = "Metrics")

      missing_sites <- setdiff(sites_inc, names(fytd_summary))
      fytd_summary[missing_sites] <- NA
      fytd_summary$NYEE <- as.numeric(fytd_summary$NYEE)
      
      
      # Merge FYTD and Current Period Metrics Summary 
      metrics_summary <- merge(fytd_summary, current_summary, by = c("Section","Metric_Name"), all = TRUE)
      # metrics_summary <- metrics_summary[order(factor(metrics_summary$Metric_Name, levels=unique(summary_tab_metrics$Metric_Name))),] 
      metrics_summary <- metrics_summary[order(factor(metrics_summary$Metric_Name, levels=unique(summary_tab_metrics$Summary_Metric_Name))),] 
      
      

      # Format units
      metrics_summary$Metric_Unit <- metric_unit_filter_summary$Metric_Unit[match(metrics_summary$Metric_Name, 
                                                                                  metric_unit_filter_summary$Summary_Metric_Name)]
      
      metrics_summary <- metrics_summary %>%
        mutate_if(is.numeric, funs(ifelse(is.na(Metric_Unit), prettyNum(round(.), big.mark = ','),
                                          ifelse(Metric_Unit == "Dollar", dollar(round(.)), percent(.,2)))))
      
      metrics_summary$Metric_Unit <- NULL
      
    
      # Create and Format Comparison Table
      metrics_summary[metrics_summary == "NA"] <- NA
      metrics_summary[metrics_summary == "NaN"] <- NA
      metrics_summary[metrics_summary == "NA%"] <- NA
      metrics_summary[metrics_summary == "$NA"] <- NA
      metrics_summary[metrics_summary == "NaN%"] <- NA
      metrics_summary[metrics_summary == "$NaN"] <- NA
      
      metrics_summary[is.na(metrics_summary)] <- "-"
      metrics_summary <- metrics_summary[order(factor(metrics_summary$Metric_Name, levels=unique(summary_tab_metrics$Metric_Name))),] 
      row.names(metrics_summary) <- NULL
      
     # Current Period Target
      current_target <- merge(target_section_metrics, current_summary_data, by = c("Metric_Group","Metric_Name"))
      current_target <- current_target %>%
        filter(!is.na(Status)) %>%
        mutate(`Current Period` = ifelse(str_detect(Premier_Reporting_Period, "/"), 
                                         paste0("Rep. Pd. Ending ", Premier_Reporting_Period), Premier_Reporting_Period)) %>%
        select(Metric_Group, Summary_Metric_Name, `Current Period`, Site, Status) %>%
        `colnames<-` (c("Section","Metric_Name","Current Period","Site","target")) %>%
        mutate(Section = "Variance to Target") %>%
        pivot_wider(names_from = Site, values_from = target) 
      
      missing_sites <- setdiff(sites_inc, names(current_target))
      current_target[missing_sites] <- NA
      
      #FYTD Summary Target 
      fytd_target_metrics <- merge(target_section_metrics, fytd_merged, 
                                       by = c("Metric_Group","Metric_Name"))
     
      fytd_target <- merge(fytd_target_metrics, 
                           metric_targets[,c("Site","Metric_Group","Metric_Name","Target","Range_1","Range_2","Status")], 
                           by = c("Site","Metric_Group","Metric_Name"))
      
      if("Budget to Actual MOM" %in% as.vector(target_section_metrics$Metric_Name)){
        
        #Calculate FYTD Budget to Actual Targets
        budget_to_actual_target <- metrics_final_df %>% 
          filter((Service == service_input) & (Metric_Name %in% c("Budget_Total", "Budget to Actual MOM")) & 
                   (Reporting_Month_Ref %in% unique((fytd_period %>% filter(Metric_Name == "Budget to Actual MOM"))$Reporting_Month_Ref))) %>%
          group_by(Service, Site, Metric_Group, Metric_Name) %>%
          summarise(value_rounded = sum(value_rounded)) %>%
          pivot_wider(names_from = "Metric_Name",
                      values_from = "value_rounded") %>%
          mutate(Target = round(`Budget to Actual MOM`/ Budget_Total,2),
                 Status = ifelse(Target >= 0, "Green", ifelse(Target < -0.02, "Red", "Yellow"))) %>%
          pivot_longer(4:5,
                       names_to = "Summary_Metric_Name",
                       values_to = "value_rounded") %>%
          filter(Summary_Metric_Name == "Budget to Actual MOM") %>%
          mutate(Section = "Variance to Target")
        
        budget_to_actual_target <- budget_to_actual_target[,c("Section", "Summary_Metric_Name", "Site", "Status")]
        budget_to_actual_target$`Fiscal Year to Date` <- fytd_target$`Fiscal Year to Date`[match(budget_to_actual_target$Summary_Metric_Name, fytd_target$Metric_Name)]
      }else{
        budget_to_actual_target <- data.frame(Section = c("NA"),
                                                Summary_Metric_Name = c("NA"),
                                                `Fiscal Year to Date` = c("NA"),
                                                Site = c("NA"),
                                                Status = c("NA"))
        colnames(budget_to_actual_target) <- c("Section","Summary_Metric_Name","Fiscal Year to Date","Site","Status")
      }
      
      if("Variance to Budget" %in% as.vector(target_section_metrics$Metric_Name)){
        #Calculate FYTD Budget to Actual Targets
        variance_to_budget_target <- metrics_final_df %>% 
          filter((Service == service_input) & (Metric_Group == "Total Revenue to Budget Variance") &
                   (Metric_Name %in% c("Budget", "Variance to Budget")) & 
                   (Reporting_Month_Ref %in% unique((fytd_period %>% filter(Metric_Name == "Variance to Budget"))$Reporting_Month_Ref))) %>%
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
          mutate(Section = "Variance to Target")
        
        variance_to_budget_target <- variance_to_budget_target[,c("Section", "Summary_Metric_Name", "Site", "Status")]
        variance_to_budget_target$`Fiscal Year to Date` <- fytd_target$`Fiscal Year to Date`[match(variance_to_budget_target$Summary_Metric_Name, fytd_target$Metric_Name)]
      } else{
        variance_to_budget_target <- data.frame(Section = c("NA"),
                                                   Summary_Metric_Name = c("NA"),
                                                   `Fiscal Year to Date` = c("NA"),
                                                   Site = c("NA"),
                                                   Status = c("NA"))
        colnames(variance_to_budget_target) <- c("Section","Summary_Metric_Name","Fiscal Year to Date","Site","Status")
      }
      
      
      # Merge with rest of the metrics 
      fytd_target <- fytd_target %>%
        mutate(Variance =  between(value_rounded, Range_1, Range_2)) %>%
        filter(Variance == TRUE) %>%
        mutate(Section = "Variance to Target") %>%
        select(Section, Summary_Metric_Name, `Fiscal Year to Date`, Site, Status) 
      
      fytd_target <- bind_rows(fytd_target, budget_to_actual_target, variance_to_budget_target)
      
      fytd_target <- fytd_target %>%
        filter(Section != "NA") %>%
        pivot_wider(names_from = Site, values_from = Status)
      names(fytd_target)[names(fytd_target) == 'Summary_Metric_Name'] <- 'Metric_Name'
      
      missing_sites <- setdiff(sites_inc, names(fytd_target))
      fytd_target[missing_sites] <- NA

      
      # Merge FYTD and Current Period Targets
      targets_summary <- left_join(fytd_target, current_target,by = c("Section", "Metric_Name"))
      targets_summary <- targets_summary[order(factor(targets_summary$Metric_Name, levels=unique(summary_tab_metrics$Metric_Name))),] 
      targets_summary <- as.data.frame(targets_summary)
      
      # Create traffic lights for the targets
      col_red <- which(targets_summary == "Red", arr.ind = TRUE)
      col_red_rows <- as.integer(col_red[,1])
      col_red_cols <- as.integer(col_red[,2])
      col_yellow <- which(targets_summary == "Yellow", arr.ind = TRUE)
      col_green <- which(targets_summary == "Green", arr.ind = TRUE)
      
      colors_comb <- as.data.frame(rbind(col_red, col_yellow, col_green))
      
      if(nrow(colors_comb) != 0){
        for (i in 1:nrow(colors_comb)){
          targets_summary[colors_comb[i,1],colors_comb[i,2]] <- fa('fas fa-circle')
        }
      }
      
      if(nrow(col_red) != 0){
        for (i in 1:nrow(col_red)){
          targets_summary[col_red[i,1],col_red[i,2]] <- cell_spec(targets_summary[col_red[i,1],col_red[i,2]], 'html', color = 'red', escape = FALSE)
        }
      }
      
      if(nrow(col_green) != 0){
        for(i in 1:nrow(col_green)){
          targets_summary[col_green[i,1],col_green[i,2]] <- cell_spec(targets_summary[col_green[i,1],col_green[i,2]], 'html', color = 'green', escape = FALSE)
        }
      }
      
      if(nrow(col_yellow != 0)){
        for (i in 1:nrow(col_yellow)){
          targets_summary[col_yellow[i,1],col_yellow[i,2]] <- cell_spec(targets_summary[col_yellow[i,1],col_yellow[i,2]], 'html', color = 'yellow', escape = FALSE)
        }
      }
      
      
      summary_tab_tb <- rbind(metrics_summary, targets_summary[,1:18])
      summary_tab_tb$NYEE <- NULL
      
      
      # Create and Format Table
      summary_tab_tb[summary_tab_tb == "NA"] <- NA
      summary_tab_tb[summary_tab_tb == "NaN"] <- NA
      summary_tab_tb[is.na(summary_tab_tb)] <- "-"
      row.names(summary_tab_tb) <- NULL
      colnames(summary_tab_tb) <- gsub("\\..*", " ", colnames(summary_tab_tb))
      colnames(summary_tab_tb) <- gsub("_", " ", colnames(summary_tab_tb), fixed = TRUE)
      
      ## Create table output headers
      header_above <- c(" " = 2, "ytd_header" = 7, " " = 1, "current_header" = 7)
      names(header_above) <- c(" ", "Year to Date", " ", "Current Period")
      
      kable_col_names <- colnames(summary_tab_tb)[2:length(summary_tab_tb)]
      
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
    
    
    # 2. Comparison Tab Output -------------------------------------------------------------------------------
    output$siteComp_title <- renderText({
      paste0("MSHS ",input$selectedService2, " Key Metric Rollup")
    })
    
    output$siteComp_table <- function(){
      
      input$submit_prod
      input$submit_engineering
      input$submit_finance
      input$submit_food
      input$submit_evs
      
      
      service_input <- input$selectedService2
      month_input <- input$selectedMonth2
      site_input <- input$selectedCampus2
      # 
      # service_input <- "Food Services"
      # month_input <- "09-2021"
      # site_input <- "MSB"

      # Code Starts ---------------------------------------------------------------------------------
      summary_tab_metrics <- unique((summary_metric_filter %>%
                                       filter(Service == service_input))[,c("Service","Metric_Group","Metric_Name","Summary_Metric_Name")]) # Filter out summary tab metrics only
      
      target_section_metrics <- unique((target_mapping %>%
                                          filter(Service == service_input))[,c("Service","Metric_Group","Metric_Name")])
      
      metric_targets <- target_mapping %>% filter(Service == service_input)

      current_period <- as.Date(fast_strptime(month_input, "%m-%Y"), "%Y-%m-%d")
      fiscal_year <- format(current_period,  "%Y")
      
      # Process data to include missing sites for each metric
      data <- metrics_final_df %>% 
        filter(Site %in% site_input) %>%
        filter(Service == service_input) %>% # input$selectedService
        filter(Reporting_Month_Ref <= current_period) %>%
        arrange(Site, Metric_Group, Metric_Name, desc(Reporting_Month_Ref)) %>%
        group_by(Site, Metric_Group, Metric_Name) %>%
        mutate(id = row_number()) 
      
      data <- merge(data, summary_tab_metrics[,c("Metric_Group","Metric_Name","Summary_Metric_Name")], 
                             by = c("Metric_Group","Metric_Name"))
      
      # Selected Month/Year Metric
      # Current Period Table
      current_breakdown <- data %>%
        filter(Reporting_Month_Ref ==  current_period) %>%
        mutate(`Current Period` = ifelse(str_detect(Premier_Reporting_Period, "/"), 
                                         paste0("Rep. Pd. Ending ", Premier_Reporting_Period), Premier_Reporting_Period)) %>%
        select(Metric_Group, Summary_Metric_Name, Site, value_rounded, Status, Target)

      current_breakdown <- current_breakdown[,c("Metric_Group","Summary_Metric_Name","Site","value_rounded","Status","Target")]
      current_breakdown <- as.data.frame(current_breakdown)
      
      ## Create target traffic lights for current month metrics
      # Create traffic lights for the targets
      col_red <- which(current_breakdown == "Red", arr.ind = TRUE)
      col_red_rows <- as.integer(col_red[,1])
      col_red_cols <- as.integer(col_red[,2])
      col_yellow <- which(current_breakdown == "Yellow", arr.ind = TRUE)
      col_green <- which(current_breakdown == "Green", arr.ind = TRUE)
      
      colors_comb <- as.data.frame(rbind(col_red, col_yellow, col_green))
      
      if(nrow(colors_comb) != 0){
        for (i in 1:nrow(colors_comb)){
          current_breakdown[colors_comb[i,1],colors_comb[i,2]] <- fa('fas fa-circle')
        }
      }
      
      if(nrow(col_red) != 0){
        for (i in 1:nrow(col_red)){
          current_breakdown[col_red[i,1],col_red[i,2]] <- cell_spec(current_breakdown[col_red[i,1],col_red[i,2]], 'html', color = 'red', escape = FALSE)
        }
      }
      
      if(nrow(col_green) != 0){
        for(i in 1:nrow(col_green)){
          current_breakdown[col_green[i,1],col_green[i,2]] <- cell_spec(current_breakdown[col_green[i,1],col_green[i,2]], 'html', color = 'green', escape = FALSE)
        }
      }
      
      if(nrow(col_yellow != 0)){
        for (i in 1:nrow(col_yellow)){
          current_breakdown[col_yellow[i,1],col_yellow[i,2]] <- cell_spec(current_breakdown[col_yellow[i,1],col_yellow[i,2]], 'html', color = 'yellow', escape = FALSE)
        }
      }
      
      
      # Previous Months Summary
      ## Past 12 months of Summary 
      past_avg <- data %>%
        filter(id >= 2 & id <= 13) %>%
        group_by(Metric_Group, Summary_Metric_Name, Site) %>%
        summarise(`Avg. of Past Months Shown` = round(mean(value_rounded, na.rm = TRUE),2))
        # mutate_if(is.numeric, .funs=list(~prettyNum(.,big.mark=",")))
      

      ## Past 12 months of Breakout
      past_breakdown <- data %>%
        filter(id >= 2 & id <= 13) %>%
        group_by(Metric_Group,Summary_Metric_Name, Site, Reporting_Month_Ref) %>%
        summarise(value_rounded = round(mean(value_rounded, na.rm = TRUE),2)) %>%
        arrange(Reporting_Month_Ref) %>%
        mutate(Reporting_Month_Ref = format(as.Date(Reporting_Month_Ref, format = "%Y-%m-%d"),"%b-%Y")) %>%
        pivot_wider(names_from = Reporting_Month_Ref, values_from = value_rounded) 
        # mutate_if(is.numeric, .funs=list(~prettyNum(.,big.mark=",")))
      
      
      # Merge Current and Previous Months Breakdown
      breakdown_all <- merge(current_breakdown, past_avg, by = c("Metric_Group","Summary_Metric_Name","Site"))
      breakdown_all <- merge(breakdown_all, past_breakdown, by = c("Metric_Group","Summary_Metric_Name","Site"))
      
      
      # Rename column "value rounded" column with current period selected
      names(breakdown_all)[names(breakdown_all) == 'value_rounded'] <- format(as.Date(current_period, format = "%Y-%m-%d"),"%b-%Y")
      
      # Format units
      breakdown_all <- merge(breakdown_all, metric_unit_filter_summary)#,
      # by.x = c("Metric_Group","Summary_Metric_Name"),
      # by.y = c("Metric_Group","Metric_Name"))
      
      breakdown_all <- breakdown_all %>%
        mutate_if(is.numeric, funs(ifelse(is.na(Metric_Unit), prettyNum(round(.,1), big.mark = ','),
                                          ifelse(Metric_Unit == "Dollar", dollar(round(.)), percent(.,2)))))
      
      breakdown_all$Metric_Unit <- NULL
      
      # Create and Format Comparison Table
      breakdown_all[breakdown_all == "NA"] <- NA
      breakdown_all[breakdown_all == "NaN"] <- NA
      breakdown_all[breakdown_all == "NA%"] <- NA
      breakdown_all[breakdown_all == "$NA"] <- NA
      breakdown_all[breakdown_all == "NaN%"] <- NA
      breakdown_all[breakdown_all == "$NaN"] <- NA
      
      breakdown_all[is.na(breakdown_all)] <- "-"

      
      
      breakdown_all <- breakdown_all[order(factor(breakdown_all$Metric_Group,
                                                  levels=unique(summary_tab_metrics$Metric_Group))),]
      
      breakdown_all$Target[breakdown_all$Summary_Metric_Name %in% c("Variance to Budget", "Budget to Actual MOM")] <- ">= Budget"
      breakdown_all$Target[breakdown_all$Summary_Metric_Name %in% c("Budget to Actual MOM")] <- "<= Budget"
      
      # Set metric groups and metric names as factors and reorder dataframe accordingly
      breakdown_all <- breakdown_all %>%
        mutate(Metric_Group = factor(Metric_Group,
                                     levels = unique(summary_tab_metrics$Metric_Group),
                                     ordered = TRUE),
               Summary_Metric_Name = factor(Summary_Metric_Name,
                                            levels = unique(summary_tab_metrics$Summary_Metric_Name),
                                            ordered = TRUE)) %>%
        arrange(Metric_Group,
                Summary_Metric_Name,
                Site) %>%
        mutate(Metric_Group = as.character(Metric_Group),
               Summary_Metric_Name = as.character(Summary_Metric_Name))
      
      row.names(breakdown_all) <- NULL
      
      metric_group_order <- as.vector(unique(breakdown_all$Metric_Group))
      metric_name_order <- as.vector(unique(breakdown_all$Summary_Metric_Name))
      
      #breakdown_all <- merge(breakdown_all, metric_grouping[,c("Metric_Group", "Metric_Name")])
      
      # breakdown_all <- breakdown_all[order(factor(breakdown_all$Metric_Group, levels=unique(summary_tab_metrics$Metric_Group))),]
      
      
            
      factor_ordering <- table(breakdown_all$Summary_Metric_Name)
      factor_ordering <- factor_ordering[order(factor(names(factor_ordering), levels = metric_name_order))]
      
      ## Get the months in the df
      month_included <- breakdown_all %>%
                          select(-Summary_Metric_Name,-Metric_Group,-Site,-Status,-Target,-`Avg. of Past Months Shown`)
    
      original_columns <- as.Date(sprintf("%s-01",colnames(month_included)), format= "%b-%Y-%d")
      
      #Subtract 12 months from the latest month joing drop the day and add the first of the month back in
      latest_month_shown <- as.Date(paste0(format(original_columns[1] %m-% months(12), "%Y-%m"), "-01"), format = "%Y-%m-%d")
      
      columns_being_removed <- which(original_columns < latest_month_shown)
      columns_being_removed <- original_columns[columns_being_removed]
      columns_being_removed <- format(columns_being_removed, "%b-%Y")
      
      
      breakdown_all <- breakdown_all %>% select(-all_of(columns_being_removed))
      
      breakdown_all[,3:length(breakdown_all)] %>%
        kable(align = "l", escape = FALSE) %>%
         #pack_rows(index = table(breakdown_all$Metric_Group)[metric_group_order], label_row_css = "background-color: #212070; color: white;") %>%
        #pack_rows(index = table(breakdown_all$Summary_Metric_Name)[metric_name_order], label_row_css = "background-color: #212070; color: white;") %>%
        pack_rows(index = factor_ordering, label_row_css = "background-color: #212070; color: white;") %>%
        kable_styling(bootstrap_options = c("hover","bordered","striped"), full_width = FALSE,
                      position = "center", row_label_position = "c", font_size = 16) %>%
        add_header_above(c(" " = 1, "Selected Month-Year" = 2, " " = 2, "Monthly Breakout (Shows Previous Periods)" = length(breakdown_all)-7),
                         font_size = 16, bold = TRUE, color = "white", background = c("white", "#d80b8c", "white", "#00AEEF")) %>% 
        row_spec(0,  background = "#212070", color = "white") %>%
        column_spec(1, bold = TRUE) %>%
        column_spec(2:3, background = "#fee7f5", bold = TRUE) %>%
        column_spec(6:(length(breakdown_all)-2), 
                    background = "#E6F8FF")

    }
    
    
    # 3. Breakout Tab Output -----------------3----------------------------------------------------------------
    output$siteBreakout_title <- renderText({
      paste0(input$selectedCampus3," ",input$selectedService3, " Breakout")
    })
    
    output$siteBreakout_table <- function(){
      
      input$submit_prod
      input$submit_engineering
      input$submit_finance
      input$submit_food
      input$submit_evs
      
      
      service_input <- input$selectedService3
      month_input <- input$selectedMonth3
      site_input <- input$selectedCampus3

      # service_input <- "Environmental Services"
      # month_input <- "08-2021"
      # site_input <- "MSH"

      # Code Starts ---------------------------------------------------------------------------------
      breakout_tab_metrics <- unique((metric_grouping_filter %>%
                                       filter(Service == service_input))[,c("Service","Metric_Group","Metric_Name","Metric_Name_Submitted")]) # Filter out summary tab metrics only
      
      target_section_metrics <- unique((target_mapping %>%
                                          filter(Service == service_input))[,c("Service","Metric_Group","Metric_Name")])
      
      metric_targets <- target_mapping %>% filter(Service == service_input)
      
      current_period <- as.Date(fast_strptime(month_input, "%m-%Y"), "%Y-%m-%d")
      fiscal_year <- format(current_period,  "%Y")
      
      data <- metrics_final_df %>% 
        filter(Service == service_input) %>% # input$selectedService
        filter(Site == site_input) %>%
        filter(Reporting_Month_Ref <= current_period) %>%
        arrange(Site, Metric_Group, Metric_Name, desc(Reporting_Month_Ref)) %>%
        group_by(Site, Metric_Group, Metric_Name) #%>%
        # mutate(id = row_number()) 
      
      months <- metrics_final_df %>% 
        filter(Service == service_input) %>% # input$selectedService
        filter(Site == site_input) %>%
        filter(Reporting_Month_Ref <= current_period) %>%
        distinct(Reporting_Month_Ref) %>%
        arrange(desc(Reporting_Month_Ref)) %>%
        mutate(id = row_number())
      
      data <- left_join(data, months,
                        by = c("Reporting_Month_Ref" = "Reporting_Month_Ref"))

      
      data <- merge(data, breakout_tab_metrics[,c("Metric_Group","Metric_Name","Metric_Name_Submitted")], 
                    by = c("Metric_Group","Metric_Name"))
      data$Metric_Name <- NULL
      names(data)[names(data) == "Metric_Name_Submitted"] <- "Metric_Name"
      
      # Selected Month/Year Metric
      # Current Period Table
      current_site_breakdown <- data %>%
        filter(Reporting_Month_Ref ==  current_period) %>%
        select(Metric_Group, Metric_Name, value_rounded, Status, Target) 
        # mutate_if(is.numeric, .funs=list(~prettyNum(.,big.mark=",")))
      
      current_site_breakdown <- current_site_breakdown[,c("Metric_Group","Metric_Name","value_rounded","Status","Target")]
      current_site_breakdown <- as.data.frame(current_site_breakdown)
      
      ## Create target traffic lights for current month metrics
      # Create traffic lights for the targets
      col_red <- which(current_site_breakdown == "Red", arr.ind = TRUE)
      col_red_rows <- as.integer(col_red[,1])
      col_red_cols <- as.integer(col_red[,2])
      col_yellow <- which(current_site_breakdown == "Yellow", arr.ind = TRUE)
      col_green <- which(current_site_breakdown == "Green", arr.ind = TRUE)
      
      colors_comb <- as.data.frame(rbind(col_red, col_yellow, col_green))
      
      if(nrow(colors_comb) != 0){
        for (i in 1:nrow(colors_comb)){
          current_site_breakdown[colors_comb[i,1],colors_comb[i,2]] <- fa('fas fa-circle')
        }
      }
      
      if(nrow(col_red) != 0){
        for (i in 1:nrow(col_red)){
          current_site_breakdown[col_red[i,1],col_red[i,2]] <- cell_spec(current_site_breakdown[col_red[i,1],col_red[i,2]], 'html', color = 'red', escape = FALSE)
        }
      }
      
      if(nrow(col_green) != 0){
        for(i in 1:nrow(col_green)){
          current_site_breakdown[col_green[i,1],col_green[i,2]] <- cell_spec(current_site_breakdown[col_green[i,1],col_green[i,2]], 'html', color = 'green', escape = FALSE)
        }
      }
      
      if(nrow(col_yellow != 0)){
        for (i in 1:nrow(col_yellow)){
          current_site_breakdown[col_yellow[i,1],col_yellow[i,2]] <- cell_spec(current_site_breakdown[col_yellow[i,1],col_yellow[i,2]], 'html', color = 'yellow', escape = FALSE)
        }
      }
      
      
      # Previous Months Summary
      ## Past 12 months of Summary In
      past_avg_site <- data %>%
        filter(id >= 2 & id <= 13) %>%
        group_by(Metric_Group, Metric_Name) %>%
        summarise(`Avg. of Past Months Shown` = mean(value_rounded, na.rm = TRUE))
        # mutate_if(is.numeric, .funs=list(~prettyNum(.,big.mark=",")))
      
      ## Past 12 months of Breakout
      past_site_breakdown <- data %>%
        filter(id >= 2 & id <= 13) %>%
        group_by(Metric_Group, Metric_Name, Reporting_Month_Ref) %>%
        summarise(value_rounded = mean(value_rounded, na.rm = TRUE)) %>%
        arrange(Reporting_Month_Ref) %>%
        mutate(Reporting_Month_Ref = format(as.Date(Reporting_Month_Ref, format = "%Y-%m-%d"),"%b-%Y")) %>%
        pivot_wider(names_from = Reporting_Month_Ref, values_from = value_rounded)
        # mutate_if(is.numeric, .funs=list(~prettyNum(.,big.mark=",")))
      
      
      # Merge Current and Previous Months Breakdown
      breakdown_all_site <- merge(current_site_breakdown, past_avg_site, by = c("Metric_Group","Metric_Name"), all = TRUE)
      breakdown_all_site <- merge(breakdown_all_site, past_site_breakdown, by = c("Metric_Group","Metric_Name"))
      breakdown_all_site <- breakdown_all_site[order(factor(breakdown_all_site$Metric_Group, levels=unique(breakout_tab_metrics$Metric_Group))),] 
      metric_group_order <- as.vector(unique(breakdown_all_site$Metric_Group))
      
      names(breakdown_all_site)[names(breakdown_all_site) == 'value_rounded'] <- format(as.Date(current_period, format = "%Y-%m-%d"),"%b-%Y")
      
      # Format units
      breakdown_all_site <- merge(breakdown_all_site, metric_unit_filter,
                             by.x = c("Metric_Group","Metric_Name"),
                             by.y = c("Metric_Group","Metric_Name"),
                             all.x = TRUE)
      
      breakdown_all_site <- breakdown_all_site %>%
        mutate_if(is.numeric, funs(ifelse(is.na(Metric_Unit), prettyNum(round(.,1), big.mark = ','),
                                          ifelse(Metric_Unit == "Dollar", dollar(round(.)), percent(.,2)))))
      
      breakdown_all_site$Metric_Unit <- NULL
      
      # Create and Format Comparison Table
      breakdown_all_site[breakdown_all_site == "NA"] <- NA
      breakdown_all_site[breakdown_all_site == "NaN"] <- NA
      breakdown_all_site[breakdown_all_site == "NA%"] <- NA
      breakdown_all_site[breakdown_all_site == "$NA"] <- NA
      breakdown_all_site[breakdown_all_site == "NaN%"] <- NA
      breakdown_all_site[breakdown_all_site == "$NaN"] <- NA
      
      breakdown_all_site[is.na(breakdown_all_site)] <- "-"
      breakdown_all_site <- breakdown_all_site[order(factor(breakdown_all_site$Metric_Group, levels=unique(breakout_tab_metrics$Metric_Group))),] 
      row.names(breakdown_all_site) <- NULL
      
      breakdown_all_site$Target[breakdown_all_site$Metric_Name %in% c("Variance to Budget")] <- ">= Budget"
      breakdown_all_site$Target[breakdown_all_site$Metric_Name %in% c("Budget to Actual MOM")] <- "<= Budget"
      
      factor_ordering <- table(breakdown_all_site$Metric_Group)
      factor_ordering <- factor_ordering[order(factor(names(factor_ordering), levels = metric_group_order))]
      
      
      breakdown_all_site[,2:length(breakdown_all_site)] %>%
        kable(align = "l", escape = FALSE) %>%
        # pack_rows(index = table(breakdown_all_site$Metric_Group)[metric_group_order], label_row_css = "background-color: #212070; color: white;") %>%
        #pack_rows(index = table(breakdown_all_site$Metric_Group)[metric_group_order], label_row_css = "background-color: #212070; color: white;") %>%
        pack_rows(index = factor_ordering, label_row_css = "background-color: #212070; color: white;") %>%
        kable_styling(bootstrap_options = c("hover","bordered","striped"), full_width = FALSE,
                      position = "center", row_label_position = "c", font_size = 16) %>%
        add_header_above(c(" " = 1, "Selected Month-Year" = 2, " " = 2, "Monthly Breakout (Shows Previous Periods)" = length(breakdown_all_site)-6),
                         font_size = 16, bold = TRUE, color = "white", background = c("white", "#d80b8c", "white", "#00AEEF")) %>% 
        row_spec(0,  background = "#212070", color = "white") %>%
        column_spec(1, bold = TRUE) %>%
        column_spec(2:3, background = "#fee7f5", bold = TRUE) %>%
        column_spec(6:(length(breakdown_all_site)-1), 
                    background = "#E6F8FF")
      
    }
    
    
    # 4. Data Tab Output -----------------3----------------------------------------------------------------
    observeEvent(input$submit_finance,{
      inFile_msh_msm_msq <- input$finance_msh_msm_msq
      flag <- 0
      
      
      if (is.null(inFile_msh_msm_msq)) {
        return(NULL)
      }else{
        tryCatch({sheet_names <- excel_sheets(inFile_msh_msm_msq$datapath)
           sheet_names <- sheet_names[sheet_names != "Sheet3"]
            for (i in 1:length(sheet_names)){
              data <- read_excel(inFile_msh_msm_msq$datapath, sheet = sheet_names[i])
              sheet_month <- colnames(data)[1]
              data <- data %>% row_to_names(row_number = 1)
              data$Month <- sheet_month
              
              if(i == 1){
                prev <- data
              } else{
                data <- full_join(data,prev)
              }
              prev <- data
            }
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
        
        if(flag == 1){
          
          tryCatch({exptrend_data <- exptrend_process(prev)
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
      }
      
      metrics_final_df <<- budget_to_actual_process(exptrend_data)
      saveRDS(metrics_final_df, metrics_final_df_path)
      

      
    })
    
    observeEvent(input$submit_finance,{
      
      inFile_census <- input$finance_census

      
      
      if (is.null(inFile_census)) {
        return(NULL)
      }else{
        data_census <- read_excel(inFile_census$datapath)
      }

    })
    
    observeEvent(input$submit_finance,{

      inFile_msbi_msb_msw <- input$finance_msbi_msb_msw
      flag <- 0

      if (is.null(inFile_msbi_msb_msw)) {
        return(NULL)
      }else{
        bislr_data <- read_excel(inFile_msbi_msb_msw$datapath)
        # bislr_data <- read_excel("Data_Dashboard/Finance/September 2020 and 2021 Actual BISLR at 10-20-21 mp.xlsx")
        
      }
      
      bislr_data <- bislr_preprocess(bislr_data)
      metrics_final_df <<- budget_to_actual_process(bislr_data)
      
      saveRDS(metrics_final_df, metrics_final_df_path)
      
      budget_to_actual_tbl <- read_excel(budget_to_actual_path)
      
      updated_rows <- unique(bislr_data[c("Service","Site","Cost Center2", "Account Category Desc2", "Month")])
      updated_rows$Month <- as.Date(updated_rows$Month, "%Y-%m-%d")
      
      budget_to_actual_tbl <- anti_join(budget_to_actual_tbl, updated_rows)
      budget_to_actual_tbl <- budget_to_actual_tbl %>% filter(!is.na(Month))
      
      bislr_data$Month <- as.Date(bislr_data$Month, "%Y-%m-%d")
      bislr_data$`Month Actual` <- as.double(bislr_data$`Month Actual`)
      budget_to_actual_tbl <- full_join(budget_to_actual_tbl, bislr_data)
      
      
      budget_to_actual_tbl <- as.data.frame(budget_to_actual_tbl)
      write_xlsx(budget_to_actual_tbl, budget_to_actual_path, row.names = FALSE)
      
      })
    
    
    ## Read in Press Ganey data -----------------------------------
    # ED Monthly Data Observe Event -------------------
    observeEvent(input$submit_monthly_press_ganey, {
      
      # Name ED monthly data
      ed_monthly <- input$pg_ed_monthly
      
      if (is.null(ed_monthly)) {
        return(NULL)
      } else {
        ed_monthly_filepath <- ed_monthly$datapath
        
        ed_monthly_filepath <- paste0(home_path,
                                      "Input Data Raw/Press Ganey/",
                                      "ED 09-2021.csv")

        data_ed_monthly <- read_csv(ed_monthly_filepath,
                                    show_col_types = FALSE)
      }
      
      # Save prior version of Press Ganey Dept Summary data
      write_xlsx(press_ganey_data,
                 paste0(hist_archive_path,
                        "Press Ganey Pre-ED Monthly ",
                        format(Sys.time(), "%Y%m%d_%H%M%S"),
                        ".xlsx"))
      
      # Process ED monthly data
      pg_ed_monthly_summary_data <- press_ganey_dept_summary(data_ed_monthly)
      
      # Append Press Ganey summary with new data
      # First, identify the sites, months, and metrics in the new data
      pg_new_data <- unique(
        pg_ed_monthly_summary_data[c("Service",
                                     "Site",
                                     "ReportingType",
                                     "Reporting_Date_Start",
                                     "Reporting_Date_End",
                                     "Question_Clean")]
        )
      
      # Second, remove these sites, months, and metrics from the historical data, if they exist there.
      # This allows us to ensure no duplicate entries for the same site, metric, and time period
      press_ganey_data <<- anti_join(press_ganey_data,
                                        pg_new_data,
                                        by = c("Service" = "Service",
                                               "Site" = "Site",
                                               "Question_Clean" = "Question_Clean",
                                               "ReportingType" = "ReportingType",
                                               "Reporting_Date_Start" = "Reporting_Date_Start",
                                               "Reporting_Date_End" = "Reporting_Date_End")
      )
      
      # Third, combine the updated historical data with the new data
      press_ganey_data <<- full_join(press_ganey_data,
                                     pg_ed_monthly_summary_data)
      
      # Next, arrange the department summary by month, metric name, and site
      press_ganey_data <<- press_ganey_data %>%
        arrange(Service,
                Site,
                ReportingType,
                Reporting_Date_End)
      
      # Lastly, save the updated summary data
      write_xlsx(press_ganey_data, press_ganey_table_path)
      
      # Update metrics_final_df with latest ED Press Ganey data using custom function
      metrics_final_df <<- press_ganey_metrics_final_df(pg_ed_monthly_summary_data)
      
      # Save updated metrics_final_df
      saveRDS(metrics_final_df, metrics_final_df_path)
      
      # Update "Reporting Month" drop down in each tab
      picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
      updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])

    })
    
    # Nursing Press Ganey Monthly Data Observe Event -------------------
    observeEvent(input$submit_monthly_press_ganey, {
      
      # Name Nursing monthly data
      nursing_monthly <- input$pg_nursing_monthly
      
      if (is.null(nursing_monthly)) {
        return(NULL)
      } else {
        nursing_monthly_filepath <- nursing_monthly$datapath
        
        # nursing_monthly_filepath <- paste0(home_path,
        #                               "Input Data Raw/Press Ganey/",
        #                               "Nursing 08-2021.csv")
        
        data_nursing_monthly <- read_csv(nursing_monthly_filepath,
                                         show_col_types = FALSE)
      }
      
      # Save prior version of Press Ganey Dept Summary data
      write_xlsx(press_ganey_data,
                 paste0(hist_archive_path,
                        "Press Ganey Pre-RN Monthly ",
                        format(Sys.time(), "%Y%m%d_%H%M%S"),
                        ".xlsx"))
      
      # Process Nursing monthly data
      pg_nursing_monthly_summary_data <- press_ganey_dept_summary(data_nursing_monthly)
      
      # Append Press Ganey summary with new data
      # First, identify the sites, months, and metrics in the new data
      pg_new_data <- unique(
        pg_nursing_monthly_summary_data[c("Service",
                                          "Site",
                                          "ReportingType",
                                          "Reporting_Date_Start",
                                          "Reporting_Date_End",
                                          "Question_Clean")]
      )
      
      # Second, remove these sites, months, and metrics from the historical data, if they exist there.
      # This allows us to ensure no duplicate entries for the same site, metric, and time period
      press_ganey_data <<- anti_join(press_ganey_data,
                                     pg_new_data,
                                     by = c("Service" = "Service",
                                            "Site" = "Site",
                                            "Question_Clean" = "Question_Clean",
                                            "ReportingType" = "ReportingType",
                                            "Reporting_Date_Start" = "Reporting_Date_Start",
                                            "Reporting_Date_End" = "Reporting_Date_End")
      )
      
      # Third, combine the updated historical data with the new data
      press_ganey_data <<- full_join(press_ganey_data,
                                     pg_nursing_monthly_summary_data)
      
      # Next, arrange the department summary by month, metric name, and site
      press_ganey_data <<- press_ganey_data %>%
        arrange(Service,
                Site,
                ReportingType,
                Reporting_Date_End)
      
      # Lastly, save the updated summary data
      write_xlsx(press_ganey_data, press_ganey_table_path)
      
      # Update metrics_final_df with latest Nursing Press Ganey data using custom function
      metrics_final_df <<- press_ganey_metrics_final_df(pg_nursing_monthly_summary_data)
      
      # Save updated metrics_final_df
      saveRDS(metrics_final_df, metrics_final_df_path)
      
      # Update "Reporting Month" drop down in each tab
      picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
      updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      
    })
    
    # Support Services Press Ganey Monthly Data Observe Event -------------------
    observeEvent(input$submit_monthly_press_ganey, {
      
      # Name Support Services monthly data
      support_monthly <- input$pg_support_monthly
      
      if (is.null(support_monthly)) {
        return(NULL)
      } else {
        support_monthly_filepath <- support_monthly$datapath
        
        # support_monthly_filepath <- paste0(home_path,
        #                                    "Input Data Raw/Press Ganey/",
        #                                    "Support Services 08-2021.csv")
        
        data_support_monthly <- read_csv(support_monthly_filepath,
                                         show_col_types = FALSE)
      }
      
      # Save prior version of Press Ganey Dept Summary data
      write_xlsx(press_ganey_data,
                 paste0(hist_archive_path,
                        "Press Ganey Pre-Support Monthly ",
                        format(Sys.time(), "%Y%m%d_%H%M%S"),
                        ".xlsx"))
      
      # Process Support Services monthly data
      pg_support_monthly_summary_data <- press_ganey_dept_summary(data_support_monthly)
      
      # Append Press Ganey summary with new data
      # First, identify the sites, months, and metrics in the new data
      pg_new_data <- unique(
        pg_support_monthly_summary_data[c("Service",
                                          "Site",
                                          "ReportingType",
                                          "Reporting_Date_Start",
                                          "Reporting_Date_End",
                                          "Question_Clean")]
      )
      
      # Second, remove these sites, months, and metrics from the historical data, if they exist there.
      # This allows us to ensure no duplicate entries for the same site, metric, and time period
      press_ganey_data <<- anti_join(press_ganey_data,
                                     pg_new_data,
                                     by = c("Service" = "Service",
                                            "Site" = "Site",
                                            "Question_Clean" = "Question_Clean",
                                            "ReportingType" = "ReportingType",
                                            "Reporting_Date_Start" = "Reporting_Date_Start",
                                            "Reporting_Date_End" = "Reporting_Date_End")
      )
      
      # Third, combine the updated historical data with the new data
      press_ganey_data <<- full_join(press_ganey_data,
                                     pg_support_monthly_summary_data)
      
      # Next, arrange the department summary by month, metric name, and site
      press_ganey_data <<- press_ganey_data %>%
        arrange(Service,
                Site,
                ReportingType,
                Reporting_Date_End)
      
      # Lastly, save the updated summary data
      write_xlsx(press_ganey_data, press_ganey_table_path)
      
      # Update metrics_final_df with latest Support Services Press Ganey data using custom function
      metrics_final_df <<- press_ganey_metrics_final_df(pg_support_monthly_summary_data)
      
      # Save updated metrics_final_df
      saveRDS(metrics_final_df, metrics_final_df_path)
      
      # Update "Reporting Month" drop down in each tab
      picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
      updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      
    })
    
    
    # ED YTD Data Observe Event -------------------
    observeEvent(input$submit_ytd_press_ganey, {
      
      # Name ED YTD data
      ed_ytd <- input$pg_ed_ytd
      
      if (is.null(ed_ytd)) {
        return(NULL)
      } else {
        ed_ytd_filepath <- ed_ytd$datapath
        
        # ed_ytd_filepath <- paste0(home_path,
        #                               "Input Data Raw/Press Ganey/",
        #                               "ED YTD 012021 to 092021.csv")
        
        data_ed_ytd <- read_csv(ed_ytd_filepath,
                                show_col_types = FALSE)
      }
      
      # Save prior version of Press Ganey Dept Summary data
      write_xlsx(press_ganey_data,
                 paste0(hist_archive_path,
                        "Press Ganey Pre-ED YTD ",
                        format(Sys.time(), "%Y%m%d_%H%M%S"),
                        ".xlsx"))
      
      # Process ED YTD data
      pg_ed_ytd_summary_data <- press_ganey_dept_summary(data_ed_ytd)
      
      # Append Press Ganey summary with new data
      # First, identify the sites, months, and metrics in the new data
      pg_new_data <- unique(
        pg_ed_ytd_summary_data[c("Service",
                                 "Site",
                                 "ReportingType",
                                 "Reporting_Date_Start",
                                 "Reporting_Date_End",
                                 "Question_Clean")]
      )
      
      # Second, remove these sites, months, and metrics from the historical data, if they exist there.
      # This allows us to ensure no duplicate entries for the same site, metric, and time period
      press_ganey_data <<- anti_join(press_ganey_data,
                                     pg_new_data,
                                     by = c("Service" = "Service",
                                            "Site" = "Site",
                                            "Question_Clean" = "Question_Clean",
                                            "ReportingType" = "ReportingType",
                                            "Reporting_Date_Start" = "Reporting_Date_Start",
                                            "Reporting_Date_End" = "Reporting_Date_End")
      )
      
      # Third, combine the updated historical data with the new data
      press_ganey_data <<- full_join(press_ganey_data,
                                     pg_ed_ytd_summary_data)
      
      # Next, arrange the department summary by month, metric name, and site
      press_ganey_data <<- press_ganey_data %>%
        arrange(Service,
                Site,
                ReportingType,
                Reporting_Date_End)
      
      # Lastly, save the updated summary data
      write_xlsx(press_ganey_data, press_ganey_table_path)
      
      # Update "Reporting Month" drop down in each tab
      picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
      updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      
    })
    
    # Nursing YTD Data Observe Event -------------------
    observeEvent(input$submit_ytd_press_ganey, {
      
      # Name Nursing YTD data
      nursing_ytd <- input$pg_nursing_ytd
      
      if (is.null(nursing_ytd)) {
        return(NULL)
      } else {
        nursing_ytd_filepath <- nursing_ytd$datapath
        
        # nursing_ytd_filepath <- paste0(home_path,
        #                               "Input Data Raw/Press Ganey/",
        #                               "Nursing YTD 012021 to 082021.csv")
        
        data_nursing_ytd <- read_csv(nursing_ytd_filepath,
                                     show_col_types = FALSE)
      }
      
      # Save prior version of Press Ganey Dept Summary data
      write_xlsx(press_ganey_data,
                 paste0(hist_archive_path,
                        "Press Ganey Pre-RN YTD ",
                        format(Sys.time(), "%Y%m%d_%H%M%S"),
                        ".xlsx"))
      
      # Process Nursing YTD data
      pg_nursing_ytd_summary_data <- press_ganey_dept_summary(data_nursing_ytd)
      
      # Append Press Ganey summary with new data
      # First, identify the sites, months, and metrics in the new data
      pg_new_data <- unique(
        pg_nursing_ytd_summary_data[c("Service",
                                      "Site",
                                      "ReportingType",
                                      "Reporting_Date_Start",
                                      "Reporting_Date_End",
                                      "Question_Clean")]
      )
      
      # Second, remove these sites, months, and metrics from the historical data, if they exist there.
      # This allows us to ensure no duplicate entries for the same site, metric, and time period
      press_ganey_data <<- anti_join(press_ganey_data,
                                     pg_new_data,
                                     by = c("Service" = "Service",
                                            "Site" = "Site",
                                            "Question_Clean" = "Question_Clean",
                                            "ReportingType" = "ReportingType",
                                            "Reporting_Date_Start" = "Reporting_Date_Start",
                                            "Reporting_Date_End" = "Reporting_Date_End")
      )
      
      # Third, combine the updated historical data with the new data
      press_ganey_data <<- full_join(press_ganey_data,
                                     pg_nursing_ytd_summary_data)
      
      # Next, arrange the department summary by month, metric name, and site
      press_ganey_data <<- press_ganey_data %>%
        arrange(Service,
                Site,
                ReportingType,
                Reporting_Date_End)
      
      # Lastly, save the updated summary data
      write_xlsx(press_ganey_data, press_ganey_table_path)
      
      # Update "Reporting Month" drop down in each tab
      picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
      updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      
    })
    
    # Support Services YTD Data Observe Event -------------------
    observeEvent(input$submit_ytd_press_ganey, {
      
      # Name Support Services YTD data
      support_ytd <- input$pg_support_ytd
      
      if (is.null(support_ytd)) {
        return(NULL)
      } else {
        support_ytd_filepath <- support_ytd$datapath
        
        # support_ytd_filepath <- paste0(home_path,
        #                               "Input Data Raw/Press Ganey/",
        #                               "Support Services YTD 012021 to 082021.csv")
        
        data_support_ytd <- read_csv(support_ytd_filepath,
                                     show_col_types = FALSE)
      }
      
      # Save prior version of Press Ganey Dept Summary data
      write_xlsx(press_ganey_data,
                 paste0(hist_archive_path,
                        "Press Ganey Pre-Support YTD ",
                        format(Sys.time(), "%Y%m%d_%H%M%S"),
                        ".xlsx"))
      
      # Process Support Services YTD data
      pg_support_ytd_summary_data <- press_ganey_dept_summary(data_support_ytd)
      
      # Append Press Ganey summary with new data
      # First, identify the sites, months, and metrics in the new data
      pg_new_data <- unique(
        pg_support_ytd_summary_data[c("Service",
                                      "Site",
                                      "ReportingType",
                                      "Reporting_Date_Start",
                                      "Reporting_Date_End",
                                      "Question_Clean")]
      )
      
      # Second, remove these sites, months, and metrics from the historical data, if they exist there.
      # This allows us to ensure no duplicate entries for the same site, metric, and time period
      press_ganey_data <<- anti_join(press_ganey_data,
                                     pg_new_data,
                                     by = c("Service" = "Service",
                                            "Site" = "Site",
                                            "Question_Clean" = "Question_Clean",
                                            "ReportingType" = "ReportingType",
                                            "Reporting_Date_Start" = "Reporting_Date_Start",
                                            "Reporting_Date_End" = "Reporting_Date_End")
      )
      
      # Third, combine the updated historical data with the new data
      press_ganey_data <<- full_join(press_ganey_data,
                                     pg_support_ytd_summary_data)
      
      # Next, arrange the department summary by month, metric name, and site
      press_ganey_data <<- press_ganey_data %>%
        arrange(Service,
                Site,
                ReportingType,
                Reporting_Date_End)
      
      # Lastly, save the updated summary data
      write_xlsx(press_ganey_data, press_ganey_table_path)
      
      # Update "Reporting Month" drop down in each tab
      picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
      updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      
    })
    
    
    ## Productivity data --------------------------------
    ## Read in productivity data and process
    observeEvent(input$submit_prod,{
      inFile <- input$productiviy_data
      inFile_nursing_radiology <- input$productiviy_data_nursing_radiology 
      data <- read_excel(inFile$datapath)
      data_nursing_radiology <- read_excel(inFile_nursing_radiology$datapath)
      

      tryCatch({metrics_final_df <<- productivity_process(data,data_nursing_radiology)
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
      
    })
    
    observeEvent(input$submit_food,{
      food_file <- input$food_cost_and_revenue
      flag <- 0
      
      if(is.null(food_file)){
        return(NULL)
      }else{
       food_file_path <- food_file$datapath
       #food_file_path <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/Food/MSHS Workforce Data Request_Food_RecurringRequest 2021_Oct21.xlsx"
       tryCatch({food_data <- read_excel(food_file_path, sheet = "Cost and Revenue")
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

      # Save prior version of Food Dept Summary data
      write_xlsx(cost_and_revenue_repo,
                 paste0(hist_archive_path,
                        "Food Services Updates ",
                        format(Sys.time(), "%Y%m%d_%H%M%S"),
                        ".xlsx"))
      
      if (flag == 1){
      # Process Cost and Revenue data
        tryCatch({food_summary_data <- cost_and_revenue_file_process(food_data)
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
        
        # Third, combine the updated historical data with the new data
        cost_and_revenue_repo <<- full_join(cost_and_revenue_repo,
                                            food_summary_data)
        
        # Lastly, save the updated summary data
        write_xlsx(cost_and_revenue_repo, ops_metrics_lab_tat_path)
        
        # Update metrics_final_df with latest SCC data using custom function
        metrics_final_df <<- census_days_metrics_final_process(food_summary_data)
        
        # Save updated metrics_final_df
        saveRDS(metrics_final_df, paste0(home_path, "Summary Repos/Food Services Cost and Revenue.xlsx"))
  
        # Update "Reporting Month" drop down in each tab
        picker_choices <- format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
        updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      }
      
    })
    
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
        picker_choices <- format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
        updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        
      }
      
    })
    
    
    observeEvent(input$submit_engineering,{
      if(input$name_engineering_kpi == ""){
        showModal(modalDialog(
          title = "Error",
          paste0("Please fill in the required fields"),
          easyClose = TRUE,
          footer = NULL
        ))
      }
      
      engineering_data <<- hot_to_r(input$engineering_kpi)
      
      metrics_final_df <<- cm_kpi(engineering_data)
      #saveRDS(metrics_final_df, metrics_final_df_path)


      engineering_table <- read_excel(engineering_table_path)
      engineering_data <- engineering_data_process(engineering_data)

      updated_rows <- unique(engineering_data[c("Site", "Month")])
      updated_rows$Month <- as.Date(updated_rows$Month, "%Y-%m-%d")
      
      engineering_table <- anti_join(engineering_table, updated_rows)
      engineering_table <- engineering_table %>% filter(!is.na(Month))
      
      engineering_data$Month <- as.Date(engineering_data$Month, "%Y-%m-%d")
      engineering_data$`Number of Work Orders Created with a Life Safety Priority` <- as.double(engineering_data$`Number of Work Orders Created with a Life Safety Priority`)
      engineering_data$`EOC/Patient Care Work Orders Received` <- as.double(engineering_data$`EOC/Patient Care Work Orders Received`)
      
      
      engineering_table <- full_join(engineering_table, engineering_data)
    
    })
    
    
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
          paste0("There seems to be an issue with one of the files"),
          easyClose = TRUE,
          footer = NULL
        ))})
      }
      
      if(flag == 1){
      tryCatch({evs_data <- evs_file_process(evs_data,month)
                  flag <- 2
        
        showModal(modalDialog(
          title = "Success",
          paste0("The data has been imported succesfully"),
          easyClose = TRUE,
          footer = NULL
        ))
        },
        error = function(err){  showModal(modalDialog(
          title = "Error",
          paste0("There seems to be an issue with one of the files"),
          easyClose = TRUE,
          footer = NULL
        ))})
      }
      

      if(flag == 2){
        # Save prior version of Lab TAT Dept Summary data
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
        write_xlsx(evs_summary_repo, evs_table_path, row.names = FALSE)
        
        picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
        updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      }
      
    })
    
    data_react <- reactive({
      
      data  <- operational_metrics %>% filter(`Revenue Type` == "Actual")
      data <- data[order(data$Site),]
      
      data <- data[ , -which(names(data) %in% c("Revenue Type", "Metric Group"))]
      
      data <- data %>%
        mutate_if(is.logical, as.character) %>%
        mutate_if(is.double, as.character)
      
      
      
    })
    
    
    output$hand_table <- renderRHandsontable({
      unique_sites <- unique(data_react()$Site)
      site_1 <- which(data_react()$Site == unique_sites[1])
      site_2 <- which(data_react()$Site == unique_sites[2])
      site_3 <- which(data_react()$Site == unique_sites[3])
      site_4 <- which(data_react()$Site == unique_sites[4])
      site_5 <- which(data_react()$Site == unique_sites[5])
      site_6 <- which(data_react()$Site == unique_sites[6])
      site_7 <- which(data_react()$Site == unique_sites[7])
      
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
      
      
      col_highlight <- as.array(10:15)
      
      
      rhandsontable(data_react(), overflow= 'visible', col_highlight = col_highlight, rowHeaders = FALSE, readOnly = FALSE) %>%
        hot_table(mergeCells = list(
          list(row = 0, col = 0, rowspan = nrow(data_react()), colspan = 1),
          list(row = min(site_1)-1, col = 1, rowspan = length(site_1), colspan = 1),
          list(row = min(site_2)-1, col = 1, rowspan = length(site_2), colspan = 1),
          list(row = min(site_3)-1, col = 1, rowspan = length(site_3), colspan = 1),
          list(row = min(site_4)-1, col = 1, rowspan = length(site_4), colspan = 1),
          list(row = min(site_5)-1, col = 1, rowspan = length(site_5), colspan = 1),
          list(row = min(site_6)-1, col = 1, rowspan = length(site_6), colspan = 1),
          list(row = min(site_7)-1, col = 1, rowspan = length(site_7), colspan = 1)
        )) %>%
        hot_cols(renderer = rendederer_string)  %>%
        hot_col(1:3, readOnly = T)
    })
    
    
    
    
    
    data_food_budget <- reactive({
      data  <- operational_metrics %>% filter(Service == "Food Services" &`Revenue Type` == "Budget" & `Metric Group` == "Cost and Revenue")
      data <- data[order(data$Site),]
      data <- data[ , -which(names(data) %in% c("Revenue Type", "Metric Group"))]

      data <- data %>%
        mutate_if(is.logical, as.character) %>%
        mutate_if(is.double, as.character)
      
    })
    
    
    output$food_budget <- renderRHandsontable({
      unique_sites <- unique(data_food_budget()$Site)
      site_1 <- which(data_food_budget()$Site == unique_sites[1])
      site_2 <- which(data_food_budget()$Site == unique_sites[2])
      site_3 <- which(data_food_budget()$Site == unique_sites[3])
      site_4 <- which(data_food_budget()$Site == unique_sites[4])
      site_5 <- which(data_food_budget()$Site == unique_sites[5])
      site_6 <- which(data_food_budget()$Site == unique_sites[6])
      site_7 <- which(data_food_budget()$Site == unique_sites[7])
      
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
      
      
      col_highlight <- as.array(10:15)
      
      
      rhandsontable(data_food_budget(), overflow= 'visible', col_highlight = col_highlight, rowHeaders = FALSE, readOnly = FALSE) %>%
        hot_table(mergeCells = list(
          list(row = 0, col = 0, rowspan = nrow(data_food_budget()), colspan = 1),
          list(row = min(site_1)-1, col = 1, rowspan = length(site_1), colspan = 1),
          list(row = min(site_2)-1, col = 1, rowspan = length(site_2), colspan = 1),
          list(row = min(site_3)-1, col = 1, rowspan = length(site_3), colspan = 1),
          list(row = min(site_4)-1, col = 1, rowspan = length(site_4), colspan = 1),
          list(row = min(site_5)-1, col = 1, rowspan = length(site_5), colspan = 1),
          list(row = min(site_6)-1, col = 1, rowspan = length(site_6), colspan = 1),
          list(row = min(site_7)-1, col = 1, rowspan = length(site_7), colspan = 1)
        )) %>%
        hot_cols(renderer = rendederer_string)  %>%
        hot_col(1:3, readOnly = T)
    })
    
    data_food_census <- reactive({
      
      
      
      ### Census from template
      data  <- operational_metrics %>% filter(Service == "Food Services" & Metric == "Census Days" & `Metric Group` == "Cost and Revenue")
      data <- data[order(data$Site),]
      data <- data[ , -which(names(data) %in% c("Revenue Type", "Metric Group"))]
      
      data <- data %>%
        mutate_if(is.logical, as.character) %>%
          mutate_if(is.double, as.character)
      
      
    })
    
    
    output$food_census <- renderRHandsontable({
      unique_sites <- unique(data_food_census()$Site)
      site_1 <- which(data_food_census()$Site == unique_sites[1])
      site_2 <- which(data_food_census()$Site == unique_sites[2])
      site_3 <- which(data_food_census()$Site == unique_sites[3])
      site_4 <- which(data_food_census()$Site == unique_sites[4])
      site_5 <- which(data_food_census()$Site == unique_sites[5])
      site_6 <- which(data_food_census()$Site == unique_sites[6])
      site_7 <- which(data_food_census()$Site == unique_sites[7])
      
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
      
      
      col_highlight <- as.array(9:15)
      
      
      rhandsontable(data_food_census(), overflow= 'visible', col_highlight = col_highlight, rowHeaders = FALSE, readOnly = FALSE) %>%
        hot_table(mergeCells = list(
          list(row = 0, col = 0, rowspan = nrow(data_food_census()), colspan = 1),
          list(row = min(site_1)-1, col = 1, rowspan = length(site_1), colspan = 1),
          list(row = min(site_2)-1, col = 1, rowspan = length(site_2), colspan = 1),
          list(row = min(site_3)-1, col = 1, rowspan = length(site_3), colspan = 1),
          list(row = min(site_4)-1, col = 1, rowspan = length(site_4), colspan = 1),
          list(row = min(site_5)-1, col = 1, rowspan = length(site_5), colspan = 1),
          list(row = min(site_6)-1, col = 1, rowspan = length(site_6), colspan = 1),
          list(row = min(site_7)-1, col = 1, rowspan = length(site_7), colspan = 1)
        )) %>%
        hot_cols(renderer = rendederer_string)  %>%
        hot_col(1:3, readOnly = T)
    })
    
    
    data_engineering_kpi <- reactive({
      
      
      
      ### Census from template
      data  <- operational_metrics_engineering
      data <- data[order(data$Site),]
      
      ##### Code that adds months missing months to the rhandsontable
      months_only <- data %>% select(-Site,-Metric)
      months <- format(as.Date(colnames(months_only)), "%m-%Y")
      
      max_month <- as.Date(paste0(format(Sys.Date() %m-% months(1), "%m-%Y"), "-01"), "%m-%Y-%d")
      
      months <- as.Date(sprintf("%s-01", months), format = "%m-%Y-%d")
      
      complete_months <- seq.Date(months[1], max_month, by= 'month')
      
      missing_months <- which(!(complete_months %in% months))
      missing_months <- as.character(format(complete_months[missing_months], "%m-%Y"))
      
      data[,missing_months] <- NA
      ##########
      
      data <- data %>% 
        mutate_if(is.logical, as.character) %>%
        mutate_if(is.double, as.character)
      
      
      
      
    })
    
    
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

   
    # Lab KPI - Turnaround Time ------------
    # SCC Data submission -----------------
    observeEvent(input$submit_lab_tat,{

      # Name SCC file
      scc_file <- input$lab_scc
      
      if (is.null(scc_file)) {
        return(NULL)
      }else{
        scc_file_path <- scc_file$datapath
        #file_path <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/EVS/MSHS Normal Clean vs Iso Clean TAT Sept 2021.xlsx"
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
      
      # Process data if the right file format was submitted
      if(flag == 1) {
        tryCatch({
          # Process SCC data
          scc_summary_data <- lab_scc_tat_dept_summary(scc_data)
          
          flag <- 2
          
          showModal(modalDialog(
            title = "Success",
            paste0("This SCC data has been imported successfully"),
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
        
        # Save prior version of Lab TAT Dept Summary data
        write_xlsx(ops_metrics_lab_tat,
                   paste0(hist_archive_path,
                          "Lab TAT Metrics Pre-SCC Updates ",
                          format(Sys.time(), "%Y%m%d_%H%M%S"),
                          ".xlsx"))
        
        # Append Lab TAT summary with new data
        # First, identify the sites, months, and metrics in the new data
        scc_new_data <- unique(
          scc_summary_data[  c("Service", "Site", "Month", "Metric")]
        )
        
        # Second, remove these sites, months, and metrics from the historical data, if they exist there.
        # This allows us to ensure no duplicate entries for the same site, metric, and time period
        ops_metrics_lab_tat <<- anti_join(ops_metrics_lab_tat,
                                          scc_new_data,
                                          by = c("Service" = "Service",
                                                 "Site" = "Site",
                                                 "Month" = "Month",
                                                 "Metric" = "Metric")
        )
        
        # Third, combine the updated historical data with the new data
        ops_metrics_lab_tat <<- full_join(ops_metrics_lab_tat,
                                          scc_summary_data)
        
        # Next, arrange the department summary by month, metric name, and site
        ops_metrics_lab_tat <<- ops_metrics_lab_tat %>%
          # mutate(Site = factor(Site,
          #                      levels = lab_sites_ordered,
          #                      ordered = TRUE)) %>%
          arrange(Month,
                  desc(Metric),
                  Site) #%>%
        # mutate(Site = as.character(Site))
        
        # Lastly, save the updated summary data
        write_xlsx(ops_metrics_lab_tat, ops_metrics_lab_tat_path)
        
        # Update metrics_final_df with latest SCC data using custom function
        metrics_final_df <<- lab_scc_tat_metrics_final_df(scc_summary_data)
        
        # Save updated metrics_final_df
        saveRDS(metrics_final_df, metrics_final_df_path)
        
        # Update "Reporting Month" drop down in each tab
        picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
        updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      }
      })
    
    # Sunquest data submission -------------------
    observeEvent(input$submit_lab_tat,{
      
      # Name Sunquest file
      sun_file <- input$lab_sun
      
      if (is.null(sun_file)) {
        return(NULL)
      }else{
        sun_file_path <- sun_file$datapath
        #file_path <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/EVS/MSHS Normal Clean vs Iso Clean TAT Sept 2021.xlsx"
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
      
      # Process data if the right file format was submitted
      if(flag == 1) {
        tryCatch({
          # Process Sunquest data
          sun_summary_data <- lab_sun_tat_dept_summary(sun_data)
          
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
        # Save prior version of Lab TAT Dept Summary data
        write_xlsx(ops_metrics_lab_tat,
                   paste0(hist_archive_path,
                          "Lab TAT Metrics Pre-Sun Updates ",
                          format(Sys.time(), "%Y%m%d_%H%M%S"),
                          ".xlsx"))
        
        # Append Lab TAT summary with new data
        # First, identify the sites, months, and metrics in the new data
        sun_new_data <- unique(
          sun_summary_data[  c("Service", "Site", "Month", "Metric")]
        )
        
        # Second, remove these sites, months, and metrics from the historical data, if they exist there.
        # This allows us to ensure no duplicate entries for the same site, metric, and time period
        ops_metrics_lab_tat <<- anti_join(ops_metrics_lab_tat,
                                          sun_new_data,
                                          by = c("Service" = "Service",
                                                 "Site" = "Site",
                                                 "Month" = "Month",
                                                 "Metric" = "Metric")
        )
        
        # Third, combine the updated historical data with the new data
        ops_metrics_lab_tat <<- full_join(ops_metrics_lab_tat,
                                          sun_summary_data)
        
        # Next, arrange the department summary by month, metric name, and site
        ops_metrics_lab_tat <<- ops_metrics_lab_tat %>%
          # mutate(Site = factor(Site,
          #                      levels = lab_sites_ordered,
          #                      ordered = TRUE)) %>%
          arrange(Month,
                  desc(Metric),
                  Site) #%>%
        # mutate(Site = as.character(Site))
        
        # Lastly, save the updated summary data
        write_xlsx(ops_metrics_lab_tat, ops_metrics_lab_tat_path)
        
        # Update metrics_final_df with latest Sunquest data using custom function
        metrics_final_df <<- lab_sun_tat_metrics_final_df(sun_summary_data)
        
        # Save updated metrics_final_df
        saveRDS(metrics_final_df, metrics_final_df_path)
        
        
        picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
        updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      }
    })
    
    # Lab Metrics - Proficiency Testing (Manual Entry) -----------------------
    # Create reactive data table for manual entry
    data_lab_prof_test <- reactive({
      
      data <- prof_test_manual_table
      
      # Arrange by sites in alphabetical order
      data <- data %>%
        arrange(Site)
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
        hot_col(1:2, readOnly = TRUE)
      
    })
    
    
    # Create observe event actions for manual data submission
    observeEvent(input$submit_lab_pt, {
      print(input$name_1)
      if(input$lab_pt_username == "") {
        showModal(modalDialog(
          title = "Error",
          "Please fill in the required fields",
          easyClose = TRUE,
          footer = NULL
        ))
      }

      # Convert rhandsontable to R object
      prof_test_manual_updates <<- hot_to_r(input$lab_prof_test)

      # Save prior version of Lab Proficiency Testing Dept Summary data
      write_xlsx(ops_metrics_lab_pt,
                 paste0(hist_archive_path,
                        "Lab Prof Testing Metrics Pre Updates ",
                        format(Sys.time(), "%Y%m%d_%H%M%S"),
                        ".xlsx"))

      # Reformat data from manual input table into department summary format
      prof_test_summary_data <-
        # lab_prof_test_dept_summary(prof_test_manual_table)
        lab_prof_test_dept_summary(prof_test_manual_updates)


      # Append Lab Proficiency Testing summary with new data
      # First, identify the sites, months, and metrics in the new data
      prof_test_new_data <- unique(
        prof_test_summary_data[, c("Service", "Site", "Month", "Metric")]
      )

      # Second, remove these sites, months, and metrics from the historical data, if they exist there
      # This allows us to ensure no duplicate entries for the same site, metric, and time period
      ops_metrics_lab_pt <<- anti_join(ops_metrics_lab_pt,
                                       prof_test_new_data,
                                       by = c("Service" = "Service",
                                              "Site" = "Site",
                                              "Month" = "Month",
                                              "Metric" = "Metric"))

      # Third, combine the updated historical data with the new data
      ops_metrics_lab_pt <<- full_join(ops_metrics_lab_pt,
                                       prof_test_summary_data)

      # Next, arrange the proficiency test summary data by month, metric name, and site
      ops_metrics_lab_pt <<- ops_metrics_lab_pt %>%
        mutate(Site = factor(Site,
                             levels = lab_sites_ordered,
                             ordered = TRUE)) %>%
        arrange(Month,
                desc(Metric),
                Site) %>%
        mutate(Site = as.character(Site))

      # Lastly, save the updated summary data
      write_xlsx(ops_metrics_lab_pt, ops_metrics_lab_prof_test_path)

      # Update metrics_final_df with latest Proficiency Testing data using custom function
      metrics_final_df <<- lab_prof_test_metrics_final_df(prof_test_summary_data)

      # Save updated metrics_final_df
      saveRDS(metrics_final_df, metrics_final_df_path)


      picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
      updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      
    })

      
      # Security Metrics - Incident Reports (Manual Entry) -----------------------
      # Create reactive data table for manual entry
      data_sec_inc_rpts <- reactive({
        
        data <- sec_inc_rpts_manual_table
        
        # Arrange by sites in alphabetical order
        data <- data %>%
          arrange(Site)
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
        }
        
        # Convert rhandsontable to R object
        sec_inc_rpts_manual_updates <<- hot_to_r(input$sec_inc_rpts)
        
        # Save prior version of Security Incident Reports Dept Summary data
        write_xlsx(security_incident_reports,
                   paste0(hist_archive_path,
                          "Security Incident Reports Pre Updates ",
                          format(Sys.time(), "%Y%m%d_%H%M%S"),
                          ".xlsx"))
        
        # Reformat data from manual input table into department summary format
        sec_inc_rpts_summary_data <-
          sec_inc_rpts_dept_summary(sec_inc_rpts_manual_updates)
        
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
        
      })
      
      # Security Metrics - Security Events (Manual Entry) -------------------
      # Create reactive data table for manual entry
      data_sec_events <- reactive({
        
        data <- sec_events_manual_table
        
        # Arrange by sites in alphabetical order
        data <- data %>%
          arrange(Site)
        
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
        }
        
        # Convert rhandsontable to R object
        sec_events_manual_updates <<- hot_to_r(input$sec_events)
        
        # Save prior version of Monthly Security Events Dept Summary data
        write_xlsx(security_events,
                   paste0(hist_archive_path,
                          "Security Events Monthly Pre Updates ",
                          format(Sys.time(), "%Y%m%d_%H%M%S"),
                          ".xlsx"))
        
        # Reformat data from manual input table into department summary format
        sec_events_summary_data <-
          sec_events_dept_summary(sec_events_manual_updates)
        
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

      })

    # 5. Overtime - Data Input -----------------3----------------------------------------------------------------
    observeEvent(input$submit_finance, {
      census_file <- input$finance_census
      flag <- 0
      
      if(is.null(census_file)){
        return(NULL)
      }else{
        census_filepath <- census_file$datapath
        #census_filepath <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/Food/Monthly Stats Summary for benchmarking 20211013.xlsx"
        #Read in census file
        census_data <- read_excel(census_filepath)
      }
      
      # Save prior version of COst and Revenue Summary data
      write_xlsx(cost_and_revenue_repo,
                 paste0(hist_archive_path,
                        "Cost and Revenue ",
                        format(Sys.time(), "%Y%m%d_%H%M%S"),
                        ".xlsx"))
      
      ## Process Census Data
      tryCatch({census_summary_data <- census_days_file_process(census_data)
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
        metrics_final_df <<- census_days_metrics_final_process(census_summary_data)
        
        # Save updated metrics_final_df
        saveRDS(metrics_final_df, metrics_final_df_path)
        
        # Update "Reporting Month" drop down in each tab
        picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
        updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      }
      
    })
    
      
    observeEvent(input$submit_finance, {
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
          overtime_summary_data[  c("Service", "Site", "Associated Dashboard Month", "Metric_name")]
        )
        
        # Second, remove these sites, months, and metrics from the historical data, if they exist there.
        # This allows us to ensure no duplicate entries for the same site, metric, and time period
        summary_repos_overtime <- anti_join(summary_repos_overtime,
                                            overtime_new_data,
                                         by = c("Service" = "Service",
                                                "Site" = "Site",
                                                "Associated Dashboard Month" = "Associated Dashboard Month",
                                                "Metric_name" = "Metric_name")
        )
        
        # Third, combine the updated historical data with the new data
        summary_repos_overtime <- full_join(summary_repos_overtime,
                                            overtime_summary_data)
        
        # Lastly, save the updated summary data
        write_xlsx(summary_repos_overtime, summary_repos_overtime_path)
        
        #metrics_final_df <<- overtime_metrics_final_df_process(overtime_summary_data)
        
        picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
        updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
      }
        
    })
    
    
      
      
      
      
    # })
    
    # Transport Metrics - Patient Data (Manual Entry) -----------------------
      observeEvent(input$submit_npt_tat,{
        
        npt_file <- input$non_patient_transport
        
        if (is.null(npt_file)) {
          return(NULL)
        }else{
          file_path <- npt_file$datapath
          #file_path <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/EVS/MSHS Normal Clean vs Iso Clean TAT Sept 2021.xlsx"
          npt_data <- read_excel(file_path)
        }
        
        data <- process_NPT_raw_data(npt_data)
        
        npt_data <- data[[1]]
        
        summary_repo_format <- data[[2]]
        
        metrics_final_df <<- transport__metrics_final_df_process(npt_data)
        
        saveRDS(metrics_final_df, metrics_final_df_path)
        
        transport_summary_repo <- read_excel(transport_table_path)
        transport_summary_repo$Month <- as.Date(transport_summary_repo$Month)
        
        updated_rows <- unique(summary_repo_format[c("Site","Date","Month","Transport Type")])
        updated_rows$Month <- as.Date(updated_rows$Month, "%m/%d/%Y")
        
        transport_summary_repo <- anti_join(transport_summary_repo, updated_rows)
        transport_summary_repo <- transport_summary_repo %>% filter(!is.na(Month))
        transport_summary_repo <- full_join(transport_summary_repo, summary_repo_format)
        transport_summary_repo <- as.data.frame(transport_summary_repo)
        write_xlsx(transport_summary_repo, transport_table_path)
        
        picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
        updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        
        
      })
      
      observeEvent(input$submit_pt_tat,{
        
        pt_file <- input$patient_transport
        
        if (is.null(pt_file)) {
          return(NULL)
        }else{
          file_path <- pt_file$datapath
          #file_path <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Balanced Scorecards Automation/Data_Dashboard/Input Data Raw/EVS/MSHS Normal Clean vs Iso Clean TAT Sept 2021.xlsx"
          #pt_data <- read_excel(file_path)

        }
        
        data <- process_PT_data(file_path)
        
        pt_data <- data[[1]]
        
        summary_repo_format <- data[[2]]
        
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
        
        picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
        updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
        
        
      })
      
      # Biomed KPI Outout Table -------
      
      data_bimoed_kpi <- reactive({
        data  <- kpibme_reports_ui
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
          hot_col(1:3, readOnly = T)
      })
      
      # Biomed Disruptions and Issues Outout Table -------
      
      data_bimoed_di <- reactive({
        data  <- disruptions_issues_reports_ui
      })
      
      
      output$bimoed_di <- renderRHandsontable({
        #data <- data
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
          hot_col(1:3, readOnly = T)
      })
      
      # Create observer event actions for manual data submission KPIs Biomed ----- 
      observeEvent(input$submit_biomedkpis, {
        if(input$sec_inc_rpts_username == "") {
          showModal(modalDialog(
            title = "Error",
            "Please fill in the required fields.",
            easyClose = TRUE,
            footer = NULL
          ))
        }
        
        # Convert rhandsontable to R object
        bme_kpi_manual_updates <<- hot_to_r(input$biomed_kpi)
        
        # Save prior version of Security Incident Reports Dept Summary data
        write_xlsx(kpibme_reports,
                   paste0(hist_archive_path,
                          "KPIs Biomed and Clinical Engineering ",
                          format(Sys.time(), "%Y%m%d_%H%M%S"),
                          ".xlsx"))
        
        # Reformat data from manual input table into summary repo format
        bme_kpi_summary_data <-
          process_manual_entry_to_summary_repo_format_biomed(bme_kpi_manual_updates,"KPI")
        
        # Append Security Incident Reports summary with new data
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
        kpibme_reports <<- full_join(kpibme_reports,
                                                kpi_bme)
        
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
        
      })
      
      # Create observer event actions for manual data submission D&I Biomed ----- 
      observeEvent(input$submit_biomeddi, {
        if(input$sec_inc_rpts_username == "") {
          showModal(modalDialog(
            title = "Error",
            "Please fill in the required fields.",
            easyClose = TRUE,
            footer = NULL
          ))
        }
        
        # Convert rhandsontable to R object
        bme_di_manual_updates <<- hot_to_r(input$bimoed_di)
        
        # Save prior version of Security Incident Reports Dept Summary data
        write_xlsx(disruptions_issues_reports,
                   paste0(hist_archive_path,
                          "DI Biomed and Clinical Engineering ",
                          format(Sys.time(), "%Y%m%d_%H%M%S"),
                          ".xlsx"))
        
        # Reformat data from manual input table into summary repo format
        bme_di_summary_data <-
          process_manual_entry_to_summary_repo_format_biomed(bme_di_manual_updates,"DI")
        
        # Append Security Incident Reports summary with new data
        # First, identify the sites, months, and metrics in the new data
        bme_di_new_data <- unique(
          bme_di_summary_data[, c("Service", "Site", "Month", "Metric")]
        )
        
        # Second, remove these sites, months, and metrics from the historical data,
        # if they exist there. This allows us to ensure no duplicate entries for
        # the same site, metric, and time period.
        di_bme <<- anti_join(disruptions_issues_reports,
                             bme_di_new_data,
                              by = c("Service" = "Service",
                                     "Site" = "Site",
                                     "Month" = "Month",
                                     "Metric" = "Metric"))
        
        # Third, combine the updated historical data with the new data
        disruptions_issues_reports <<- full_join(disruptions_issues_reports,
                                     di_bme)
        
        # Next, arrange the incident reports summary data by month, metric, and site
        disruptions_issues_reports <<- disruptions_issues_reports %>%
          arrange(Month,
                  Site)
        
        # Lastly, save the updated summary data
        write_xlsx(disruptions_issues_reports, bmedi_table_path)
        
        # Update metrics_final_df with latest data using custom function
        metrics_final_df <<- biomed__metrics_final_df_process(disruptions_issues_reports)
        
        # Save updates metrics_final_df
        saveRDS(metrics_final_df, metrics_final_df_path)
        
      })
      
      
      
      

  
} # Close Server



