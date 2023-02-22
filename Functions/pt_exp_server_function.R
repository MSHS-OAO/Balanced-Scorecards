pt_exp_server_function <- function(button_name, file, service, updated_user, type) {
  flag <- 0
  shinyjs::disable(button_name)
  
  
  if(is.null(file)) {
    return(NULL)
  } else{
    file_path <- file$datapath
    
    # TryCatch statement to ensure file type if correct
    tryCatch({
      
      submitted_data <- read_csv(file_path,
                              show_col_types = FALSE)
      
      flag <- 1
    },
    error = function(err){
      showModal(modalDialog(
        title = "Error",
        paste0("There seems to be an issue with this Patient Experience ", service, " file."),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
  }
  
  # Process data if the right file format was submitted
  if(flag == 1) {
    
    tryCatch({
      
      # Process data
      pt_exp_summary_data <- pt_exp_dept_summary(submitted_data)

      flag <- 2
      
  
    },
    error = function(err){
      showModal(modalDialog(
        title = "Error",
        paste0("There seems to be an issue with this Patient Experience ", service, " file."),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  }
  
  
  if(flag == 2) {
    
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
    
     # Second, remove these sites, months, and metrics from the historical data, if they exist there.
    # This allows us to ensure no duplicate entries for the same site, metric, and time period
    pt_exp_data_updated <- anti_join(pt_exp_summary_data,
                                     pt_exp_data,
                              by = c("Service" = "Service",
                                     "Site" = "Site",
                                     "Question_Clean" = "Question_Clean",
                                     "ReportingType" = "ReportingType",
                                     "Reporting_Date_Start" = "Reporting_Date_Start",
                                     "Reporting_Date_End" = "Reporting_Date_End")
    )
    
    # # Third, combine the updated historical data with the new data
    # pt_exp_data <- full_join(pt_exp_data,
    #                           pt_exp_summary_data)
    
    # Next, arrange the department summary by month, metric name, and site
    pt_exp_data_updated <- pt_exp_data_updated %>%
      arrange(Service,
              Site,
              ReportingType,
              Reporting_Date_End)
    names(pt_exp_data_updated) <- toupper(names(pt_exp_data_updated))
    pt_exp_data_updated$UPDATED_USER <- updated_user
    
    
    
    # # Lastly, save the updated summary data
    # write_xlsx(pt_exp_data, pt_exp_table_path)
    write_temporary_table_to_database_and_merge_pt_exp(pt_exp_data_updated, button_name)
    
    pt_exp_data_updated <- anti_join(pt_exp_summary_data,
                                     pt_exp_data,
                                     by = c("Service" = "Service",
                                            "Site" = "Site",
                                            "Question_Clean" = "Question_Clean",
                                            "ReportingType" = "ReportingType",
                                            "Reporting_Date_Start" = "Reporting_Date_Start",
                                            "Reporting_Date_End" = "Reporting_Date_End")
    )
    
    
    if(type == "Monthly"){
      pt_exp_summaty_repo_format <- pt_exp_summary_repo(pt_exp_data_updated)
      pt_exp_summaty_repo_format$UPDATED_USER <- updated_user
      names(pt_exp_summaty_repo_format) <- toupper(names(pt_exp_summaty_repo_format))
      write_temporary_table_to_database_and_merge(pt_exp_summaty_repo_format, "pt_exp", button_name)
      
    }
    # conn <- dbConnect(odbc(), dsn)
    # pt_exp_data <<- tbl(conn, "BSC_PATIENT_EXPERIENCE_REPO") %>% 
    #   rename(Service = SERVICE,
    #          Site = SITE,
    #          Question_Clean = QUESTION_CLEAN,
    #          ReportingType = REPORTINGTYPE,
    #          Reporting_Date_Start = REPORTING_DATE_START,
    #          Reporting_Date_End = REPORTING_DATE_END,
    #          Site_Mean = SITE_MEAN,
    #          Site_N = SITE_N,
    #          All_PG_Database_Mean = ALL_PG_DATABASE_MEAN,
    #          All_PG_Database_N = ALL_PG_DATABASE_N,
    #          All_PG_Database_Rank = ALL_PG_DATABASE_RANK) %>%
    #   collect()
    # pt_exp_data <<- pt_exp_data %>%
    #   mutate(Reporting_Date_Start = as.Date(Reporting_Date_Start),
    #          Reporting_Date_End = as.Date(Reporting_Date_End)) %>%
    #   filter(!(Site %in% "All"))
    # dbDisconnect(conn)
    
  }
  shinyjs::enable(button_name)
  
  
}
