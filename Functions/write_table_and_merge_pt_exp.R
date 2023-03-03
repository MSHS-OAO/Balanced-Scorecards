# function to convert the record of data frame to into satement
get_values_pt_exp <- function(x,table_name){
  
  service <- x[1]
  site <- x[2]
  question_clean <- x[3]
  reporting_type <- x[4]
  reporting_date_start <- x[5]
  reporting_date_end <- x[6]
  site_mean <- x[7]
  site_n <- x[8]
  all_pg_database_mean <- x[9]
  all_pg_database_n <- x[10]
  all_pg_database_rank <- x[11]
  updated_user <- x[12]
  updated_time <- x[13]
  
  values <- glue("INTO \"{table_name}\" (SERVICE,SITE,QUESTION_CLEAN,REPORTINGTYPE,REPORTING_DATE_START,REPORTING_DATE_END,SITE_MEAN,SITE_N, 
                  ALL_PG_DATABASE_MEAN, ALL_PG_DATABASE_N, ALL_PG_DATABASE_RANK, UPDATED_TIME, UPDATED_USER) 
                 VALUES('{service}','{site}', '{question_clean}', '{reporting_type}', TO_DATE('{reporting_date_start}','YYYY-MM-DD'),
                 TO_DATE('{reporting_date_end}','YYYY-MM-DD'),'{site_mean}','{site_n}', '{all_pg_database_mean}', '{all_pg_database_n}', 
                 '{all_pg_database_rank}', TO_TIMESTAMP('{updated_time}','YYYY-MM-DD HH24:MI:SS'),'{updated_user}')")
  
  return(values)
}


# function to write the summary repo table to database: ----

write_temporary_table_to_database_and_merge_pt_exp <- function(processed_input_data, button_name){
  if(nrow(processed_input_data) == 0) {
    if(isRunning()) {
      showModal(modalDialog(
        title = "Alert",
        paste0("There was no new data in the file"),
        easyClose = TRUE,
        footer = NULL
      ))
    } else{
      print("no new data")
    }
  } else{
    
  TABLE_NAME <- "BSC_PATIENT_EXPERIENCE_REPO"
    
    # Add UPDATE_TIME and check for all the fields are characters
    processed_input_data <- processed_input_data %>%
      mutate(REPORTING_DATE_START = as.character(REPORTING_DATE_START),
             REPORTING_DATE_END = as.character(REPORTING_DATE_END),
             UPDATED_TIME = format(Sys.time(), "%Y-%m-%d %H:%M"))%>%
      select(SERVICE,
             SITE,
             QUESTION_CLEAN,
             REPORTINGTYPE,
             REPORTING_DATE_START,
             REPORTING_DATE_END,
             SITE_MEAN,
             SITE_N, 
             ALL_PG_DATABASE_MEAN, 
             ALL_PG_DATABASE_N, 
             ALL_PG_DATABASE_RANK, 
             UPDATED_USER,
             UPDATED_TIME
             )
    ##substitue single ' for '' so the query can escape
    # processed_input_data$METRIC_NAME_SUBMITTED <- gsub("\'", "''", processed_input_data$METRIC_NAME_SUBMITTED)
    # processed_input_data$METRIC_NAME_SUBMITTED <- gsub("&", "' || chr(38) || '", processed_input_data$METRIC_NAME_SUBMITTED)
    # Convert the each record/row of tibble to INTO clause of insert statment
    inserts <- lapply(
      lapply(
        lapply(split(processed_input_data , 
                     1:nrow(processed_input_data)),
               as.list), 
        as.character),
      FUN = get_values_pt_exp ,TABLE_NAME)
    
    values <- glue_collapse(inserts,sep = "\n\n")
    
    # Combine into statements from get_values() function and combine with
    # insert statements
    all_data <- glue('INSERT ALL
                        {values}
                      SELECT 1 from DUAL;')
    
    
    
    
    # conn <- dbConnect(drv = odbc::odbc(),  ## Create connection for updating picker choices
    #                   dsn = dsn)
    print("before conn")
    conn <- dbConnect(odbc(), dsn)
    print("after conn")
    dbBegin(conn)
    # ## Execute staments and if there is an error  with one of them rollback changes
    tryCatch({
      dbExecute(conn,all_data)
      dbCommit(conn)
      dbDisconnect(conn)
      if(isRunning()) {
        showModal(modalDialog(
          title = "Success",
          paste0("The Patient Experience data has been submitted successfully."),
          easyClose = TRUE,
          footer = NULL
        ))
      } else{
        print(paste0("The Patient Expereince data has been submitted successfully."))
      }
      
    },
    error = function(err){
      #print(err)
      dbRollback(conn)
      dbDisconnect(conn)
      print("error")
      if(isRunning()) {
        shinyjs::enable(button_name)
        showModal(modalDialog(
          title = "Error",
          paste0("There was an issue submitting the data."),
          easyClose = TRUE,
          footer = NULL
        ))
      } else{
        print(paste0("There was an issue submitting the Patient Experience data."))
      }
    })
  }
  
}


pt_exp_summary_repo <- function(pt_exp_summary) {
  
  # Filter out YTD data
  pt_exp_metrics_final_df <- pt_exp_summary %>%
    filter(ReportingType %in% "Monthly" &
             !is.na(Site_Mean))
  
  # Update Press Ganey mapping file used to determine which metrics to include
  pt_exp_mapping_simple <- pt_exp_mapping %>%
    select(-Questions)
  
  # Crosswalk Press Ganey data with mapping file to determine which metrics to include
  pt_exp_metrics_final_df <- left_join(pt_exp_metrics_final_df,
                                       pt_exp_mapping_simple,
                                       by = c("Service" = "Service",
                                              "Question_Clean" = "Question_Clean"))
  
  pt_exp_metrics_final_df <- pt_exp_metrics_final_df %>%
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
    # Remove metrics that are not included in reporting from metrics_final_df
    # (ie, sample size not reported for all metrics, not all service lines report hospital ranking)
    filter(!is.na(Metric_Name_Submitted)) %>%
    mutate(Premier_Reporting_Period = format(Reporting_Date_Start, "%b %Y"),
           Reporting_Month = format(Reporting_Date_Start, "%Y-%m-%d"),
           Question_Clean = NULL,
           ReportingType = NULL,
           Reporting_Date_Start = NULL,
           Reporting_Date_End = NULL,
           Incl_N = NULL,
           Incl_AllHosp_Rank = NULL,
           Metric = NULL) %>%
          select(-Raw_Pt_Exp_Service)  
  
}
