library(odbc)
library(DBI)
library(dbplyr)
library(doParallel)
library(parallel)
options(odbc.batch_rows = 1000000)


# function to convert the record of data frame to into satement
get_values <- function(x,table_name){
  
  service <- x[1]
  site <- x[2]
  month <- x[3]
  premier_reporting_period <- x[4]
  metric_name_submitted <- x[5]
  value <- x[6]
  update_time <- x[7]
  updated_user <- x[8]
  
  values <- glue("INTO \"{table_name}\" (SERVICE,SITE,REPORTING_MONTH,METRIC_NAME_SUBMITTED,VALUE,UPDATED_TIME,PREMIER_REPORTING_PERIOD,UPDATED_USER) 
                 VALUES('{service}','{site}',TO_DATE('{month}','YYYY-MM-DD'),'{metric_name_submitted}',{value},TO_TIMESTAMP('{update_time}','YYYY-MM-DD HH24:MI:SS'),'{premier_reporting_period}','{updated_user}')")
  
  return(values)
}


# function to write the summary repo table to database: ----

write_temporary_table_to_database_and_merge <- function(processed_input_data,table_name, button_name){
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
  
    # Constants
    DATA_TYPES <- c(SERVICE = "Varchar2(100 CHAR)",
                    SITE = "Varchar2(10 CHAR)",
                    REPORTING_MONTH = "DATE",
                    METRIC_NAME_SUBMITTED = "Varchar2(200 CHAR)",
                    VALUE = "numeric(30,10)",
                    PREMIER_REPORTING_PERIOD = "Varchar2(20 CHAR)",
                    UPDATED_TIME = "Timestamp",
                    UPDATED_USER = "Varchar2(100 CHAR)" )
    
    TABLE_NAME <- paste0("STAGING.MERGE_TABLE")
    
    
    productivity_check <- unique(processed_input_data$METRIC_NAME_SUBMITTED)
    
    DEST_TABLE <- if("Worked Hours Productivity Index" %in% productivity_check){
      paste0("BSC_SYSTEM_WIDE_PRODUCTIVITY_FINANCE")
    }else{
      paste0("SUMMARY_REPO")
    }
    
    
    # Add UPDATE_TIME and check for all the fields are characters
    processed_input_data <- processed_input_data %>%
      mutate(REPORTING_MONTH = as.character(REPORTING_MONTH),
             # UPDATED_TIME = as.character(Sys.time()),
             UPDATED_TIME = format(Sys.time(), "%Y-%m-%d %H:%M"),
             VALUE = as.character(VALUE),
             VALUE = coalesce(VALUE,"NULL"))%>%
      select(SERVICE, 
             SITE, 
             REPORTING_MONTH,
             PREMIER_REPORTING_PERIOD, 
             METRIC_NAME_SUBMITTED,
             VALUE,UPDATED_TIME,
             UPDATED_USER)
    
    processed_input_data <- processed_input_data %>% filter(SITE %in% c("MSBI", "MSQ", "MSH", "MSW", "MSB", "NYEE", "MSM", "SYSTEM"))
    ##substitue single ' for '' so the query can escape
    processed_input_data$METRIC_NAME_SUBMITTED <- gsub("\'", "''", processed_input_data$METRIC_NAME_SUBMITTED)
    processed_input_data$METRIC_NAME_SUBMITTED <- gsub("&", "' || chr(38) || '", processed_input_data$METRIC_NAME_SUBMITTED)
    # Convert the each record/row of tibble to INTO clause of insert statment
    
    data_records <- split(processed_input_data ,1:nrow(processed_input_data))
    
    
    registerDoParallel()
    inserts <- foreach(record = data_records) %dopar% {
      tmp <- as.list(record)
      tmp <- as.character(tmp)
      tmp <- get_values(tmp,TABLE_NAME)
    }
    registerDoSEQ()
    
    chunk_length <- 500
    split_queries <- split(inserts, ceiling(seq_along(inserts)/chunk_length))
    
    
    split_queries_sql_statements <- list()
    for (i in 1:length(split_queries)) {
      row <- glue_collapse(split_queries[[i]], sep = "\n\n")
      # row <- gsub('NA', "", row)
      # row <- gsub("&", " ' || chr(38) || ' ", row)
      sql <- glue('INSERT ALL {row} SELECT 1 FROM DUAL;')
      split_queries_sql_statements <- append(split_queries_sql_statements, sql)
    }
    
    
    # inserts <- lapply(
    #   lapply(
    #     lapply(split(processed_input_data , 
    #                  1:nrow(processed_input_data)),
    #            as.list), 
    #     as.character),
    #   FUN = get_values ,TABLE_NAME)
    
    # values <- glue_collapse(inserts,sep = "\n\n")
    # 
    # Combine into statements from get_values() function and combine with
    # insert statements
    # all_data <- glue('INSERT ALL
    #                     {values}
    #                   SELECT 1 from DUAL;')
    
    
    # glue statement for dropping table
    truncate_query <- glue('TRUNCATE TABLE "{TABLE_NAME}";')
    
    # Clear the staging data
    tryCatch({
      ch = dbConnect(odbc(), dsn)
      dbBegin(ch)
      dbExecute(ch,truncate_query)
      dbCommit(ch)
      dbDisconnect(ch)
    },
    error = function(err){
      print("error")
      dbRollback(ch)
      dbDisconnect(ch)
      
    })
    
    
    registerDoParallel()
    system.time(
      outputPar <- foreach(i = 1:length(split_queries_sql_statements), .packages = c("DBI", "odbc"))%dopar%{
        #Connecting to database through DBI
        ch = dbConnect(odbc(), dsn)
        #Test connection
        tryCatch({
          dbBegin(ch)
          dbExecute(ch, split_queries_sql_statements[[i]])
          dbCommit(ch)
        },
        error = function(err){
          print("error")
          dbRollback(ch)
          dbDisconnect(ch)
          
        })
      }
    )
    registerDoSEQ()
    
    
    # glue() query to merge data from temporary table to summary_repo table
    query = glue('MERGE INTO "{DEST_TABLE}" SR
                    USING "{TABLE_NAME}" SOURCE_TABLE
                    ON (  SR."SITE" = SOURCE_TABLE."SITE" AND
                          SR."REPORTING_MONTH" = SOURCE_TABLE."REPORTING_MONTH" AND
                          SR."SERVICE" = SOURCE_TABLE."SERVICE" AND
                          SR."METRIC_NAME_SUBMITTED" = SOURCE_TABLE."METRIC_NAME_SUBMITTED")
                    WHEN MATCHED THEN 
                    UPDATE  SET SR."VALUE" = SOURCE_TABLE."VALUE",
                                SR."UPDATED_TIME" = SOURCE_TABLE."UPDATED_TIME",
                                SR."UPDATED_USER" = SOURCE_TABLE."UPDATED_USER",
                                SR."PREMIER_REPORTING_PERIOD" = SOURCE_TABLE."PREMIER_REPORTING_PERIOD"
                    WHEN NOT MATCHED THEN
                    INSERT( SR."SITE",
                            SR."REPORTING_MONTH",
                            SR."SERVICE",
                            SR."METRIC_NAME_SUBMITTED",
                            SR."VALUE", 
                            SR."UPDATED_TIME", 
                            SR."UPDATED_USER",
                            SR."PREMIER_REPORTING_PERIOD")  
                    VALUES( SOURCE_TABLE."SITE",
                            SOURCE_TABLE."REPORTING_MONTH",
                            SOURCE_TABLE."SERVICE",
                            SOURCE_TABLE."METRIC_NAME_SUBMITTED",
                            SOURCE_TABLE."VALUE", 
                            SOURCE_TABLE."UPDATED_TIME",
                            SOURCE_TABLE."UPDATED_USER",
                            SOURCE_TABLE."PREMIER_REPORTING_PERIOD");')
    

    print("before conn")
    # conn <- dbConnect(drv = odbc::odbc(),  ## Create connection for updating picker choices
    #                   dsn = dsn)
    
    conn <- dbConnect(odbc(), dsn_oracle)
    
    print("after conn")
    dbBegin(conn)
    # ## Execute staments and if there is an error  with one of them rollback changes
    tryCatch({
          print("1")
          dbExecute(conn,query)
          print("2")
          dbExecute(conn,truncate_query)
          print("3")
          dbCommit(conn)
          dbDisconnect(conn)
          if(isRunning()) {
            showModal(modalDialog(
              title = "Success",
              paste0("The data has been submitted successfully."),
              easyClose = TRUE,
              footer = NULL
            ))
          } else{
            print(paste0("The data has been submitted successfully."))
          }
  
    },
    error = function(err){
      #print(err)
      dbRollback(conn)
      dbDisconnect(conn)
      dbExecute(conn,truncate_query)
      print("error")
      shinyjs::enable(button_name)
      if(isRunning()) {
        showModal(modalDialog(
          title = "Error",
          paste0("There was an issue submitting the data."),
          easyClose = TRUE,
          footer = NULL
        ))
      } else{
        print(paste0("There was an issue submitting the data."))
      }
    })
  }
  
}


# Test ---

# write_temporary_table_to_database_and_merge(processed_new_data, "SOM",button_name = "NA")
