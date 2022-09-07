library(odbc)
library(DBI)
library(dbplyr)
library(pool)

# get the credentials ----
#source("credentials.R")

# establish connection ----
# con <- dbConnect(odbc(), 
#                  Driver = "Oracle 21_5 ODBC driver", 
#                  Trusted_Connection = "True",
#                  uid = .username,
#                  pwd = .password)
# con <- dbConnect(odbc::odbc(), "OAO Cloud DB", timeout = 30)

options(odbc.batch_rows = 1000000)

poolcon <- dbPool(drv = odbc::odbc(), 
                  dsn = "OAO Cloud DB")

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
  
  values <- glue("INTO \"{table_name}\"(SERVICE,SITE,REPORTING_MONTH,METRIC_NAME_SUBMITTED,VALUE,UPDATED_TIME,PREMIER_REPORTING_PERIOD,UPDATED_USER) 
                 VALUES('{service}','{site}',TO_DATE('{month}','YYYY-MM-DD'),'{metric_name_submitted}',{value},TO_TIMESTAMP('{update_time}','YYYY-MM-DD HH24:MI:SS'),'{premier_reporting_period}','{updated_user}')")
  
  return(values)
}


# function to write the summary repo table to database: ----

write_temporary_table_to_database_and_merge <- function(processed_input_data,table_name){
  
  # Constants
  DATA_TYPES <- c(SERVICE = "Varchar2(100 CHAR)",
                  SITE = "Varchar2(10 CHAR)",
                  REPORTING_MONTH = "DATE",
                  METRIC_NAME_SUBMITTED = "Varchar2(200 CHAR)",
                  VALUE = "numeric(30,10)",
                  PREMIER_REPORTING_PERIOD = "Varchar2(20 CHAR)",
                  UPDATED_TIME = "Timestamp",
                  UPDATED_USER = "Varchar2(100 CHAR)" )
  
  TABLE_NAME <- paste0("STAGING.",table_name)
  
  
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
  
  # Convert the each record/row of tibble to INTO clause of insert statment
  inserts <- lapply(
    lapply(
      lapply(split(processed_input_data , 
                   1:nrow(processed_input_data)),
             as.list), 
      as.character),
    FUN = get_values ,TABLE_NAME)
  
  values <- glue_collapse(inserts,sep = "\n\n")
  
  # Combine into statements from get_values() function and combine with
  # insert statements
  all_data <- glue('INSERT ALL
                      {values}
                    SELECT 1 from DUAL;')
  
  # glue() query to merge data from temporary table to summary_repo table
  query = glue('MERGE INTO SUMMARY_REPO SR
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
  
  # glue query for dropping the table
  drop_query <- glue('DROP TABLE "{TABLE_NAME}";')
  
  # Combining all SQL transactions into one atomic unit
  # poolWithTransaction(poolcon, function(conn) {
  #     dbCreateTable(conn,
  #                   TABLE_NAME,
  #                   processed_input_data,
  #                   field.types  = DATA_TYPES)
  # 
  #     dbExecute(conn,all_data)
  #     dbExecute(conn,query)
  #     dbExecute(conn,drop_query)
  # })
  
  
  conn <- poolCheckout(poolcon)
  
  tryCatch({
        dbBegin(conn)
        dbCreateTable(conn,
                      TABLE_NAME,
                      processed_input_data,
                      field.types  = DATA_TYPES)

        dbExecute(conn,all_data)
        dbExecute(conn,query)
        dbExecute(conn,drop_query)
        #dbExecute(conn, "SELECT * FROM TBALE_NOT_EXISt")
        dbCommit(conn)
        poolReturn(conn)
    
  },
  error = function(err){
    dbRollback(conn)
    poolReturn(conn)
    showModal(modalDialog(
      title = "Error",
      paste0("There seems to be an issue with writing to the Database"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
}




