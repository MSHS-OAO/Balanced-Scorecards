library(odbc)
library(DBI)
library(dplyr)
library(dbplyr)
library(readxl)

# get the credentials ----
source("credentials.R")

# establish connection ----
con <- dbConnect(odbc(), 
                 Driver = "Oracle 21_5 ODBC driver", 
                 Trusted_Connection = "True",
                 uid = .username,
                 pwd = .password)

# function to read data----
# input: connection string, and table_name as string
# output: dataframe from database
read_table_from_db <- function(connection_string,
                               table_name){
  # query_glue <- glue("Select *
  #                  FROM {table_name}")
  
  tbl_db <- tbl(connection_string, table_name)
  
  dbplyr_data <- tbl_db %>%
    collect()
  
}

# connection_str <- global var

merge_staging_summary_repo <- function(connection_string, staging_table_name){
  
  
  query = glue('MERGE INTO SUMMARY_REPO SR
                  USING "{staging_table_name}" SOURCE_TABLE
                  ON (  SR."SITE" = SOURCE_TABLE."SITE" AND
                        SR."MONTH" = SOURCE_TABLE."MONTH" AND
                        SR."SERVICE" = SOURCE_TABLE."SERVICE" AND
                        SR."METRIC_NAME_SUBMITTED" = SOURCE_TABLE."METRIC_NAME_SUBMITTED")
                  WHEN MATCHED THEN 
                  UPDATE  SET SR."VALUE" = SOURCE_TABLE."VALUE",
                              SR."UPDATE_TIME" = SOURCE_TABLE."UPDATE_TIME",
                              SR."PREMIER_REPORTING_PERIOD" = SOURCE_TABLE."PREMIER_REPORTING_PERIOD"
                  WHEN NOT MATCHED THEN
                  INSERT( SR."SITE",
                          SR."MONTH",
                          SR."SERVICE",
                          SR."METRIC_NAME_SUBMITTED",
                          SR."VALUE", 
                          SR."UPDATE_TIME", 
                          SR."PREMIER_REPORTING_PERIOD")  
                  VALUES( SOURCE_TABLE."SITE",
                          SOURCE_TABLE."MONTH",
                          SOURCE_TABLE."SERVICE",
                          SOURCE_TABLE."METRIC_NAME_SUBMITTED",
                          SOURCE_TABLE."VALUE", 
                          SOURCE_TABLE."UPDATE_TIME",
                          SOURCE_TABLE."PREMIER_REPORTING_PERIOD");')
  

  dbSendStatement(connection_string,query)
  
}


# function to write the  summary repo table to database: ----
# Inputs : connection_string : connection variable returned by  dbConnect() method
#          data (dplyr data frame) : data to be written to Oracle cloud. data should have Service
#                                                                                         Site ,
#                                                                                         Month ,
#                                                                                         Metric_Name_Submitted ,
#                                                                                         Value ,
#                                                                                         `Premier Reporting Period`, and
#                                                                                         Update_time fields
#         table_name (String) : Example : "LAB_TAT" in all letters UPPER_CASE
# Output  Boolean: TRUE

write_temporary_table_to_database_and_merge <- function(connection_string,processed_input_data,table_name){
  
  # Constants
  DATA_TYPES <- c(SERVICE = "Varchar2(10 CHAR)",
                  SITE = "Varchar2(10 CHAR)",
                  MONTH = "DATE",
                  METRIC_NAME_SUBMITTED = "Varchar2(20 CHAR)",
                  VALUE = "numeric(10,10)",
                  PREMIER_REPORTING_PERIOD = "Varchar2(20 CHAR)",
                  UPDATE_TIME = "Timestamp")
  
  # Add UPDATE_TIME 
  processed_input_data <- processed_scc_may_data
  processed_input_data <- processed_input_data %>%
    mutate(UPDATE_TIME = as.character(Sys.time()),
           PREMIER_REPORTING_PERIOD = format(Month,"%b %Y"),
           Month = format(Month,"%Y-%m-%d")) %>%
    rename(METRIC_NAME_SUBMITTED = Metric,
           VALUE=Number)
  
  
  
  # Rename the column names to database format
  fields <- str_replace_all( toupper(names(processed_input_data)),
                             " ",
                             "_")
  
  names(processed_input_data) <- fields
  
  table_name <- "TEST"
  table_name = paste0("STAGING.",table_name)
  
  dbWriteTable(con,
               table_name,
               processed_input_data,
               overwrite = TRUE,
               field.types = DATA_TYPES)
  
  #merge_staging_summary_repo(connection_string, table_name)
  
}





# read raw scc data & process it ----
scc_may_data <- read.xlsx(paste0(home_path,"Input Data Raw/Lab & Blood Bank/SCC/SCC HGB Report May 2022.xlsx"))
processed_scc_may_data <- lab_scc_tat_dept_summary(scc_may_data)
# read raw sunsquest data & process it ----
sunquest_may_data <- read_excel(paste0(home_path,"Input Data Raw/Lab & Blood Bank/SUNQUEST/SQ Monthly TROP-HGB May 2022.xls"))
processed_sunquest_may_data <- lab_sun_tat_dept_summary(sunquest_may_data)




data<- write_temporary_table_to_database(con,processed_sunquest_may_data,"SUNQUEST")
data<- write_temporary_table_to_database(con,processed_scc_may_data,"SCC")




data <- read_table_from_db(con,"STAGING.LAB_TAT_SUNQUEST")




