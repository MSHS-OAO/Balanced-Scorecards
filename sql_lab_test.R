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

# merge_staging_summary_repo <- function(staging_table_name){
#   
#   
#   
# }
# 

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


write_temporary_table_to_database_and_merge <- function(processed_input_data,table_name){
  
  
  dbBegin(con)
  
  # Constants
  DATA_TYPES <- c(SERVICE = "Varchar2(10 CHAR)",
                  SITE = "Varchar2(10 CHAR)",
                  REPORTING_MONTH = "DATE",
                  METRIC_NAME_SUBMITTED = "Varchar2(20 CHAR)",
                  VALUE = "numeric(10,10)",
                  PREMIER_REPORTING_PERIOD = "Varchar2(20 CHAR)",
                  UPDATED_TIME = "Timestamp",
                  UPDATED_USER = "Varchar2(100 CHAR)" )
  
  TABLE_NAME <- paste0("STAGING.",table_name)
  
  
  # Add UPDATE_TIME 
  processed_input_data <- processed_input_data %>%
    mutate(UPDATED_TIME = as.character(Sys.time()),
           PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
           REPORTING_MONTH = format(REPORTING_MONTH,"%Y-%m-%d"),
           UPDATED_USER = NA_character_) #%>%
    # rename(METRIC_NAME_SUBMITTED = Metric,
    #        VALUE=Number)
  
  # Rename the column names to database format
  fields <- str_replace_all( toupper(names(processed_input_data)),
                             " ",
                             "_")
  
  names(processed_input_data) <- fields
  
  
  processed_input_data <<- processed_input_data

  inserts <- lapply(
    lapply(
      lapply(split(processed_input_data , 
                   1:nrow(processed_input_data)),
             as.list), 
      as.character),
    FUN = get_values ,TABLE_NAME)
  
  values <- glue_collapse(inserts,sep = "\n\n")
  
  all_data <- glue('INSERT ALL
                      {values}
                    SELECT 1 from DUAL;')
  
  print(all_data)

  
  dbCreateTable(con,
                TABLE_NAME,
                processed_input_data,
                field.types  = DATA_TYPES)
  
  dbExecute(con,all_data)
  
  #dbAppendTable(con,TABLE_NAME,processed_input_data)
  
  
  query = glue('MERGE INTO SUMMARY_REPO SR
                  USING "{TABLE_NAME}" SOURCE_TABLE
                  ON (  SR."SITE" = SOURCE_TABLE."SITE" AND
                        SR."MONTH" = SOURCE_TABLE."MONTH" AND
                        SR."SERVICE" = SOURCE_TABLE."SERVICE" AND
                        SR."METRIC_NAME_SUBMITTED" = SOURCE_TABLE."METRIC_NAME_SUBMITTED")
                  WHEN MATCHED THEN 
                  UPDATE  SET SR."VALUE" = SOURCE_TABLE."VALUE",
                              SR."UPDATED_TIME" = SOURCE_TABLE."UPDATED_TIME",
                              SR."PREMIER_REPORTING_PERIOD" = SOURCE_TABLE."PREMIER_REPORTING_PERIOD",
                              SR."UPDATED_USER" = SOURCE_TABLE."UPDATED_USER"
                  WHEN NOT MATCHED THEN
                  INSERT( SR."SITE",
                          SR."MONTH",
                          SR."SERVICE",
                          SR."METRIC_NAME_SUBMITTED",
                          SR."VALUE", 
                          SR."UPDATED_TIME", 
                          SR."PREMIER_REPORTING_PERIOD",
                          SR."UPDATED_USER")  
                  VALUES( SOURCE_TABLE."SITE",
                          SOURCE_TABLE."MONTH",
                          SOURCE_TABLE."SERVICE",
                          SOURCE_TABLE."METRIC_NAME_SUBMITTED",
                          SOURCE_TABLE."VALUE", 
                          SOURCE_TABLE."UPDATED_TIME",
                          SOURCE_TABLE."PREMIER_REPORTING_PERIOD",
                          SOURCE_TABLE."UPDATED_USER");')
  
  #dbExecute(con,query)
  #dbExecute(con,glue('DROP TABLE "{TABLE_NAME}";'))
  dbCommit(con)
}





# read raw scc data & process it ----
scc_may_data <- read.xlsx(paste0(home_path,"Input Data Raw/Lab & Blood Bank/SCC/SCC HGB Report May 2022.xlsx"))
processed_scc_may_data <- lab_scc_tat_dept_summary(scc_may_data)
# read raw sunsquest data & process it ----
sunquest_may_data <- read_excel(paste0(home_path,"Input Data Raw/Lab & Blood Bank/SUNQUEST/SQ Monthly TROP-HGB May 2022.xls"))
processed_sunquest_may_data <- lab_sun_tat_dept_summary(sunquest_may_data)

#read LAB TAT summary repo:
summary_repo_lab_tat <- read_excel(paste0(home_path,"Summary Repos for Database/Lab TAT Metrics.xlsx"))

summary_repo_lab_tat1 <- summary_repo_lab_tat%>%head(10)
data<- write_temporary_table_to_database_and_merge(summary_repo_lab_tat1,"SR3")





data<- write_temporary_table_to_database_and_merge(processed_sunquest_may_data,"SUNQUEST")
data<- write_temporary_table_to_database_and_merge(processed_scc_may_data,"SCC")





