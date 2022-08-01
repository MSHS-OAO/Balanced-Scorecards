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
con <- dbConnect(odbc::odbc(), "OAO Cloud DB", timeout = 30)

options(odbc.batch_rows = 1000000)

poolcon <- dbPool(drv = odbc::odbc(), 
                  dsn = "OAO Cloud DB")

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
# Inputs : 
#          processed_input_data (dplyr data frame) : data to be written to Oracle cloud. data should have Service
#                                                                                         Site ,
#                                                                                         Month ,
#                                                                                         Metric_Name_Submitted ,
#                                                                                         Value ,
#                                                                                         `Premier Reporting Period`, and
#                                                                                         Update_time fields
#         table_name (String) : Example : "LAB_TAT" in all letters UPPER_CASE
# Output  Boolean: TRUE

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
  
  
  # Add UPDATE_TIME 
  
  # check for all the fields are characters
  
  processed_input_data <- processed_input_data %>%
    mutate(UPDATED_TIME = as.character(Sys.time()),
           VALUE = as.character(VALUE),
           VALUE = coalesce(VALUE,"NULL"))%>%
    select(SERVICE, 
           SITE, 
           REPORTING_MONTH,
           PREMIER_REPORTING_PERIOD, 
           METRIC_NAME_SUBMITTED,
           VALUE,UPDATED_TIME,
           UPDATED_USER)#,
           # PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
           # REPORTING_MONTH = format(REPORTING_MONTH,"%Y-%m-%d"),
           # UPDATED_USER = NA_character_) %>%
    # rename(METRIC_NAME_SUBMITTED = Metric,
    #        VALUE=Number)
  
  view(processed_input_data)
  
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
  
  drop_query <- glue('DROP TABLE "{TABLE_NAME}";')
  
  print(drop_query)
  
  dbCreateTable(con,
                TABLE_NAME,
                processed_input_data,
                field.types  = DATA_TYPES)
  
  dbExecute(con,all_data)
  dbExecute(con,query)
  dbExecute(con,drop_query)
}




# Tests ----
# # read raw scc data & process it ----
# scc_may_data <- read.xlsx(paste0(home_path,"Input Data Raw/Lab & Blood Bank/SCC/SCC HGB Report May 2022.xlsx"))
# processed_scc_may_data <- lab_scc_tat_dept_summary(scc_may_data)
# # read raw sunsquest data & process it ----
# sunquest_may_data <- read_excel(paste0(home_path,"Summary Repos for Database/Lab TAT Metrics.xlsx")) %>%
#   mutate(UPDATED_USER = NA_character_)
# processed_sunquest_may_data <- lab_sun_tat_dept_summary(sunquest_may_data)
# 
# #read LAB TAT summary repo:
# summary_repo_lab_tat <- read_excel(paste0(home_path,"Summary Repos for Database/Lab TAT Metrics.xlsx"))
# 
# summary_repo_lab_tat1 <- summary_repo_lab_tat%>%head(10)
# data<- write_temporary_table_to_database_and_merge(summary_repo_lab_tat1,"SR3")


# EVS <- read_excel(paste0(home_path,"Summary Repos for Database/TAT - EVS.xlsx"))
# Nursing <- read_excel(paste0(home_path,"Summary Repos for Database/Nursing.xlsx"))
# 
# data<- write_temporary_table_to_database_and_merge(ImagingDR,"ImagingDR")
# data<- write_temporary_table_to_database_and_merge(ImagingIR,"ImagingIR")
# data<- write_temporary_table_to_database_and_merge(BiomedKPIS,"BiomedKPIS")
# data<- write_temporary_table_to_database_and_merge(BiomedDI,"BiomedDI")
# data<- write_temporary_table_to_database_and_merge(EVS,"EVS")
# data<- write_temporary_table_to_database_and_merge(Nursing,"Nursing")


# data<- write_temporary_table_to_database_and_merge(processed_sunquest_may_data,"SUNQUEST")
# data<- write_temporary_table_to_database_and_merge(processed_scc_may_data,"SCC")





