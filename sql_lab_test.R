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



# read raw scc data & process it ----
scc_may_data <- read.xlsx(paste0(home_path,"Input Data Raw/Lab & Blood Bank/SCC/SCC HGB Report May 2022.xlsx"))
processed_scc_may_data <- lab_scc_tat_dept_summary(scc_may_data)
# read raw sunsquest data & process it ----
sunquest_may_data <- read_excel(paste0(home_path,"Input Data Raw/Lab & Blood Bank/SUNQUEST/SQ Monthly TROP-HGB May 2022.xls"))
processed_sunquest_may_data <- lab_sun_tat_dept_summary(sunquest_may_data)

processed_scc_may_data <- processed_scc_may_data%>%
  mutate(`Premier Reporting Period` = format(Month,"%b %Y"),
         Update_time = NA_character_)%>%
  rename(Value = Number,
         Metric_Name_Submitted = Metric)

processed_sunquest_may_data <- processed_sunquest_may_data%>%
  mutate(`Premier Reporting Period` = format(Month,"%b %Y"),
         Update_time = NA_character_)%>%
  rename(Value = Number,
         Metric_Name_Submitted = Metric)



processed_scc_may_data$Month <- format(processed_scc_may_data$Month,"%Y-%m-%d")
processed_sunquest_may_data$Month <- format(processed_sunquest_may_data$Month, "%Y-%m-%d")



# Add Processed data to table ----
lab_data_for_db <- read_excel(paste0(home_path,"Summary Repos for Database/Lab TAT Metrics.xlsx"))

lab_data_for_db <- lab_data_for_db%>%
  mutate(`Premier Reporting Period` = format(Month,"%b %Y"),
         Update_time = NA_character_,
         Month = format(Month,"%Y-%m-%d"))


dbWriteTable(con,
             "LAB_TAT",
             lab_data_for_db,
             #field.types = dbDataType(con,processed_scc_may_data)),
             overwrite = TRUE,
             field.types = c(Service = "Varchar2(10 CHAR)",
                             Site = "Varchar2(10 CHAR)",
                             Month = "Date",
                             Metric_Name_Submitted = "Varchar2(20 CHAR)",
                             Value = "numeric(10,10)",
                             `Premier Reporting Period` = "Varchar2(20 CHAR)",
                             Update_time = "Timestamp"))


# write the processed data to database before adding ----

if(DBI::dbExistsTable(con, "STAGING.LAB_TAT_SCC") & DBI::dbExistsTable(con, "STAGING.LAB_TAT_SUNQUEST")){
  
  
  
  dbWriteTable(con,
               "STAGING.LAB_TAT_SCC",
               processed_scc_may_data,
               overwrite = TRUE,
               #field.types = dbDataType(con,processed_scc_may_data))
               field.types = c(Service = "Varchar2(10 CHAR)",
                               Site = "Varchar2(10 CHAR)",
                               Month = "DATE",
                               Metric_Name_Submitted = "Varchar2(20 CHAR)",
                               Value = "numeric(10,10)",
                               `Premier Reporting Period` = "Varchar2(20 CHAR)",
                               Update_time = "Timestamp"))
  
  dbWriteTable(con,
               "STAGING.LAB_TAT_SUNQUEST",
               processed_sunquest_may_data,
               overwrite = TRUE,
               #field.types = dbDataType(con,processed_scc_may_data))
               field.types = c(Service = "Varchar2(10 CHAR)",
                               Site = "Varchar2(10 CHAR)",
                               Month = "Date",
                               Metric_Name_Submitted = "Varchar2(20 CHAR)",
                               Value = "numeric(10,10)",
                               `Premier Reporting Period` = "Varchar2(20 CHAR)",
                               Update_time = "Timestamp"))
} else{
  

  dbWriteTable(con,
               "STAGING.LAB_TAT_SCC",
               processed_scc_may_data,
               #field.types = dbDataType(con,processed_scc_may_data))
               field.types = c(Service = "Varchar2(10 CHAR)",
                               Site = "Varchar2(10 CHAR)",
                               Month = "Date",
                               Metric_Name_Submitted = "Varchar2(20 CHAR)",
                               Value = "numeric(10,10)",
                               `Premier Reporting Period` = "Varchar2(20 CHAR)",
                               Update_time = "Timestamp"))
  
  dbWriteTable(con,
               "STAGING.LAB_TAT_SUNQUEST",
               processed_sunquest_may_data,
               #field.types = dbDataType(con,processed_scc_may_data))
               field.types = c(Service = "Varchar2(10 CHAR)",
                               Site = "Varchar2(10 CHAR)",
                               Month = "Date",
                               Metric_Name_Submitted = "Varchar2(20 CHAR)",
                               Value = "numeric(10,10)",
                               `Premier Reporting Period` = "Varchar2(20 CHAR)",
                               Update_time = "Timestamp"))
}

data <- read_table_from_db(con,"STAGING.LAB_TAT_SUNQUEST")

query = 'MERGE INTO LAB_TAT LT
USING "STAGING.LAB_TAT_SUNQUEST" SOURCE_TABLE
ON (LT."Site" = SOURCE_TABLE."Site" AND
    LT."Month" = SOURCE_TABLE."Month" AND
    LT."Service" = SOURCE_TABLE."Service" AND
    LT."Metric_Name_Submitted" = SOURCE_TABLE."Metric_Name_Submitted")
WHEN MATCHED THEN 
UPDATE  SET LT."Value" = SOURCE_TABLE."Value",
LT."Update_time" = SOURCE_TABLE."Update_time",
LT."Premier Reporting Period" = SOURCE_TABLE."Premier Reporting Period"
WHEN NOT MATCHED THEN
INSERT(LT."Site",LT."Month",LT."Service",LT."Metric_Name_Submitted",LT."Value", LT."Update_time", LT."Premier Reporting Period")  
VALUES(SOURCE_TABLE."Site",SOURCE_TABLE."Month",SOURCE_TABLE."Service",SOURCE_TABLE."Metric_Name_Submitted",SOURCE_TABLE."Value", SOURCE_TABLE."Update_time",SOURCE_TABLE."Premier Reporting Period");'

dbSendStatement(con,query)

