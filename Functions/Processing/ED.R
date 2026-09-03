ed_data_preprocess <- function(ed_data_ts,ed_data_percentiles){
  
  ed_data_ts[1,"X2"] <- "Measure Names"
  ed_data_percentiles[1,"X2"] <- "Measure Names"
  
  ed_data_ts <- ed_data_ts %>%
    row_to_names(row_number = 1)%>%
    pivot_longer(cols = c(-`Month of Arrival Date`,-`Measure Names`),
                 names_to = "Arrv Dept (group)",
                 values_to = "Measure Values") %>%
    filter(!`Month of Arrival Date`=="Grand Total") %>%
    mutate(`Month of Arrival Date` = as.Date(paste(`Month of Arrival Date`,"01"),format="%b %Y %d"))%>%
    mutate(`Measure Values` = as.numeric(`Measure Values`))
  
  
  ed_data_percentiles <- ed_data_percentiles %>%
    row_to_names(row_number = 1)%>%
    pivot_longer(cols = c(-`Month of Arrival Date`,-`Measure Names`),
                 names_to = "Arrv Dept (group)",
                 values_to = "Measure Values") %>%
    filter(!`Month of Arrival Date`=="Grand Total") %>%
    filter(!`Measure Names`=="Volume") %>%
    mutate(`Month of Arrival Date` = as.Date(paste(`Month of Arrival Date`,"01"),format="%b %Y %d"))%>%
    mutate(`Measure Values` = as.numeric(`Measure Values`))
  
  results <-list(ed_data_ts ,ed_data_percentiles)
  
  return(results)
  
  
}



ed_dept_summary <- function(ed_data_ts,ed_data_percentiles,updated_user){
  
  
  
  mapping <- tibble(`Measure Names`=c("Acuity Null",
                                      "Acuity 5",
                                      "Acuity 4",
                                      "Acuity 3",
                                      "Acuity 2",
                                      "Acuity 1",
                                      "SUM Admit to Depart (Boarder Hrs)",
                                      "Median Admit to Depart (Boarder mins)",
                                      "Median Arrival to Admit Decision",
                                      "Median ED LOS Discharge (mins)",
                                      "Median ED LOS Admit (mins)",
                                      "LWBS",
                                      "Volume",
                                      "Percentile (90) of ED LOS Discharge",
                                      "Percentile (90) of ED LOS Admit",
                                      "Percentile (90) of Admit to Depart",
                                      "Median ED Arrival to Admit Decision"), 
                    KPI=c("Acuity Null",
                          "Acuity 5",
                          "Acuity 4",
                          "Acuity 3",
                          "Acuity 2",
                          "Acuity 1",
                          "Total Boarder Hours",
                          "Admit to Depart Boarder Hours (Median)",
                          "Door to Admit (Median)",
                          "ED LOS T&R Patients (Median)",
                          "ED LOS Admitted Patients (Median)",
                          "LWBS",
                          "Visit Volume (Epic)",
                          "ED LOS T&R Patients (90th Percentile)",
                          "ED LOS Admitted Patients (90th Percentile)",
                          "Admit to Depart Boarder Hours (90th Percentile)",
                          "Door to Admit (Median)"))
  
  summary_repo <- rbind(ed_data_ts,ed_data_percentiles) %>%
    mutate(`Measure Names` = trimws(`Measure Names`))
  
  
  summary_repo <-left_join(summary_repo,
                           mapping) %>% filter(!is.na(KPI)) %>% distinct()
  
  summary_repo <- summary_repo %>%
    rename(`SITE` = `Arrv Dept (group)`,
           REPORTING_MONTH = `Month of Arrival Date`,
           Metric = `Measure Values`) %>%
    select(-`Measure Names`) %>%
    pivot_wider(names_from = "KPI",
                values_from = "Metric",values_fill=0) %>%
    mutate(`LWBS %` = `LWBS`/`Visit Volume (Epic)`,
           `Admit to Depart Boarder Hours (90th Percentile)` = `Admit to Depart Boarder Hours (90th Percentile)`/60,
           `ED LOS Admitted Patients (90th Percentile)` = `ED LOS Admitted Patients (90th Percentile)`/60,
           `ED LOS T&R Patients (90th Percentile)` = `ED LOS T&R Patients (90th Percentile)`/60,
           `ED LOS Admitted Patients (Median)` = `ED LOS Admitted Patients (Median)`/60,
           `ED LOS T&R Patients (Median)` = `ED LOS T&R Patients (Median)`/60,
           `Door to Admit (Median)` = `Door to Admit (Median)`/60,
           `Admit to Depart Boarder Hours (Median)` = `Admit to Depart Boarder Hours (Median)`/60,
           `Acuity Total` = `Acuity Null`+`Acuity 1`+ `Acuity 2` +`Acuity 3`+`Acuity 4`+`Acuity 5`,
           `Acuity 1 count AAAEM` = `Acuity 1`/`Acuity Total`,
           `Acuity 2 count AAAEM` = `Acuity 2`/`Acuity Total`,
           `Acuity 3 count AAAEM` = `Acuity 3`/`Acuity Total`,
           `Acuity 4 count AAAEM` = `Acuity 4`/`Acuity Total`,
           `Acuity 5 count AAAEM` = `Acuity 5`/`Acuity Total`,
           `Acuity Null count AAAEM` = `Acuity Null`/`Acuity Total`) %>%
    select(-`Acuity Total`) %>%
    pivot_longer(cols = c(-SITE,-REPORTING_MONTH),
                 names_to = "METRIC_NAME_SUBMITTED",
                 values_to = "VALUE")%>%
    mutate(SERVICE = "Emergency Department",
           UPDATED_USER = updated_user,
           PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
           REPORTING_MONTH = as.Date(format(REPORTING_MONTH,"%Y-%m-%d"))) %>%
    select(SERVICE,SITE,REPORTING_MONTH,PREMIER_REPORTING_PERIOD,METRIC_NAME_SUBMITTED,VALUE,UPDATED_USER)
  
  
}


# Code for processing mew metrics  Quality: Door-to-EKG for Chest Pain &  PX: Door-to-Head CT for Stroke patients ----

# datapath <- "Tests/ED Test New.xlsx"
# updated_user = "TEST"
# 
# DTEData <- read_excel(datapath,sheet = "Sheet4",skip=1)
# DTHData <- read_excel(datapath,sheet = "Sheet3",skip=1)
# 
# raw_data_dte <- DTEData
# raw_data_dth <- DTHData


# Process data for Door-to-EKG for Chest Pain ----
process_dte_data <- function(raw_data,updated_user){
  
  processed_data <- raw_data %>%
    fill(`Month of Arrival_date`) %>%
    rename(METRIC_NAME_SUBMITTED = `...2`,
           PREMIER_REPORTING_PERIOD = `Month of Arrival_date`) %>%
    filter(PREMIER_REPORTING_PERIOD != "Grand Total") %>%
    mutate(METRIC_NAME_SUBMITTED = ifelse(METRIC_NAME_SUBMITTED == "Median Arrival to Collect", 
                                          "Door to EKG for Chest Pain (Median)",
                                          "Door to EKG for Chest Pain (95th Percentile)"),
           SERVICE = "Emergency Department",
           REPORTING_MONTH = as.Date(paste(PREMIER_REPORTING_PERIOD,"01"),format="%b %Y %d"),
           REPORTING_MONTH = as.Date(format(REPORTING_MONTH,"%Y-%m-%d")),
           UPDATED_USER = updated_user) %>%
    pivot_longer(cols=c("MSB","MSBI","MSH","MSM","MSQ","MSW" ),
                 names_to='SITE',
                 values_to='VALUE') %>%
    mutate(VALUE = ifelse(SITE == "MSQ", 
                          NA, VALUE)) %>% #Update in the future
  select(SERVICE,SITE,REPORTING_MONTH,PREMIER_REPORTING_PERIOD,METRIC_NAME_SUBMITTED,VALUE,UPDATED_USER)

}


# Process data for Door-to-Head CT for Stroke patients ----
process_dth_data <- function(raw_data,updated_user){

  processed_data <- raw_data %>%
    fill(`Month of Date of Encounter`) %>%
    rename(METRIC_NAME_SUBMITTED = `...2`,
           PREMIER_REPORTING_PERIOD = `Month of Date of Encounter`) %>%
    mutate(METRIC_NAME_SUBMITTED = gsub(" ", "", METRIC_NAME_SUBMITTED)) %>% 
    filter(PREMIER_REPORTING_PERIOD != "Grand Total") %>%
    mutate(METRIC_NAME_SUBMITTED = ifelse(METRIC_NAME_SUBMITTED == "MedianDoor/DiscoverytoCT", 
                                          "Door to Door CT for Stroke Patients (Median)",
                                          "Door to Door CT for Stroke Patients (95th Percentile)"),
           SERVICE = "Emergency Department",
           REPORTING_MONTH = as.Date(paste(PREMIER_REPORTING_PERIOD,"01"),format="%b %Y %d"),
           REPORTING_MONTH = as.Date(format(REPORTING_MONTH,"%Y-%m-%d")),
           UPDATED_USER = updated_user) %>%
    pivot_longer(cols=c("MSB","MSBI","MSH","MSM","MSQ","MSW" ),
                 names_to='SITE',
                 values_to='VALUE') %>%
  select(SERVICE,SITE,REPORTING_MONTH,PREMIER_REPORTING_PERIOD,METRIC_NAME_SUBMITTED,VALUE,UPDATED_USER)
  
  processed_data <- processed_data %>% mutate(VALUE = ifelse(VALUE < 0, 1, VALUE))
  
}

# processed_dte_data <- process_dte_data(raw_data_dte,updated_user)
# processed_dth_data <- process_dth_data(raw_data_dth,updated_user)

