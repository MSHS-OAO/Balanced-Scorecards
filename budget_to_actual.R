bislr_preprocess <- function(finance_bislr){
  ### Split out to actual data
  actual_index <- which(colnames(finance_bislr) == "Actual")
  finance_bislr_actual <- finance_bislr[,1:actual_index]
  col_names <- colnames(finance_bislr_actual)
  year_index <- str_find(col_names, "ACTUAL", 0)[1]
  year <- gsub(".*ACTUAL", "", col_names[6])
  
  finance_bislr_actual <- finance_bislr_actual %>% row_to_names(row_number = 1)  
  
  colname <- names(finance_bislr_actual)
  colname[7:length(finance_bislr_actual)-1] <- paste0(colname[7:length(finance_bislr_actual)-1], "-01-",year)
  colname[7:length(finance_bislr_actual)-1] <- format(as.Date(colname[7:length(finance_bislr_actual)-1], "%B-%d-%Y"),"%Y-%m-%d")
  
  
  colnames(finance_bislr_actual) <- colname
  
  names(finance_bislr_actual)[length(names(finance_bislr_actual))] <- "YTD" 
  
  colname <- names(finance_bislr_actual)
  
  finance_bislr_actual <- merge(finance_bislr_actual, budget_mapping[,c("Service", "Site","Cost Center2", "FTI - Mapping")],
                                by.x = c("Cost Center2"), by.y = c("Cost Center2"))
  
  finance_bislr_actual <- finance_bislr_actual %>%
    select(-`Functional Level`, -`Cost Center Desc`) %>%
    rename(Service = Service) %>%
    rename(Site = Site) %>%
    select(-YTD)
  finance_bislr_actual <- finance_bislr_actual %>%
    pivot_longer(cols = c(-`Cost Center2`, -`Division Level 3 Desc`, -`Account Category Desc2`, -Service, -Site, -`FTI - Mapping`),
                 names_to = "Month",
                 values_to = "Month Actual") 
  finance_bislr_actual <- finance_bislr_actual[,c("Service", "Site", "Division Level 3 Desc", "Cost Center2", "Account Category Desc2", "Month", "Month Actual", "FTI - Mapping")]
  
  
  ###Split out to Budget Data
  budget_index <- which(colnames(finance_bislr) == "Budget")
  index_start <- actual_index+1
  
  finance_bislr_budget <- finance_bislr[,c(1:5,(actual_index+1):(budget_index-1))]
  
  col_names <- colnames(finance_bislr_budget)
  year_index <- str_find(col_names, "BUDGET", 0)[1]
  
  year <- gsub(".*BUDGET", "", col_names[6])
  
  finance_bislr_budget <- finance_bislr_budget %>% row_to_names(row_number = 1)  
  
  colname <- names(finance_bislr_budget)
  colname[6:length(finance_bislr_budget)] <- paste0(colname[6:length(finance_bislr_budget)], "-01-",year)
  colname[6:length(finance_bislr_budget)] <- format(as.Date(colname[6:length(finance_bislr_budget)], "%B-%d-%Y"),"%Y-%m-%d")
  
  colnames(finance_bislr_budget) <- colname
  
  finance_bislr_budget <- merge(finance_bislr_budget, budget_mapping[,c("Service", "Site","Cost Center2", "FTI - Mapping")],
                                by.x = c("Cost Center2"), by.y = c("Cost Center2"))
  
  finance_bislr_budget <- finance_bislr_budget %>%
    select(-`Functional Level`, -`Cost Center Desc`) %>%
    rename(Service = Service) %>%
    rename(Site = Site) 
  
  finance_bislr_budget <- finance_bislr_budget %>%
    pivot_longer(cols = c(-`Cost Center2`, -`Division Level 3 Desc`, -`Account Category Desc2`, -Service, -Site, -`FTI - Mapping`),
                 names_to = "Month",
                 values_to = "Month Budget") 
  finance_bislr_budget <- finance_bislr_budget[,c("Service", "Site", "Division Level 3 Desc", "Cost Center2", "Account Category Desc2", "Month", "Month Budget", "FTI - Mapping")]
  
  finance_bislr_budget <- finance_bislr_budget %>% filter(!is.na(Service))
  
  finance_bislr_actual <- finance_bislr_actual %>% filter(!is.na(Service))
  
  finance_bislr_comb <- full_join(finance_bislr_actual,finance_bislr_budget)
}



exptrend_process <- function(finance_exptrend){
  
  month <- colnames(finance_exptrend)[1]
  finance_exptrend <- finance_exptrend %>% row_to_names(row_number = 1)
  finance_exptrend <-  finance_exptrend %>%
    select(-FDIV,-SUBACCT,-ACCTNAME,-NAME, -YTD_BUD, -YTD_ACT) %>%
    rename(`Account Category Desc2` = EXPTYPE,
           Site = SITE,
           `Cost Center2` = CC,
           `Month Budget` = MO_BUD,
           `Month Actual` = MO_ACT)
  
  
  finance_exptrend$`Account Category Desc2` <- gsub(".*2-", "", finance_exptrend$`Account Category Desc2`)
  finance_exptrend$`Account Category Desc2` <- gsub(".*1-", "", finance_exptrend$`Account Category Desc2`)
  
  finance_exptrend$`Account Category Desc2` <- gsub(".*SUPPLIES", "Supplies", finance_exptrend$`Account Category Desc2`)
  finance_exptrend$`Account Category Desc2` <- gsub(".*SALARIES", "Salary", finance_exptrend$`Account Category Desc2`)
  
  finance_exptrend$Month <- paste0(month, " 01")
  finance_exptrend$Month <- format(as.Date(finance_exptrend$Month, format = '%B %Y %d'),"%Y-%m-%d")
  
  finance_exptrend$`Cost Center2` <- stringr::str_replace(finance_exptrend$`Cost Center2`, '\\-', "")
  
  finance_exptrend <- merge(finance_exptrend, budget_mapping[,c("Service", "Site","Cost Center2", "FTI - Mapping")],
                            by.x = c("Cost Center2"), by.y = c("Cost Center2"))
  finance_exptrend <- finance_exptrend %>% filter(!is.na(Service))
  
  finance_exptrend <- finance_exptrend %>% rename(Service = Service)
}



budget_to_actual_process <- function(data){
  raw_budget_actual <- data
  
  raw_budget_actual$`Month Actual`[is.na(raw_budget_actual$`Month Actual`)] <- 0
  raw_budget_actual$`Month Budget`[is.na(raw_budget_actual$`Month Budget`)] <- 0
  
  
  raw_budget_actual$Service <- ifelse(raw_budget_actual$`FTI - Mapping` == "Bloodbank","Lab - Bloodbank", raw_budget_actual$Service)
  ## Budget to Actual data pre-processing
  budget_actual_df <- raw_budget_actual %>%
    mutate(Month_Var = ifelse(`Month Budget` == "Not Received", " ", 
                              round(as.numeric(`Month Budget`) - as.numeric(`Month Actual`))),
           Metric_Name = ifelse(`Account Category Desc2` == "Salary", 
                                "Budget to Actual Variance - Labor", "Budget to Actual Variance - Non Labor")) %>%
    group_by(Service, Site, Metric_Name, Month) %>%
    summarise(Month_Var= sum(as.numeric(Month_Var), na.rm = TRUE)) %>%
    pivot_wider(
      names_from = Metric_Name,
      values_from = Month_Var) %>%
    select(Service, Site, Month, `Budget to Actual Variance - Labor`, `Budget to Actual Variance - Non Labor`)
  
  budget_actual_df$`Budget to Actual Variance - Labor`[is.na(budget_actual_df$`Budget to Actual Variance - Labor`)] <- 0
  
  budget_actual_df <- budget_actual_df %>%
    mutate(`Budget to Actual Variance - Total` = 
             `Budget to Actual Variance - Labor` + `Budget to Actual Variance - Non Labor`)
  
  ## Calculate Target and Status
  budget_actual_target <- raw_budget_actual %>%
    group_by(Service, Site, Month) %>%
    summarise(Budget_Total = sum(as.numeric(`Month Budget`), na.rm = TRUE)) 
  
  
  budget_actual_df <- merge(budget_actual_df, budget_actual_target)
  budget_actual_df <- budget_actual_df %>% 
    mutate(Target = round(`Budget to Actual Variance - Total`/Budget_Total,2),
           Status = ifelse(Target >= 0, "Green", ifelse(Target < -0.02, "Red", "Yellow"))) %>%
    pivot_longer(4:7,
                 names_to = "Metric_Name",
                 values_to = "value_rounded") %>%
    mutate(Metric_Group = "Budget to Actual",
           Reporting_Month = format(as.Date(Month, format = "%Y-%m-%d"),"%m-%Y"),
           Premier_Reporting_Period = format(as.Date(Month, format = "%Y-%m-%d"),"%b %Y")) %>%
    filter(!is.na(Reporting_Month))
  
  budget_actual_df$Metric_Name_Submitted <- metric_group_mapping$Metric_Name[match(budget_actual_df$Metric_Name, metric_group_mapping$Metric_Name_Submitted)]
  budget_actual_df <- budget_actual_df %>% 
    mutate(Metric_Name = ifelse(is.na(Metric_Name_Submitted), Metric_Name, Metric_Name_Submitted))
  
  ### Subset processed data for merge
  budget_actual_df_merge <- budget_actual_df[,processed_df_cols] 
  
  budget_actual_df_merge$Reporting_Month_Ref <- as.Date(paste('01', as.yearmon(budget_actual_df_merge$Reporting_Month, "%m-%Y")), format='%d %b %Y')
  
  budget_actual_df_merge$Metric_Name <- ifelse(budget_actual_df_merge$Service == "Lab - Bloodbank", paste0(budget_actual_df_merge$Metric_Name, " (Blood Bank)"),budget_actual_df_merge$Metric_Name)
  budget_actual_df_merge$Service <- ifelse(budget_actual_df_merge$Service == "Lab - Bloodbank", "Lab", budget_actual_df_merge$Service)
  
  
  updated_rows <- unique(budget_actual_df_merge[c("Metric_Name","Reporting_Month","Service", "Site")])
  metrics_final_df <- anti_join(metrics_final_df, updated_rows)
  
  metrics_final_df <- full_join(metrics_final_df,budget_actual_df_merge)
  
  return(metrics_final_df)
}