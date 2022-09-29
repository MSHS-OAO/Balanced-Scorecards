# function to append the new data to summary repo- KPIs & Disruptions and Issues -----
process_manual_entry_to_summary_repo_format_biomed <- function(data,type,updated_user){
  
  # Code block to process KPI input data
  if(type=="KPI"){

      summary_repo_kpi_format <- data %>%
      rename(SITE = Site,
             METRIC_NAME_SUBMITTED = Metric ) %>%
      pivot_longer(cols = c(-SITE,-METRIC_NAME_SUBMITTED),
                   names_to = "REPORTING_MONTH",
                   values_to = "VALUE") %>%
      mutate(REPORTING_MONTH = as.Date(format(parse_date_time(paste0("01-",REPORTING_MONTH),orders = "dmy"),"%Y-%m-%d")),
             SERVICE = "Biomed / Clinical Engineering",
             PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
             #REPORTING_MONTH = format(REPORTING_MONTH,"%Y-%m-%d"),
             UPDATED_USER = updated_user)
      
      summary_repo_kpi_format <- as.data.frame(summary_repo_kpi_format)
      summary_repo_kpi_format <- summary_repo_kpi_format[complete.cases(summary_repo_kpi_format), ]    
      
  
    return(summary_repo_kpi_format)
  }
  else{
    
    summary_repo_di_format <- data %>%
      rename(SITE = Site) %>%
      select(-Metric) %>%
      #mutate(vars(col.names.to.numeric),as.numeric()) %>%
      pivot_longer(cols = c(-SITE),
                   names_to = "REPORTING_MONTH",
                   values_to = "VALUE") %>%
      mutate(REPORTING_MONTH = as.Date(format(parse_date_time(paste0("01-",REPORTING_MONTH),orders = "dmy"),"%Y-%m-%d")),
             SERVICE = "Biomed / Clinical Engineering",
             METRIC_NAME_SUBMITTED = "Total Disruptions or Equipment Issues",
             PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
             #REPORTING_MONTH = format(REPORTING_MONTH,"%Y-%m-%d"),
             UPDATED_USER = updated_user)
    
    summary_repo_di_format <- as.data.frame(summary_repo_di_format)
    summary_repo_di_format <- summary_repo_di_format[complete.cases(summary_repo_di_format), ]    
    
    
    return(summary_repo_di_format)
    
    
    
    
  }

}


# didata <- process_manual_entry_to_summary_repo_format_biomed(disruptions_issues_reports_ui,"DI")
# kpidata <- process_manual_entry_to_summary_repo_format_biomed(kpibme_reports_ui,"KPI")

# # function to append data into metrics_final_df- KPIs & Disruptions and Issues -----
# biomed__metrics_final_df_process <- function(data,type){
#   if(type=="KPIs"){
#     
#     metrics_final_df_form <- data %>% 
#       rename(value_rounded= Number,
#              Metric_Name_Submitted = Metric,
#              Reporting_Month_Ref = Month) %>%
#       mutate(Reporting_Month = format(Reporting_Month_Ref,"%m-%Y"),
#              Premier_Reporting_Period = format(Reporting_Month_Ref,"%b %Y"),
#              value_rounded = as.numeric(value_rounded))%>%
#       select(-Reporting_Month_Ref)
#     
#     # summary_metric_filter_subset <- summary_metric_filter %>% select(Metric_Group,Metric_Name,Metric_Name_Submitted)
#     # 
#     # metrics_final_df_form <- left_join(metrics_final_df_form,
#     #                                    summary_metric_filter_subset,
#     #                                    by = c("Metric_Name_Submitted"))
#     # metrics_final_df_form <- left_join(metrics_final_df_form,
#     #                                          target_mapping, 
#     #                                          by = c("Service","Site","Metric_Group", "Metric_Name","Metric_Name_Submitted"))
#     # 
#     # metrics_final_df_form <- metrics_final_df_form %>%
#     #   mutate(Variance = between(value_rounded, Range_1, Range_2)) %>%
#     #   filter(Variance %in% c(TRUE,NA))
#     # 
#     # metrics_final_df_form <- metrics_final_df_form[,c("Service",
#     #                                                   "Site",
#     #                                                   "Metric_Group",
#     #                                                   "Metric_Name",
#     #                                                   "Premier_Reporting_Period",
#     #                                                   "Reporting_Month",
#     #                                                   "value_rounded",
#     #                                                   "Target",
#     #                                                   "Status",
#     #                                                   "Reporting_Month_Ref")]
#     # 
#     # updated_rows <- unique(metrics_final_df_form[c("Metric_Name","Reporting_Month","Service", "Site")])
#     # 
#     # metrics_final_df <- anti_join(metrics_final_df, updated_rows)
#     # 
#     # metrics_final_df <- full_join(metrics_final_df,metrics_final_df_form)
#     
#     metrics_final_df <- metrics_final_df_subset_and_merge(metrics_final_df_form)
#     return(metrics_final_df)
# 
#   }
#   else{
#     
#     metrics_final_df_form <- data %>% 
#       rename(value_rounded= `Total Disruptions/Issues`,
#              Reporting_Month_Ref = Month) %>%
#       mutate(Reporting_Month = format(Reporting_Month_Ref,"%m-%Y"),
#              Premier_Reporting_Period = format(Reporting_Month_Ref,"%b %Y"),
#              Metric_Name_Submitted = "Total Disruptions or Equipment Issues")%>%
#       select(-Reporting_Month_Ref)
#     
#     # summary_metric_filter_subset <- summary_metric_filter %>% 
#     #   select(Metric_Group,Metric_Name,Metric_Name_Submitted)
#     # 
#     # metrics_final_df_form <- left_join(metrics_final_df_form,
#     #                                    summary_metric_filter_subset,
#     #                                    by = c("Metric_Name_Submitted"))
#     # 
#     # metrics_final_df_form <- left_join(metrics_final_df_form,
#     #                                    target_mapping, 
#     #                                    by = c("Service","Site","Metric_Group", "Metric_Name","Metric_Name_Submitted"))
#     # 
#     # metrics_final_df_form <- metrics_final_df_form[,c("Service",
#     #                                                   "Site",
#     #                                                   "Metric_Group",
#     #                                                   "Metric_Name",
#     #                                                   "Premier_Reporting_Period",
#     #                                                   "Reporting_Month",
#     #                                                   "value_rounded",
#     #                                                   "Target",
#     #                                                   "Status",
#     #                                                   "Reporting_Month_Ref")]
#     # 
#     # updated_rows <- unique(metrics_final_df_form[c("Metric_Name","Reporting_Month","Service", "Site")])
#     # 
#     # metrics_final_df <- anti_join(metrics_final_df, updated_rows)
#     # 
#     # metrics_final_df <- full_join(metrics_final_df,metrics_final_df_form)
#     
#     metrics_final_df <- metrics_final_df_subset_and_merge(metrics_final_df_form)
#     return(metrics_final_df)
#     
#     
#   }
#   
# }

