# Source code for processing Biomed/CE KPIs

# Disruptions and Issues Reports -------------
# Import historical summary
disruptions_issues_reports <- read_excel(bmedi_table_path)

# Reformat "Month" and "Date" column in Disruptions and Issues for merging
disruptions_issues_reports <- disruptions_issues_reports %>%
  mutate(Month = date(Month))

# Determine last month and next month for Security Incident Reports
biomedDI_last_month <- max(disruptions_issues_reports$Month)

# Transform data to UI form
disruptions_issues_reports_ui <- disruptions_issues_reports %>%
  select(-Service) %>%
  filter(Month >= biomedDI_last_month - months(7)) %>%
  mutate(Month = format(Month, "%B-%Y")) %>%
  pivot_wider(names_from = "Month",values_from = `Total Disruptions/Issues`,values_fill=0) %>%
  mutate(Metric = "Total Disruptions and Issues") %>%
  relocate(Metric,.after=Site) %>%
  mutate('{format(biomedDI_last_month + months(1), "%B-%Y")}' := "")


# KPIs Biomed -------------
# Import historical summary
kpibme_reports <- read_excel(bmekpi_table_path,sheet = "KPIs")

# Reformat "Month" column in KPIs for merging
kpibme_reports <- kpibme_reports %>%
  mutate(Month = date(Month))

# Determine last month and next month for KPIs
kpibme_last_month <- max(kpibme_reports$Month)

# Transform data to UI form
kpibme_reports_ui <- kpibme_reports %>%
  select(-Service) %>%
  filter(Month >= kpibme_last_month - months(7)) %>%
  mutate(Month = format(Month, "%B-%Y"),
         Number = round(as.numeric(Number),2),
         Number = if_else(Metric %in% c("Documented Status","PM Compliance - High Risk Equipment","PM Compliance - All Medical Equipment"),Number*100,Number)) %>%
  pivot_wider(names_from = "Month",values_from = Number,values_fill=0) %>%
  group_by(Site) %>%
  mutate('{format(kpibme_last_month + months(1), "%B-%Y")}' := NA) %>%
  arrange(Site)

# function to append the new data to summary repo- KPIs & Disruptions and Issues -----
process_manual_entry_to_summary_repo_format_biomed <- function(data,type){
  
  # Code block to process KPI input data
  if(type=="KPI"){

      summary_repo_kpi_format <- data %>%
      pivot_longer(cols = c(-Site,-Metric),
                   names_to = "Month",
                   values_to = "Number") %>%
      mutate(Month = format(parse_date_time(paste0("01-",Month),orders = "dmy"),"%Y-%m-%d"),
             Service = "Biomed / Clinical Engineering") %>%
      select(Service,Site,Month,Metric,Number)
  
    return(summary_repo_kpi_format)
  }
  else{
    
    summary_repo_di_format <- data %>%
      mutate(vars(col.names.to.numeric),as.numeric()) %>%
      pivot_longer(cols = c(-Site,-Metric),
                   names_to = "Month",
                   values_to = "Total Disruptions/Issues") %>%
      mutate(Month = format(parse_date_time(paste0("01-",Month),orders = "dmy"),"%Y-%m-%d"),
             Service = "Biomed / Clinical Engineering") %>%
      select(Service,Site,Month,`Total Disruptions/Issues`)
    
    return(summary_repo_di_format)
    
    
  }

}


#didata <- process_manual_entry_to_summary_repo_format_biomed(disruptions_issues_reports_ui,"DI")


# function to append data into metrics_final_df- KPIs & Disruptions and Issues -----
biomed__metrics_final_df_process <- function(data){
  
}

