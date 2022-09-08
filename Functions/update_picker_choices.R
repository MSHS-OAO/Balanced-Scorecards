update_picker_choices <- function(session, service_input_summary, service_input_site, service_input_breakdown){
  
  # Summary tab month drop down update
  #service_input_summary <- input$selectedService
  
  picker_choices_summary <- format(
    metrics_final_df %>%
       filter(Service %in% service_input_summary) %>%
       select(Reporting_Month_Ref) %>%
       distinct() %>%
       arrange(Reporting_Month_Ref) %>%
       pull(Reporting_Month_Ref),
    "%m-%Y"
    )
  
  updatePickerInput(session, "selectedMonth",
                    choices = picker_choices_summary,
                    selected = picker_choices_summary[length(picker_choices_summary)])
  
  # Site tab month drop down update
  #service_input_site <- input$selectedService2
  
  picker_choices_site <- format(
    metrics_final_df %>%
      filter(Service %in% service_input_site) %>%
      select(Reporting_Month_Ref) %>%
      distinct() %>%
      arrange(Reporting_Month_Ref) %>%
      pull(Reporting_Month_Ref),
    "%m-%Y"
  )
  
  updatePickerInput(session, "selectedMonth2",
                    choices = picker_choices_site,
                    selected = picker_choices_site[length(picker_choices_site)])
  
  # KPI Breakout tab month drop down update
  #service_input_breakdown <- input$selectedService3
  
  picker_choices_breakdown <- format(
    metrics_final_df %>%
      filter(Service %in% service_input_breakdown) %>%
      select(Reporting_Month_Ref) %>%
      distinct() %>%
      arrange(Reporting_Month_Ref) %>%
      pull(Reporting_Month_Ref),
    "%m-%Y"
  )
  
  updatePickerInput(session, "selectedMonth3",
                    choices = picker_choices_breakdown,
                    selected = picker_choices_breakdown[length(picker_choices_breakdown)])
  
}




update_picker_choices_sql <- function(session, service_input_summary, service_input_site, service_input_breakdown){
  
  conn <- dbConnect(drv = odbc::odbc(),  ## Create connection for updating picker choices
                    dsn = "OAO Cloud DB") 
  mdf_tbl <- tbl(conn, "BSC_METRICS_FINAL_DF") # Create link to mdf table in Oracle
  # Summary tab month drop down update
  #service_input_summary <- input$selectedService
  
  picker_choices_summary <- format(
    mdf_tbl %>%
      filter(SERVICE %in% service_input_summary) %>%
      select((REPORTING_MONTH) ) %>%
      distinct() %>% collect() %>%
      arrange((REPORTING_MONTH) ) %>%
      pull((REPORTING_MONTH) ),
    "%m-%Y"
  )
  
  updatePickerInput(session, "selectedMonth",
                    choices = picker_choices_summary,
                    selected = picker_choices_summary[length(picker_choices_summary)])
  
  # Site tab month drop down update
  #service_input_site <- input$selectedService2
  
  picker_choices_site <- format(
    mdf_tbl %>%
      filter(SERVICE %in% service_input_site) %>%
      select(REPORTING_MONTH) %>%
      distinct() %>% collect() %>%
      arrange(REPORTING_MONTH) %>%
      pull(REPORTING_MONTH),
    "%m-%Y"
  )
  
  updatePickerInput(session, "selectedMonth2",
                    choices = picker_choices_site,
                    selected = picker_choices_site[length(picker_choices_site)])
  
  # KPI Breakout tab month drop down update
  #service_input_breakdown <- input$selectedService3
  
  picker_choices_breakdown <- format(
    mdf_tbl %>%
      filter(SERVICE %in% service_input_breakdown) %>%
      select(REPORTING_MONTH) %>%
      distinct() %>% collect() %>%
      arrange(REPORTING_MONTH) %>%
      pull(REPORTING_MONTH),
    "%m-%Y"
  )
  
  updatePickerInput(session, "selectedMonth3",
                    choices = picker_choices_breakdown,
                    selected = picker_choices_breakdown[length(picker_choices_breakdown)])
  
}
