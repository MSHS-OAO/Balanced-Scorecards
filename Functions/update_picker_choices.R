update_picker_choices <- function(session){
  
  # Summary tab month drop down update
  service_input_summary <- input$selectedService
  
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
  service_input_site <- input$selectedService2
  
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
  service_input_breakdown <- input$selectedService3
  
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
  
  # picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
  # updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
  # updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
  # updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
}
