update_picker_choices <- function(session){
  picker_choices <-  format(sort(unique(metrics_final_df$Reporting_Month_Ref)), "%m-%Y")
  updatePickerInput(session, "selectedMonth", choices = picker_choices, selected = picker_choices[length(picker_choices)])
  updatePickerInput(session, "selectedMonth2", choices = picker_choices, selected = picker_choices[length(picker_choices)])
  updatePickerInput(session, "selectedMonth3", choices = picker_choices, selected = picker_choices[length(picker_choices)])
}
