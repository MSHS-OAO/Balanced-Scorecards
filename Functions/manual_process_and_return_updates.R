manual_process_and_return_updates <- function(df, service, manual_table_name, updated_user, button_name) {
  
  
 
  tryCatch({
    if(manual_table_name == "security_events") {
      manual_data <- sec_events_dept_summary(df, updated_user)
      manual_data <- manual_data %>% filter(REPORTING_MONTH >= max(REPORTING_MONTH) %m-% months(8))
    }else {
      ##Summary Repo processing where FUN is the function specific service line
      manual_data <- to_summary_repos_form(df,service, updated_user)
      
    }
    flag <- 2
    ##returns only updated rows by doing an anti_join of what is currently in the Summary  Repos
    manual_data <- return_updated_manual_data(service, manual_table_name, manual_data)
    
    #return flag and the updated rows
    return_list <- list("flag" = flag, "updated_rows" = manual_data)
    return(return_list)
  },
  error = function(err){
    print("manual error")
    flag <- 1
    shinyjs::enable(button_name)
    if(isRunning()) {
      showModal(modalDialog(
        title = "Error",
        paste0("There seems to be an issue with the, ", service, " data entered."),
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
        print(paste0("There seems to be an issue with the, ", service, " data entered."))
    }
    
    return_list <- list("flag" = flag, "updated_rows" = manual_data)
    return(return_list)
})
}
