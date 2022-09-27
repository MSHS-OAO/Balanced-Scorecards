manual_process_and_return_updates <- function(df, service, manual_table_name, updated_user, FUN) {

  tryCatch({
    manual_data <- FUN(df, updated_user)
    manual_data <- return_updated_manual_data(service, manual_table_name, manual_data)
    
    flag <- 2
    
    if(isRunning()) {
      showModal(modalDialog(
        title = "Success",
        paste0("The ", service, " data has been submitted successfully."),
        easyClose = TRUE,
        footer = NULL
      ))
    } else{
      print(paste0("The ", service, " data has been submitted successfully."))
    }
    
    return_list <- list("flag" = flag, "updated_rows" = manual_data)
    return(return_list)
  },
  error = function(err){
    flag <- 1
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
