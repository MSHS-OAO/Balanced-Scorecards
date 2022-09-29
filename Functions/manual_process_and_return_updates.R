manual_process_and_return_updates <- function(df, service, manual_table_name, updated_user, FUN) {

  tryCatch({
    ##Summary Repo processing where FUN is the function specific service line
    manual_data <- FUN(df, updated_user)
    ##returns only updated rows by doing an anti_join of what is currently in the Summary  Repos
    manual_data <- return_updated_manual_data(service, manual_table_name, manual_data)
    
    flag <- 2
    
    #return flag and the updated rows
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
