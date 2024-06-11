
SuccessUI <- function(id) {
  ns <- NS(id)
  tagList(
    if(isRunning()) {
      showModal(modalDialog(
        title = "Success",
        paste0("The data has been submitted successfully."),
        easyClose = TRUE,
        footer = NULL
      ))
    } else{
      print(paste0("The data has been submitted successfully."))
    }
    
  )
}

ErrorUI <- function(id) {
  ns <- NS(id)
  tagList(
    if(isRunning()) {
      showModal(modalDialog(
        title = "Error",
        paste0("There was an issue submitting the data."),
        easyClose = TRUE,
        footer = NULL
      ))
    } else{
      print(paste0("There was an issue submitting the data."))
    }
    
  )
}

