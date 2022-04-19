# Check data to make sure user entered data in correct format
# ie, number between 0 and 1, no spaces, percentage signs, etc.

manual_format_check <- function(manual_table){
  user_format_error <<- any(
    apply(X = manual_table[, 3:ncol(manual_table)],
          MARGIN = 2,
          function(x)
            # Determine if there are issues converting any user entries to numeric values
            # ie, if the user enters "%" or text, the entry will be converted to NA
            is.na(
              suppressWarnings(
                as.numeric(
                  str_replace_na(x, replacement = "0")
                )
              )
            )
    )
  ) |
    any(
      apply(X = manual_table[, 3:ncol(manual_table)],
            MARGIN = 2,
            function(x)
              # Determine if numeric value is greater than 1
              max(
                suppressWarnings(
                  as.numeric(
                    str_replace_na(x, replacement = "0")
                  )
                ), na.rm = TRUE
              ) > 1
      )
    )
  return(user_format_error)
}
