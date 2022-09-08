user_format_error <- function(df) {

  user_format_error <- any(
    apply(X = df[, 3:ncol(df)],
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
      apply(X = df[, 3:ncol(df)],
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
