manual_table_month_order <- function(manual_repo){
  data <- manual_repo
  
  data2_months <- as.Date(paste0(colnames(data %>%
                                            select(-Site, -Metric)),
                                 "-01"),
                          format = "%m-%Y-%d")
  max_month_2 <- as.Date(
    paste0(format(Sys.Date() %m-% months(1), "%m-%Y"), "-01"),
    format = "%m-%Y-%d")
  
  complete_months_2 <- seq.Date(from = min(data2_months),
                                to = max_month_2,
                                by = "month")
  
  # Move this after complete_months in case multiple months of data is missing
  months_to_drop_2 <- data2_months[which(
    data2_months < max_month_2 %m-% months(6)
  )
  ]
  
  missing_months_2 <- complete_months_2[which(!(complete_months_2 %in% data2_months))]
  missing_months_2 <- format(missing_months_2, "%m-%Y")
  
  data[, missing_months_2] <- NA_character_
  
  tbl <- data %>%
    pivot_longer(cols = !contains(c("Site", "Metric")),
                 names_to = "Month",
                 values_to = "value") %>%
    mutate(Month = as.Date(paste0(Month, "-01"),
                           format = "%m-%Y-%d")) %>%
    arrange(Site, Month) %>%
    filter(!(Month %in% months_to_drop_2)) %>%
    mutate(Month = format(Month, "%m-%Y")) %>%
    pivot_wider(names_from = Month,
                values_from = value,
                names_sort = FALSE)
  
  
  
  return(tbl)
}
