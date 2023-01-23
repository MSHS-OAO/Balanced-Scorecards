to_summary_repos_form <- function(df,service, updated_user) {
  df <- df %>% pivot_longer(c(-Site, -Metric), names_to = "REPORTING_MONTH", values_to = "VALUE")
  df <- df %>% mutate(REPORTING_MONTH = as.Date(paste0(REPORTING_MONTH, "-01"),"%m-%Y-%d"),
                      UPDATED_USER = updated_user,
                      PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
                      SERVICE = service,
                      VALUE = as.numeric(VALUE)
                      ) %>%
              rename(SITE = Site,
                     METRIC_NAME_SUBMITTED = Metric)
  return(df)
}
