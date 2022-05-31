month_input <- "04-2022"
metrics_final_df_recent <- readRDS(paste0(home_path,"metrics_final_df_server_5-28.rds"))
metrics_final_df_recent <- metrics_final_df_recent %>%
  select(-Target,-Status,-Month) %>%
  drop_na(value_rounded)
# Code Starts ---------------------------------------------------------------------------------     

breakout_tab_metrics <- metric_mapping_breakout %>%
  select(-General_Group, -Metric_Name_Summary, -Display_Order)

# Create a data frame with units for each metric.
# Crosswalk this after current period, past average, and past breakout are merged to ensure all
# metrics with units are accurately represented
breakout_tab_metrics_units <- breakout_tab_metrics %>%
  select(Metric_Group, Metric_Name, Metric_Unit)

metric_group_order <- unique(breakout_tab_metrics$Metric_Group)

metric_name_order <- unique(breakout_tab_metrics$Metric_Name)

current_period <- as.Date(fast_strptime(month_input, "%m-%Y"), "%Y-%m-%d")
fiscal_year <- format(current_period,  "%Y")

# Try to do this the same was we do it in the Summary and Site tabs for consistency
# First crosswalk metrics to include and metrics_final_df
data <- left_join(breakout_tab_metrics,
                  metrics_final_df_recent,
                  by = c("Service",
                         "Metric_Group",
                         "Metric_Name"))

data <- data %>%
  filter(Reporting_Month_Ref <= current_period,
         Reporting_Month_Ref >= current_period - months(11)) %>%
  arrange(Service,Site, Metric_Group, Metric_Name,
          desc(Reporting_Month_Ref)) %>%
  distinct()

# Do we need this?
# This will be needed until duplicate monthly entries are corrected in metrics_final_df
months <- metrics_final_df_recent %>% 
  filter(Reporting_Month_Ref <= current_period) %>%
  distinct(Reporting_Month_Ref) %>%
  arrange(desc(Reporting_Month_Ref)) %>%
  mutate(id = row_number()) %>%
  filter(Reporting_Month_Ref >= current_period - months(11))

data <- left_join(data, months,
                  by = "Reporting_Month_Ref")

# Crosswalk with metric targets and determine status
data <- left_join(data,
                  target_mapping_analysis,
                  by = c("Service",
                         "Site",
                         "Metric_Group",
                         "Metric_Name",
                         "Metric_Name_Submitted"))

# Determine status based on status definitions
data <- data %>%
  mutate(Status = ifelse(is.na(Target), NA,
                         ifelse(between(value_rounded,
                                        Green_Start, Green_End),
                                "Green",
                                ifelse(between(value_rounded,
                                               Yellow_Start, Yellow_End),
                                       "Yellow",
                                       ifelse(between(value_rounded,
                                                      Red_Start, Red_End),
                                              "Red", NA))))) %>%
  select(-contains(c("_Start", "_End")), -Metric_Name_Submitted)

# # Selected Month/Year Metric
# # Current Period Table
current_site_breakdown_new <- data %>%
  filter(Reporting_Month_Ref == current_period) %>%
  select(Service,Site,Metric_Group, Metric_Name, Metric_Unit,
         value_rounded, Status, Target)

# Convert to data frame for color formatting
current_site_breakdown_new <- as.data.frame(current_site_breakdown_new)




# Previous 11 Months Summary      
past_avg_site_new <- data %>%
  filter(id >= 2) %>%
  group_by(Service, Site,Metric_Group, Metric_Name) %>%
  summarise(`Avg. of Past Months Shown` = mean(value_rounded,
                                               na.rm = TRUE))


# ## Breakdown of prior 11 months     
past_site_breakdown_new <- data %>%
  filter(id >= 2) %>%
  select(-Metric_Unit) %>%
  group_by(Service,Site,Metric_Group, Metric_Name,
           Reporting_Month_Ref) %>%
  summarise(value_rounded = mean(value_rounded, na.rm = TRUE)) %>%
  arrange(Reporting_Month_Ref) %>%
  mutate(Reporting_Month_Ref = format(
    as.Date(Reporting_Month_Ref, format = "%Y-%m-%d"),"%b-%Y")) %>%
  pivot_wider(names_from = Reporting_Month_Ref,
              values_from = value_rounded)

# Merge Current and Previous Months Breakdown
# Combine current reporting period with average from prior 11 months
breakdown_all_site_new <- merge(current_site_breakdown_new,
                                past_avg_site_new,
                                by = c("Metric_Group",
                                       "Metric_Name",
                                       "Service",
                                       "Site"),
                                all = TRUE)
# # Combine with monthly breakdown from prior 11 months
breakdown_all_site_new <- merge(breakdown_all_site_new,
                                past_site_breakdown_new,
                                by = c("Metric_Group",
                                       "Metric_Name",
                                       "Service",
                                       "Site"),
                                all = TRUE)


# Rename value_rounded column with selected reporting period
names(breakdown_all_site_new)[names(breakdown_all_site_new) == 'value_rounded'] <-
  format(as.Date(current_period, format = "%Y-%m-%d"),"%b-%Y")


# Create and Format Comparison Table
breakdown_all_site_new <- breakdown_all_site_new %>%
  mutate(
    # Format numbers based on metric unit
    across(where(is.numeric),
           .fns = function(x) {
             ifelse(Metric_Unit %in% "Dollar",
                    dollar(round(x)),
                    ifelse(Metric_Unit %in% "Percent",
                           percent(x, 0.1),
                           prettyNum(round(x, digits = 1),
                                     big.mark = ",")))
           }),
    # Remove metric_unit column
    Metric_Unit = NULL,
    # Replace NAs
    across(.cols = everything(),
           .fns = function(x) {
             str_replace(x,
                         pattern = paste("NA",
                                         "NaN",
                                         "NA%",
                                         "%NA",
                                         "NaN%",
                                         "$NaN",
                                         sep = "|"),
                         NA_character_)
           })) %>%
  replace(is.na(.), "-") %>%
  # Reorder rows based on Metric_Group and Metric_Name
  mutate(Metric_Group = factor(Metric_Group,
                               levels = metric_group_order,
                               ordered = TRUE),
         Metric_Name = factor(Metric_Name,
                              levels = metric_name_order,
                              ordered = TRUE)) %>%
  arrange(Metric_Group, Metric_Name) %>%
  mutate(Metric_Group = as.character(Metric_Group),
         Metric_Name = as.character(Metric_Name))


# Manually update Targets for budget related metrics
breakdown_all_site_new <- breakdown_all_site_new %>%
  mutate(Target = ifelse(Metric_Name %in% c("Variance to Budget"),
                         ">= Budget",
                         ifelse(str_detect(Metric_Name,
                                           "(Budget to Actual)"),
                                "<= Budget", Target)))

# Determine order metrics should appear
factor_ordering_new <- table(breakdown_all_site_new$Metric_Group)
factor_ordering_new <- factor_ordering_new[order(
  factor(names(factor_ordering_new),
         levels = metric_group_order))]



## Get the months in the df
month_included <- breakdown_all_site_new %>%
  select(-Metric_Name, -Metric_Group,
         -Status, -Target, -`Avg. of Past Months Shown`,-Site,-Service)

original_columns <- as.Date(sprintf("%s-01",colnames(month_included)), format= "%b-%Y-%d")

#Subtract 12 months from the latest month drop the day and add the first of the month back in
# NOTE: IS THIS LATEST MONTH OR EARLIEST MONTH?
latest_month_shown <- as.Date(paste0(
  format(original_columns[1] %m-% months(11), "%Y-%m"), "-01"),
  format = "%Y-%m-%d")

columns_being_removed <- which(original_columns < latest_month_shown)
columns_being_removed <- original_columns[columns_being_removed]
columns_being_removed <- format(columns_being_removed, "%b-%Y")


breakdown_all_site_new <- breakdown_all_site_new %>%
  select(-all_of(columns_being_removed))

### Add missing months
months_breakdown <-  breakdown_all_site_new %>%
  select(-Metric_Name, -Metric_Group,
         -Status, -Target, -`Avg. of Past Months Shown`,-Service,-Site)

months_breakdown <- as.Date(sprintf("%s-01", colnames(months_breakdown)),
                            "%b-%Y-%d")

complete_months <- seq.Date(min(months_breakdown), max(months_breakdown), by= 'month')

missing_months <- which(!(complete_months %in% months_breakdown))
missing_months <- as.character(format(complete_months[missing_months], "%b-%Y"))

# Do we need to do anything to format the order this occurs in?
breakdown_all_site_new[, missing_months] <- NA

#breakdown_all <- breakdown_all %>% relocate(`Aug-2021`, .before = `Mar-2021`) ##to test ordering

# QUESTION: Can any of this be simplified?

subset_data <- breakdown_all_site_new[, 8:ncol(breakdown_all_site_new)]

date_names <- sprintf("%s-01",colnames(subset_data))
colnames(subset_data) <- date_names

dates_order <- as.Date(names(subset_data), format = "%b-%Y-%d")
subset_data <- subset_data[order(dates_order)]


breakdown_all_site_new <- breakdown_all_site_new[, 1:7]
breakdown_all_site_new <- bind_cols(breakdown_all_site_new, subset_data)

breakdown_all_cols <- colnames(breakdown_all_site_new)[8:ncol(breakdown_all_site_new)]
breakdown_all_cols <- format(as.Date(breakdown_all_cols, "%b-%Y-%d"), "%b-%Y")
colnames(breakdown_all_site_new)[8:ncol(breakdown_all_site_new)] <- breakdown_all_cols


names(breakdown_all_site_new)[names(breakdown_all_site_new) == 'Metric_Name'] <- 'Metric'
names(breakdown_all_site_new)[names(breakdown_all_site_new) == 'Metric_Group'] <- 'Metric Group'
names(breakdown_all_site_new)[19] <- 'Avg. of Past Months Shown'



write_xlsx(breakdown_all_site_new, 'BreakdownDownAllSites.xlsx')
