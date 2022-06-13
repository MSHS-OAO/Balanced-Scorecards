
service_input <- unique(metrics_final_df$Service)
month_input <- "04-2022"
site_input <- unique(metrics_final_df$Site)



# Code Starts ---------------------------------------------------------------------------------     
summary_tab_metrics <- metric_mapping_summary_site %>%
  filter(Service %in% service_input) %>%
  select(-General_Group, -Display_Order)

summary_metric_group_order <- unique(summary_tab_metrics$Metric_Group)

summary_metric_name_order <- unique(summary_tab_metrics$Metric_Name_Summary)

# Target mappings ---------------------
# Subset target mapping to select Metric_Group and  Metric_Names to be 
# displayed in status indicator section based on the selected service line
status_section_metrics <- target_mapping_analysis %>%
  filter(Service %in% service_input) %>%
  select(Service, Metric_Group, Metric_Name) %>%
  distinct()

# Subset target mapping to select Targets and Status Definitions for selected service line
metric_targets_status <- target_mapping_analysis %>%
  filter(Service %in% service_input)

current_period <- as.Date(fast_strptime(month_input, "%m-%Y"), "%Y-%m-%d")
fiscal_year <- format(current_period,  "%Y")

# Subset data for selected servicel line based on metrics to be included in Site tab
data <- left_join(summary_tab_metrics,
                  metrics_final_df,
                  by = c("Service",
                         "Metric_Group",
                         "Metric_Name"))

# Filter based on selected time period
data <- data %>%
  filter(Site %in% site_input &
           Reporting_Month_Ref <= current_period &
           Reporting_Month_Ref >= current_period - months(11)) %>%
  distinct() %>%
  arrange(Site, Metric_Group, Metric_Name_Summary, Metric_Name, 
          desc(Reporting_Month_Ref)) %>%
  group_by(Site, Metric_Group, Metric_Name_Summary, Metric_Name) %>%
  mutate(id = row_number())

# Crosswalk with metric targets and status definitions
data <- left_join(data,
                  metric_targets_status,
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

# get the max date for pe data ----
maxdata <- metrics_final_df %>% group_by(Service)%>%
  filter(Metric_Group == "Patient Experience") %>%
  mutate(max_pe = max(Reporting_Month_Ref),na.rm=TRUE) %>%
  select(Service,max_pe) %>%
  distinct()

data %>%
# Selected Month/Year Metric
# Current Period Table
current_breakdown <- data %>%
  ungroup() %>%
  filter(Reporting_Month_Ref == current_period) %>%
  mutate(`Current Period` = ifelse(str_detect(Premier_Reporting_Period,
                                              "/"), 
                                   paste0("Rep. Pd. Ending ",
                                          Premier_Reporting_Period),
                                   Premier_Reporting_Period)) %>%
  select(Metric_Group, Metric_Name_Summary, Site,
         value_rounded, Status, Target, Metric_Unit)

# # Not sure if this code is needed anymore
# current_breakdown <- as.data.frame(current_breakdown)

## Create target traffic lights for current month metrics
current_breakdown <- current_breakdown %>%
  ungroup() %>%
  mutate(Status = ifelse(Status %in% c("Red", "Yellow", "Green"),
                         paste0('<div style="text-align:center">',
                                '<span style="color:',
                                Status,'">',
                                fa('fas fa-circle'),
                                '</span>',
                                '</div>'),
                         paste0('<div style="text-align:center">',
                                '-',
                                '</div>')))

# Previous Months Summary
## Past 12 months of Summary 
past_avg <- data %>%
  filter(id >= 2) %>%
  group_by(Metric_Group, Metric_Name_Summary, Site) %>%
  summarise(`Avg. of Past Months Shown` = round(
    mean(value_rounded, na.rm = TRUE), 2))

## Past 12 months of Breakout
past_breakdown <- data %>%
  filter(id >= 2) %>%
  group_by(Metric_Group, Metric_Name_Summary,
           Site, Reporting_Month_Ref) %>%
  summarise(value_rounded = round(
    mean(value_rounded, na.rm = TRUE), 2)) %>%
  arrange(Reporting_Month_Ref) %>%
  mutate(Reporting_Month_Ref = format(
    as.Date(Reporting_Month_Ref, format = "%Y-%m-%d"),"%b-%Y")) %>%
  pivot_wider(names_from = Reporting_Month_Ref,
              values_from = value_rounded)

# Merge Current and Previous Months Breakdown
breakdown_all <- merge(current_breakdown, past_avg,
                       by = c("Metric_Group",
                              "Metric_Name_Summary",
                              "Site"))
breakdown_all <- merge(breakdown_all, past_breakdown,
                       by = c("Metric_Group",
                              "Metric_Name_Summary",
                              "Site"))

# Rename column "value rounded" column with current period selected
names(breakdown_all)[names(breakdown_all) == 'value_rounded'] <-
  format(as.Date(current_period, format = "%Y-%m-%d"),"%b-%Y")

breakdown_all <- breakdown_all %>%
  mutate(across(where(is.numeric),
                .fns = function(x) {
                  ifelse(Metric_Unit %in% "Dollar",
                         dollar(round(x)),
                         ifelse(Metric_Unit %in% "Percent",
                                percent(x, 0.1),
                                prettyNum(round(x, digits = 1),
                                          big.mark = ",")))
                }),
         Metric_Unit = NULL)


# Create and Format Comparison Table
# NOTE: Roll this into single dplyr with above
breakdown_all <- breakdown_all %>%
  mutate_all(function(x) str_replace(x,
                                     pattern = paste("NA",
                                                     "NaN",
                                                     "NA%",
                                                     "%NA",
                                                     "NaN%",
                                                     "$NaN",
                                                     sep = "|"),
                                     NA_character_)) %>%
  replace(is.na(.), "-")



breakdown_all <- breakdown_all %>%
  mutate(
    # Format budget related metrics
    Target = ifelse(Metric_Name_Summary %in% c("Total Revenue to Budget Variance"),
                    ">= Budget",
                    ifelse(Metric_Name_Summary %in% c("Budget to Actual"),
                           "<= Budget", Target)),
    # Set columns as factors and rearrange dataframe
    Metric_Name_Summary = factor(
      Metric_Name_Summary,
      levels = summary_metric_name_order,
      ordered = TRUE)) %>%
  arrange(#Metric_Group,
    Metric_Name_Summary,
    Site) %>%
  mutate(#Metric_Group = as.character(Metric_Group),
    Metric_Name_Summary = as.character(Metric_Name_Summary))

factor_ordering <- table(breakdown_all$Metric_Name_Summary)
factor_ordering <- factor_ordering[order(factor(names(factor_ordering),
                                                levels = summary_metric_name_order))]

# NOTE: MAY BE WORTH REVIEWING THE LOGIC IN THESE LINES
## Get the months in the df
month_included <- breakdown_all %>%
  select(-Metric_Name_Summary,
         -Metric_Group,
         -Site,
         -Status,
         -Target,
         -`Avg. of Past Months Shown`)

original_columns <- as.Date(sprintf("%s-01",colnames(month_included)), format= "%b-%Y-%d")

#Subtract 12 months from the latest month drop the day and add the first of the month back in
latest_month_shown <- as.Date(
  paste0(format(original_columns[1] %m-% months(11), "%Y-%m"), "-01"),
  format = "%Y-%m-%d")

columns_being_removed <- which(original_columns < latest_month_shown)
columns_being_removed <- original_columns[columns_being_removed]
columns_being_removed <- format(columns_being_removed, "%b-%Y")


breakdown_all <- breakdown_all %>%
  select(-all_of(columns_being_removed))

### Add missing months
months_breakdown <- breakdown_all %>%
  select(-Metric_Name_Summary,
         -Metric_Group,
         -Site,
         -Status,
         -Target,
         -`Avg. of Past Months Shown`)

months_breakdown <- as.Date(sprintf("%s-01",colnames(months_breakdown)),
                            "%b-%Y-%d")

complete_months <- seq.Date(min(months_breakdown), max(months_breakdown),
                            by= 'month')

missing_months <- which(!(complete_months %in% months_breakdown))
missing_months <- as.character(format(complete_months[missing_months],
                                      "%b-%Y"))

breakdown_all[,missing_months] <- NA

# Do we need any code to account for the order of the dates?

#breakdown_all <- breakdown_all %>% relocate(`Aug-2021`, .before = `Mar-2021`) ##to test ordering

subset_data <- breakdown_all[,8:ncol(breakdown_all)]

date_names <- sprintf("%s-01",colnames(subset_data))
colnames(subset_data) <- date_names

dates_order <- as.Date(names(subset_data), format = "%b-%Y-%d")
subset_data <- subset_data[order(dates_order)]


breakdown_all <- breakdown_all[,1:7]
breakdown_all <- bind_cols(breakdown_all,subset_data)

breakdown_all_cols <- colnames(breakdown_all)[8:ncol(breakdown_all)]
breakdown_all_cols <- format(as.Date(breakdown_all_cols, "%b-%Y-%d"), "%b-%Y")
colnames(breakdown_all)[8:ncol(breakdown_all)] <- breakdown_all_cols
