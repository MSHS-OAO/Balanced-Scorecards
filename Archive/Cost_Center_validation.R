key_vol_mapping <- key_vol_mapping %>% mutate(Service = ifelse(grepl("Radiology", CORPORATE.SERVICE.LINE), "Imaging",
                                                               ifelse(grepl("Biomed", CORPORATE.SERVICE.LINE), "Biomed / Clinical Engineering",
                                                                      ifelse(CORPORATE.SERVICE.LINE == "Support Services - Engineering", "Engineering",
                                                                             ifelse(CORPORATE.SERVICE.LINE == "Support Services - Environmental Services", "Environmental Services",
                                                                                    ifelse(CORPORATE.SERVICE.LINE == "Support Services - Food Services", "Food Services",
                                                                                           ifelse(grepl("Nursing", CORPORATE.SERVICE.LINE), "Nursing",
                                                                                                  ifelse(CORPORATE.SERVICE.LINE == "Support Services - Patient Transport", "Patient Transport",
                                                                                                         ifelse(CORPORATE.SERVICE.LINE == "Support Services - Security", "Security", NA
                                                                                                         )
                                                                                                  )
                                                                                           )
                                                                                    )
                                                                             )
                                                                      )
                                                               )
)
) %>%
  filter(!is.na(Service)) %>%
  filter(FTE.TREND == 1)

key_vol_mapping <- key_vol_mapping %>% mutate(COST.CENTER = ifelse(nchar(COST.CENTER) == 7, paste0("0", COST.CENTER), COST.CENTER))

key_vol_mapping <- key_vol_mapping[,c("CORPORATE.SERVICE.LINE", "COST.CENTER")]

# cost_mapping <- read_excel("tests/COATranslationData.xlsx")
# cost_mapping <- cost_mapping[,c("Legacy CostCenter/Fund", "Legacy Costcenter/Fund Description")]
# cost_mapping_final <- left_join(key_vol_mapping, cost_mapping, by = c("COST.CENTER" = "Legacy CostCenter/Fund")) %>%
#   select(COST.CENTER, `Legacy Costcenter/Fund Description`, CORPORATE.SERVICE.LINE) %>%
#   filter(!is.na(COST.CENTER))

#ot_raw_data <- read_excel("tests/2022 OT data v2.xlsx")
ot_raw_data <- raw_data
ot_raw_data <- ot_raw_data %>% filter(`Discharge Fisc Year-Period` == "2022-12")
ot_raw_data_geac <- ot_raw_data %>% filter(nchar(CostCenter) == 7) %>% mutate(CostCenter = paste0("0", CostCenter))

ot_raw_data_ebs <- ot_raw_data %>% filter(nchar(CostCenter) == 4) %>% mutate(`Location Code` = ifelse(nchar(`Location Code`) == 1, paste0("0", `Location Code`), `Location Code`)) %>%
  mutate(CostCenter = paste0(COFT, `Location Code`, CostCenter))

mapped_geac_cc <- inner_join(ot_raw_data_geac, key_vol_mapping, by = c("CostCenter" = "COST.CENTER")) %>%
  filter(!is.na(Service))

mapped_ebs_cc <- inner_join(ot_raw_data_ebs, key_vol_mapping, by = c("CostCenter" = "COST.CENTER")) %>%
  filter(!is.na(Service))
