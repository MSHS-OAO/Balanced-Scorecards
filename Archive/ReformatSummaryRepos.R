ImagingIR <- read_excel(paste0(home_path,"Summary Repos Production 7-20/Imaging-IR.xlsx"))
ImagingDR <- read_excel(paste0(home_path,"Summary Repos Production 7-20/Imaging-DR.xlsx"))
EVS <- read_excel(paste0(home_path,"Summary Repos Production 7-20/TAT - EVS.xlsx"))
Nursing <- read_excel(paste0(home_path,"Summary Repos Production 7-20/Nursing.xlsx"))
BiomedKPIS <- read_excel(paste0(home_path,"Summary Repos Production 7-20/KPIs.xlsx"))
BiomedDI <- read_excel(paste0(home_path,"Summary Repos Production 7-20/DisruptionsAndIssuesMonthly.xlsx"))

ImagingIR <- ImagingIR %>%
  rename(SITE = Site,
         SERVICE = Service,
         METRIC_NAME_SUBMITTED = Metric_Name_Submitted,
         VALUE = value_rounded) %>%
  mutate(REPORTING_MONTH = format(Reporting_Month_Ref,"%Y-%m-%d"),
         PREMIER_REPORTING_PERIOD = format(Reporting_Month_Ref,"%b %Y"),
         UPDATED_TIME = as.character(Sys.time()),
         UPDATED_USER = NA_character_) %>%
  select(SERVICE, 
         SITE, 
         REPORTING_MONTH,
         PREMIER_REPORTING_PERIOD, 
         METRIC_NAME_SUBMITTED,
         VALUE,
         UPDATED_TIME,
         UPDATED_USER)

ImagingDR <- ImagingDR %>%
  rename(SITE = Site,
         SERVICE = Service,
         METRIC_NAME_SUBMITTED = Metric_Name_Submitted,
         VALUE = value_rounded) %>%
  mutate(REPORTING_MONTH = as.Date(paste(Month,"01"), format="%b %Y %d"),
         PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
         REPORTING_MONTH = format(REPORTING_MONTH,"%Y-%m-%d"),
         UPDATED_TIME = as.character(Sys.time()),
         UPDATED_USER = NA_character_) %>%
  select(SERVICE, 
         SITE, 
         REPORTING_MONTH,
         PREMIER_REPORTING_PERIOD, 
         METRIC_NAME_SUBMITTED,
         VALUE,
         UPDATED_TIME,
         UPDATED_USER)


EVS <- EVS %>%
  pivot_longer(cols = c(-Service,-Site,-Month),
               names_to = "METRIC_NAME_SUBMITTED",
               values_to = "VALUE") %>%
  rename(SERVICE = Service,
         SITE = Site,
         REPORTING_MONTH = Month) %>%
  mutate(PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
         REPORTING_MONTH = format(REPORTING_MONTH,"%Y-%m-%d"),
         UPDATED_TIME = as.character(Sys.time()),
         UPDATED_USER = NA_character_) %>%
  select(SERVICE, 
         SITE, 
         REPORTING_MONTH,
         PREMIER_REPORTING_PERIOD, 
         METRIC_NAME_SUBMITTED,
         VALUE,
         UPDATED_TIME,
         UPDATED_USER)

Nursing <- Nursing %>%
  rename( REPORTING_MONTH = Month,
          SERVICE = Service,
          SITE = Site) %>%
  mutate(REPORTING_MONTH = as.Date(REPORTING_MONTH,"%Y-%m-%d"), #get rid of parse_date_time
         SERVICE = "Nursing",
         `All Falls (per 1,000 PD)` = `All Falls`/(`Denominator (Patient Days)`/1000),
         `Falls with Injury (per 1,000 PD)` = `Falls with Injury`/(`Denominator (Patient Days)`/1000),
         `HAPU (per 1,000 PD)` = HAPU/(`Denominator (Patient Days)`/1000),
         PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
         REPORTING_MONTH = format(REPORTING_MONTH,"%Y-%m-%d")) %>%
  select(-`Denominator (Patient Days)`) %>%
  pivot_longer(cols = c(-REPORTING_MONTH,-SERVICE,-SITE,-PREMIER_REPORTING_PERIOD),
               names_to = "METRIC_NAME_SUBMITTED",
               values_to = "VALUE") %>%
  mutate(UPDATED_TIME = as.character(Sys.time()),
         UPDATED_USER = NA_character_)%>%
  select(SERVICE, 
         SITE, 
         REPORTING_MONTH,
         PREMIER_REPORTING_PERIOD, 
         METRIC_NAME_SUBMITTED,
         VALUE,
         UPDATED_TIME,
         UPDATED_USER)


BiomedKPIS <- BiomedKPIS %>%
  rename( REPORTING_MONTH = Month,
          SERVICE = Service,
          SITE = Site,
          METRIC_NAME_SUBMITTED = Metric,
          VALUE = Number) %>%
  mutate(PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
         REPORTING_MONTH = format(REPORTING_MONTH,"%Y-%m-%d"),
         UPDATED_TIME = as.character(Sys.time()),
         UPDATED_USER = NA_character_)%>%
  select(SERVICE, 
         SITE, 
         REPORTING_MONTH,
         PREMIER_REPORTING_PERIOD, 
         METRIC_NAME_SUBMITTED,
         VALUE,
         UPDATED_TIME,
         UPDATED_USER)

BiomedDI <- BiomedDI %>%
  rename( REPORTING_MONTH = Month,
          SERVICE = Service,
          SITE = Site,
          VALUE = `Total Disruptions/Issues`) %>%
  mutate(PREMIER_REPORTING_PERIOD = format(REPORTING_MONTH,"%b %Y"),
         REPORTING_MONTH = format(REPORTING_MONTH,"%Y-%m-%d"),
         UPDATED_TIME = as.character(Sys.time()),
         UPDATED_USER = NA_character_,
         METRIC_NAME_SUBMITTED = "Total Disruptions/Issues")%>%
  select(SERVICE, 
         SITE, 
         REPORTING_MONTH,
         PREMIER_REPORTING_PERIOD, 
         METRIC_NAME_SUBMITTED,
         VALUE,
         UPDATED_TIME,
         UPDATED_USER)

ImagingIR <- write_xlsx(ImagingIR,paste0(home_path,"Summary Repos for Database/Imaging-IR.xlsx"))
ImagingDR <- write_xlsx(ImagingDR,paste0(home_path,"Summary Repos for Database/Imaging-DR.xlsx"))
EVS <- write_xlsx(EVS,paste0(home_path,"Summary Repos for Database/TAT - EVS.xlsx"))
Nursing <- write_xlsx(Nursing,paste0(home_path,"Summary Repos for Database/Nursing.xlsx"))
BiomedKPIS <- write_xlsx(BiomedKPIS,paste0(home_path,"Summary Repos for Database/KPIs.xlsx"))
BiomedDI <- write_xlsx(BiomedDI,paste0(home_path,"Summary Repos for Database/DisruptionsAndIssuesMonthly.xlsx"))

