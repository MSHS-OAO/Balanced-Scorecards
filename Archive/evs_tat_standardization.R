evs_raw <- read_excel(paste0("J:/deans/Presidents/HSPI-PM/",
                             "Operations Analytics and Optimization/Projects/",
                             "System Operations/Balanced Scorecards Automation/",
                             "Data_Dashboard/Input Data Raw/EVS/",
                             "MSHS Normal Clean vs Iso Clean TAT_March21.xlsx")
)


# str_detect(NA, "gh")

evs_raw <- evs_raw %>%
  mutate(Hrs = ifelse(
           # First check to see if the letter h followed by a space is detected
           grepl("h\\s", TAT...4),
           # If hours are detected in the TAT, extract the characters before "h"
           # and convert to an integer
           as.integer(str_extract(TAT...4, "[0-9]*(?=h\\s)")),
           # Otherwise, set hours to 0
           0), 
         Mins = # Extract the numbers preceeding "m" and convert to integer
           as.integer(
             str_extract(TAT...4, "[0-9]+(?=m)")
           ),
         TATMin = Hrs*60 + Mins)

