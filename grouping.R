# Function to create icd groupings based on NRS summary list cause of death
library(dplyr)
library(fuzzyjoin)

icd_grouping <- read_xlsx("/conf/LIST_analytics/West Dunbartonshire/AdHoc/Deaths/icd_groupings.xlsx")
icd_grouping <- icd_grouping %>% 
  filter(!is.na(Regex))

# trying on real data
cause_deaths_matched <- cause_deaths %>%
  regex_left_join(icd_grouping, by = c("code1" = "regex")) |> 
  select(-regex)



