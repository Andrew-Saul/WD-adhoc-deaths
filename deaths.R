##########################################

# West Dunbartonshire Deaths Analysis

# Written by: Csilla Scharle
# Date: 28/01/2025
# Written/run on Posit Workbench, R 4.1.2
# Description: Extracts deaths among West Dunbartonshire residents for
# latest available year, produces breakdowns by age group and cause of death.
# Approximate run time:
# Approximate memory usage:

##########################################

### 1. Housekeeping ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(odbc,
               dplyr,
               readr,
               readxl,
               tidylog,
               tidyr,
               stringr,
               janitor,
               arrow,
               writexl,
               openxlsx,
               phsmethods,
               ggplot2,
               zoo,
               fuzzyjoin)
# # Load Packages
# library(odbc)
# #library(haven)
# library(dplyr)
# library(readr)
# library(readxl)
# library(tidylog)
# library(tidyr)
# library(stringr)
# library(janitor)
# library(arrow)
# #library(magrittr)
# #library(lubridate)
# library(writexl)
# library(phsmethods)
# library(ggplot2)
# library(zoo)
# library(fuzzyjoin)

# Filepaths

deaths_folder <- "/conf/LIST_analytics/West Dunbartonshire/AdHoc/Deaths/"
lookups_folder <- "/conf/linkage/output/lookups/Unicode/"

# Lookups

# icd codes (3-digit)
icd_codes3 <- read_csv(paste0(lookups_folder, "National Reference Files/icd10.csv")) %>% 
  clean_names() %>% 
  filter(v7 == 3) %>% # this selects 3-digit code headings
  select(code1, description)


# nrs cause of deaths groupings
nrs_summary_list <- read_xlsx(paste0(deaths_folder, "vital-events-refernce-tables-2023-all-tables.xlsx"),
                              sheet = "6.01", range = "A3:C150") %>%
  clean_names() %>%
  filter(!is.na(icd10_summary_list)) %>%
  select(icd10_summary_list, cause_of_death)

icd_chapters <- nrs_summary_list %>%
  filter(str_detect(cause_of_death, "\\."))

#creating a temp LUT with all icd3 codes but not with all family codes
# family code ranges extracted and for icd3 testing 
temp_icd_LUT <- read_xlsx(paste0(deaths_folder, "vital-events-refernce-tables-2023-all-tables.xlsx"),
                      sheet = "6.04", range = "A3:C1729") %>%
  clean_names() %>%
  filter(icd_10_3_digit_code != "Total") %>%
  distinct() %>%
  mutate(icd3 = substring(icd_10_3_digit_code, 1, 3),
         code_first = str_extract(icd_10_family, "^[A-Za-z0-9]+"),
         code_last = str_extract(icd_10_family, "(?<=-).{3}")) %>% 
  select(-c(icd_10_3_digit_code, icd_10_chapter)) %>% 
  full_join(icd_codes3, join_by(icd3 == code1)) %>% 
  arrange(icd3) 
 #select(icd3, "icd3_description" = icd_10_3_digit_code, icd_10_family, icd_10_chapter) 

# nrs_summary_without_chapters <- nrs_summary_list %>%
#   anti_join(icd_chapters)

# read in icd groupings with regex 
icd_grouping <- read_xlsx(paste0(deaths_folder, "icd_groupings.xlsx"))
icd_grouping <- icd_grouping %>% 
  clean_names() %>%
  filter(!is.na(regex)) %>% # removes rows containing NA values in regex column
  select(regex, description)

# 1 letter codes for deaths
codes_1_letter <- readxl::read_xlsx('/conf/LIST_analytics/West Hub/ICD10 first letter codes.xlsx', 
                                    col_names = FALSE) %>% 
  rename(diag_1_letter = `...1`, condition_catergory = `...2`)

### 2. SMRA extraction ----

# Connect to SMRA tables using odbc connection
# The suppressWarnings function prevents your password from appearing in the 
# console if the connection is unsuccessful
channel <- suppressWarnings(
  dbConnect(odbc(),
            dsn = "SMRA",
            uid = Sys.info()[['user']],
            pwd = .rs.askForPassword("What is your LDAP password?"),
            SRVR="SMRA.nss.scot.nhs.uk",
            port="1527",
            host = "nssstats01.csa.scot.nhs.uk",
            SVC = "SMRA.nss.scot.nhs.uk"))

# odbcPreviewObject(channel, table="ANALYSIS.GRO_DEATHS_C", rowLimit=0)

deaths_extract_c <- as_tibble(dbGetQuery(channel, statement = "SELECT UPI_NUMBER, YEAR_OF_REGISTRATION, DATE_OF_REGISTRATION, AGE,
                                UNDERLYING_CAUSE_OF_DEATH, POSTCODE, COUNCIL_AREA, COUNCIL_AREA_2019, SEX
                                FROM ANALYSIS.GRO_DEATHS_C
                                WHERE YEAR_OF_REGISTRATION >= 2014")) %>%
  
  # 'Clean' the variable names for a consistent naming style
  clean_names()

# Close odbc connection
dbDisconnect(channel)

# save extract
#write_rds(deaths_extract_c, paste0(deaths_folder, "temp_extract.rds"))
#read extract
#deaths_extract_c <- read_rds(paste0(deaths_folder, "temp_extract.rds"))

### 3. Populations up until 2023 ----

# ------  age_band 0-24, 25-49, 50-74, 75+ ----------------------------------
# ------ age_group 0, 1-4, 5-9, 10-14, ... 

# read in population estimates
pops <- read_rds(paste0(lookups_folder, "Populations/Estimates/HSCP2019_pop_est_1981_2023.rds")) %>%
  filter(year %in% 2014:2023 & hscp2019name == "West Dunbartonshire") %>%
  group_by(year, hscp2019name, age) %>%
  summarise(pop = sum(pop), .groups = "drop")

# calculate pops across age bands
pops_age_band <- pops %>%
  mutate(age_band = create_age_groups(age, 0, 99, 25)) %>% 
  summarise(pop = sum(pop), .by = c(year, hscp2019name, age_band))

# calculate pop for 5 year aggregate
pops_age_band_5y <- pops_age_band %>%
  filter(year %in% 2019:2023) %>%
  summarise(pop = sum(pop), .by = c(hscp2019name, age_band)) %>%
  mutate(period = "2019-2023")

# calculate for more granular age groups matching NRS pub
pops_age_group <- pops %>%
  mutate(age_group = create_age_groups(age, 0, 90, 5)) %>% 
  mutate(age_group = case_when(age == 0 ~ "0", 
                               age != 0 & age_group == "0-4" ~ "1-4",
                               .default = age_group)
  ) %>%
  summarise(pop = sum(pop), .by = c(year, hscp2019name, age_group))

### 4. Analysis - Deaths by age band ----

#filter relevant years and council area
# using NRS council area field (captures records with missing postcode)
wd_deaths_pre <- deaths_extract_c %>% 
  filter(council_area == "07" & year_of_registration %in% 2014:2023) 

# adds family to icd3 codes without - requires further processing as 
# values have incorrect families assigned
icd_LUT_1stpass <- wd_deaths_pre %>% 
  mutate(icd3 = substr(underlying_cause_of_death, 1, 3)) %>% 
  select(icd3) %>% 
  distinct(icd3) %>% 
  full_join(temp_icd_LUT) %>% 
  arrange(icd3) %>% 
  fill(c(icd_10_family, code_first, code_last), .direction = "downup") %>% 
  mutate(icd_10_family = 
            case_when(
           #   icd3 < first ~ "error",
           #   (icd3 > second & icd3 < lead(first)) ~ NA ,
           #   icd3 > second ~ lead(icd_10_family),
           #   .default = icd_10_family
           # )
             #icd3 < code_first ~ "error",
             (between(icd3, code_first, code_last) & is.na(icd_10_family)) ~ lag(icd_10_family), # use previous entry
             (icd3 > code_last & icd3 < lead(code_first) & !is.na(lead(code_first))) ~ NA_character_ , # not contained in a family range
             (icd3 > code_last & !is.na(lead(icd_10_family))) ~ lead(icd_10_family), # assumes if > lead(code_first) will be contained in next family range
             .default = (icd_10_family)
             )
  ) 

# some families miscategorised because values not contained in wd_deaths.
# rerun lookup table - re-extract icd begin and end codes and check. if not between then give family and chapter values NA
icd_LUT <- 
  icd_LUT_1stpass %>% 
  select(-c(code_first, code_last)) %>% 
  mutate(code_first = str_extract(icd_10_family, "^[A-Za-z0-9]+"),
         code_last = str_extract(icd_10_family, "(?<=-).{3}")) %>% 
  mutate(icd_10_family = if_else(icd3>code_last, NA, icd_10_family))
  

##########################################################################################################################
wd_deaths <- 
  wd_deaths_pre %>% 
  rename(year = year_of_registration) %>% 
  mutate(age_band = create_age_groups(age, 0, 99, 25)) %>% 
  mutate(age_group = create_age_groups(age, 0, 90, 5)) %>% 
  mutate(age_group = case_when(age == 0 ~ "0", 
                               age != 0 & age_group == "0-4" ~ "1-4",
                               .default = age_group)
  )

# Number of annual deaths by age band
age_band_deaths <- wd_deaths %>%
 # mutate(age_band = create_age_groups(age, 0, 99, 25)) %>% 
  group_by(year, age_band) %>% 
  summarise(deaths = n()) %>% 
  ungroup() %>%
  full_join(pops_age_band) %>%
  mutate(deaths = if_else(is.na(deaths), 0, deaths)) %>%
  mutate(deaths_rate = deaths/pop*100000) %>%
  arrange(year, age_band)

# Number of deaths by age band 5 year rolling average
# calculate confidence intervals using Byer's method for rates
# https://fingertips.phe.org.uk/documents/APHO%20Tech%20Briefing%203%20Common%20PH%20Stats%20and%20CIs.pdf
age_band_deaths_5y <- age_band_deaths %>%
  arrange(age_band, year) %>%
  group_by(age_band) %>%
  mutate(deaths_5y = rollmeanr(deaths, 5, fill = NA),
         pop_5y = rollmeanr(pop, 5, fill = NA),
         deaths_5y_rate = deaths_5y/pop_5y*100000) %>%
  ungroup() %>%
  mutate(
    lowci = (deaths_5y * (1-1/9/deaths_5y - 1.96/3/sqrt(deaths_5y))^3) / (pop_5y) * 100000,
    upci = ((deaths_5y + 1) *(1 - 1/9/(deaths_5y + 1) + 1.96/3/sqrt(deaths_5y + 1))^3) / (pop_5y) * 100000
  ) %>%
  filter(!is.na(deaths_5y))

# # Number of annual deaths by age group
age_group_deaths <- wd_deaths %>%
  # mutate(age_group = create_age_groups(age, 0, 90, 5)) %>% 
  # mutate(age_group = case_when(age == 0 ~ "0", 
  #                              age != 0 & age_group == "0-4" ~ "1-4",
  #                              .default = age_group)
  # ) %>% 
  group_by(year, age_group) %>% 
  summarise(deaths = n()) %>% 
  ungroup() %>%
  full_join(pops_age_group) %>%
  mutate(deaths = if_else(is.na(deaths), 0, deaths)) %>%
  mutate(deaths_rate = deaths/pop*100000) %>%
  arrange(year, age_group)

### 5. Analysis - Deaths by cause grouping and age band ----

# Number of deaths by primary cause of death & age band
cause_deaths <- wd_deaths %>%
  mutate(icd3 = substr(underlying_cause_of_death, 1, 3)) %>% 
  left_join(icd_LUT, join_by(icd3)) %>% 
  mutate(icd_1digit = substring(icd3, 1, 1)) %>%
  mutate(icd_chapter = case_when(
    icd_1digit %in% c("A", "B") ~ "I.  Certain infectious and parasitic diseases",
    icd_1digit == "C" | str_detect(icd3, "D[0-4][0-9]") ~ "II.  Neoplasms",
    str_detect(icd3, "D[5-8][0-9]") ~ "III. Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
    icd_1digit == "E" ~ "IV. Endocrine, nutritional and metabolic diseases",
    icd_1digit =="F" ~ "V. Mental and behavioural disorders",
    icd_1digit %in% c("G", "H") ~ "VI-VIII.  Diseases of the nervous system and the sense organs",
    icd_1digit == "I" ~ "IX. Diseases of the circulatory system",
    icd_1digit == "J" ~ "X. Diseases of the respiratory system",
    icd_1digit == "K" ~ "XI. Diseases of the digestive system",
    icd_1digit == "L" ~ "XII. Diseases of the skin and subcutaneous tissue",
    icd_1digit == "M" ~ "XIII. Diseases of the musculoskeletal system and connective tissue",
    icd_1digit == "N" ~ "XIV. Diseases of the genitourinary system",
    icd_1digit == "O" ~ "XV. Pregnancy, childbirth and the puerperium",
    icd_1digit == "P" ~ "XVI. Certain conditions originating in the perinatal period",
    icd_1digit == "Q" ~ "XVII. Congenital malformations, deformations and chromosomal abnormalities",
    icd_1digit == "R" ~ "XVIII. Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
    icd_1digit %in% c("V", "W", "X", "Y") ~ "XX.  External causes of morbidity and mortality",
    icd_1digit == "U" ~ "XXII. Codes for Special Purposes"
  ))  %>%
  #regex_left_join(icd_grouping,  by = c("code1" = "regex")) %>%
  regex_full_join(icd_grouping,  by = c("icd3" = "regex")) %>% 
  select(-regex) %>%
  drop_na(year) %>% # remove entries not matched with icd_grouping
  mutate(description.y = case_when(
    underlying_cause_of_death == "Y870" ~ "Intentional self-harm",
    underlying_cause_of_death == "Y871" ~ "Assault",
    underlying_cause_of_death == "Y872" ~ "Event of undetermined intent",
    .default = description.y
  )) %>%
  mutate(nrs_sum_list = if_else(
    is.na(description.y), paste("Other in", icd_chapter), description.y
  )) %>%
  mutate(nrs_sum_list = if_else(
    icd_1digit == "C", "Malignant neoplasms", nrs_sum_list 
  )) %>% 
  mutate(icd_10_family_code = str_extract(icd_10_family, "^.*?\\.")) 



#####  Identified icd3 codes without family or chapter descriptions - C97
# unclassified_patient_codes <- 
#   cause_deaths %>% 
#   filter(is.na(icd_10_family)) %>% 
#   mutate(icd3 = code1,
#          description.x = str_to_title(description.x)) %>% 
#   unite("icd3_description", code1, description.x, sep = "    ") %>% 
#   select(icd3, icd3_description) %>% 
#   distinct() %>% 
#   drop_na() %>% 
#   bind_cols(icd_10_family = NA, 
#             icd_10_chapter = NA) %>% filter(icd3 == "C97") %>% 
#  mutate(icd_10_family = "C97 Malignant Neoplasms Of Independent (Primary) Multiple Sites",
#         icd_10_chapter = "II.  Neoplasms")

                                
  #         icd_description = )
  

  # fills in absent codes in nrs_icd3 df that exist in smr deaths df
  # df arrange in asc order by icd3 code.
  # if icd3 code is > than second value of code previoius entry, 
  # but < value of code for next entry then NA value - not classified
  # ie. for C97 code
  # nrs_icd3_LUT <- 
  #   split_icd_code(nrs_icd3) %>% 
  # bind_rows(unclassified_patient_codes) %>% 
  # drop_na(icd3) %>% 
  # arrange(icd3) %>% 
  # fill(c(first,second)) %>% 
  # mutate(icd_10_family = 
  #          case_when(
  #            icd3 < first ~ "error",
  #            (icd3 > second & icd3 < lead(first)) ~ NA ,
  #            icd3 > second ~ lead(icd_10_family),
  #            .default = icd_10_family)
  # ) %>%  
  # mutate(icd_10_chapter = 
  #          case_when(
  #            icd3 < first ~ "error",
  #            (icd3 > second & icd3 < lead(first)) ~ NA ,
  #            icd3 > second ~ lead(icd_10_chapter),
  #            .default = icd_10_chapter)
  # )  %>% 
  #   mutate(icd_10_family = if_else(is.na(icd_10_family), 
  #                                  "C97 Malignant Neoplasms Of Independent (Primary) Multiple Sites",
  #                                  icd_10_family),
  #          icd_10_chapter = if_else(is.na(icd_10_chapter),
  #                                   "II.  Neoplasms",
  #                                   icd_10_chapter)
  #   )
                                    
# cause of death by age band and icd chapter
deaths_age_chapter <- cause_deaths %>%
  group_by(year, age_band, icd_chapter) %>%
  summarise(deaths = n(), .groups = "drop")

# cause of death by age band and icd chapter, 5 y av
deaths_age_chapter_5y <- deaths_age_chapter %>%
  filter(year %in% 2019:2023) %>%
  mutate(period = "2019-2023") %>%
  group_by(period, age_band, icd_chapter) %>%
  summarise(deaths = sum(deaths), .groups = "drop")

# #ALTERNATIVE 1 digit
# deaths_age_1dig <- cause_deaths %>%
#   left_join(codes_1_letter, by = c('icd_1digit' = 'diag_1_letter')) %>% 
#   mutate(condition_catergory = case_when(icd_1digit == 'U' ~ 'COVID-19 confirmed/suspected',
#                                          TRUE ~ condition_catergory)) %>% 
#   group_by(year, age_band, condition_catergory) %>% 
#   summarise(deaths = n(), .groups = "drop") %>% 
#   arrange(desc(deaths))
# 
# deaths_age_1dig_5y <- deaths_age_1dig %>%
#   filter(year %in% 2019:2023) %>%
#   mutate(period = "2019-2023") %>%
#   group_by(period, age_band, condition_catergory) %>%
#   summarise(deaths = sum(deaths), .groups = "drop")

# cause of death by summary list condition and age band
deaths_age_sumlist <- cause_deaths %>%
  group_by(year, age_band, nrs_sum_list, icd_10_family_code) %>%
  summarise(deaths = n(), .groups = "drop")

# cause of death by summary list condition and age band, 5y av
deaths_age_sumlist_5y <- deaths_age_sumlist %>%
  filter(year %in% 2019:2023) %>%
  mutate(period = "2019-2023") %>%
  group_by(period, age_band, nrs_sum_list, icd_10_family_code) %>%
  summarise(deaths = sum(deaths), .groups = "drop") %>%
  arrange(period, age_band, desc(deaths))

# cause of death by icd family and age band
deaths_age_family <- 
  cause_deaths %>%
 # select(year, age_band, icd3, icd3_description = description.x) %>% 
  #select(-c(icd3_description, icd_10_family, icd_10_chapter)) %>% 
 # left_join(nrs_icd3, join_by(icd3)) %>% 
  group_by(year, age_band, icd_10_family) %>%
  summarise(deaths = n(), .groups = "drop")

# cause of death by icd family and age band, 5y av
deaths_age_family_5y <- deaths_age_family %>%
  filter(year %in% 2019:2023) %>%
  mutate(period = "2019-2023") %>%
  group_by(period, age_band, icd_10_family) %>%
  summarise(deaths = sum(deaths), .groups = "drop") %>%
  arrange(period, age_band, desc(deaths))


### 5. Outputs ----

# save initial extracts 
#write_xlsx(list(deaths_age_chapter_5y, deaths_age_sumlist_5y, deaths_age_family_5y),
#paste0(deaths_folder, "initial_extracts.xlsx"))

# A Time trend of deaths by age band, 5y rolling crude rate
age_band_deaths_5y

# B Time trend of deaths by age group, number only
wide_age_group_deaths_u25 <- age_group_deaths %>%
  filter(age_group %in% c("0", "1-4", "5-9", "10-14", "15-19", "20-24")) %>%
  arrange(year, age_group) %>%
  select(-deaths_rate) %>%
  pivot_wider(names_from = age_group, values_from = c(deaths, pop)) %>%
  select(year, deaths_0, pop_0, "deaths_1-4", "pop_1-4", "deaths_5-9", "pop_5-9",
         "deaths_10-14", "pop_10-14", "deaths_15-19", "pop_15-19", "deaths_20-24", "pop_20-24")

# prepare wb for excel export

wb <- createWorkbook("draft_output_v2")
addWorksheet(wb, sheetName = "Death_by_Cause")
writeData(wb, sheet = "Death_by_Cause", 
          x = "Number of deaths by ICD Chapter, broken down by age group, 2019-2023", 
          startCol = 1, 
          startRow = 1)
# Create a bold text style
boldStyle <- createStyle(textDecoration = "bold")
# Apply the bold text style to the cell
addStyle(wb, sheet = "Death_by_Cause", style = boldStyle, rows = 2, cols = 1, gridExpand = TRUE)

# C Deaths by ICD chapter and age band, last 5 years
# wide table by ICD chapter 
# values_fill : all NAs to 0
wide_deaths_age_chapter <- deaths_age_chapter_5y %>%
  mutate(age_band = paste0("Ages ", age_band)) %>% 
  pivot_wider(names_from = age_band, values_from = deaths, values_fill = list(deaths = 0)) %>%
  arrange(icd_chapter) %>% 
  select(-period)

# add wide_deaths_age_chapter to worksheet
writeData(wb, sheet = "Death_by_Cause", x = wide_deaths_age_chapter, startCol = 1, startRow = 3)
#create border style
borderStyle <- createStyle(border = "TopBottomLeftRight", borderColour = "black")

#apply border to cells
addStyle(wb, sheet = "Death_by_Cause", 
         style = borderStyle, 
         rows = 1:(nrow(wide_deaths_age_chapter) + 3), 
         cols = 1:ncol(wide_deaths_age_chapter), 
         gridExpand = TRUE)

writeData(wb, sheet = "Death_by_Cause",
          x = "Top 10 Causes among 0-24 year olds, 2019-2023 - ICD families",
          startRow = 25,
          startCol = 1)

addStyle(wb, sheet = "Death_by_Cause", style = boldStyle, rows = 25, cols = 1, gridExpand = TRUE)

top10fam_0_24 <- 
  deaths_age_family_5y %>%
  filter(age_band == "0-24") %>% 
  count(icd_10_family, wt=deaths, name = "deaths") %>% 
  slice_max(deaths, n=10) %>% 
  select(icd_10_family) %>% 
  mutate(family_code = str_extract(icd_10_family, "^[^.]+\\."),
         icd_10_family = str_replace(icd_10_family, "^[^.]+\\.", "") %>% str_trim(),
         family_code = paste0("(", family_code, ")")) %>% 
  unite(family_name, icd_10_family, family_code)
  
writeData(wb, sheet = "Death_by_Cause",
          x = top10fam_0_24,
          startRow = 26,
          startCol = 1)

writeData(wb, sheet = "Death_by_Cause",
          x = "Top 5 Causes among 0-24 year olds, 2019-2023 - ICD chapters",
          startRow = 41,
          startCol = 1)

addStyle(wb, sheet = "Death_by_Cause", style = boldStyle, rows = 41, cols = 1, gridExpand = TRUE)

chapter_0_24_5y <- 
  deaths_age_chapter_5y %>%
  filter(age_band == "0-24") %>% 
  count(icd_chapter, wt=deaths, name = "deaths") 

top5chapter_0_24 <- 
  chapter_0_24_5y %>% 
  slice_max(deaths, n=5) %>% 
  select(icd_chapter) %>% 
  mutate(chapter_code = str_extract(icd_chapter, "^[^.]+\\."),
         icd_chapter = str_replace(icd_chapter, "^[^.]+\\.", "") %>% str_trim(),
         chapter_code = paste0("(", chapter_code, ")")) %>% 
  unite(chapter_name, icd_chapter, chapter_code)

writeData(wb, sheet = "Death_by_Cause",
          x = top5chapter_0_24,
          startRow = 42,
          startCol = 1)

  
saveWorkbook(wb, file = paste0(deaths_folder, "draft_output_v2.xlsx"), overwrite = T)
# #ALTERNATIVE 1dig
# wide_deaths_1dig <- deaths_age_1dig_5y %>%
#   pivot_wider(names_from = age_band, values_from = deaths) %>%
#   arrange(condition_catergory)

chapter_0_24_prop <- 
  chapter_0_24_5y %>%
  dplyr::add_count(wt = deaths) %>% 
  mutate(prop = deaths/n) %>% 
  mutate(chapter_code = str_extract(icd_chapter, "^[^.]+\\."),
         icd_chapter = str_replace(icd_chapter, "^[^.]+\\.", "") %>% str_trim(),
         chapter_code = paste0("(", chapter_code, ")")) %>% 
  unite(chapter_name, icd_chapter, chapter_code)
  
# D List of cause of death icd families among CYP, last 5 year period

# primary cause of death 2019-2023 age 00-25
# deaths_cause_19_23 <- cause_deaths %>%
#   #filter(year %in% 2019:2023 & age_band %in% c("0-9", "10-19")) %>%
#   filter(year %in% 2019:2023 & age_band == "0-24") %>%
#   mutate(period = "2019-2023") %>%
#   group_by(period, age_band, code1, description.x) %>%
#   summarise(deaths = n(), .groups = "drop") %>%
#   arrange(period, age_band, desc(deaths)) %>%
#   left_join(nrs_icd3, by = c("code1" = "icd3"))

# write_xlsx(list(age_band_deaths_5y = age_band_deaths_5y, 
#                 wide_age_group_deaths_u25 = wide_age_group_deaths_u25,
#                 wide_deaths_age_chapter = wide_deaths_age_chapter, 
#                 deaths_cause_19_23 = deaths_cause_19_23),
#            path = paste0(deaths_folder, "data_tables_25yr_groups.xlsx"))


### Data visualisation ----
# Plot chapter 0 - 24 proportion 
chapter_0_24_prop_plot <- 
  chapter_0_24_prop %>% 
  ggplot(aes(x = reorder(chapter_name, prop),
             y = prop))+
  geom_col(fill = phsstyles::phs_colours()[3])+
  coord_flip()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  theme_profiles()+
  ylim(0,0.45)+
  labs(x = "",
       y = "Proportion of all deaths in 0-24 age group",
       title = "Proportion of all deaths in 0-24 age group by ICD - Chapter",
       subtitle = "Integers on plot represent cases")+
  geom_text(aes(label = deaths), hjust = -0.5, vjust = 0.5)  # Add text labels above the bars

# Save the plot to a file
#ggsave(paste0(deaths_folder, "chapter_0_25_1019_2024.png"), plot = chapter_0_24_prop_plot, width = 10, height = 6)

####################################################################################################################
# Plot 1 - time trend of crude rate by age band (5y)
# based on locality profiles theme
theme_profiles <- function() {
  fontStyle <- "sans"
  gridLineColor <- grDevices::rgb(190 / 255, 190 / 255, 190 / 255)
  fontSize <- 11
  
  ggplot2::theme(
    
    # Text format:
    # This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(
      family = fontStyle,
      size = fontSize,
      face = "bold"
    ),
    
    # Legend format
    # This sets the position and alignment of the legend, removes a title and
    # background for it and sets the requirements for any text within the legend.
    # The legend may often need some more manual tweaking when it comes to its
    # exact position based on the plot coordinates.
    legend.position = "bottom",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(
      family = fontStyle,
      size = fontSize
    ),
    
    # Axis format
    # This sets the text font, size and colour for the axis test, as well as
    # setting the margins and removes lines and ticks.
    # In some cases, axis lines and axis ticks are things we would want to have
    # in the chart - the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_text(
      family = fontStyle,
      size = fontSize
    ),
    axis.text = ggplot2::element_text(
      family = fontStyle,
      size = fontSize
    ),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    # Grid Lines
    # This removes all minor gridlines and adds major vertical gridlines.
    # In many cases you will want to change this to remove vertical gridlines
    # and add horizontal gridlines. The cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(color = gridLineColor),
    panel.grid.major.y = ggplot2::element_blank(),
    
    # Blank Background
    # This sets the panel background as blank, removing the standard grey ggplot
    # background colour from the plot
    panel.background = ggplot2::element_blank()
  )
}



## PHS colour palette from phsstyles
palette <- phsstyles::phs_colours(c(
  "phs-purple", "phs-magenta", "phs-blue", "phs-green",
  "phs-graphite", "phs-teal", "phs-liberty", "phs-rust", "phs-blue-50", "phs-purple-50"
))

# Plot chapter 0 - 24 proportion 
chapter_0_24_prop_plot <- 
  chapter_0_24_prop %>% 
  ggplot(aes(x = reorder(chapter_name, prop),
             y = prop))+
  geom_col(fill = palette[3])+
  coord_flip()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  theme_profiles()+
  ylim(0,0.45)+
  labs(x = "",
       y = "Proportion of all deaths in 0-24 age group",
       title = "Proportion of all deaths in 0-24 age group by ICD - Chapter",
       subtitle = "Integers on plot represent cases")+
  geom_text(aes(label = deaths), hjust = -0.5, vjust = 0.5)  # Add text labels above the bars
  
# Save the plot to a file
ggsave(paste0(deaths_folder, "chapter_0_25_1019_2024.png"), plot = chapter_0_24_prop_plot, width = 10, height = 6)

###################################################################################################################
age_band_deaths_5y %>%
  mutate(period = paste0(year-4, "-", year)) %>%
  # plot
  ggplot(aes(
    x = str_wrap(period, width = 120), y = deaths_5y_rate,
    group = age_band, fill = age_band, linetype = age_band
  )) +
  geom_line(aes(colour = age_band), linewidth = 1) +
  geom_point(aes(colour = age_band), size = 2) +
  geom_ribbon(
    aes(
      x = str_wrap(period, width = 120),
      ymin = lowci,
      ymax = upci
    ),
    alpha = 0.1
  ) +
  scale_fill_manual(values = palette) +
  scale_colour_manual(values = palette) +
  theme_profiles() +
  theme(axis.text.x = element_text(angle = 45)) +
  expand_limits(y = 0) +
  labs(
    title = "Crude rate of deaths by age in West Dunbartonshire",
    x = "Year (5-year average)",
    y = "Crude rate of deaths (per 100,000)",
    caption = "Source: NRS Deaths"
  ) +
  guides(
    linetype = "none", shape = "none", fill = "none",
    colour = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  facet_wrap("age_band", scales = "free_y")


# Plot 2 - time trend number of deaths by age group, under 25s
# Plot 3 - Proportion of deaths by age band and ICD chapter ?


### CHECKS ----
# ## Read in NRS trend by age group data for checks
# # read in and reshape nrs deaths time trend by age and council area data
# library(purrr)
# nrs_trend <- read_xlsx(path = paste0(deaths_folder, "NRS downloads/dt-13-deaths-time-series-2023.xlsx"), sheet = "2023",
#                        range = "A4:V39") %>%
#   rename(area = "...1", all_ages = "...2") %>%
#   filter(area == "West Dunbartonshire") %>%
#   pivot_longer(cols = 2:22, names_to = "age_group", values_to = "deaths") %>%
#   mutate(year == 2023)
# 
# read_nrs_data <- function(sheet_year){
#   data <- read_xlsx(path = paste0(deaths_folder, "NRS downloads/dt-13-deaths-time-series-2023.xlsx"), 
#                     sheet = sheet_year, range = "A4:V39") %>%
#     rename(area = "...1", all_ages = "...2") %>%
#     filter(area == "West Dunbartonshire") %>%
#     pivot_longer(cols = 2:22, names_to = "age_group", values_to = "deaths") %>%
#     mutate(year == sheet_year)
# }



