library(tidyverse)
library(psrcelmer)
library(psrccensus)
library(psrcplot)
library(openxlsx)
library(rlang) #required for psrccensus
library(emmeans) #required for rlang
library(magrittr)
library(stringr) #str_extract()
library(stringdist) #for fuzzy joining
library(fuzzyjoin) #for fuzzy joining

# Code to retrieve data for the Regional Economic Strategy update
# previous document: https://www.psrc.org/media/1688
# Mary Richards
# Summer 2025

# current age estimates using the same cohort groups as in the published chart (0-4, 5-17, 18-39, 40-64, 65-84, 85+)
# ACS vs. OFM comparison for 2023

# Age (ACS) 2023 ----------------------------
# chart in previous document on page 13 as reference

data_year <- "2023"
folder_loc <- "T:/60day-TEMP/Mary/Other/reg_econ_strategy"

# getting age data for region
base_acs_data <- get_acs_recs(geography ='county',
                              table.names = 'S0101', #subject table code
                              years = c(as.numeric(data_year)),
                              acs.type = 'acs5')

# report age ranges: 0-4, 5-17, 18-39, 40-64, 65-84, 85+
# these age groups don't line up with the ACS age groups, but they can be aggregated based on the reported groups and the selected age categories
ages_5_17 <- c('SELECTED AGE CATEGORIES!!5 to 14 years',
               'SELECTED AGE CATEGORIES!!15 to 17 years')
ages_18_39 <- c('SELECTED AGE CATEGORIES!!18 to 24 years', 
                'AGE!!25 to 29 years', 
                'AGE!!30 to 34 years',
                'AGE!!35 to 39 years')
ages_40_64 <- c('AGE!!40 to 44 years', 
                'AGE!!45 to 49 years', 
                'AGE!!50 to 54 years',
                'AGE!!55 to 59 years',
                'AGE!!60 to 64 years')
ages_65_84 <- c('AGE!!65 to 69 years', 
                'AGE!!70 to 74 years', 
                'AGE!!75 to 79 years',
                'AGE!!80 to 84 years')

acs_refined <- base_acs_data %>% 
  filter(grepl("S0101_C01", variable) &
           grepl("years", label),
         name=="Region") %>% 
  mutate(report_cat= case_when(grepl("AGE!!Under 5 years", label)~ "0-4 years",
                               grepl(paste(ages_5_17, collapse='|'), label)~ "5-17 years",
                               grepl(paste(ages_18_39, collapse='|'), label)~ "18-39 years",
                               grepl(paste(ages_40_64, collapse='|'), label)~ "40-64 years",
                               grepl(paste(ages_65_84, collapse='|'), label)~ "65-84 years",
                               grepl("AGE!!85 years and over", label)~ "85+ years")) %>% 
  filter(!is.na(report_cat)) %>% 
  group_by(report_cat, name, year) %>%
  dplyr::summarise(estimate_totals = sum(estimate, 
                                         na.rm = TRUE),
                   .groups='drop')

acs_calc <- acs_refined %>% 
  mutate(total_pop = sum(estimate_totals),
         prop = estimate_totals/total_pop,
         data = "ACS 5y",
         year = as.character(year))



# Reorder age categories
age_levels <- c("0-4 years",
                "5-17 years",
                "18-39 years", 
                "40-64 years", 
                "65-84 years",
                "85+ years")

acs_calc$report_cat <- factor(acs_calc$report_cat, levels = age_levels)

acs_calc <- acs_calc %>% 
  arrange(report_cat) 


# Age (ACS) 2023 ----------------------------
# downloaded from: https://ofm.wa.gov/washington-data-research/population-demographics/population-estimates/estimates-april-1-population-age-sex-race-and-hispanic-origin#age 
ofm_raw <- read_excel(file.path(folder_loc, "raw_data",
                                "ofm_pop_age_sex_postcensal_2020_2024.xlsx"),
                      sheet = "Population")
ofm_raw_s <- read_excel(file.path(folder_loc, "raw_data",
                                  "ofm_pop_age_sex_postcensal_2020_2024_s.xlsx"),
                        sheet = "Population")

ofm_raw_comb <- rbind(ofm_raw, ofm_raw_s)


ofm_psrc <- ofm_raw_comb %>% 
  filter(Year=="2023",
         `Area Name`=="King"|
           `Area Name`=="Kitsap"|
           `Area Name`=="Pierce" |
           `Area Name`=="Snohomish") %>% 
  arrange(`Area Name`)

# some of the age groups are repeated with the different age categorizations (85+, Total)
ofm_psrc_distinct <- ofm_psrc %>% 
  distinct(`Area Name`,`Age Group`, .keep_all = TRUE)

# sum totals to region
ofm_psrc_region <- ofm_psrc_distinct %>% 
  group_by(`Age Group`, Year) %>% 
  dplyr::summarise(estimate=sum(as.numeric(Total),
                                na.rm=TRUE),
                   .groups='drop')

ages_40_64 <- c('40-44', 
                '45-49', 
                '50-54',
                '55-59',
                '60-64')
ages_65_84 <- c('65-69', 
                '70-74', 
                '75-79',
                '80-84')

ofm_refined <- ofm_psrc_region %>%
  mutate(report_cat= case_when(`Age Group`=="0-4"~ "0-4 years",
                               grepl("5-17", `Age Group`)~ "5-17 years",
                               grepl("18-39", `Age Group`)~ "18-39 years",
                               grepl(paste(ages_40_64, collapse='|'), `Age Group`)~ "40-64 years",
                               grepl(paste(ages_65_84, collapse='|'), `Age Group`)~ "65-84 years",
                               grepl("85+", `Age Group`)~ "85+ years")) %>% 
  
  filter(!is.na(report_cat)) %>% 
  group_by(report_cat, Year) %>% 
  dplyr::summarise(estimate_totals = sum(estimate, 
                                         na.rm = TRUE),
                   .groups='drop')


ofm_calc <- ofm_refined %>% 
  mutate(name = "Region",
         total_pop = sum(estimate_totals),
         prop = estimate_totals/total_pop,
         data = "OFM") %>% 
  relocate(name, .after = report_cat) %>% 
  rename(year=Year)

ofm_calc$report_cat <- factor(ofm_calc$report_cat, levels = age_levels)

ofm_calc <- ofm_calc %>% 
  arrange(report_cat)


# Calculate the differences between the two data sources -----------------------------
comb_data <- acs_calc %>% 
  left_join(ofm_calc, by = c('report_cat', 'name', 'year'))

comb_data_dif <- comb_data %>% 
  mutate(propdif_acs_ofm = prop.x-prop.y)


# Export to excel file -----------------------------
dataset_names <- list('ACS' = acs_calc, 
                      'OFM' = ofm_calc,
                      'Comparing' = comb_data_dif)

write.xlsx(dataset_names, 
           file.path(folder_loc, "age_fixed.xlsx"))


# current vs. project age estimates in 5-year increments
# current: 2023 and 2024
# projected: 2030, 2035, 2040, 2045, 2050

# downloaded from: https://ofm.wa.gov/washington-data-research/population-demographics/population-forecasts-and-projections/growth-management-act-county-projections/growth-management-act-population-projections-counties-2020-2050
# Population by age and sex, five-year age groups

ofm_proj_raw <- read_excel(file.path(folder_loc, "raw_data",
                                     "gma_2022_age_sex_med.xlsx"),
                           sheet = "Population")
