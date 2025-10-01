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

# Educational attainment by Race/Ethnicity 2023 ----------------------------
# chart in previous document on page 17 as reference

data_year <- "2023"
folder_loc <- "T:/60day-TEMP/Mary/Other/reg_econ_strategy"

# PUMS
# reporting method has changed from overlapping to non-overlapping so PUMS may be better than ACS
library_loc <- "C:/Users/mrichards/AppData/Local/R/win-library/4.4"
dir <- "J:/Projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"


get_data <- get_psrc_pums(5, data_year , "p",
                          c("PRACE", "SCHL", "AGEP"), # add any additional variables you may want
                          dir = dir) # pull network archived copy because Census Bureau ftp site not allowing downloads

# all educational levels for reference
# simplify data
get_data_ref <- get_data %>% 
  filter(AGEP>24)

# calculate statistics for county and region 
# by county
pums_data_count_county <- psrc_pums_count(get_data_ref,
                                          group_vars =c("COUNTY", "PRACE", "SCHL"),
                                          incl_na = FALSE,
                                          rr = TRUE)
# by region
pums_data_count_region <- psrc_pums_count(get_data_ref,
                                          group_vars =c("PRACE", "SCHL"),
                                          incl_na = FALSE,
                                          rr = TRUE)


# Reorder PUMS 'SCHL' categories
school_levels <- c("No schooling completed",
                   "Nursery school, preschool",
                   "Kindergarten", 
                   "Grade 1", 
                   "Grade 2",
                   'Grade 3', 
                   "Grade 4",
                   "Grade 5",
                   "Grade 6",
                   "Grade 7",
                   "Grade 8",
                   "Grade 9",
                   "Grade 10",
                   "Grade 11",
                   "12th grade - no diploma",
                   "Regular high school diploma",
                   "GED or alternative credential",
                   "Some college, but less than 1 year",
                   "1 or more years of college credit, no degree",
                   "Associate's degree",
                   "Bachelor's degree",
                   "Master's degree",
                   "Professional degree beyond a bachelor's degree",
                   "Doctorate degree")

# combine region and county
pums_data_count <- rbind(pums_data_count_region, 
                         pums_data_count_county)

# relevel school field
pums_data_count$SCHL <- factor(pums_data_count$SCHL, levels = school_levels)

# organize data
pums_data <- pums_data_count %>% 
  arrange(COUNTY, PRACE, SCHL)

# aggregating the educational level data to 2 groups:
# % High school diploma (or equivalent) or higher |	% Bachelor's degree or higher

# simplify data
get_data_binary <- get_data %>% 
  filter(AGEP>24) %>% 
  mutate(total_hshigher = case_when(SCHL == "Regular high school diploma" |
                                      SCHL == "GED or alternative credential" |
                                      grepl("(college|Assoc|Bach|Mast|Prof|Doct)", SCHL)~ "H.S. degree and higher",
                                    TRUE~"Less than high school"),
         total_bachhigher = case_when(grepl("(Bach|Mast|Prof|Doct)", SCHL)~ "Bachelor's degree and higher",
                                      TRUE~"Less than bachelor's degree"))

# calculate statistics for county and region
# high school and higher
pums_data_count_hshigher_county <- psrc_pums_count(get_data_binary,
                                                   group_vars =c("COUNTY", "PRACE", "total_hshigher"),
                                                   incl_na = FALSE,
                                                   rr = TRUE)
pums_data_count_hshigher_region <- psrc_pums_count(get_data_binary,
                                                   group_vars =c("PRACE", "total_hshigher"),
                                                   incl_na = FALSE,
                                                   rr = TRUE)
pums_data_count_hshigher <- rbind(pums_data_count_hshigher_region,
                                  pums_data_count_hshigher_county)

# bachelor's degree and higher
pums_data_count_bachhigher_county <- psrc_pums_count(get_data_binary,
                                                     group_vars =c("COUNTY", "PRACE", "total_bachhigher"),
                                                     incl_na = FALSE,
                                                     rr = TRUE)
pums_data_count_bachhigher_region <- psrc_pums_count(get_data_binary,
                                                     group_vars =c("PRACE", "total_bachhigher"),
                                                     incl_na = FALSE,
                                                     rr = TRUE)
pums_data_count_bachhigher <- rbind(pums_data_count_bachhigher_region,
                                    pums_data_count_bachhigher_county)

# Create final dataset 
# filter rows, rename fields
pums_data_hshigher <- pums_data_count_hshigher %>% 
  filter(total_hshigher=="H.S. degree and higher") %>% 
  rename(education_level=total_hshigher)
pums_data_bachhigher <- pums_data_count_bachhigher %>% 
  filter(total_bachhigher=="Bachelor's degree and higher") %>% 
  rename(education_level=total_bachhigher)

# merge 2 educational level datasets
pums_data_edu2groups <- rbind(pums_data_hshigher,
                              pums_data_bachhigher)

# reorder educational levels
school_levels_binary <- c("H.S. degree and higher",
                          "Bachelor's degree and higher")

# relevel school field
pums_data_edu2groups$education_level <- factor(pums_data_edu2groups$education_level, 
                                               levels = school_levels_binary)
# organize data
pums_edu2groups <- pums_data_edu2groups %>% 
  arrange(COUNTY, PRACE, education_level)


# Kindergarten Readiness by Race/Ethnicity 2023-24----------------------------
# downloaded from: https://data.wa.gov/education/Report-Card-WaKids-2023-24-School-Year/vumg-9sgs/about_data
ospi_raw <- read.csv(file.path(folder_loc, "raw_data",
                               "Report_Card_WaKids_2023-24_School_Year_20250731.csv"))

ospi_psrc <- ospi_raw %>% 
  select("schoolyear", "ESDName", "County", "DistrictName", "SchoolName", "OrganizationLevel",
         "StudentGroupType", "StudentGroup", "Domain", "Measure", "MeasureValue", 
         "Numerator", "Denominator", "Suppress") %>% 
  filter(County == "King" |
           County == "Kitsap" |
           County == "Pierce" |
           County == "Snohomish",
         StudentGroupType == "FederalRaceEthnicity",
         Measure == "NumberofDomainsReadyforKindergarten",
         MeasureValue == 6) 

ospi_psrc_calc <- ospi_psrc %>% 
  group_by(StudentGroup) %>% 
  summarise(Numerator=sum(Numerator, na.rm=TRUE),
            Denominator=sum(Denominator, na.rm=TRUE), 
            .groups = "drop") %>% 
  mutate(kg_ready=Numerator/Denominator)



# Export to excel file
dataset_names <- list('EduAttain' = pums_data,
                      'EduAttain_2groups' = pums_edu2groups,
                      'KGready' = ospi_psrc_calc)
write.xlsx(dataset_names, 
           file.path(folder_loc, "education_race_eth.xlsx"))



# # Educational attainment by Race/Ethnicity 2023 -----------------------------
# simplified code from Christy
# at_least_bach <- "(Bach|Mast|Prof|Doct)"
# at_least_hs <- c("Regular high school diploma", 
#                  "GED or alternative credential", 
#                  "Associate's degree", 
#                  "1 or more years of college credit, no degree", 
#                  "Some college, but less than 1 year")
# 
# df_new <- df |> 
#   mutate(edu_binary = ifelse(grepl(at_least_bach, SCHL), "Bachelor's Degree and higher", NA)) |> 
#   mutate(edu_binary = ifelse((SCHL %in% at_least_hs & is.na(edu_binary)), "H.S. Degree and Higher", edu_binary)) |> 
#   mutate(edu_binary = ifelse(is.na(edu_binary) & !is.na(SCHL), "Less than high school", edu_binary))




# # ACS 
# # reporting method has changed from overlapping to non-overlapping so PUMS may be better than ACS
# # getting educational data by race for region
# base_acs_data <- get_acs_recs(geography ='county', 
#                               table.names = 'S1501', #subject table code
#                               years = c(as.numeric(data_year)),
#                               acs.type = 'acs5')
# race_acs <- base_acs_data %>% 
#   filter(grepl("RACE AND HISPANIC OR LATINO ORIGIN", label) & 
#            grepl("S1501_C01", variable) &
#            name == "Region") 
# race_acs_2023 <- race_acs %>% 
#   mutate(simple = str_split_fixed(label, "!!", 4)[,4]) %>% 
#   mutate(race_eth = str_extract(simple, "[^!!]+"),
#          education = str_extract(simple, "[^!!]+$")) %>% 
#   select(-simple) %>% 
#   filter(race_eth!="White alone") %>% 
#   mutate(education = case_when(grepl("higher",education)~education,
#                                TRUE~NA))
# 
# # getting age data by race for region - need population >25 years for denominator
# ## B01001B Sex by Age (Black or African American Alone)
# ## B01001C Sex by Age (American Indian and Alaska Native Alone)
# ## B01001D Sex by Age (Asian Alone) 
# ## B01001E Sex by Age (Native Hawaiian and Other Pacific Islander Alone)
# ## B01001F Sex by Age (Some Other Race Alone)
# ## B01001G Sex by Age (Two or More Races)
# ## B01001H Sex by Age (White Alone, Not Hispanic or Latino)
# ## B01001I Sex by Age (Hispanic or Latino)
# 
# race_tables <- c('B01001B', 'B01001C', 'B01001D',
#                  'B01001E', 'B01001F', 'B01001G',
#                  'B01001H', 'B01001I')
# 
# population_race <- function(race_table){
#   base_acs_data <- get_acs_recs(geography ='county', 
#                                 table.names = race_table, #subject table code
#                                 years = c(as.numeric(data_year)),
#                                 acs.type = 'acs5')
#   
#   acs_data <- base_acs_data %>% 
#     filter(name == "Region",
#            grepl(c('25|35|45|55|65|75|85'), label)) %>% #filtering age groups >25y
#     mutate(race_eth = str_extract(concept, "(?<=\\().+?(?=\\))")) %>% #extracting race/eth data as new column
#     group_by(name, race_eth) %>% 
#     summarise(pop_25=sum(estimate), .groups = "drop") #sum all age groups together for total
# }
# 
# 
# pop_25_race_2023 <- map(race_tables, ~population_race(.x)) %>% 
#   bind_rows()
# 
# # combine education data (numerator) w/ population data (denominator)
# # Perform fuzzy matching because race/ethnicity categories are slightly different
# matched_data <- stringdist_join(
#   race_acs_2023, pop_25_race_2023,
#   by = "race_eth",  # Column to match on
#   mode = "left",  # Type of join: left, inner, or full
#   method = "jw",  # Jaro-Winkler distance metric
#   max_dist = 0.3  # Maximum allowable distance for a match
# )
# 
# edu_attain_calc <- matched_data %>% 
#   mutate(educational_attain = estimate/pop_25)