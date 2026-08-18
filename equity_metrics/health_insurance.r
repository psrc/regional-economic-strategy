library(psrccensus)
library(magrittr)
library(dplyr)

# required variables: PRACE, HICOV, ESR
prep_health_insurance_data <- function(raw_pumsdata_p){
  prepped_pumsdata_p <- raw_pumsdata_p %>% mutate(
    health_insurance = factor(case_when(
        !grepl("^(Civilian|Armed)", ESR) | AGEP < 16 ~ NA_character_,
        grepl("^With", HICOV) ~ "With health insurance",
        grepl("^No", HICOV) ~ "No health insurance")))
  return(prepped_pumsdata_p)
}

summarize_health_insurance_by_race <- function(prepped_pumsdata_p){
  result_df <- psrc_pums_count(prepped_pumsdata_p, group_vars = c("PRACE", "health_insurance"), incl_na=FALSE)
  return(result_df)
}

summarize_health_insurance <- function(prepped_pumsdata_p){
  result_df <- psrc_pums_count(prepped_pumsdata_p, group_vars = "health_insurance", incl_na=FALSE)
  return(result_df)
}
