library(psrccensus)
library(magrittr)
library(dplyr)
library(data.table)

# Add custom grouping variables; limit to working age (16-64)
# required variables: AGEP, ESR, PRACE, SEX
prep_labor_force_data <- function(raw_pumsdata_p){
  prepped_pumsdata_p <- mutate(raw_pumsdata_p,
    labor_force_status = case_when(
      AGEP < 16 ~ NA_character_,
      between(AGEP, 16, 64) & grepl("^(Civilian|Armed|Unemployed)", ESR) ~ "In labor force",
      TRUE ~ ESR
    ),
    employment_status = case_when(
      AGEP < 16 | grepl("^Not", ESR) ~ NA_character_,
      between(AGEP, 16, 64) & grepl("^(Civilian|Armed)", ESR) ~ "Employed",
      TRUE ~ ESR
    ),
    PRACE_x_SEX = paste0(PRACE, ";", SEX)
  )
  return(prepped_pumsdata_p)
}

# Summarize labor force participation by race and sex
summarize_lf_by_race_x_sex <- function(prepped_pumsdata_p){
  result_lfprate <- psrc_pums_count(prepped_pumsdata_p, 
                                    group_vars = c("PRACE_x_SEX", "labor_force_status"), 
                                    incl_na=FALSE) %>%
   tidyr::separate(PRACE_x_SEX, into = c("PRACE", "SEX"), sep = ";")
  return(result_lfprate)
}

# Summarize employment status by race and sex
summarize_empstatus_by_race_x_sex <- function(prepped_pumsdata_p){
  result_emprate <- psrc_pums_count(prepped_pumsdata_p,
                                    group_vars = c("PRACE_x_SEX", "employment_status"), 
                                    incl_na=FALSE) %>%
   tidyr::separate(PRACE_x_SEX, into = c("PRACE", "SEX"), sep = ";")
  return(result_emprate)
}

# Summarize workforce-wide labor force participation
summarize_lf <- function(prepped_pumsdata_p){
  result_lfprate <- psrc_pums_count(prepped_pumsdata_p,
                                    group_vars = "labor_force_status",
                                    incl_na = FALSE)
  return(result_lfprate)
}

# Summarize workforce-wide employment status
summarize_empstatus <- function(prepped_pumsdata_p){
  result_emprate <- psrc_pums_count(prepped_pumsdata_p,
                                    group_vars = "employment_status",
                                    incl_na = FALSE)
  return(result_emprate)
}

## Example -------------------------
# pumsdata_p5_2024 <- get_pums_res_equity_p(dyear = 2024, span = 5)
# prep_labor_force_data(pumsdata_p5_2024)
# lfp_rates <- summarize_lf_by_race_x_sex(pumsdata_p5_2024)
# unemp_rates <- summarize_empstatus_by_race_x_sex(pumsdata_p5_2024)
# plot_lfpr <- plot_lf_rate(filter(lfp_rates, labor_force_status == "In labor force"), "share", whiskers = TRUE)
# plot_unempr <- plot_lf_rate(filter(unemp_rates, employment_status == "Unemployed"), "share", whiskers = TRUE)
