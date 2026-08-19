library(psrccensus)
library(magrittr)
library(dplyr)

## Household median income -----------------
# required variables: HINCP, PRACE
summarize_hhincome_by_race <- function(pumsdata_h){
  result_df <- psrc_pums_median(pumsdata_h, 
                                stat_var = HINCP, 
                                group_vars = "PRACE", 
                                incl_na=FALSE
                                )
  return(result_df)
}

summarize_hhincome <- function(pumsdata_h){
  result_df <- psrc_pums_median(pumsdata_h, 
                                stat_var = HINCP, 
                                incl_na=FALSE
                                )
  return(result_df)
}

## Personal wage/salary earnings (before taxes) for employed persons 16 and over ---------
# required variables: WAGP, ESR, PRACE
prep_wage_data <- function(raw_pumsdata_p){
  prepped_pumsdata_p <- raw_pumsdata_p %>% mutate(
    wages = if_else(
        !grepl("^(Civilian|Armed)", ESR) | AGEP < 16, NA_real_,
         WAGP))
  return(prepped_pumsdata_p)
}

summarize_wages_by_race <- function(prepped_pumsdata_p){
  result_df <- psrc_pums_median(prepped_pumsdata_p, 
                                stat_var = wages, 
                                group_vars = "PRACE", 
                                incl_na=FALSE
                                )
  return(result_df)
}

summarize_wages <- function(prepped_pumsdata_p){
  result_df <- psrc_pums_median(prepped_pumsdata_p, 
                                stat_var = wages, 
                                incl_na=FALSE
                                )
  return(result_df)
}
