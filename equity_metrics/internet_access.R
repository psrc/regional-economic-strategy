library(psrccensus)
library(magrittr)
library(dplyr)

# required variables: HRACE, ACCESSINET
prep_internet_data <- function(raw_pumsdata_h){
  prepped_pumsdata_h <- raw_pumsdata_h %>% mutate(
    internet = factor(case_when(grepl("^Yes", ACCESSINET) ~ "With internet access",
                                grepl("^No", ACCESSINET) ~ "Without internet access")))
  return(prepped_pumsdata_h)
}

summarize_internet_by_race <- function(prepped_pumsdata_h){
  result_df <- psrc_pums_count(prepped_pumsdata_h, group_vars = c("HRACE", "internet"))
  return(result_df)
}

summarize_internet <- function(prepped_pumsdata_h){
  result_df <- psrc_pums_count(prepped_pumsdata_h, group_vars = "internet")
  return(result_df)
}

# Example:
# pumsdata <- prep_internet_data(get_pums_resequity_h(2024))
# result_df <- summarize_internet_by_race(pumsdata)
# net_access_p <- psrcplot::static_bar_chart(
#                     t = filter(result_df, internet != "Total"), 
#                     y = "HRACE", 
#                     x = "share", 
#                     fill = "internet",
#                     title = "Internet Access by Race/Ethnicity",
#                     alt = "Chart of Internet Access by Race/Ethnicity",
#                     source = paste("Source: ACS PUMS 2020-2024 5-year microdata",
#                                  "for King, Kitsap, Pierce and Snohomish counties.",
#                                  sep = "\n"),
#                     color="pgnobgy_5", pos = "stack")
