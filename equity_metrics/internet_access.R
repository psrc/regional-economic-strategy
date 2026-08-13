library(psrccensus)
library(magrittr)
library(dplyr)

# for faster/more reliable access; use pums_rds = NULL when lacking access to J drive
jrds = "J:/Projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"

get_regional_hrace_internet_access <- function(dyear, pums_rds = jrds){
  if(dyear < 2017){
    stop("Internet access data is not available for years prior to 2017.")
  }
  hvars <- c("HRACE", "ACCESSINET")
  # Variable changed names w/ 2020 data; swap name temporarily for older data
  if(dyear %in% 2017:2019){hvars <- replace(hvars, hvars=="ACCESSINET","ACCESS")}
  pumsdata <- get_psrc_pums(span=5, dyear, "h", hvars, dir=pums_rds)
  if("ACCESS" %in% colnames(pumsdata)){pumsdata %<>% rename("ACCESSINET"="ACCESS")}
  pumsdata %<>% mutate(
    internet = factor(case_when(grepl("^Yes", ACCESSINET) ~ "With internet access",
                                grepl("^No", ACCESSINET) ~ "Without internet access")))
  return(pumsdata)
}

summarize_regional_hrace_internet_access <- function(pumsdata){
  result_df <- psrc_pums_count(pumsdata, group_vars = c("HRACE", "internet"))
  return(result_df)
}

# Example:
# pumsdata <- get_regional_hrace_internet_access(2024)
# result_df <- summarize_regional_hrace_internet_access(pumsdata)
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
