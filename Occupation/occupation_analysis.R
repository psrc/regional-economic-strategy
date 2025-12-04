library(psrccensus)
library(magrittr)
library(dplyr)
library(srvyr)
library(stringr)
library(data.table)

source("get_soc_projections.R")
#source("get_kc_living_wage.R")
dir  = "C:/Users/mjensen/projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"
living_wage <- 81868 #get_mit_living_wage_king_2adults_2children() * 2080 
datayr <- 2023

# PUMS variables for population-scale analysis
pvars <- c(
  "AGEP",                   # Age
  "SEX",
  "ED_ATTAIN",              # Educational attainment
  "DIS",                    # Disability
  "ENG",                    # Ability to speak English
  "PRACE",                  # Individual race (PSRC categories)
  "HRACE",                  # Household race (PSRC categories)
  "ESR",                    # Employment status
  "COW",                    # Class of worker
  "WAGP",                   # Wage/Salary income
  "HINCP",                  # Household income
  "POVPIP",                 # Income-to-poverty ratio
  "SOCP",                   # Detailed occupation
  "SOCP3",                  # Occupational group
  "SOCP5",                  # Occupational sector
  "OCCP",                   # Census occupational grouping
  "NAICSP"                  # Detailed industry
)

soc_projections <- get_soc_projections() %>% setDT() %>% 
  .[, soc:=gsub("-","", soc)]

# Since psrccensus delivers labels, create lookup to return SOC code itself
soc_lookup <- tidycensus::pums_variables %>%
  filter(var_code == "SOCP", year == datayr) %>%
  select(c(val_max, val_label)) %>% setDT() %>%
  setnames(c("val_max", "val_label"), c("soc_cb","SOCP_label"))

# Retrieve the PUMS data; filter to +16 workforce and add SOC code
pums2023_5 <- get_psrc_pums(5, datayr, "p", pvars, dir) 
pums2023_5_wkfrc16 <- pums2023_5 %>%
  filter(
    !grepl("^Unemployed", as.character(SOCP)),
    grepl("^(Civilian|Armed) ", as.character(ESR)),
    !is.na(ESR),
    AGEP > 15
  ) %>%
  mutate(
    soc_cb = soc_lookup$soc_cb[
      match(as.character(SOCP), as.character(soc_lookup$SOCP_label))
    ]
  ) %>% ungroup() 

soc_median_pay  <- psrc_pums_median(pums2023_5_wkfrc16, 
                                    stat_var="WAGP", 
                                    group_vars=c("soc_cb"), 
                                    incl_na=FALSE) %>% setDT()

wkfrc16_x_race <- psrc_pums_count(pums2023_5_wkfrc16, 
                                  group_vars=c("PRACE"), 
                                  incl_na=FALSE) %>% setDT()

wkfrc16_x_sex  <- psrc_pums_count(pums2023_5_wkfrc16, 
                                  group_vars=c("SEX"), 
                                  incl_na=FALSE) %>% setDT()

focus_soc  <- copy(soc_median_pay) %>% .[WAGP_median > living_wage, .(soc_cb)] %>% 
  .[, soc_match:=gsub("X", "0", soc_cb)] %>% 
  .[soc_projections[cagr_23_33>0], on = .(soc_match==soc), nomatch = 0] %>%
  .[, .(soc_cb, soc_match)]

# Mark occupations meeting wage + growth criteria; use soc_cb codes
pums2023_5_wkfrc16 %<>% mutate(
  soc_focus = if_else(soc_cb %chin% focus_soc$soc_cb, soc_cb, NA_character_)
)

focus_pay_x_race <- psrc_pums_median(pums2023_5_wkfrc16, 
                                     stat_var="WAGP", 
                                     group_vars=c("soc_focus","PRACE"), 
                                     incl_na=FALSE) %>% 
  setDT() %>% .[PRACE!="Total"]

focus_pay_x_sex  <- psrc_pums_median(pums2023_5_wkfrc16, 
                                     stat_var="WAGP", 
                                     group_vars=c("soc_focus","SEX"), 
                                     incl_na=FALSE) %>% 
  setDT() %>% .[SEX!="Total"]

focus_share_x_race <- psrc_pums_count(pums2023_5_wkfrc16, 
                                      group_vars=c("soc_focus","PRACE"), 
                                      incl_na=FALSE) %>% 
  setDT() %>% .[PRACE!="Total"]

focus_share_x_sex  <- psrc_pums_count(pums2023_5_wkfrc16, 
                                      group_vars=c("soc_focus","SEX"), 
                                      incl_na=FALSE) %>% 
  setDT() %>% .[SEX!="Total"]


# ---- Total Variation Distance (TVD) metrics ----
# TVD(P,Q) = 0.5 * sum_i |p_i - q_i| across categories i.
# Approximate MOE of TVD (if desired) treats category differences as independent:
# diff_moe_i = sqrt(share_moe_focus_i^2 + share_moe_overall_i^2)
# tvd_moe ≈ 0.5 * sqrt(sum(diff_moe_i^2)) [root-sum-square after scaling].

compute_tvd <- function(focus_dt, overall_dt, category_col, soc_col = "soc_cb",
                        share_col = "share", share_moe_col = "share_moe",
                        include_moe = TRUE) {
  # Base R implementation to avoid NSE lint issues
  f <- as.data.frame(focus_dt)
  o <- as.data.frame(overall_dt)

  # Shares
  fsub <- f[, c(soc_col, category_col, share_col)]
  osub <- o[, c(category_col, share_col)]
  colnames(fsub) <- c("soc_cb","cat","focus_share")
  colnames(osub) <- c("cat","overall_share")
  merged <- merge(osub, fsub, by = "cat", all.x = TRUE)
  merged$focus_share[is.na(merged$focus_share)] <- 0
  merged$overall_share[is.na(merged$overall_share)] <- 0
  merged$diff <- abs(merged$focus_share - merged$overall_share)
  tvd <- aggregate(diff ~ soc_cb, data = merged, FUN = sum)
  tvd$tvd <- 0.5 * tvd$diff
  tvd$diff <- NULL

  # Optionally compute MOE of TVD using root-sum-square of category MOEs
  if (isTRUE(include_moe) && share_moe_col %in% names(f) && share_moe_col %in% names(o)) {
    fmoe <- f[, c(soc_col, category_col, share_moe_col)]
    omoe <- o[, c(category_col, share_moe_col)]
    colnames(fmoe) <- c("soc_cb","cat","focus_moe")
    colnames(omoe) <- c("cat","overall_moe")
    merged_moe <- merge(omoe, fmoe, by = "cat", all.x = TRUE)
    merged_moe$focus_moe[is.na(merged_moe$focus_moe)] <- 0
    merged_moe$overall_moe[is.na(merged_moe$overall_moe)] <- 0
    merged_moe$diff_moe <- sqrt(merged_moe$focus_moe^2 + merged_moe$overall_moe^2)
    tvd_moe <- aggregate(diff_moe ~ soc_cb, data = merged_moe, FUN = function(x) 0.5 * sqrt(sum(x^2)))
    colnames(tvd_moe)[colnames(tvd_moe) == "diff_moe"] <- "tvd_moe"
    tvd <- merge(tvd, tvd_moe, by = "soc_cb", all.x = TRUE)
  }

  setDT(tvd)
  tvd[]
}

# Race TVD
if (exists("wkfrc16_x_race") && exists("focus_share_x_race") && 
    !is.null(wkfrc16_x_race) && !is.null(focus_share_x_race)) {
  # Use soc_col matching focus tables (`soc_focus`), not default `soc_cb`
  focus_tvd_race <- compute_tvd(focus_share_x_race, 
                                wkfrc16_x_race, 
                                category_col = "PRACE", 
                                soc_col = "soc_focus")
} else {
  focus_tvd_race <- NULL
}

# Sex TVD
if (exists("wkfrc16_x_sex") && exists("focus_share_x_sex") && 
    !is.null(wkfrc16_x_sex) && !is.null(focus_share_x_sex)) {
  focus_tvd_sex <- compute_tvd(focus_share_x_sex, 
                               wkfrc16_x_sex, 
                               category_col = "SEX", 
                               soc_col = "soc_focus")
} else {
  focus_tvd_sex <- NULL
}
