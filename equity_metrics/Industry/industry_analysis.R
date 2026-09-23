library(psrccensus)
library(magrittr)
library(dplyr)
library(srvyr)
library(data.table)
library(tibble)
library(psrcplot)
library(ggplot2)

dir  = "C:/projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"
living_wage <- 81868 #get_mit_living_wage_king_2adults_2children() * 2080 
datayr <- 2024

source("get_naics_projections.R")
source("naics_crosswalk.R")
source("../pums_concentration_helpers.R")
#source("get_kc_living_wage.R")

naicsp_from_label <- make_naicsp_from_label(datayr)

# PUMS variables for population-scale analysis
pvars <- c(
  "AGEP",                   # Age
  "SEX",
  "ED_ATTAIN",              # Educational attainment
  "PRACE",                  # Individual race (PSRC categories)
  "ESR",                    # Employment status
  "COW",                    # Class of worker
  "WAGP",                   # Wage/Salary income
  "SOCP",                   # Detailed occupation
  "SOCP3",                  # Occupational group
  "SOCP5",                  # Occupational sector
  "NAICSP"                  # Detailed industry
)

# Get MSA industry projections
naics_projections <- get_naics_projections() %>%
  mutate(title = trimws(title)) %>%
  inner_join(
    industry_regex %>% select(display_order, division, sector),
    by = c("title" = "sector")
  ) %>%
  mutate(sector = factor(title, levels = sector_levels)) %>%
  select(-title) %>%
  arrange(sector) %>% setDT()

# naics_crosswalk.R supplies the CSV rules and first-match classifier.

# Retrieve the PUMS data; filter to +16 workforce and add NAICS-projection industry code
pums2024_5 <- get_psrc_pums(5, datayr, "p", pvars, dir)
pums2024_5_wkfrc16 <- pums2024_5 %>%
  filter(
    !grepl("^Unemployed", as.character(SOCP)),
    grepl("^(Civilian|Armed) ", as.character(ESR)),
    !is.na(ESR),
    AGEP > 15
  ) %>%
  mutate(
    poc = factor(case_when(PRACE=="White" ~ "Non-POC",
                                  !is.na(PRACE) ~ "POC"),
                 levels=c("POC","Non-POC")),
    naicsp_code = naicsp_from_label[as.character(NAICSP)],
    prace_adj = factor(fifelse(grepl("Native|Other|^Two ", PRACE), 
                               "All else", as.character(PRACE)))) %>%
  mutate(
    industry_row = match_industry_row(naicsp_code, COW),
    display_order = industry_regex$display_order[industry_row],
    division = industry_regex$division[industry_row],
    sector = factor(industry_regex$sector[industry_row], levels = sector_levels)
  ) %>% select(-industry_row) %>% ungroup()

naics_median_pay <- psrc_pums_median(pums2024_5_wkfrc16,
                                     stat_var = "WAGP",
                                     group_vars = c("sector"),
                                     incl_na = FALSE) %>% setDT()

wkfrc16_x_race <- psrc_pums_count(pums2024_5_wkfrc16,
                                  group_vars = c("prace_adj"),
                                  incl_na = FALSE) %>% setDT()

wkfrc16_x_sex  <- psrc_pums_count(pums2024_5_wkfrc16,
                                  group_vars = c("SEX"),
                                  incl_na = FALSE) %>% setDT()

wkfrc16_x_poc  <- psrc_pums_count(pums2024_5_wkfrc16,
                                  group_vars = c("poc"),
                                  incl_na = FALSE) %>% setDT()

# Flag groups with at least one observation
pums2024_5_wkfrc16 <- pums2024_5_wkfrc16 %>%
  group_by(sector, PRACE) %>%
  mutate(n_naics_x_race = sum(!is.na(WAGP))) %>%
  ungroup() %>%
  group_by(sector, poc) %>%
  mutate(n_naics_x_poc = sum(!is.na(WAGP))) %>%
  ungroup() %>%
  group_by(sector, SEX) %>%
  mutate(n_naics_x_sex = sum(!is.na(WAGP))) %>%
  ungroup()

pay_x_race <- psrc_pums_median(filter(pums2024_5_wkfrc16, n_naics_x_race > 0), 
                               stat_var = "WAGP",
                               group_vars = c("sector", "prace_adj"),
                               incl_na = FALSE) %>%
  filter(prace_adj != "Total")

pay_x_sex  <- psrc_pums_median(filter(pums2024_5_wkfrc16, n_naics_x_sex > 0), 
                               stat_var = "WAGP",
                               group_vars = c("sector", "SEX"),
                               incl_na = FALSE) %>%
  filter(SEX != "Total")

share_x_race <- psrc_pums_count(filter(pums2024_5_wkfrc16, n_naics_x_race > 0), 
                                group_vars = c("sector", "prace_adj"),
                                incl_na = FALSE) %>%
  filter(prace_adj != "Total")

share_x_sex  <- psrc_pums_count(filter(pums2024_5_wkfrc16, n_naics_x_sex > 0), 
                                group_vars = c("sector", "SEX"),
                                incl_na = FALSE) %>%
  filter(SEX != "Total")

share_x_poc  <- psrc_pums_count(filter(pums2024_5_wkfrc16, n_naics_x_poc > 0),
                                group_vars = c("sector", "poc"),
                                incl_na = FALSE) %>%
  filter(poc != "Total")

# compute_tvd() and compute_signed_binary_diff() are defined in pums_concentration_helpers.R.
# Pass soc_col so the returned grouping key is named sector.

# Race TVD
if (exists("wkfrc16_x_race") && exists("share_x_race") &&
    !is.null(wkfrc16_x_race) && !is.null(share_x_race)) {
  tvd_race <- compute_tvd(share_x_race,
                                wkfrc16_x_race,
                                category_col = "prace_adj",
                                soc_col = "sector") %>% setDT() %>%
    setnames(c("tvd", "tvd_moe"), c("tvd_race", "tvd_race_moe"), skip_absent = TRUE) 
} else {
  tvd_race <- NULL
}

# Female signed difference in share
if (exists("wkfrc16_x_sex") && exists("share_x_sex") &&
    !is.null(wkfrc16_x_sex) && !is.null(share_x_sex)) {
  signed_female <- compute_signed_binary_diff(
    share_x_sex,
    wkfrc16_x_sex,
    category_col = "SEX",
    positive_category = "Female",
    soc_col = "sector"
  ) %>% setDT() %>%
    setnames(
      c("signed_diff", "signed_diff_moe"),
      c("female_share_diff", "female_share_diff_moe"),
      skip_absent = TRUE
    )
} else {
  signed_female <- NULL
}

# POC signed difference in share
if (exists("wkfrc16_x_poc") && exists("share_x_poc") &&
    !is.null(wkfrc16_x_poc) && !is.null(share_x_poc)) {
  signed_poc <- compute_signed_binary_diff(
    share_x_poc,
    wkfrc16_x_poc,
    category_col = "poc",
    positive_category = "POC",
    soc_col = "sector"
  ) %>% setDT() %>%
    setnames(
      c("signed_diff", "signed_diff_moe"),
      c("poc_share_diff", "poc_share_diff_moe"),
      skip_absent = TRUE
    )
} else {
  signed_poc <- NULL
}

# --- Combined table for comparisons ---
naics_stats <- naics_projections %>%
  .[naics_median_pay,  on = .(sector)] %>%
  .[tvd_race, on = .(sector)] %>%
  .[signed_female, on = .(sector)] %>%
  .[signed_poc, on = .(sector)] %>%
  .[!is.na(cagr_24_34), .(sector, display_order, division, emp_2024, emp_2034,
                          cagr_24_34, chg_24_34,
                          WAGP_median, WAGP_median_moe, tvd_race, tvd_race_moe,
                          female_share_diff, female_share_diff_moe,
                          poc_share_diff, poc_share_diff_moe)] %>%
  mutate(sector = factor(sector, levels = sector_levels)) %>% 
    setDT() %>% setorder(display_order)

# --- Industry concentration by race/ethnicity ---

share_x_race_dt <- copy(share_x_race) %>%
  left_join(
    naics_projections %>% select(sector, cagr_24_34, chg_24_34),
    by = "sector"
  ) %>%
  setDT() %>% .[
    !is.na(sector),
    .(sector, prace_adj, cagr_24_34, chg_24_34,
      share_focus = share, share_moe_focus = share_moe)
  ]

wkfrc16_x_race_dt <- copy(wkfrc16_x_race) %>%
  setDT() %>% .[
    ,
    .(prace_adj, share_overall = share, share_moe_overall = share_moe)
  ]

# Long format: difference in race/ethnicity share and its Z-score
naics_conc_long <- merge(
  wkfrc16_x_race_dt,
  share_x_race_dt,
  by = "prace_adj",
  allow.cartesian = TRUE
)

# Difference in shares: + means overrepresented in industry vs workforce
naics_conc_long[
  ,
  diff_share := share_focus - share_overall
]

# Approximate MOE of the share difference and corresponding Z-score
# Assumes share_moe_* are 90% MOEs (so z_crit ≈ qnorm(0.95))
z_crit <- qnorm(0.95)

naics_conc_long[
  ,
  diff_moe := sqrt(share_moe_focus^2 + share_moe_overall^2)
][
  ,
  z_score := fifelse(
    diff_moe > 0,
    diff_share * z_crit / diff_moe,
    NA_real_
  )
]

# Make prace_adj into safe column names
naics_conc_long[, race_code := make.names(as.character(prace_adj))]

# Wide crosstab: one row per industry, two cols per prace_adj (diff + z)
race_levels <- sort(unique(naics_conc_long$race_code))

naics_race_diff_wide <- dcast(
  naics_conc_long,
  sector ~ race_code,
  value.var = "diff_share"
)
setnames(
  naics_race_diff_wide,
  old = race_levels,
  new = paste0(race_levels, "_diff")
)

naics_race_z_wide <- dcast(
  naics_conc_long,
  sector ~ race_code,
  value.var = "z_score"
)
setnames(
  naics_race_z_wide,
  old = race_levels,
  new = paste0(race_levels, "_z")
)

# Final crosstab: one row per naics industry
naics_conc_crosstab <- merge(
  naics_race_diff_wide,
  naics_race_z_wide,
  by = "sector",
  all = TRUE
)

# Restore the CSV display order after summary helpers and reshaping.
naics_conc_long[, sector := factor(sector, levels = sector_levels)]
setorder(naics_conc_long, sector)
naics_conc_crosstab[, sector := factor(sector, levels = sector_levels)]
setorder(naics_conc_crosstab, sector)

