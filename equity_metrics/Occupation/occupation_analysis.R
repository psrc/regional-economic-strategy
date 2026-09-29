library(psrccensus)
library(magrittr)
library(dplyr)
library(srvyr)
library(data.table)
library(tibble)
library(psrcplot)
library(ggplot2)

# Pin here::here() to the repository root, even when this script is run from
# another subdirectory (for example, equity_metrics/Industry).
here::i_am("equity_metrics/Occupation/occupation_analysis.R")

source(here::here("equity_metrics", "Occupation", "get_soc_projections.R"))
source(here::here("equity_metrics", "Occupation", "get_soc_ed_requirements.R"))
source(here::here("equity_metrics", "Occupation", "soc3_helpers.R"))
source(here::here("equity_metrics", "pums_concentration_helpers.R"))
source(here::here("equity_metrics", "render_table.R"))
# source(here::here("equity_metrics", "Occupation", "get_kc_living_wage.R"))

dir  = "C:/projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"
living_wage <- 81868 #get_mit_living_wage_king_2adults_2children() * 2080 
datayr <- 2024

# PUMS variables for population-scale analysis
pvars <- c(
  "AGEP",                   # Age
  "SEX",
  "ED_ATTAIN",              # Educational attainment
  "PRACE",                  # Individual race (PSRC categories)
  "ESR",                    # Employment status
  "WAGP",                   # Wage/Salary income
  "SOCP",                   # Detailed occupation
  "SOCP3",                  # Occupational group
  "SOCP5",                  # Occupational sector
  "NAICSP"                  # Detailed industry
)

# Get MSA projections
soc_projections <- get_soc_projections() %>% setDT() %>%
  .[, soc_code := gsub("-", "", soc)]

# Get training requirements
soc_training_req <- readxl::read_excel(
    here::here("equity_metrics", "Occupation", "raw_data", "education.xlsx"),
    sheet = "Table 5.4"
  ) %>% setDT() %>% .[row.names.data.frame(.)!=1, 1:5] %>%
  setnames(c("Label", "Code", "Education", "Experience", "Training")) %>%
  .[, soc_code := gsub("-", "", Code)]

# Education is joined to projections at the detailed level only in this preparation.
soc3_inputs <- roll_up_soc3(soc_projections, soc_training_req)
soc3_projections <- soc3_inputs$projections
soc3_education_shares <- soc3_inputs$education_shares

# Since psrccensus delivers labels, create lookup to return SOCP code itself
socp_from_label <- tidycensus::pums_variables %>% setDT() %>%
  .[var_code == "SOCP" & year == datayr, .(val_max, val_label)] %>%
  as_tibble() %>%
  transmute(socp_label = as.character(val_label),
            socp_code  = as.character(val_max)) %>%
  tibble::deframe()

# Retrieve the PUMS data; filter to +16 workforce and add SOC code
pums_all <- get_psrc_pums(5, datayr, "p", pvars, dir)
pums_wkfrc16 <- pums_all %>%
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
    socp_code = socp_from_label[as.character(SOCP)],
    prace_adj = factor(fifelse(grepl("Native|Other|^Two ", PRACE), 
                               "All else", as.character(PRACE)))) %>%
  mutate(
    soc3_code = if_else(grepl("^[0-9]{3}", socp_code),
                        substr(socp_code, 1, 3), NA_character_)
  ) %>% ungroup()

# Use the occupation-group labels already supplied by psrccensus.
soc3_labels <- as.data.table(pums_wkfrc16$variables)[
  !is.na(soc3_code), .(soc3_code, Label = sub("^[A-Z]+-", "", as.character(SOCP3)))] %>%
  unique()
stopifnot(!anyDuplicated(soc3_labels$soc3_code))
soc3_education_shares <- merge(soc3_education_shares, soc3_labels,
                              by = "soc3_code", all.x = TRUE)

soc_median_age <- psrc_pums_median(pums_wkfrc16,
                                   stat_var = "AGEP",
                                   group_vars = c("soc3_code"),
                                   incl_na = FALSE) %>% setDT()

soc_median_pay  <- psrc_pums_median(pums_wkfrc16,
                                    stat_var = "WAGP",
                                    group_vars = c("soc3_code"),
                                    incl_na = FALSE) %>% setDT()

wkfrc16_x_race <- psrc_pums_count(pums_wkfrc16,
                                  group_vars = c("prace_adj"),
                                  incl_na = FALSE) %>% setDT()

wkfrc16_x_sex  <- psrc_pums_count(pums_wkfrc16,
                                  group_vars = c("SEX"),
                                  incl_na = FALSE) %>% setDT()

wkfrc16_x_poc  <- psrc_pums_count(pums_wkfrc16,
                                  group_vars = c("poc"),
                                  incl_na = FALSE) %>% setDT()

soc_combined <- merge(soc3_projections, soc_median_pay, by = "soc3_code") %>%
  merge(soc3_labels, by = "soc3_code", all.x = TRUE)
focus_soc <- select_focus_soc3(soc_combined, filter_type = filter_type, living_wage = living_wage)

# Include all workers in qualifying groups, regardless of detailed occupation.
pums_wkfrc16 %<>% mutate(
  soc3_focus = if_else(soc3_code %chin% focus_soc$soc3_code, soc3_code, NA_character_)
)

# Flag groups with at least one observation
pums_wkfrc16 <- pums_wkfrc16 %>%
  group_by(soc3_focus, PRACE) %>%
  mutate(n_soc_x_race = sum(!is.na(WAGP))) %>%
  ungroup() %>%
  group_by(soc3_focus, poc) %>%
  mutate(n_soc_x_poc = sum(!is.na(WAGP))) %>%
  ungroup() %>%
  group_by(soc3_focus, SEX) %>%
  mutate(n_soc_x_sex = sum(!is.na(WAGP))) %>%
  ungroup()

# Restrict the survey design/data to those combos
pums_focus <- pums_wkfrc16 %>% filter(!is.na(soc3_focus))

focus_pay_x_race <- psrc_pums_median(filter(pums_focus, n_soc_x_race > 0), 
                                     stat_var = "WAGP",
                                     group_vars = c("soc3_focus", "prace_adj"),
                                     incl_na = FALSE) %>%
  filter(prace_adj != "Total")

focus_pay_x_sex  <- psrc_pums_median(filter(pums_focus, n_soc_x_sex > 0), 
                                     stat_var = "WAGP",
                                     group_vars = c("soc3_focus", "SEX"),
                                     incl_na = FALSE) %>%
  filter(SEX != "Total")

focus_share_x_race <- psrc_pums_count(filter(pums_focus, n_soc_x_race > 0), 
                                      group_vars = c("soc3_focus", "prace_adj"),
                                      incl_na = FALSE) %>%
  filter(prace_adj != "Total")

focus_share_x_sex  <- psrc_pums_count(filter(pums_focus, n_soc_x_sex > 0), 
                                      group_vars = c("soc3_focus", "SEX"),
                                      incl_na = FALSE) %>%
  filter(SEX != "Total")

focus_share_x_poc  <- psrc_pums_count(filter(pums_focus, n_soc_x_poc > 0),
                                      group_vars = c("soc3_focus", "poc"),
                                      incl_na = FALSE) %>%
  filter(poc != "Total")

# compute_tvd() and compute_signed_binary_diff() are defined in pums_concentration_helpers.R

# Race TVD
if (exists("wkfrc16_x_race") && exists("focus_share_x_race") &&
    !is.null(wkfrc16_x_race) && !is.null(focus_share_x_race)) {
  focus_tvd_race <- compute_tvd(focus_share_x_race,
                                wkfrc16_x_race,
                                category_col = "prace_adj",
                                soc_col = "soc3_focus") %>% setDT() %>%
    setnames(c("tvd", "tvd_moe"), c("tvd_race", "tvd_race_moe"), skip_absent = TRUE) 
} else {
  focus_tvd_race <- NULL
}

# Female signed difference in share
if (exists("wkfrc16_x_sex") && exists("focus_share_x_sex") &&
    !is.null(wkfrc16_x_sex) && !is.null(focus_share_x_sex)) {
  focus_signed_female <- compute_signed_binary_diff(
    focus_share_x_sex,
    wkfrc16_x_sex,
    category_col = "SEX",
    positive_category = "Female",
    soc_col = "soc3_focus"
  ) %>% setDT() %>%
    setnames(
      c("signed_diff", "signed_diff_moe"),
      c("female_share_diff", "female_share_diff_moe"),
      skip_absent = TRUE
    )
} else {
  focus_signed_female <- NULL
}

# POC signed difference in share
if (exists("wkfrc16_x_poc") && exists("focus_share_x_poc") &&
    !is.null(wkfrc16_x_poc) && !is.null(focus_share_x_poc)) {
  focus_signed_poc <- compute_signed_binary_diff(
    focus_share_x_poc,
    wkfrc16_x_poc,
    category_col = "poc",
    positive_category = "POC",
    soc_col = "soc3_focus"
  ) %>% setDT() %>%
    setnames(
      c("signed_diff", "signed_diff_moe"),
      c("poc_share_diff", "poc_share_diff_moe"),
      skip_absent = TRUE
    )
} else {
  focus_signed_poc <- NULL
}

# --- Combined table for comparisons --- 
soc_stats <- copy(focus_soc) %>%
  merge(focus_tvd_race, by.x = "soc3_code", by.y = "soc3_focus", all.x = TRUE) %>%
  merge(focus_signed_female, by.x = "soc3_code", by.y = "soc3_focus", all.x = TRUE) %>%
  merge(focus_signed_poc, by.x = "soc3_code", by.y = "soc3_focus", all.x = TRUE) %>%
  .[, soc3_focus := soc3_code] %>%
  setorder(-openings_total_24_34)

# --- Occupational concentration by race/ethnicity--- 

focus_share_x_race_dt <- copy(focus_share_x_race) %>%
  setDT() %>% .[
    !is.na(soc3_focus),
    .(soc3_focus, prace_adj, share_focus = share, share_moe_focus = share_moe)
  ]

wkfrc16_x_race_dt <- copy(wkfrc16_x_race) %>%
  setDT() %>% .[
    ,
    .(prace_adj, share_overall = share, share_moe_overall = share_moe)
  ]

# Long format: difference in race/ethnicity share and its Z-score
race_conc_long <- merge(
  wkfrc16_x_race_dt,
  focus_share_x_race_dt,
  by = "prace_adj",
  allow.cartesian = TRUE
)

# Difference in shares: + means overrepresented in occupation vs workforce
race_conc_long[
  ,
  diff_share := share_focus - share_overall
]

# Approximate MOE of the share difference and corresponding Z-score
# Assumes share_moe_* are 90% MOEs (so z_crit ≈ qnorm(0.95))
z_crit <- qnorm(0.95)

race_conc_long[
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
race_conc_long[, race_code := make.names(as.character(prace_adj))]

# Wide crosstab: one row per occupation, two cols per prace_adj (diff + z)
race_levels <- sort(unique(race_conc_long$race_code))

race_diff_wide <- dcast(
  race_conc_long,
  soc3_focus ~ race_code,
  value.var = "diff_share"
)
setnames(
  race_diff_wide,
  old = race_levels,
  new = paste0(race_levels, "_diff")
)

race_z_wide <- dcast(
  race_conc_long,
  soc3_focus ~ race_code,
  value.var = "z_score"
)
setnames(
  race_z_wide,
  old = race_levels,
  new = paste0(race_levels, "_z")
)

# Final crosstab: one row per focus_soc occupation
race_conc_crosstab <- merge(
  race_diff_wide,
  race_z_wide,
  by = "soc3_focus",
  all = TRUE
)
race_conc_crosstab <- merge(race_conc_crosstab, soc3_labels,
                            by.x = "soc3_focus", by.y = "soc3_code", all.x = TRUE)
