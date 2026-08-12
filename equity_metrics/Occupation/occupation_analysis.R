library(psrccensus)
library(magrittr)
library(dplyr)
library(srvyr)
library(data.table)
library(tibble)
library(psrcplot)
library(ggplot2)

source("Occupation/get_soc_projections.R")
source("Occupation/get_soc_ed_requirements.R")
#source("get_kc_living_wage.R")
dir  = "C:/Users/mjensen/projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"
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
    "Occupation/raw_data/education.xlsx",
    sheet = "Table 5.4"
  ) %>% setDT() %>% .[row.names.data.frame(.)!=1, 1:5] %>%
  setnames(c("Label", "Code", "Education", "Experience", "Training")) %>%
  .[, soc_code := gsub("-", "", Code)]

# Since psrccensus delivers labels, create lookup to return SOCP code itself
socp_from_label <- tidycensus::pums_variables %>% setDT() %>%
  .[var_code == "SOCP" & year == datayr, .(val_max, val_label)] %>%
  as_tibble() %>%
  transmute(socp_label = as.character(val_label),
            socp_code  = as.character(val_max)) %>%
  tibble::deframe()

# Retrieve the PUMS data; filter to +16 workforce and add SOC code
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
    socp_code = socp_from_label[as.character(SOCP)],
    prace_adj = factor(fifelse(grepl("Native|Other|^Two ", PRACE), 
                               "All else", as.character(PRACE)))) %>%
  mutate(
    soc_code = gsub("-", "", socp_code)
  ) %>% ungroup()

soc_median_age <- psrc_pums_median(pums2024_5_wkfrc16,
                                   stat_var = "AGEP",
                                   group_vars = c("socp_code"),
                                   incl_na = FALSE) %>% setDT()

soc_median_pay  <- psrc_pums_median(pums2024_5_wkfrc16,
                                    stat_var = "WAGP",
                                    group_vars = c("socp_code"),
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

soc_combined  <- soc_median_pay[WAGP_median > living_wage, .(socp_code)] %>%
  .[, soc_code := sub("X", "0", socp_code)] %>%
  .[soc_projections, on = .(soc_code), nomatch = 0] %>%
  .[soc_training_req,  on = .(soc_code), nomatch = 0] %>%
soc_combined_2 <- soc_combined[soc_median_age,  on = .(soc_code=socp_code)]

# Filter to positive growth, no college required
focus_soc <- soc_combined[cagr_23_33 > 0 & !grepl("^(Bach|Mast|Doct)", Education)] %>%
  .[, .(soc_code, socp_code)]

# Mark occupations meeting wage + growth criteria; use socp codes
pums2024_5_wkfrc16 %<>% mutate(
  soc_focus = if_else(soc_code %chin% focus_soc$soc_code, soc_code, NA_character_)
)

# Flag groups with at least one observation
pums2024_5_wkfrc16 <- pums2024_5_wkfrc16 %>%
  group_by(soc_focus, PRACE) %>%
  mutate(n_soc_x_race = sum(!is.na(WAGP))) %>%
  ungroup() %>%
  group_by(soc_focus, poc) %>%
  mutate(n_soc_x_poc = sum(!is.na(WAGP))) %>%
  ungroup() %>%
  group_by(soc_focus, SEX) %>%
  mutate(n_soc_x_sex = sum(!is.na(WAGP))) %>%
  ungroup()

# Restrict the survey design/data to those combos
pums_23_5_focus <- pums2024_5_wkfrc16 %>% filter(!is.na(soc_focus))

focus_pay_x_race <- psrc_pums_median(filter(pums_23_5_focus, n_soc_x_race > 0), 
                                     stat_var = "WAGP",
                                     group_vars = c("soc_focus", "prace_adj"),
                                     incl_na = FALSE) %>%
  filter(prace_adj != "Total")

focus_pay_x_sex  <- psrc_pums_median(filter(pums_23_5_focus, n_soc_x_sex > 0), 
                                     stat_var = "WAGP",
                                     group_vars = c("soc_focus", "SEX"),
                                     incl_na = FALSE) %>%
  filter(SEX != "Total")

focus_share_x_race <- psrc_pums_count(filter(pums_23_5_focus, n_soc_x_race > 0), 
                                      group_vars = c("soc_focus", "prace_adj"),
                                      incl_na = FALSE) %>%
  filter(prace_adj != "Total")

focus_share_x_sex  <- psrc_pums_count(filter(pums_23_5_focus, n_soc_x_sex > 0), 
                                      group_vars = c("soc_focus", "SEX"),
                                      incl_na = FALSE) %>%
  filter(SEX != "Total")

focus_share_x_poc  <- psrc_pums_count(filter(pums_23_5_focus, n_soc_x_poc > 0),
                                      group_vars = c("soc_focus", "poc"),
                                      incl_na = FALSE) %>%
  filter(poc != "Total")

# ---- Concentration metrics ----
# TVD(P,Q) = 0.5 * sum_i |p_i - q_i| across categories i.
# Approximate MOE of TVD (if desired) treats category differences as independent:
# diff_moe_i = sqrt(share_moe_focus_i^2 + share_moe_overall_i^2)
# tvd_moe ≈ 0.5 * sqrt(sum(diff_moe_i^2)) [root-sum-square after scaling].

compute_tvd <- function(focus_dt, overall_dt, category_col, soc_col = "soc_focus",
                        share_col = "share", share_moe_col = "share_moe",
                        include_moe = TRUE) {
  # Base R implementation to avoid NSE lint issues
  f <- as.data.frame(focus_dt)
  o <- as.data.frame(overall_dt)

  # Shares
  fsub <- f[, c(soc_col, category_col, share_col)]
  osub <- o[, c(category_col, share_col)]
  colnames(fsub) <- c("soc_focus", "cat", "focus_share")
  colnames(osub) <- c("cat", "overall_share")
  merged <- merge(osub, fsub, by = "cat", all.x = TRUE)
  merged$focus_share[is.na(merged$focus_share)] <- 0
  merged$overall_share[is.na(merged$overall_share)] <- 0
  merged$diff <- abs(merged$focus_share - merged$overall_share)
  tvd <- aggregate(diff ~ soc_focus, data = merged, FUN = sum)
  tvd$tvd <- 0.5 * tvd$diff
  tvd$diff <- NULL

  # Optionally compute MOE of TVD using root-sum-square of category MOEs
  if (isTRUE(include_moe) && share_moe_col %in% names(f) && share_moe_col %in% names(o)) {
    fmoe <- f[, c(soc_col, category_col, share_moe_col)]
    omoe <- o[, c(category_col, share_moe_col)]
    colnames(fmoe) <- c("soc_focus", "cat", "focus_moe")
    colnames(omoe) <- c("cat", "overall_moe")
    merged_moe <- merge(omoe, fmoe, by = "cat", all.x = TRUE)
    merged_moe$focus_moe[is.na(merged_moe$focus_moe)] <- 0
    merged_moe$overall_moe[is.na(merged_moe$overall_moe)] <- 0
    merged_moe$diff_moe <- sqrt(merged_moe$focus_moe^2 + merged_moe$overall_moe^2)
    tvd_moe <- aggregate(diff_moe ~ soc_focus, data = merged_moe, FUN = function(x) 0.5 * sqrt(sum(x^2)))
    colnames(tvd_moe)[colnames(tvd_moe) == "diff_moe"] <- "tvd_moe"
    tvd <- merge(tvd, tvd_moe, by = "soc_focus", all.x = TRUE)
  }

  setDT(tvd)
  tvd[]
}

# Signed difference in proportions for dichotomous variables.
# Positive values indicate concentration in the named positive category;
# negative values indicate concentration in the complementary category.
compute_signed_binary_diff <- function(
    focus_dt,
    overall_dt,
    category_col,
    positive_category,
    soc_col = "soc_focus",
    share_col = "share",
    share_moe_col = "share_moe",
    include_moe = TRUE) {
  f <- as.data.frame(focus_dt)
  o <- as.data.frame(overall_dt)

  o_cat_values <- unique(stats::na.omit(as.character(o[[category_col]])))
  positive_match <- match(tolower(positive_category), tolower(o_cat_values))

  if (is.na(positive_match)) {
    stop(sprintf(
      "Positive category '%s' was not found in overall_dt$%s",
      positive_category,
      category_col
    ), call. = FALSE)
  }

  positive_value <- o_cat_values[[positive_match]]
  focus_soc <- data.frame(
    soc_focus = unique(as.character(f[[soc_col]])),
    stringsAsFactors = FALSE
  )

  f_cat_values <- as.character(f[[category_col]])
  o_cat_values <- as.character(o[[category_col]])

  fsub <- f[f_cat_values == positive_value, c(soc_col, share_col), drop = FALSE]
  colnames(fsub) <- c("soc_focus", "focus_share")

  osub <- o[o_cat_values == positive_value, share_col, drop = FALSE]
  overall_share <- if (nrow(osub) > 0) as.numeric(osub[[share_col]][1]) else 0

  signed_diff <- merge(focus_soc, fsub, by = "soc_focus", all.x = TRUE)
  signed_diff$focus_share[is.na(signed_diff$focus_share)] <- 0
  signed_diff$signed_diff <- signed_diff$focus_share - overall_share
  signed_diff$focus_share <- NULL

  if (isTRUE(include_moe) && share_moe_col %in% names(f) && share_moe_col %in% names(o)) {
    fmoe <- f[f_cat_values == positive_value, c(soc_col, share_moe_col), drop = FALSE]
    colnames(fmoe) <- c("soc_focus", "focus_moe")

    omoe <- o[o_cat_values == positive_value, share_moe_col, drop = FALSE]
    overall_moe <- if (nrow(omoe) > 0) as.numeric(omoe[[share_moe_col]][1]) else 0

    signed_diff <- merge(signed_diff, fmoe, by = "soc_focus", all.x = TRUE)
    signed_diff$focus_moe[is.na(signed_diff$focus_moe)] <- 0
    signed_diff$signed_diff_moe <- sqrt(signed_diff$focus_moe^2 + overall_moe^2)
    signed_diff$focus_moe <- NULL
  }

  setDT(signed_diff)
  signed_diff[]
}

# Race TVD
if (exists("wkfrc16_x_race") && exists("focus_share_x_race") &&
    !is.null(wkfrc16_x_race) && !is.null(focus_share_x_race)) {
  focus_tvd_race <- compute_tvd(focus_share_x_race,
                                wkfrc16_x_race,
                                category_col = "prace_adj",
                                soc_col = "soc_focus") %>% setDT() %>%
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
    soc_col = "soc_focus"
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
    soc_col = "soc_focus"
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
  .[soc_projections,   on = .(soc_code)] %>%
  .[soc_training_req,  on = .(soc_code)] %>%
  .[soc_median_pay,    on = .(socp_code)] %>%
  .[focus_tvd_race, on = .(soc_code = soc_focus)] %>%
  .[focus_signed_female, on = .(soc_code = soc_focus)] %>%
  .[focus_signed_poc, on = .(soc_code = soc_focus)] %>%
  .[!is.na(cagr_23_33), .(soc_code, Label, emp_2023, emp_2033, cagr_23_33,
                          openings_total_23_33, Education, WAGP_median, 
                          WAGP_median_moe, tvd_race, tvd_race_moe,
                          female_share_diff, female_share_diff_moe,
                          poc_share_diff, poc_share_diff_moe)] %>%
  setorder(-openings_total_23_33)

# --- Occupational concentration by race/ethnicity--- 

focus_share_x_race_dt <- copy(focus_share_x_race) %>%
  setDT() %>% .[
    !is.na(soc_focus),
    .(soc_focus, prace_adj, share_focus = share, share_moe_focus = share_moe)
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
race_conc_long[
  ,
  race_code := make.names(as.character(prace_adj))
]

# Wide crosstab: one row per occupation, two cols per prace_adj (diff + z)
race_levels <- sort(unique(race_conc_long$race_code))

race_diff_wide <- dcast(
  race_conc_long,
  soc_focus ~ race_code,
  value.var = "diff_share"
)
setnames(
  race_diff_wide,
  old = race_levels,
  new = paste0(race_levels, "_diff")
)

race_z_wide <- dcast(
  race_conc_long,
  soc_focus ~ race_code,
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
  by = "soc_focus",
  all = TRUE
)
