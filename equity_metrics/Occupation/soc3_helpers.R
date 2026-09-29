# Prepare SOC3 reporting inputs before joining to PUMS summaries.
roll_up_soc3 <- function(projections, education) {
  p <- data.table::copy(data.table::as.data.table(projections))
  e <- data.table::copy(data.table::as.data.table(education))
  # The ESD file includes major/minor/intermediate subtotals ending in 00.
  # Retain published leaf occupations, including combined codes ending in 0.
  p <- p[grepl("^[0-9]{6}$", soc_code) & !grepl("00$", soc_code)]
  e <- e[grepl("^[0-9]{6}$", soc_code), .(soc_code, Education)]
  stopifnot(!anyDuplicated(p$soc_code), !anyDuplicated(e$soc_code))
  detail <- merge(p, e, by = "soc_code", all.x = TRUE)
  detail[, soc3_code := substr(soc_code, 1, 3)]
  detail[is.na(Education) | trimws(Education) == "", Education := "Unknown"]
  below_bachelors <- c("No formal educational credential",
                       "High school diploma or equivalent", "Some college, no degree",
                       "Postsecondary nondegree award", "Associate's degree")
  # Missing projected values stay missing rather than silently becoming zero.
  groups <- detail[, .(
    emp_2024 = sum(emp_2024),
    emp_2034 = sum(emp_2034),
    openings_total_24_34 = sum(openings_total_24_34),
    openings_below_bachelors = sum(openings_total_24_34[Education %in% below_bachelors]),
    openings_education_unknown = sum(openings_total_24_34[Education == "Unknown"])
  ), by = soc3_code]
  groups[, `:=`(
    cagr_24_34 = data.table::fifelse(emp_2024 > 0 & emp_2034 > 0,
                                  (emp_2034 / emp_2024)^(1 / 10) - 1, NA_real_),
    below_bachelors_share = data.table::fifelse(openings_total_24_34 > 0,
      openings_below_bachelors / openings_total_24_34, NA_real_),
    education_unknown_share = data.table::fifelse(openings_total_24_34 > 0,
      openings_education_unknown / openings_total_24_34, NA_real_)
  )]
  education_shares <- detail[, .(openings = sum(openings_total_24_34)),
                             by = .(soc3_code, Education)]
  education_shares <- merge(education_shares,
    groups[, .(soc3_code, openings_total_24_34)], by = "soc3_code", all.x = TRUE)
  education_shares[, openings_share := data.table::fifelse(openings_total_24_34 > 0,
    openings / openings_total_24_34, NA_real_)]
  list(projections = groups, education_shares = education_shares)
}

select_focus_soc3 <- function(soc_combined, filter_type, living_wage) {
  filter_type <- match.arg(filter_type, c("low_barriers", "high_growth", "living_wage"))
  focus <- data.table::copy(data.table::as.data.table(soc_combined))[cagr_24_34 > 0 & openings_total_24_34 > 5000]
  if (filter_type == "low_barriers") {
    focus <- focus[below_bachelors_share > 0.5 & WAGP_median > living_wage]
  } else if (filter_type == "high_growth") {
    focus <- focus[cagr_24_34 > 0.01]
  } else if (filter_type == "living_wage") {
    focus <- focus[WAGP_median > living_wage]
  }
  focus
}
