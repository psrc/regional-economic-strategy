# Shared, domain-agnostic concentration metrics for PUMS "focus group" analyses
# (used by both the Occupation/SOC and Industry/NAICS equity workflows).
#
# Usage:
#   source("pums_concentration_helpers.R")

# ---- Concentration metrics ----
# TVD(P,Q) = 0.5 * sum_i |p_i - q_i| across categories i.
# Approximate MOE of TVD (if desired) treats category differences as independent:
# diff_moe_i = sqrt(share_moe_focus_i^2 + share_moe_overall_i^2)
# tvd_moe ≈ 0.5 * sqrt(sum(diff_moe_i^2)) [root-sum-square after scaling].

compute_tvd <- function(focus_dt, overall_dt, category_col, soc_col = "soc3_focus",
                        share_col = "share", share_moe_col = "share_moe",
                        include_moe = TRUE) {
  # Base R implementation to avoid NSE lint issues
  f <- as.data.frame(focus_dt)
  o <- as.data.frame(overall_dt)

  # Shares
  fsub <- f[, c(soc_col, category_col, share_col)]
  osub <- o[, c(category_col, share_col)]
  colnames(fsub) <- c("soc3_focus", "cat", "focus_share")
  colnames(osub) <- c("cat", "overall_share")
  merged <- merge(osub, fsub, by = "cat", all.x = TRUE)
  merged$focus_share[is.na(merged$focus_share)] <- 0
  merged$overall_share[is.na(merged$overall_share)] <- 0
  merged$diff <- abs(merged$focus_share - merged$overall_share)
  tvd <- aggregate(diff ~ soc3_focus, data = merged, FUN = sum)
  tvd$tvd <- 0.5 * tvd$diff
  tvd$diff <- NULL

  # Optionally compute MOE of TVD using root-sum-square of category MOEs
  if (isTRUE(include_moe) && share_moe_col %in% names(f) && share_moe_col %in% names(o)) {
    fmoe <- f[, c(soc_col, category_col, share_moe_col)]
    omoe <- o[, c(category_col, share_moe_col)]
    colnames(fmoe) <- c("soc3_focus", "cat", "focus_moe")
    colnames(omoe) <- c("cat", "overall_moe")
    merged_moe <- merge(omoe, fmoe, by = "cat", all.x = TRUE)
    merged_moe$focus_moe[is.na(merged_moe$focus_moe)] <- 0
    merged_moe$overall_moe[is.na(merged_moe$overall_moe)] <- 0
    merged_moe$diff_moe <- sqrt(merged_moe$focus_moe^2 + merged_moe$overall_moe^2)
    tvd_moe <- aggregate(diff_moe ~ soc3_focus, data = merged_moe, FUN = function(x) 0.5 * sqrt(sum(x^2)))
    colnames(tvd_moe)[colnames(tvd_moe) == "diff_moe"] <- "tvd_moe"
    tvd <- merge(tvd, tvd_moe, by = "soc3_focus", all.x = TRUE)
  }

  setDT(tvd)
  # Return the focus-group key under the caller's requested column name.
  # The calculations use soc3_focus internally for historical compatibility.
  setnames(tvd, "soc3_focus", soc_col)
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
    soc_col = "soc3_focus",
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
    soc3_focus = unique(as.character(f[[soc_col]])),
    stringsAsFactors = FALSE
  )

  f_cat_values <- as.character(f[[category_col]])
  o_cat_values <- as.character(o[[category_col]])

  fsub <- f[f_cat_values == positive_value, c(soc_col, share_col), drop = FALSE]
  colnames(fsub) <- c("soc3_focus", "focus_share")

  osub <- o[o_cat_values == positive_value, share_col, drop = FALSE]
  overall_share <- if (nrow(osub) > 0) as.numeric(osub[[share_col]][1]) else 0

  signed_diff <- merge(focus_soc, fsub, by = "soc3_focus", all.x = TRUE)
  signed_diff$focus_share[is.na(signed_diff$focus_share)] <- 0
  signed_diff$signed_diff <- signed_diff$focus_share - overall_share
  signed_diff$focus_share <- NULL

  if (isTRUE(include_moe) && share_moe_col %in% names(f) && share_moe_col %in% names(o)) {
    fmoe <- f[f_cat_values == positive_value, c(soc_col, share_moe_col), drop = FALSE]
    colnames(fmoe) <- c("soc3_focus", "focus_moe")

    omoe <- o[o_cat_values == positive_value, share_moe_col, drop = FALSE]
    overall_moe <- if (nrow(omoe) > 0) as.numeric(omoe[[share_moe_col]][1]) else 0

    signed_diff <- merge(signed_diff, fmoe, by = "soc3_focus", all.x = TRUE)
    signed_diff$focus_moe[is.na(signed_diff$focus_moe)] <- 0
    signed_diff$signed_diff_moe <- sqrt(signed_diff$focus_moe^2 + overall_moe^2)
    signed_diff$focus_moe <- NULL
  }

  setDT(signed_diff)
  # Restore the caller's focus-group key after the internal calculation.
  setnames(signed_diff, "soc3_focus", soc_col)
  signed_diff[]
}
