## Classify PUMS industries using the single category scheme in industry_regex.csv.
suppressPackageStartupMessages(library(dplyr))

# Recover raw NAICSP codes from the labels returned by get_psrc_pums().
make_naicsp_from_label <- function(data_year) {
  tidycensus::pums_variables %>%
    dplyr::filter(var_code == "NAICSP", year == data_year) %>%
    dplyr::distinct(val_max, val_label) %>%
    dplyr::transmute(
      naicsp_label = as.character(val_label),
      naicsp_code = as.character(val_max)
    ) %>%
    tibble::deframe()
}

industry_regex <- utils::read.csv(
  "industry_regex.csv", strip.white = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"
) %>%
  dplyr::mutate(dplyr::across(c(division, sector, regex), trimws)) %>%
  dplyr::arrange(recode_order)

sector_levels <- industry_regex %>%
  dplyr::arrange(display_order) %>%
  dplyr::pull(sector)

# The CSV contains R conditions on raw NAICSP codes and labeled COW values.
# Enable PCRE for its lookaheads; preserve the conditions and their precedence.
industry_rule_env <- rlang::env(
  grepl = function(pattern, x, ...) base::grepl(pattern, x, ..., perl = TRUE)
)
industry_conditions <- lapply(industry_regex$regex, rlang::parse_expr)

match_industry_row <- function(naicsp_code, cow) {
  rule_data <- list(NAICSP = as.character(naicsp_code), COW = as.character(cow))
  matched_row <- rep(NA_integer_, length(naicsp_code))
  for (i in seq_along(industry_conditions)) {
    hits <- rlang::eval_tidy(industry_conditions[[i]], data = rule_data,
                             env = industry_rule_env)
    # Like case_when(), the first TRUE condition wins; NA is not a match.
    matched_row[is.na(matched_row) & !is.na(hits) & hits] <- i
  }
  matched_row
}


