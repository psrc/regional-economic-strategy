## get_soc_projections.R
##
## Download and process Washington ESD long-term occupational projections
## for King, Pierce, and Snohomish Counties and produce a combined total.
##
## Usage:
##   source("get_soc_projections.R")
##   data_list <- get_soc_projections()
##
## Returns:
##   A named list of tibbles: "Seattle-King County", "Tacoma-Pierce",
##   "Snohomish", and "Combined" (aggregated totals and calculated fields).

get_soc_projections <- function() {
  suppressPackageStartupMessages({
    library(readxl)
    library(dplyr)
    library(stringr)
    library(tidyr)
    library(purrr)
  })

  url_candidates <- c(
    "https://esd.wa.gov/media/xlsx/3794/long-occup-proj-alt-2025.xlsx",
    "https://esd.wa.gov/media/xlsx/3794/long-occup-proj-alt-2025xlsx"
  )

  sheet_names <- c("Seattle-King County", "Tacoma-Pierce", "Snohomish")

  canonical_targets <- c(
    soc = "SOC code",
    emp_2023 = "Estimated employment 2023",
    emp_2028 = "Estimated employment 2028",
    emp_2033 = "Estimated employment 2033",
    openings_growth_23_28 = "Average annual openings due to growth 2023-2028",
    openings_growth_28_33 = "Average annual openings due to growth 2028-2033",
    openings_total_23_28 = "Average annual total openings 2023-2028",
    openings_total_28_33 = "Average annual total openings 2028-2033"
  )

  normalize_label <- function(x) {
    x %>%
      tolower() %>%
      stringr::str_replace_all("[^a-z0-9]+", " ") %>%
      stringr::str_squish()
  }

  map_columns <- function(df, targets) {
    actual <- names(df)
    actual_norm <- normalize_label(actual)
    targets_norm <- normalize_label(unname(targets))
    idx <- match(targets_norm, actual_norm)
    missing_targets <- names(targets)[is.na(idx)]
    if (length(missing_targets) > 0) {
      warning(
        sprintf(
          "Missing expected columns: %s",
          paste(targets[missing_targets], collapse = ", ")
        ),
        call. = FALSE
      )
    }
    out <- actual[idx]
    names(out) <- names(targets)
    out
  }

  safe_download <- function(urls, destfile) {
    for (u in urls) {
      ok <- tryCatch({
        utils::download.file(u, destfile, mode = "wb", quiet = TRUE)
        file.exists(destfile) && isTRUE(file.info(destfile)$size > 0)
      }, error = function(e) FALSE, warning = function(w) FALSE)
      if (isTRUE(ok)) return(list(success = TRUE, url = u, file = destfile))
    }
    list(success = FALSE, url = NA_character_, file = destfile)
  }

  clean_and_select <- function(df_raw) {
    colmap <- map_columns(df_raw, canonical_targets)
    keep_cols <- colmap[!is.na(colmap)]
    df <- df_raw %>% dplyr::select(all_of(keep_cols))
    names(df) <- names(keep_cols)
    df <- df %>% mutate(soc = as.character(soc) %>% stringr::str_trim())
    num_cols <- setdiff(names(df), "soc")
    if (length(num_cols) > 0) {
      df <- df %>% mutate(across(all_of(num_cols), ~ {
        v <- as.character(.)
        v <- gsub(",", "", v)
        suppressWarnings(as.numeric(v))
      }))
    }
    df %>% filter(!is.na(soc), soc != "")
  }

  compute_combined <- function(df_list) {
    df_list %>%
      bind_rows(.id = "region") %>%
      group_by(soc) %>%
      summarise(
        emp_2023 = sum(emp_2023, na.rm = TRUE),
        emp_2028 = sum(emp_2028, na.rm = TRUE),
        emp_2033 = sum(emp_2033, na.rm = TRUE),
        openings_growth_23_28 = sum(openings_growth_23_28, na.rm = TRUE),
        openings_growth_28_33 = sum(openings_growth_28_33, na.rm = TRUE),
        openings_total_23_28 = sum(openings_total_23_28, na.rm = TRUE),
        openings_total_28_33 = sum(openings_total_28_33, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        openings_growth_23_33 = openings_growth_23_28 + openings_growth_28_33,
        openings_total_23_33 = openings_total_23_28 + openings_total_28_33,
        cagr_23_28 = if_else(emp_2023 > 0 & emp_2028 > 0, (emp_2028 / emp_2023)^(1/5) - 1, NA_real_),
        cagr_28_33 = if_else(emp_2028 > 0 & emp_2033 > 0, (emp_2033 / emp_2028)^(1/5) - 1, NA_real_),
        cagr_23_33 = if_else(emp_2023 > 0 & emp_2033 > 0, (emp_2033 / emp_2023)^(1/10) - 1, NA_real_)
      )
  }

  local_xlsx <- file.path(tempdir(), "long-occup-proj-alt-2025.xlsx")
  get <- safe_download(url_candidates, local_xlsx)
  if (!isTRUE(get$success)) {
    stop(
      paste0(
        "Failed to download occupational projections. Tried URLs: ",
        paste(url_candidates, collapse = "; "),
        ". Please verify the source URL."
      )
    )
  }

  raw_list <- setNames(vector("list", length(sheet_names)), sheet_names)
  for (s in sheet_names) {
    df_raw <- readxl::read_excel(get$file, sheet = s, skip = 4, guess_max = 10000)
    raw_list[[s]] <- clean_and_select(df_raw)
  }

  clean_list <- list(
    `Seattle-King County` = raw_list[["Seattle-King County"]],
    `Tacoma-Pierce` = raw_list[["Tacoma-Pierce"]],
    `Snohomish` = raw_list[["Snohomish"]]
  )

  combined_df <- compute_combined(clean_list)
  return(combined_df)
}
