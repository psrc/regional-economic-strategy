## get_naics_projections.R
##
## Download and process Washington ESD long-term aggregated industry
## projections for King, Pierce, and Snohomish Counties and produce a
## combined total.
##
## Note: unlike the occupation projections file, this workbook has no NAICS
## code column -- only named industry titles (the standard BLS CES
## supersector/sector hierarchy). Titles are filtered downstream to the
## sector scheme supplied by industry_regex.csv.
##
## Usage:
##   source("Industry/get_naics_projections.R")
##   naics_projections <- get_naics_projections()
##
## Returns:
##   A tibble with one row per industry title: combined (King+Pierce+Snohomish)
##   employment for 2024/2029/2034 and recomputed CAGRs.

get_naics_projections <- function() {
  suppressPackageStartupMessages({
    library(readxl)
    library(dplyr)
    library(stringr)
    library(tidyr)
    library(purrr)
  })

  url_candidates <- c(
    "https://esd.wa.gov/media/xlsx/3798/2026-long-term-aggregated-industry-projectionsxlsx"
  )

  sheet_names <- c("Seattle-King County", "Tacoma-Pierce", "Snohomish")

  canonical_targets <- c(
    title = "Title",
    emp_2024 = "Estimated employment 2024",
    emp_2029 = "Estimated employment 2029",
    emp_2034 = "Estimated employment 2034"
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
    df <- df %>% mutate(title = as.character(title) %>% stringr::str_trim())
    num_cols <- setdiff(names(df), "title")
    if (length(num_cols) > 0) {
      df <- df %>% mutate(across(all_of(num_cols), ~ {
        v <- as.character(.)
        v <- gsub(",", "", v)
        suppressWarnings(as.numeric(v))
      }))
    }
    df %>% filter(!is.na(title), title != "")
  }

  compute_combined <- function(df_list) {
    df_list %>%
      bind_rows(.id = "region") %>%
      group_by(title) %>%
      summarise(
        emp_2024 = sum(emp_2024, na.rm = TRUE),
        emp_2029 = sum(emp_2029, na.rm = TRUE),
        emp_2034 = sum(emp_2034, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        cagr_24_29 = if_else(emp_2024 > 0 & emp_2029 > 0, (emp_2029 / emp_2024)^(1/5) - 1, NA_real_),
        cagr_29_34 = if_else(emp_2029 > 0 & emp_2034 > 0, (emp_2034 / emp_2029)^(1/5) - 1, NA_real_),
        cagr_24_34 = if_else(emp_2024 > 0 & emp_2034 > 0, (emp_2034 / emp_2024)^(1/10) - 1, NA_real_),
        chg_24_34 = emp_2034 - emp_2024
      )
  }

  local_xlsx <- file.path(tempdir(), "2026-long-term-aggregated-industry-projections.xlsx")
  get <- safe_download(url_candidates, local_xlsx)
  if (!isTRUE(get$success)) {
    stop(
      paste0(
        "Failed to download industry projections. Tried URLs: ",
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

