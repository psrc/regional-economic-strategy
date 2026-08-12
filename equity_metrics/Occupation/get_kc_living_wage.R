## get_kc_living_wage.R
##
## Pull latest 2-earner, 2-child household living wage for King County
## from the MIT Living Wage Calculator
## cite as "Living wage data sourced from the Living Wage Institute via 
## https://livingwage.mit.edu/counties/53033, Accessed on [access date]" 
##
## Usage:
##   source("get_kc_living_wage.R")
##   living_wage <- get_mit_living_wage_king_2adults_2children()
##
## Returns:
##   A numeric value

get_mit_living_wage_king_2adults_2children <- function() {
  suppressPackageStartupMessages({
    library(rvest)
    library(dplyr)
    library(stringr)
    library(readr)
    library(purrr)
  })
  
  
  url <- "https://livingwage.mit.edu/counties/53033"  # King County, WA
  
  # Read page
  page <- read_html(url)
  
  # Parse all HTML tables on the page
  tables <- page %>% html_table(fill = TRUE)
  
  # Find the table that contains a "Living Wage" row
  living_tbl <- tables %>%
    keep(~ any(str_detect(.x[[1]], regex("living wage", ignore_case = TRUE)))) %>%
    first()
  
  if (is.null(living_tbl)) {
    stop("Could not find a table with a 'Living Wage' row on the MIT page.")
  }
  
  # Isolate the 'Living Wage' row
  lw_row <- living_tbl %>%
    filter(str_detect(.[[1]], regex("living wage", ignore_case = TRUE)))
  
  if (nrow(lw_row) == 0) {
    stop("Could not find the 'Living Wage' row in the table.")
  }
  
  # Drop the first column (row label), leaving the 12 family-type cells:
  # 1 adult (0–3 children), 2 adults 1 working (0–3), 2 adults both working (0–3)
  vals_raw <- lw_row[ , -1, drop = FALSE]
  
  # Flatten and parse as numbers (strip $ and commas)
  vals_num <- vals_raw %>%
    unlist(use.names = FALSE) %>%
    readr::parse_number(locale = locale(grouping_mark = ","))
  
  if (length(vals_num) < 11) {
    stop("Unexpected table layout: fewer than 12 Living Wage values found.")
  }
  
  # Positions (left to right):
  # 1–4:  1 adult (0–3 children)
  # 5–8:  2 adults, 1 working (0–3 children)
  # 9–12: 2 adults, both working (0–3 children)
  #
  # We want: 2 adults, both working, 2 children  -> position 11
  living_wage_2adults_both_2children <- vals_num[11]
  
  # Optionally, get the "last updated" text for metadata
  page_text <- page %>% html_text2()
  last_updated <- str_match(
    page_text,
    "data on this page was last updated on ([A-Za-z]+ \\d{1,2}, \\d{4})"
  )[ , 2]
  
  list(
    county   = "King County, Washington",
    url      = url,
    last_updated = last_updated,
    living_wage_hourly_per_adult = living_wage_2adults_both_2children
  )
}

# Example usage:
result <- get_mit_living_wage_king_2adults_2children()
result$living_wage_hourly_per_adult
result
