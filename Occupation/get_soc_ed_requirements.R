# Get BLS data on standard educational requirements per occupation

get_soc_edu_requirements <- function() {
  url  <- "https://www.bls.gov/emp/ind-occ-matrix/education.xlsx"
  dest <- file.path(tempdir(), "education.xlsx")
  
  httr::GET(
    url,
    httr::user_agent("Mozilla/5.0"),
    httr::write_disk(dest, overwrite = TRUE)
  )
  
  readxl::read_excel(dest)
}

get_bls_education <- function() {
  url  <- "https://www.bls.gov/emp/ind-occ-matrix/education.xlsx"
  dest <- file.path(tempdir(), "education.xlsx")
  
  resp <- httr::GET(
    url,
    httr::user_agent("Mozilla/5.0"),
    httr::write_disk(dest, overwrite = TRUE),
    httr::timeout(30)
  )
  
  # Check for HTTP errors
  httr::stop_for_status(resp)
  
  # Basic sanity check: Excel files start with "PK" (ZIP header)
  sig <- readBin(dest, "raw", n = 2)
  if (!identical(rawToChar(sig), "PK")) {
    stop("Downloaded file is not a valid .xlsx (got HTML or error page).")
  }
  
  readxl::read_excel(dest)
}
