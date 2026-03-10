# read_cpi_monthly.R
# Discovers and ingests the latest ABS Monthly CPI Indicator xlsx (Cat. 6484.0)
# Returns a tidy dataframe for use in app_monthly.R

library(readxl)
library(tidyverse)
library(httr)

#-----------------------------------------------------------
# 1. Find the latest release URL by scraping the ABS page
#-----------------------------------------------------------
get_latest_cpi_monthly_url <- function() {
  abs_page <- "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/monthly-consumer-price-index-indicator/latest-release"

  resp <- tryCatch(
    GET(abs_page, timeout(30)),
    error = function(e) NULL
  )

  if (is.null(resp) || http_error(resp)) {
    stop("Could not reach ABS latest-release page.")
  }

  page_text <- content(resp, as = "text", encoding = "UTF-8")

  # Extract paths matching the xlsx pattern
  matches <- regmatches(page_text, gregexpr('/statistics[^"\']*648401\\.xlsx', page_text))[[1]]

  if (length(matches) == 0) stop("No 648401.xlsx link found on ABS page.")

  url1 <- paste0("https://www.abs.gov.au", matches[1])
  url2 <- sub("648401", "648402", url1)

  list(table1 = url1, table2 = url2)
}


#-----------------------------------------------------------
# 2. Parse a single ABS monthly CPI xlsx file into tidy long format
#-----------------------------------------------------------
parse_cpi_monthly_xlsx <- function(path, measure_filter = NULL) {
  # Row 1 (after skip=0): series descriptions  "Measure ; Series name ; Region ;"
  # Row 10: Series IDs
  # Row 11+ (after skip=10): date (POSIXct) + numeric values

  header <- read_excel(path, sheet = "Data1", col_names = FALSE, n_max = 1)
  series_desc <- as.character(header[1, -1])          # drop date column

  # Extract clean series name: middle segment between first and last " ; "
  clean_name <- function(x) {
    parts <- strsplit(trimws(x), "\\s*;\\s*")[[1]]
    parts <- parts[parts != ""]
    if (length(parts) >= 2) trimws(parts[2]) else trimws(parts[1])
  }

  measure_type <- function(x) {
    parts <- strsplit(trimws(x), "\\s*;\\s*")[[1]]
    trimws(parts[1])
  }

  series_names   <- sapply(series_desc, clean_name,   USE.NAMES = FALSE)
  series_measure <- sapply(series_desc, measure_type, USE.NAMES = FALSE)

  # Read data (skip=10 → skip 10 header rows, row 11 onward)
  data_raw <- read_excel(path, sheet = "Data1", col_names = FALSE, skip = 10)

  dates <- as.Date(data_raw[[1]])
  values <- data_raw[, -1]                              # drop date column

  # Build tidy dataframe
  tidy <- map_dfr(seq_along(series_names), function(i) {
    tibble(
      date         = dates,
      CPI_components = series_names[i],
      measure      = series_measure[i],
      value        = suppressWarnings(as.numeric(values[[i]]))
    )
  })

  # Optionally filter to one measure type
  if (!is.null(measure_filter)) {
    tidy <- tidy %>% filter(grepl(measure_filter, measure, ignore.case = TRUE))
  }

  tidy
}


#-----------------------------------------------------------
# 3. Main function: download + parse → return index values
#-----------------------------------------------------------
read_cpi_monthly <- function(verbose = TRUE) {

  if (verbose) message("Finding latest ABS Monthly CPI release...")
  urls <- get_latest_cpi_monthly_url()
  if (verbose) message("  Table 1: ", urls$table1)

  tmp1 <- tempfile(fileext = ".xlsx")
  GET(urls$table1, write_disk(tmp1, overwrite = TRUE), timeout(60))

  if (verbose) message("Parsing Table 1 (index numbers + annual % change)...")
  tbl1 <- parse_cpi_monthly_xlsx(tmp1)

  # Table 1 contains two measure types:
  #   "Index Numbers"
  #   "Percentage Change from Corresponding Month of Previous Year"
  # Split into separate data frames
  index_dat <- tbl1 %>%
    filter(measure == "Index Numbers") %>%
    select(date, CPI_components, `Index value` = value)

  annual_pct <- tbl1 %>%
    filter(grepl("Corresponding Month", measure)) %>%
    select(date, CPI_components, annual_pct = value)

  # Optionally also grab Table 2 (monthly % change from previous period)
  # tmp2 <- tempfile(fileext = ".xlsx")
  # GET(urls$table2, write_disk(tmp2, overwrite = TRUE), timeout(60))
  # monthly_pct <- parse_cpi_monthly_xlsx(tmp2, measure_filter = "Previous Period") %>%
  #   select(date, CPI_components, monthly_pct = value)

  list(
    index     = index_dat,
    annual_pct = annual_pct,
    source_url = urls$table1
  )
}
