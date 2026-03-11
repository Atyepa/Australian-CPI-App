# read_cpi_6401_monthly.R
# Ingests the latest ABS CPI 6401.0 monthly xlsx tables.
# Returns tidy data for the new monthly CPI series (Apr 2024 onwards, Sep 2025 = 100).
# Sources: Table 3 (original series hierarchy) + Table 7 (seasonally adjusted).

library(readxl)
library(tidyverse)
library(httr)

#-----------------------------------------------------------
# 1. Scrape 6401.0 latest-release page for xlsx URLs
#-----------------------------------------------------------
get_latest_6401_monthly_urls <- function() {
  page_url <- "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/latest-release"
  resp <- tryCatch(GET(page_url, timeout(30)), error = function(e) NULL)
  if (is.null(resp) || http_error(resp))
    stop("Could not reach ABS 6401.0 latest-release page.")
  txt <- content(resp, as = "text", encoding = "UTF-8")

  find_url <- function(pattern) {
    hits <- regmatches(txt, gregexpr(pattern, txt))[[1]]
    if (!length(hits)) stop(paste("Link not found:", pattern))
    paste0("https://www.abs.gov.au", hits[1])
  }

  list(
    table3 = find_url("/statistics[^\"']*640103\\.xlsx"),
    table7 = find_url("/statistics[^\"']*640107\\.xlsx")
  )
}


#-----------------------------------------------------------
# 2. Parse one xlsx sheet: filter to Australia, return tidy
#    Reads all rows at once to avoid n_max/skip column-count mismatch.
#-----------------------------------------------------------
parse_6401_sheet <- function(path, sheet = "Data1") {
  # Read header rows (n_max=11 ensures all columns are detected from rows 7-10 which
  # have non-empty values in every column; n_max=1 can miss some columns).
  hdr_raw <- read_excel(path, sheet = sheet, col_names = FALSE, n_max = 11)
  # Read data rows separately so readxl correctly infers date types
  dat     <- read_excel(path, sheet = sheet, col_names = FALSE, skip = 10)

  raw   <- as.character(hdr_raw[1, -1])   # row 1 = series descriptions, drop date col
  dates <- as.Date(dat[[1]])
  vals  <- dat[, -1]

  get_part <- function(x, n) {
    p <- trimws(strsplit(x, ";")[[1]])
    p <- p[nzchar(p)]
    if (length(p) >= n) p[n] else NA_character_
  }

  s_measure <- sapply(raw, get_part, n = 1, USE.NAMES = FALSE)
  s_name    <- sapply(raw, get_part, n = 2, USE.NAMES = FALSE)
  s_region  <- sapply(raw, get_part, n = 3, USE.NAMES = FALSE)

  # Keep Australia weighted-average series; de-duplicate on measure+name
  keep <- !is.na(s_region) & trimws(s_region) == "Australia" &
          !duplicated(paste(s_measure, s_name))

  map_dfr(which(keep), function(i) {
    tibble(
      date    = dates,
      series  = s_name[i],
      measure = s_measure[i],
      value   = suppressWarnings(as.numeric(vals[[i]]))
    )
  }) %>% filter(!is.na(value))
}


#-----------------------------------------------------------
# 3. Main function: download, parse, return index + annual %
#-----------------------------------------------------------
read_cpi_6401_monthly <- function(verbose = TRUE) {

  if (verbose) message("Finding latest ABS 6401.0 monthly release...")
  urls <- get_latest_6401_monthly_urls()
  if (verbose) message("  Table 3: ", urls$table3)
  if (verbose) message("  Table 7: ", urls$table7)

  tmp3 <- tempfile(fileext = ".xlsx")
  tmp7 <- tempfile(fileext = ".xlsx")
  GET(urls$table3, write_disk(tmp3, overwrite = TRUE), timeout(90))
  GET(urls$table7, write_disk(tmp7, overwrite = TRUE), timeout(90))

  if (verbose) message("Parsing Table 3 (original series: index + annual %)...")
  tbl3 <- parse_6401_sheet(tmp3, sheet = "Data1")

  if (verbose) message("Parsing Table 7 (seasonally adjusted index numbers)...")
  tbl7 <- parse_6401_sheet(tmp7, sheet = "Data1")

  # --- Table 3: index numbers (all original series) ---
  index_t3 <- tbl3 %>%
    filter(measure == "Index Numbers") %>%
    rename(CPI_components = series, `Index value` = value) %>%
    select(date, CPI_components, `Index value`)

  # --- Table 3: ABS pre-computed annual % change ---
  annual_t3 <- tbl3 %>%
    filter(grepl("Corresponding Month", measure)) %>%
    rename(CPI_components = series, annual_pct = value) %>%
    select(date, CPI_components, annual_pct)

  # --- Table 7: analytical series (seasonally adjusted) ---
  # Include the top-level analytical series; expenditure classes available too
  # but series at group level are only in Table 3 with SA label
  analytical <- c(
    "All groups CPI, seasonally adjusted",
    "Tradables", "Non-tradables",
    "All groups, goods component", "All groups, services component"
  )

  index_t7 <- tbl7 %>%
    filter(series %in% analytical) %>%
    rename(CPI_components = series, `Index value` = value) %>%
    select(date, CPI_components, `Index value`)

  # Compute annual % change for Table 7 series (not pre-supplied by ABS)
  annual_t7 <- index_t7 %>%
    group_by(CPI_components) %>%
    arrange(date) %>%
    mutate(annual_pct = round(`Index value` / lag(`Index value`, 12) * 100 - 100, 1)) %>%
    filter(!is.na(annual_pct)) %>%
    select(date, CPI_components, annual_pct) %>%
    ungroup()

  # --- Combine ---
  index_dat  <- bind_rows(index_t3, index_t7)
  annual_dat <- bind_rows(annual_t3, annual_t7)

  list(
    index      = index_dat,
    annual_pct = annual_dat,
    source_url = urls$table3
  )
}
