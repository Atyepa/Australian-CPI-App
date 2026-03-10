library(tidyverse)
library(highcharter)
library(ggthemes)
library(DT)
library(httr)
library(readxl)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(lubridate)
library(writexl)
library(rsconnect)

#-----------------------------------------------------
#---- Ingestion: ABS Monthly CPI xlsx (Cat. 6484.0) --
#-----------------------------------------------------

# Scrape the ABS latest-release page to find the current xlsx URL
get_latest_cpi_monthly_url <- function() {
  page_url <- "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/monthly-consumer-price-index-indicator/latest-release"
  resp <- tryCatch(GET(page_url, timeout(30)), error = function(e) NULL)
  if (is.null(resp) || http_error(resp))
    stop("Could not reach ABS latest-release page.")
  txt <- content(resp, as = "text", encoding = "UTF-8")
  hits <- regmatches(txt, gregexpr('/statistics[^"\']*648401\\.xlsx', txt))[[1]]
  if (!length(hits)) stop("648401.xlsx link not found on ABS page.")
  url1 <- paste0("https://www.abs.gov.au", hits[1])
  list(table1 = url1,
       table2 = sub("648401", "648402", url1))
}

# Parse one sheet: extract series names + tidy data
parse_abs_xlsx <- function(path) {
  # Row 1 contains "Measure ; Series name ; Region ;" for each column
  header  <- read_excel(path, sheet = "Data1", col_names = FALSE, n_max = 1)
  raw_desc <- as.character(header[1, -1])

  extract_name    <- function(x) {
    p <- trimws(strsplit(x, ";")[[1]])
    p <- p[nzchar(p)]
    if (length(p) >= 2) p[2] else p[1]
  }
  extract_measure <- function(x) trimws(strsplit(x, ";")[[1]][1])

  series_name    <- sapply(raw_desc, extract_name,    USE.NAMES = FALSE)
  series_measure <- sapply(raw_desc, extract_measure, USE.NAMES = FALSE)

  # Rows 11+ (skip=10) are data: col1 = date (POSIXct), cols 2+ = numeric values
  dat <- read_excel(path, sheet = "Data1", col_names = FALSE, skip = 10)
  dates  <- as.Date(dat[[1]])
  vals   <- dat[, -1]

  # De-duplicate: keep first occurrence of each measure+name combination
  keep_idx <- which(!duplicated(paste(series_measure, series_name)))

  map_dfr(keep_idx, function(i) {
    tibble(
      date    = dates,
      series  = series_name[i],
      measure = series_measure[i],
      value   = suppressWarnings(as.numeric(vals[[i]]))
    )
  }) %>% filter(!is.na(value))
}

# Main ingestion: download Table 1 and return index + annual-pct frames
read_cpi_monthly <- function() {
  urls <- get_latest_cpi_monthly_url()
  tmp  <- tempfile(fileext = ".xlsx")
  GET(urls$table1, write_disk(tmp, overwrite = TRUE), timeout(60))

  all_dat <- parse_abs_xlsx(tmp)

  index_dat <- all_dat %>%
    filter(measure == "Index Numbers") %>%
    rename(CPI_components = series, `Index value` = value) %>%
    select(date, CPI_components, `Index value`)

  annual_dat <- all_dat %>%
    filter(grepl("Corresponding Month", measure, fixed = FALSE)) %>%
    rename(CPI_components = series, annual_pct = value) %>%
    select(date, CPI_components, annual_pct)

  list(index = index_dat, annual_pct = annual_dat, source_url = urls$table1)
}

#--- ABS preferred colours ---
abscol <- c("#4FADE7", "#1A4472", "#F29000", "#993366", "#669966", "#99CC66",
            "#CC9966", "#666666", "#8DD3C7", "#BEBADA", "#FB8072", "#80B1D3",
            "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#ffcc99")

#----------------------------------------
#---- SHINY DASHBOARD----
#----------------------------------------

ui <- fluidPage(theme = shinytheme("darkly"),

  tags$head(tags$style(HTML("
    .dataTables_length label, .dataTables_filter label, .dataTables_info {
        color: white!important; }
    .paginate_button { background: white!important; }
    thead { color: white; }
    table.dataTable { background-color: white!important; color: black!important; }
    table.dataTable th, table.dataTable td { color: black!important; }
    table.dataTable thead th, table.dataTable thead td {
        background-color: white!important; color: black!important; }
    .paginate_button, .paginate_button:hover, .paginate_button:active {
        color: black!important; background-color: white!important;
        border-color: black!important; }
  "))),

  headerPanel("Monthly CPI Indicator - weighted average of eight capital cities"),

  sidebarPanel(

    radioButtons("choosetable", "View:",
                 choices = c("Index value"            = "index",
                             "Cumulative % change"    = "pc",
                             "Year-on-year %"         = "yoy",
                             "Annual average %"       = "avg"),
                 selected = "pc", inline = FALSE),

    conditionalPanel(
      condition = "input.choosetable == 'pc' || input.choosetable == 'yoy'",
      radioButtons("plottype", "Chart type:",
                   choices = c("Line (time-series)" = "Line",
                               "Bar (latest month)"  = "Bar"),
                   selected = "Line", inline = FALSE)
    ),

    uiOutput("MonthRange_ui"),

    tags$div(tags$hr()),
    tags$div(tags$h4(tags$strong("Select CPI items:"))),

    pickerInput("List", " ",
      choices = c(
        "--- All groups ---"                                               = "--- All groups ---",
        "All groups CPI"                                                   = "All groups CPI",
        "All groups CPI excl. volatile items"                              = "All groups CPI excluding 'volatile items'",
        "All groups CPI excl. volatile items and holiday travel"           = "All groups CPI excluding 'volatile items' and holiday travel",
        "All groups CPI, seasonally adjusted"                              = "All groups CPI, seasonally adjusted",
        "All groups, goods component"                                      = "All groups, goods component",
        "All groups, services component"                                   = "All groups, services component",
        "Tradables"                                                        = "Tradables",
        "Non-tradables"                                                    = "Non-tradables",
        "--- Groups ---"                                                   = "--- Groups ---",
        "..Food and non-alcoholic beverages"                               = "Food and non-alcoholic beverages",
        "....Bread and cereal products"                                    = "Bread and cereal products",
        "....Meat and seafoods"                                            = "Meat and seafoods",
        "....Dairy and related products"                                   = "Dairy and related products",
        "....Fruit and vegetables"                                         = "Fruit and vegetables",
        "....Food products n.e.c."                                         = "Food products n.e.c.",
        "....Non-alcoholic beverages"                                      = "Non-alcoholic beverages",
        "..Clothing and footwear"                                          = "Clothing and footwear",
        "....Garments"                                                     = "Garments",
        "..Housing"                                                        = "Housing",
        "....Rents"                                                        = "Rents",
        "....New dwelling purchase by owner-occupiers"                     = "New dwelling purchase by owner-occupiers",
        "....Electricity"                                                  = "Electricity",
        "....Gas and other household fuels"                                = "Gas and other household fuels",
        "..Furnishings, household equipment and services"                  = "Furnishings, household equipment and services",
        "..Health"                                                         = "Health",
        "..Transport"                                                      = "Transport",
        "....Automotive fuel"                                              = "Automotive fuel",
        "....Holiday travel and accommodation"                             = "Holiday travel and accommodation",
        "..Communication"                                                  = "Communication",
        "..Recreation and culture"                                         = "Recreation and culture",
        "..Education"                                                      = "Education",
        "..Alcohol and tobacco"                                            = "Alcohol and tobacco",
        "....Alcoholic beverages"                                          = "Alcoholic beverages",
        "....Tobacco"                                                      = "Tobacco",
        "..Insurance and financial services"                               = "Insurance and financial services"
      ),
      selected = "All groups CPI, seasonally adjusted",
      multiple = TRUE,
      options  = list(`actions-box` = TRUE)
    ),

    downloadButton("downloadTb", "Download selection:")
  ),

  mainPanel(
    tabsetPanel(type = "tabs",
      tabPanel("Graph", verbatimTextOutput("message_graph"), highchartOutput("hcontainer", height = "720px")),
      tabPanel("Table", verbatimTextOutput("message"),       DT::dataTableOutput("table"))
    ),
    uiOutput("footer_ui")
  )
)


#------------
# Server ---
#------------
server <- function(input, output, session) {

  #--- Load data once per session (from ABS xlsx) ---
  cpi_raw    <- read_cpi_monthly()
  index_dat  <- cpi_raw$index       # date, CPI_components, Index value  (34 series)
  annual_dat <- cpi_raw$annual_pct  # date, CPI_components, annual_pct   (36 series, incl. All groups CPI)

  dmax   <- max(index_dat$date)
  latest <- format(dmax, "%b %Y")
  now    <- format(today(), "%d %B %Y")

  #--- Build month index from index data ---
  Month_df <- index_dat %>%
    select(date) %>%
    distinct() %>%
    arrange(date) %>%
    mutate(idx      = row_number(),
           MonthStr = format(date, "%b %Y"))

  Month_label <- as.list(Month_df$MonthStr)

  # Attach idx and MonthStr to both datasets
  index_dat <- index_dat %>%
    mutate(MonthStr = format(date, "%b %Y")) %>%
    left_join(Month_df %>% select(MonthStr, idx), by = "MonthStr")

  annual_dat <- annual_dat %>%
    mutate(MonthStr = format(date, "%b %Y")) %>%
    left_join(Month_df %>% select(MonthStr, idx), by = "MonthStr") %>%
    filter(!is.na(idx))   # keep only months covered by index data range


  #--- Dynamic UI ---
  output$MonthRange_ui <- renderUI({
    sliderTextInput("MonthRange", "Month range:",
                    choices  = Month_label,
                    selected = c(Month_label[1], Month_label[length(Month_label)]),
                    grid     = TRUE)
  })

  output$footer_ui <- renderUI({
    tags$div(class = "header", checked = NA,
      tags$p("Source:",
        tags$a(href = "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/monthly-consumer-price-index-indicator/latest-release",
               paste0("Australian Bureau of Statistics, Monthly CPI Indicator (Cat. 6484.0): ", latest))),
      tags$p("Retrieved from",
        tags$a(href = cpi_raw$source_url,
               paste0("ABS data file: ", basename(cpi_raw$source_url), "  (", now, ")")))
    )
  })


  I <- reactive({ list(sel = input$List) })

  #--- Filter helpers ---
  filter_by_range <- function(dat) {
    req(input$MonthRange)
    s_idx <- dat %>% filter(MonthStr == input$MonthRange[1]) %>% pull(idx) %>% first()
    e_idx <- dat %>% filter(MonthStr == input$MonthRange[2]) %>% pull(idx) %>% first()
    dat %>% filter(idx >= s_idx & idx <= e_idx)
  }

  #--- Index + cumulative % (34 series) ---
  filteredIdx <- reactive({
    filter_by_range(index_dat) %>%
      filter(CPI_components %in% I()$sel)
  })

  dfc <- reactive({
    filteredIdx() %>%
      group_by(CPI_components) %>%
      arrange(date) %>%
      mutate(indcum = cumsum(`Index value`),
             change = round(`Index value` / min(indcum) * 100 - 100, 1),
             Date   = format(date, "%b %Y"))
  })

  #--- First/last summary for bar + annual avg (index series) ---
  dfs <- reactive({
    req(input$MonthRange)
    index_dat %>%
      filter(CPI_components %in% I()$sel,
             MonthStr %in% c(input$MonthRange[1], input$MonthRange[2])) %>%
      group_by(CPI_components) %>%
      summarize(
        change       = if (n() == 2) round((last(`Index value`) - first(`Index value`)) / first(`Index value`) * 100, 1) else NA_real_,
        num_years    = (max(idx) - min(idx)) / 12,
        first_index  = first(`Index value`),
        last_index   = last(`Index value`),
        ann_avg      = if (n() == 2) round(((last_index / first_index)^(1 / num_years)) - 1, 4) * 100 else NA_real_,
        period       = paste0(input$MonthRange[1], " to ", input$MonthRange[2]),
        .groups      = "drop"
      )
  })

  #--- Year-on-year % (36 series, pre-computed by ABS) ---
  filteredYoy <- reactive({
    filter_by_range(annual_dat) %>%
      filter(CPI_components %in% I()$sel) %>%
      mutate(Date = format(date, "%b %Y"))
  })

  # Latest month's YoY for bar chart
  yoy_latest <- reactive({
    req(input$MonthRange)
    e_idx <- annual_dat %>% filter(MonthStr == input$MonthRange[2]) %>% pull(idx) %>% first()
    annual_dat %>%
      filter(CPI_components %in% I()$sel, idx == e_idx) %>%
      mutate(CPI_components = fct_reorder(CPI_components, desc(annual_pct)))
  })


  view_note <- reactive({
    if (is.null(I()$sel)) return("No CPI items selected")
    if (input$choosetable %in% c("index", "pc", "avg")) {
      index_series <- unique(index_dat$CPI_components)
      missing <- setdiff(I()$sel, index_series)
      missing <- missing[!grepl("^---", missing)]
      if (length(missing) > 0)
        return(paste0("Note: '", paste(missing, collapse = "', '"),
                      "' is published as annual % change only for the monthly indicator",
                      " \u2014 select Year-on-year % view for this series."))
    }
    ""
  })

  output$message       <- renderText({ view_note() })
  output$message_graph <- renderText({ view_note() })


  ## Font helpers
  font_bar <- function(hc) {
    hc %>%
      hc_yAxis(title  = list(text = "Per cent", style = list(fontSize = "16px")),
               labels = list(style = list(fontSize = "14px"))) %>%
      hc_xAxis(title  = list(text = "CPI items", style = list(fontSize = "18px")),
               labels = list(style = list(fontSize = "16px"))) %>%
      hc_title(style  = list(fontSize = "20px")) %>%
      hc_plotOptions(series = list(
        dataLabels = list(enabled = TRUE, style = list(fontSize = "14px")),
        marker = list(enabled = FALSE), stacking = FALSE, grouping = FALSE))
  }

  font_line <- function(hc) {
    hc %>%
      hc_yAxis(title  = list(text = "Per cent", style = list(fontSize = "18px")),
               labels = list(style = list(fontSize = "16px"))) %>%
      hc_xAxis(title  = list(text = "Month", style = list(fontSize = "16px")),
               labels = list(style = list(fontSize = "16px"))) %>%
      hc_title(style  = list(fontSize = "20px")) %>%
      hc_legend(itemStyle = list(fontSize = "16px")) %>%
      hc_plotOptions(series = list(
        dataLabels = list(enabled = FALSE),
        marker     = list(enabled = TRUE),
        lineWidth  = 3, stacking = FALSE, grouping = FALSE))
  }


  output$hcontainer <- renderHighchart({
    req(input$MonthRange)

    null_chart <- function(msg) {
      hchart(data.frame(x = 1, y = 0), type = "line", hcaes(x, y)) %>%
        hc_title(text = msg) %>% hc_add_theme(hc_theme_economist())
    }

    if (is.null(I()$sel)) return(null_chart("No CPI items selected"))

    m1 <- input$MonthRange[1];  m2 <- input$MonthRange[2]
    title_sfx <- paste0(m1, " to ", m2)

    # ---- Index ----
    if (input$choosetable == "index") {
      dfc() %>%
        hchart(type = "line", hcaes(x = Date, y = `Index value`, group = CPI_components)) %>%
        hc_xAxis(title = list(text = "Month"), crosshair = TRUE) %>%
        font_line() %>%
        hc_yAxis(title = list(text = "Index (Sep 2017 = 100)")) %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("Monthly CPI index value, ", title_sfx)) %>%
        hc_colors(abscol) %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE)))

    # ---- Cumulative % ----
    } else if (input$choosetable == "pc" && input$plottype == "Line") {
      dfc() %>%
        hchart(type = "line", hcaes(x = Date, y = round(change, 1), group = CPI_components)) %>%
        hc_xAxis(title = list(text = "Month"), crosshair = TRUE) %>%
        font_line() %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("Cumulative % change, ", title_sfx)) %>%
        hc_colors(abscol) %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE)))

    } else if (input$choosetable == "pc" && input$plottype == "Bar") {
      dfs() %>%
        filter(!is.na(change)) %>%
        mutate(CPI_components = fct_reorder(CPI_components, desc(change))) %>%
        hchart(type = "bar", hcaes(x = CPI_components, y = round(change, 1), group = CPI_components)) %>%
        font_bar() %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("Total % change, ", title_sfx)) %>%
        hc_colors("#4FADE7") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE), stacking = FALSE, grouping = FALSE))

    # ---- Year-on-year % ----
    } else if (input$choosetable == "yoy" && input$plottype == "Line") {
      filteredYoy() %>%
        hchart(type = "line", hcaes(x = Date, y = round(annual_pct, 1), group = CPI_components)) %>%
        hc_xAxis(title = list(text = "Month"), crosshair = TRUE) %>%
        font_line() %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("Year-on-year % change, ", title_sfx)) %>%
        hc_colors(abscol) %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE)))

    } else if (input$choosetable == "yoy" && input$plottype == "Bar") {
      yoy_latest() %>%
        hchart(type = "bar", hcaes(x = CPI_components, y = round(annual_pct, 1), group = CPI_components)) %>%
        font_bar() %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("Year-on-year % change, ", m2)) %>%
        hc_colors("#4FADE7") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE), stacking = FALSE, grouping = FALSE))

    # ---- Annual average ----
    } else if (input$choosetable == "avg") {
      dfs() %>%
        filter(!is.na(ann_avg)) %>%
        mutate(CPI_components = fct_reorder(CPI_components, desc(ann_avg))) %>%
        hchart(type = "bar", hcaes(x = CPI_components, y = round(ann_avg, 1), group = CPI_components)) %>%
        font_bar() %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("Annual average % change, ", title_sfx)) %>%
        hc_colors("#4FADE7") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE), stacking = FALSE, grouping = FALSE))

    } else {
      null_chart("Select a view")
    }
  })


  #--- Table reactives ---
  index_tbl <- reactive({
    dfc() %>%
      select(date, CPI_components, `Index value`) %>%
      spread(CPI_components, `Index value`) %>%
      rename_if(is.numeric, ~ paste0(., ", Index")) %>%
      mutate(Month = format(date, "%b %Y")) %>% arrange(date) %>%
      select(-date) %>% select(Month, everything())
  })

  pcl_tbl <- reactive({
    dfc() %>%
      select(date, CPI_components, change) %>%
      spread(CPI_components, change) %>%
      rename_if(is.numeric, ~ paste0(., ", Cuml. chg (%)")) %>%
      mutate(Month = format(date, "%b %Y")) %>% arrange(date) %>%
      select(-date) %>% select(Month, everything())
  })

  yoy_tbl <- reactive({
    filteredYoy() %>%
      select(date, CPI_components, annual_pct) %>%
      spread(CPI_components, annual_pct) %>%
      rename_if(is.numeric, ~ paste0(., ", YoY chg (%)")) %>%
      mutate(Month = format(date, "%b %Y")) %>% arrange(date) %>%
      select(-date) %>% select(Month, everything())
  })

  pcb_tbl <- reactive({
    dfs() %>%
      select(period, CPI_components, change) %>%
      spread(CPI_components, change) %>%
      rename_if(is.numeric, ~ paste0(., ", Total chg (%)")) %>%
      select(period, everything())
  })

  avg_tbl <- reactive({
    dfs() %>%
      select(period, CPI_components, ann_avg) %>%
      spread(CPI_components, ann_avg) %>%
      rename_if(is.numeric, ~ paste0(., ", Ann. avg chg (%)")) %>%
      select(period, everything())
  })

  active_tbl <- reactive({
    if (input$choosetable == "index")                              return(index_tbl())
    if (input$choosetable == "avg")                               return(avg_tbl())
    if (input$choosetable == "yoy")                               return(yoy_tbl())
    if (input$choosetable == "pc" && input$plottype == "Line")    return(pcl_tbl())
    if (input$choosetable == "pc" && input$plottype == "Bar")     return(pcb_tbl())
    index_tbl()
  })

  output$table <- DT::renderDataTable({ active_tbl() })

  output$downloadTb <- downloadHandler(
    filename = function() { paste("Monthly CPI", input$choosetable, ".xlsx") },
    content  = function(file) { writexl::write_xlsx(active_tbl(), path = file) }
  )
}

#========================================
shinyApp(ui, server)
#========================================
