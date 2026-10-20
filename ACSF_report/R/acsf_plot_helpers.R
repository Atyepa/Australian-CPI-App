#===================================================================================================
# acsf_plot_helpers.R
# Plot builders shared between the ACSF Shiny apps and the ACSF steps Quarto report.
#
# Rule of thumb: each app's renderHighchart() should be a thin wrapper around ONE of these
# functions, e.g.
#     output$ts_plot <- renderHighchart(plot_ts_weekly(ts_filtered(), series = input$series))
# and the report calls the same function on the same data. One function, two front-ends,
# so the plot in the report is always the plot the app shows.
#
# The bodies below are PLACEHOLDERS - swap in the real chart code from:
#   - App_ts_weekly        -> plot_ts_weekly()
#   - Coding management    -> plot_coding_coverage(), table_uncoded()
#   - ACSF sales vis       -> plot_sales_vis()
#===================================================================================================

library(tidyverse)
library(highcharter)
library(DT)

`%!in%` <- negate(`%in%`)

#--- ABS preferred colours ---
abscol <- c("#4FADE7", "#1A4472", "#F29000", "#993366", "#669966", "#99CC66",
            "#CC9966", "#666666", "#8DD3C7", "#BEBADA", "#FB8072", "#80B1D3",
            "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#ffcc99")

#----------------------------------------------------------------------
#---- Chart styling ---------------------------------------------------
#   Dark/transparent on screen (blends into the darkly theme);
#   white background on export so downloads drop straight into slides/Word.
#----------------------------------------------------------------------
style_hc <- function(hc, filename = "ACSF_chart", font_px = 14) {
  ink <- "#E6EDF3"; lab <- "#C9D1D9"; grid <- "#30363D"; axis <- "#8B949E"
  fs_lab   <- paste0(font_px, "px")
  fs_title <- paste0(font_px + 2, "px")

  hc %>%
    hc_colors(abscol) %>%
    hc_chart(backgroundColor = "transparent",
             style = list(fontFamily = "Open Sans, Arial, Helvetica, sans-serif")) %>%
    hc_title(style = list(color = ink)) %>%
    hc_subtitle(style = list(color = lab)) %>%
    hc_xAxis(lineColor = axis, tickColor = axis,
             title  = list(style = list(color = ink, fontSize = fs_title)),
             labels = list(style = list(color = lab, fontSize = fs_lab))) %>%
    hc_yAxis(gridLineColor = grid, lineColor = axis, tickColor = axis,
             title  = list(style = list(color = ink, fontSize = fs_title)),
             labels = list(style = list(color = lab, fontSize = fs_lab))) %>%
    hc_legend(itemStyle = list(color = ink, fontSize = fs_lab)) %>%
    hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
    hc_exporting(
      enabled  = TRUE,
      filename = gsub("[^A-Za-z0-9]+", "_", filename),
      chartOptions = list(
        chart  = list(backgroundColor = "#FFFFFF"),
        title  = list(style = list(color = "#0D1117")),
        xAxis  = list(labels = list(style = list(color = "#333333")),
                      title  = list(style = list(color = "#0D1117"))),
        yAxis  = list(gridLineColor = "#E6E6E6",
                      labels = list(style = list(color = "#333333")),
                      title  = list(style = list(color = "#0D1117"))),
        legend = list(itemStyle = list(color = "#0D1117"))
      )
    )
}

#----------------------------------------------------------------------
#---- 1. App_ts_weekly ------------------------------------------------
# Weekly sales time series, optionally pre vs post adjustment.
# Expects: week (Date), series (chr), stage ("Pre"/"Post"), value (dbl)
#----------------------------------------------------------------------
plot_ts_weekly <- function(df, series_sel = NULL, y_lab = "Sales ($m)",
                           title = "Weekly sales: pre vs post adjustment") {
  if (!is.null(series_sel)) df <- df %>% filter(series %in% series_sel)

  df <- df %>% mutate(grp = paste(series, stage, sep = " - "))

  hchart(df, "line", hcaes(x = week, y = value, group = grp)) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text = NULL)) %>%
    hc_yAxis(title = list(text = y_lab)) %>%
    hc_tooltip(shared = TRUE, valueDecimals = 1) %>%
    style_hc(filename = title)
}

#----------------------------------------------------------------------
#---- 2. Coding management --------------------------------------------
# Share of products / sales value coded to AUSNUT, by period.
# Expects: period (chr), status ("Coded"/"Auto-coded"/"Uncoded"), n_items, sales
#----------------------------------------------------------------------
plot_coding_coverage <- function(df, measure = c("sales", "n_items"),
                                 title = "Coding coverage by period") {
  measure <- match.arg(measure)
  y_lab   <- if (measure == "sales") "Share of sales value (%)" else "Share of items (%)"

  df %>%
    group_by(period) %>%
    mutate(pct = 100 * .data[[measure]] / sum(.data[[measure]])) %>%
    ungroup() %>%
    hchart("column", hcaes(x = period, y = pct, group = status)) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text = NULL)) %>%
    hc_yAxis(title = list(text = y_lab), max = 100) %>%
    hc_tooltip(valueDecimals = 1, valueSuffix = "%") %>%
    style_hc(filename = title)
}

# Top uncoded items by sales - the "to do" list for coders.
# Expects: product_id, description, sales, status
table_uncoded <- function(items, n = 20) {
  items %>%
    filter(status == "Uncoded") %>%
    slice_max(sales, n = n) %>%
    select(product_id, description, sales) %>%
    datatable(rownames = FALSE, options = list(pageLength = 10, dom = "tip")) %>%
    formatCurrency("sales", digits = 0)
}

#----------------------------------------------------------------------
#---- 3. ACSF sales vis -----------------------------------------------
# Annual/periodic sales by food group (AUSNUT / ADG).
# Expects: food_group (chr), period (chr), value (dbl)
#----------------------------------------------------------------------
plot_sales_vis <- function(df, type = "bar", y_lab = "Grams per capita per day",
                           title = "Apparent consumption by food group") {
  df %>%
    mutate(food_group = fct_reorder(food_group, value, .fun = max)) %>%
    arrange(desc(food_group)) %>%
    hchart(type, hcaes(x = food_group, y = value, group = period)) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text = NULL)) %>%
    hc_yAxis(title = list(text = y_lab)) %>%
    hc_tooltip(shared = TRUE, valueDecimals = 1) %>%
    style_hc(filename = title)
}

#----------------------------------------------------------------------
#---- Demo data -------------------------------------------------------
# Fake data with the same shape as the real inputs, so the report renders end-to-end
# before anything is wired up (and outside the ABS network). Delete once real loaders exist.
#----------------------------------------------------------------------
make_demo_data <- function(seed = 2024) {
  set.seed(seed)

  weeks <- seq(as.Date("2023-07-03"), as.Date("2024-06-24"), by = "week")
  ts_weekly <- expand_grid(week = weeks, series = c("Dairy", "Fruit", "Grains")) %>%
    mutate(base  = case_when(series == "Dairy" ~ 120, series == "Fruit" ~ 90, TRUE ~ 150),
           value = base + 8 * sin(2 * pi * as.numeric(week - min(week)) / 365) + rnorm(n(), 0, 4),
           # a few weeks of dodgy raw data that the adjustment smooths out
           Pre   = if_else(week %in% weeks[c(10, 30, 31)], value * 1.25, value),
           Post  = value) %>%
    select(week, series, Pre, Post) %>%
    pivot_longer(c(Pre, Post), names_to = "stage", values_to = "value")

  periods <- c("2023-Q3", "2023-Q4", "2024-Q1", "2024-Q2")
  coding_items <- tibble(
    product_id  = sprintf("P%05d", 1:2000),
    description = paste("Product", 1:2000),
    period      = sample(periods, 2000, replace = TRUE),
    status      = sample(c("Coded", "Auto-coded", "Uncoded"), 2000, replace = TRUE,
                         prob = c(0.80, 0.15, 0.05)),
    sales       = rlnorm(2000, meanlog = 8, sdlog = 1.2)
  )
  coding_summary <- coding_items %>%
    group_by(period, status) %>%
    summarise(n_items = n(), sales = sum(sales), .groups = "drop")

  sales_vis <- expand_grid(
    food_group = c("Vegetables", "Fruit", "Grains", "Meat & alternatives", "Dairy & alternatives",
                   "Discretionary"),
    period     = c("2022-23", "2023-24")
  ) %>%
    mutate(value = runif(n(), 80, 400))

  list(ts_weekly      = ts_weekly,
       coding_items   = coding_items,
       coding_summary = coding_summary,
       sales_vis      = sales_vis)
}
