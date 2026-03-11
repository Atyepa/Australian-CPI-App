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

source("read_cpi_6401_monthly.R")

#--- ABS preferred colours ---
abscol <- c("#4FADE7", "#1A4472", "#F29000", "#993366", "#669966", "#99CC66",
            "#CC9966", "#666666", "#8DD3C7", "#BEBADA", "#FB8072", "#80B1D3",
            "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#ffcc99")

#----------------------------------------
#---- CPI series picker choices ---------
#   Format: "..Display label" = "Data name"
#   Dots indicate hierarchy level.
#----------------------------------------
cpi_choices <- c(
  # --- Analytical ---
  "--- Analytical series ---"                                           = "--- Analytical ---",
  "All groups CPI, seasonally adjusted"                                 = "All groups CPI, seasonally adjusted",
  "..Tradables"                                                         = "Tradables",
  "..Non-tradables"                                                     = "Non-tradables",
  "..All groups, goods component"                                       = "All groups, goods component",
  "..All groups, services component"                                    = "All groups, services component",
  # --- All groups ---
  "--- All groups ---"                                                  = "--- All groups ---",
  "All groups CPI"                                                      = "All groups CPI",
  # --- Food ---
  "--- Food and non-alcoholic beverages ---"                            = "--- Food ---",
  "..Food and non-alcoholic beverages"                                  = "Food and non-alcoholic beverages",
  "....Bread and cereal products"                                       = "Bread and cereal products",
  "......Bread"                                                         = "Bread",
  "......Cakes and biscuits"                                            = "Cakes and biscuits",
  "......Breakfast cereals"                                             = "Breakfast cereals",
  "......Other cereal products"                                         = "Other cereal products",
  "....Meat and seafoods"                                               = "Meat and seafoods",
  "......Beef and veal"                                                 = "Beef and veal",
  "......Pork"                                                          = "Pork",
  "......Lamb and goat"                                                 = "Lamb and goat",
  "......Poultry"                                                       = "Poultry",
  "......Other meats"                                                   = "Other meats",
  "......Fish and other seafood"                                        = "Fish and other seafood",
  "....Dairy and related products"                                      = "Dairy and related products",
  "......Milk"                                                          = "Milk",
  "......Cheese"                                                        = "Cheese",
  "......Ice cream and other dairy products"                            = "Ice cream and other dairy products",
  "....Fruit and vegetables"                                            = "Fruit and vegetables",
  "......Fruit"                                                         = "Fruit",
  "......Vegetables"                                                    = "Vegetables",
  "....Food products n.e.c."                                            = "Food products n.e.c.",
  "......Eggs"                                                          = "Eggs",
  "......Jams, honey and spreads"                                       = "Jams, honey and spreads",
  "......Snacks and confectionery"                                      = "Snacks and confectionery",
  "....Non-alcoholic beverages"                                         = "Non-alcoholic beverages",
  "......Coffee, tea and cocoa"                                         = "Coffee, tea and cocoa",
  "......Waters, soft drinks and juices"                                = "Waters, soft drinks and juices",
  "....Meals out and take away foods"                                   = "Meals out and take away foods",
  "......Restaurant meals"                                              = "Restaurant meals",
  "......Take away and fast foods"                                      = "Take away and fast foods",
  # --- Alcohol and tobacco ---
  "--- Alcohol and tobacco ---"                                         = "--- Alcohol ---",
  "..Alcohol and tobacco"                                               = "Alcohol and tobacco",
  "....Alcoholic beverages"                                             = "Alcoholic beverages",
  "......Spirits"                                                       = "Spirits",
  "......Wine"                                                          = "Wine",
  "......Beer"                                                          = "Beer",
  "....Tobacco"                                                         = "Tobacco",
  # --- Clothing ---
  "--- Clothing and footwear ---"                                       = "--- Clothing ---",
  "..Clothing and footwear"                                             = "Clothing and footwear",
  "....Garments"                                                        = "Garments",
  "....Footwear"                                                        = "Footwear",
  # --- Housing ---
  "--- Housing ---"                                                     = "--- Housing ---",
  "..Housing"                                                           = "Housing",
  "....Rents"                                                           = "Rents",
  "....New dwelling purchase by owner-occupiers"                        = "New dwelling purchase by owner-occupiers",
  "....Other housing"                                                   = "Other housing",
  "....Utilities"                                                       = "Utilities",
  "......Water and sewerage"                                            = "Water and sewerage",
  "......Electricity"                                                   = "Electricity",
  "......Gas and other household fuels"                                 = "Gas and other household fuels",
  # --- Furnishings ---
  "--- Furnishings, household equipment and services ---"               = "--- Furnishings ---",
  "..Furnishings, household equipment and services"                     = "Furnishings, household equipment and services",
  "....Furniture and furnishings"                                       = "Furniture and furnishings",
  "....Household appliances, utensils and tools"                        = "Household appliances, utensils and tools",
  "......Major household appliances"                                    = "Major household appliances",
  "....Non-durable household products"                                  = "Non-durable household products",
  "......Cleaning and maintenance products"                             = "Cleaning and maintenance products",
  "......Personal care products"                                        = "Personal care products",
  "....Domestic and household services"                                 = "Domestic and household services",
  "......Child care"                                                    = "Child care",
  # --- Health ---
  "--- Health ---"                                                      = "--- Health ---",
  "..Health"                                                            = "Health",
  "....Medical products, appliances and equipment"                      = "Medical products, appliances and equipment",
  "......Pharmaceutical products"                                       = "Pharmaceutical products",
  "....Medical, dental and hospital services"                           = "Medical, dental and hospital services",
  "......Medical and hospital services"                                 = "Medical and hospital services",
  "......Dental services"                                               = "Dental services",
  # --- Transport ---
  "--- Transport ---"                                                   = "--- Transport ---",
  "..Transport"                                                         = "Transport",
  "....Private motoring"                                                = "Private motoring",
  "......Motor vehicles"                                                = "Motor vehicles",
  "......Automotive fuel"                                               = "Automotive fuel",
  "......Maintenance and repair of motor vehicles"                      = "Maintenance and repair of motor vehicles",
  "....Urban transport fares"                                           = "Urban transport fares",
  # --- Communication ---
  "--- Communication ---"                                               = "--- Communication ---",
  "..Communication"                                                     = "Communication",
  "....Telecommunication equipment and services"                        = "Telecommunication equipment and services",
  # --- Recreation and culture ---
  "--- Recreation and culture ---"                                      = "--- Recreation ---",
  "..Recreation and culture"                                            = "Recreation and culture",
  "....Audio, visual and computing equipment and services"              = "Audio, visual and computing equipment and services",
  "......Audio, visual and computing equipment"                         = "Audio, visual and computing equipment",
  "....Newspapers, books and stationery"                                = "Newspapers, books and stationery",
  "....Holiday travel and accommodation"                                = "Holiday travel and accommodation",
  "......Domestic holiday travel and accommodation"                     = "Domestic holiday travel and accommodation",
  "......International holiday travel and accommodation"                = "International holiday travel and accommodation",
  "....Other recreation, sport and culture"                             = "Other recreation, sport and culture",
  # --- Education ---
  "--- Education ---"                                                   = "--- Education ---",
  "..Education"                                                         = "Education",
  "....Preschool and primary education"                                 = "Preschool and primary education",
  "....Secondary education"                                             = "Secondary education",
  "....Tertiary education"                                              = "Tertiary education",
  # --- Insurance and financial ---
  "--- Insurance and financial services ---"                            = "--- Insurance ---",
  "..Insurance and financial services"                                  = "Insurance and financial services",
  "....Insurance"                                                       = "Insurance",
  "....Financial services"                                              = "Financial services"
)


#----------------------------------------
#---- UI ----
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

  headerPanel("Monthly CPI - weighted average of eight capital cities"),

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
      choices  = cpi_choices,
      selected = "All groups CPI, seasonally adjusted",
      multiple = TRUE,
      options  = list(`actions-box` = TRUE, liveSearch = TRUE,
                      liveSearchPlaceholder = "Search series...")
    ),

    downloadButton("downloadTb", "Download selection:")
  ),

  mainPanel(
    tabsetPanel(type = "tabs",
      tabPanel("Graph", highchartOutput("hcontainer", height = "720px")),
      tabPanel("Table", DT::dataTableOutput("table"))
    ),
    uiOutput("footer_ui")
  )
)


#------------
# Server
#------------
server <- function(input, output, session) {

  #--- Load data once per session ---
  cpi_raw    <- read_cpi_6401_monthly()

  # Trim to monthly series only (Apr 2024 onward); quarterly back-series excluded
  monthly_start <- as.Date("2024-04-01")
  index_dat  <- cpi_raw$index      %>% filter(date >= monthly_start)
  annual_dat <- cpi_raw$annual_pct %>% filter(date >= monthly_start)

  dmax   <- max(index_dat$date)
  latest <- format(dmax, "%b %Y")
  now    <- format(today(), "%d %B %Y")

  #--- Month index for slider ---
  Month_df <- index_dat %>%
    select(date) %>%
    distinct() %>%
    arrange(date) %>%
    mutate(idx      = row_number(),
           MonthStr = format(date, "%b %Y"))

  Month_label <- as.list(Month_df$MonthStr)

  index_dat <- index_dat %>%
    mutate(MonthStr = format(date, "%b %Y")) %>%
    left_join(Month_df %>% select(MonthStr, idx), by = "MonthStr")

  annual_dat <- annual_dat %>%
    mutate(MonthStr = format(date, "%b %Y")) %>%
    left_join(Month_df %>% select(MonthStr, idx), by = "MonthStr") %>%
    filter(!is.na(idx))


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
        tags$a(href = "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/latest-release",
               paste0("Australian Bureau of Statistics, Consumer Price Index (Cat. 6401.0): ", latest))),
      tags$p("Retrieved from",
        tags$a(href = cpi_raw$source_url,
               paste0("ABS data file: ", basename(cpi_raw$source_url), "  (", now, ")")))
    )
  })


  I <- reactive({ list(sel = input$List[!grepl("^---", input$List)]) })

  #--- Filter by selected month range ---
  filter_by_range <- function(dat) {
    req(input$MonthRange)
    s_idx <- dat %>% filter(MonthStr == input$MonthRange[1]) %>% pull(idx) %>% first()
    e_idx <- dat %>% filter(MonthStr == input$MonthRange[2]) %>% pull(idx) %>% first()
    dat %>% filter(idx >= s_idx & idx <= e_idx)
  }

  #--- Index + cumulative % ---
  filteredIdx <- reactive({
    filter_by_range(index_dat) %>% filter(CPI_components %in% I()$sel)
  })

  dfc <- reactive({
    filteredIdx() %>%
      group_by(CPI_components) %>%
      arrange(date) %>%
      mutate(indcum = cumsum(`Index value`),
             change = round(`Index value` / min(indcum) * 100 - 100, 1),
             Date   = format(date, "%b %Y"))
  })

  #--- First/last summary for bar + annual avg ---
  dfs <- reactive({
    req(input$MonthRange)
    index_dat %>%
      filter(CPI_components %in% I()$sel,
             MonthStr %in% c(input$MonthRange[1], input$MonthRange[2])) %>%
      group_by(CPI_components) %>%
      summarize(
        change      = if (n() == 2) round((last(`Index value`) - first(`Index value`)) /
                                           first(`Index value`) * 100, 1) else NA_real_,
        num_years   = (max(idx) - min(idx)) / 12,
        first_index = first(`Index value`),
        last_index  = last(`Index value`),
        ann_avg     = if (n() == 2) round(((last_index / first_index)^(1 / num_years)) - 1, 4) * 100
                      else NA_real_,
        period      = paste0(input$MonthRange[1], " to ", input$MonthRange[2]),
        .groups     = "drop"
      )
  })

  #--- Year-on-year % ---
  filteredYoy <- reactive({
    filter_by_range(annual_dat) %>%
      filter(CPI_components %in% I()$sel) %>%
      mutate(Date = format(date, "%b %Y"))
  })

  yoy_latest <- reactive({
    req(input$MonthRange)
    e_idx <- annual_dat %>% filter(MonthStr == input$MonthRange[2]) %>% pull(idx) %>% first()
    annual_dat %>%
      filter(CPI_components %in% I()$sel, idx == e_idx) %>%
      mutate(CPI_components = fct_reorder(CPI_components, desc(annual_pct)))
  })


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

    if (is.null(I()$sel) || length(I()$sel) == 0)
      return(null_chart("No CPI items selected"))

    m1 <- input$MonthRange[1];  m2 <- input$MonthRange[2]
    title_sfx <- paste0(m1, " to ", m2)

    # ---- Index ----
    if (input$choosetable == "index") {
      dfc() %>%
        hchart(type = "line", hcaes(x = Date, y = `Index value`, group = CPI_components)) %>%
        hc_xAxis(title = list(text = "Month"), crosshair = TRUE) %>%
        font_line() %>%
        hc_yAxis(title = list(text = "Index (Sep 2025 = 100)")) %>%
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
        hchart(type = "bar", hcaes(x = CPI_components, y = round(change, 1),
                                   group = CPI_components)) %>%
        font_bar() %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("Total % change, ", title_sfx)) %>%
        hc_colors("#4FADE7") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE),
                                     stacking = FALSE, grouping = FALSE))

    # ---- Year-on-year % ----
    } else if (input$choosetable == "yoy" && input$plottype == "Line") {
      filteredYoy() %>%
        hchart(type = "line", hcaes(x = Date, y = round(annual_pct, 1),
                                    group = CPI_components)) %>%
        hc_xAxis(title = list(text = "Month"), crosshair = TRUE) %>%
        font_line() %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("Year-on-year % change, ", title_sfx)) %>%
        hc_colors(abscol) %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE)))

    } else if (input$choosetable == "yoy" && input$plottype == "Bar") {
      yoy_latest() %>%
        hchart(type = "bar", hcaes(x = CPI_components, y = round(annual_pct, 1),
                                   group = CPI_components)) %>%
        font_bar() %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("Year-on-year % change, ", m2)) %>%
        hc_colors("#4FADE7") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE),
                                     stacking = FALSE, grouping = FALSE))

    # ---- Annual average ----
    } else if (input$choosetable == "avg") {
      dfs() %>%
        filter(!is.na(ann_avg)) %>%
        mutate(CPI_components = fct_reorder(CPI_components, desc(ann_avg))) %>%
        hchart(type = "bar", hcaes(x = CPI_components, y = round(ann_avg, 1),
                                   group = CPI_components)) %>%
        font_bar() %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("Annual average % change, ", title_sfx)) %>%
        hc_colors("#4FADE7") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE),
                                     stacking = FALSE, grouping = FALSE))

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
    filename = function() { paste("Monthly CPI 6401", input$choosetable, ".xlsx") },
    content  = function(file) { writexl::write_xlsx(active_tbl(), path = file) }
  )
}

#========================================
shinyApp(ui, server)
#========================================
