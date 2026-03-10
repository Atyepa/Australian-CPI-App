library(tidyverse)
library(highcharter)
library(ggthemes)
library(DT)
library(readsdmx)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(lubridate)
library(zoo)
library(writexl)
library(scales)
library(rsconnect)

#-----------------------------------------------------
#---- Load Monthly CPI data from ABS.Stat SDMX-XML API -
#-----------------------------------------------------

#--- SDMX data query URL: MEASURE.INDEX.TSEST.REGION.FREQ ---
sdmx_dat_m <- "https://api.data.abs.gov.au/data/ABS,CPI_M,1.2.0/1.10001+20001+30002+40005+40006+40007+40008+30003+40009+40010+131178+40012+40014+40015+30001+40001+40002+40004+114120+114121+114122+131180+40030+115520+30007+40025+40026+131179+40027+97550+115501+40034+20002+131181+20003+115522+131186+40055+115524+20004+115486+20005+40081+30033+115488+115489+115493+20006+30026+30027+126670+104101+104104+104122+999904+102675+102676.10.50.M?startPeriod=2022-07&endPeriod=2027-01"

#--- ABS preferred colours---
abscol <- c("#4FADE7", "#1A4472", "#F29000", "#993366", "#669966", "#99CC66",
            "#CC9966", "#666666", "#8DD3C7", "#BEBADA", "#FB8072", "#80B1D3",
            "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#ffcc99")

#--- Inline labels for monthly CPI series ---
cpi_m_labels <- data.frame(
  code = c(10001, 20001, 30002, 40005, 40006, 40007, 40008,
           30003, 40009, 40010, 131178, 40012, 40014, 40015,
           30001, 40001, 40002, 40004,
           114120, 114121, 114122,
           131180, 40030, 115520,
           30007, 40025, 40026,
           131179, 40027, 97550, 115501, 40034,
           20002, 131181,
           20003, 115522, 131186, 40055, 115524,
           20004, 115486, 20005, 40081, 30033,
           115488, 115489, 115493,
           20006, 30026, 30027,
           126670,
           104101, 104104, 104122, 999904, 102675, 102676),
  label = c("All groups CPI",
            "Food and non-alcoholic beverages",
            "Bread and cereal products", "Bread", "Cakes and biscuits", "Breakfast cereals", "Other cereal products",
            "Meat and seafoods", "Beef and veal", "Lamb and goat", "Pork", "Poultry", "Other meats", "Fish and other seafood",
            "Dairy and related products", "Milk", "Cheese", "Ice cream and other dairy products",
            "Fruit and vegetables", "Fruit", "Vegetables",
            "Non-alcoholic beverages", "Coffee, tea and cocoa", "Waters, soft drinks and juices",
            "Meals out and take away foods", "Restaurant meals", "Take away and fast foods",
            "Food products n.e.c.", "Eggs", "Oils and fats", "Snacks and confectionery", "Other food products n.e.c.",
            "Clothing and footwear", "Garments",
            "Housing", "Rents", "New dwelling purchase by owner-occupiers", "Electricity", "Gas and other household fuels",
            "Furnishings, household equipment and services",
            "Health", "Transport", "Automotive fuel", "Holiday travel and accommodation",
            "Communication", "Recreation and culture", "Education",
            "Alcohol and tobacco", "Alcoholic beverages", "Tobacco",
            "Insurance and financial services",
            "All groups, goods component", "All groups, services component",
            "All groups CPI excl. volatile items",
            "All groups excl. volatile items and holiday travel",
            "Tradables", "Non-tradables"),
  stringsAsFactors = FALSE
)

#----------------------------------------
#---- SHINY DASHBOARD----
#----------------------------------------

#------------
#----UI----
#------------
ui <- fluidPage(theme = shinytheme("darkly"),

                tags$head(tags$style(HTML(
                  "
    .dataTables_length label,
    .dataTables_filter label,
    .dataTables_info {
        color: white!important;
    }

    .paginate_button {
        background: white!important;
    }

    thead {
        color: white;
    }

    table.dataTable {
        background-color: white!important;
        color: black!important;
    }

    table.dataTable th,
    table.dataTable td {
        color: black!important;
    }

    table.dataTable thead th {
        background-color: white!important;
        color: black!important;
    }

    table.dataTable thead td {
        background-color: white!important;
        color: black!important;
    }

    .paginate_button,
    .paginate_button:hover,
    .paginate_button:active {
        color: black!important;
        background-color: white!important;
        border-color: black!important;
    }
    "))),

  headerPanel("Monthly CPI Indicator - weighted average of eight capital cities"),
  sidebarPanel(

    radioButtons("choosetable", "Index or Percent change:",
                 choices = c("Index value   " = "index", "Percent change" = "pc", "Annual average percent" = "avg"),
                 selected = "pc", inline = TRUE),

    conditionalPanel(
      condition = "input.choosetable == 'pc'",
      radioButtons("plottype", "Chart type:",
                   choices = c("Line (time-series)" = "Line",
                               "Bar (total change)" = "Bar"),
                   selected = "Line", inline = FALSE)
    ),

    uiOutput("MonthRange_ui"),

    tags$div(tags$hr()),
    tags$div(tags$h4(tags$strong("Select CPI items:"))),

    pickerInput("List", " ",
                choices = c(
                  "All groups CPI" = "All groups CPI",
                  "..Food and non-alcoholic beverages" = "Food and non-alcoholic beverages",
                  "....Bread and cereal products" = "Bread and cereal products",
                  "......Bread" = "Bread",
                  "......Cakes and biscuits" = "Cakes and biscuits",
                  "......Breakfast cereals" = "Breakfast cereals",
                  "......Other cereal products" = "Other cereal products",
                  "....Meat and seafoods" = "Meat and seafoods",
                  "......Beef and veal" = "Beef and veal",
                  "......Lamb and goat" = "Lamb and goat",
                  "......Pork" = "Pork",
                  "......Poultry" = "Poultry",
                  "......Other meats" = "Other meats",
                  "......Fish and other seafood" = "Fish and other seafood",
                  "....Dairy and related products" = "Dairy and related products",
                  "......Milk" = "Milk",
                  "......Cheese" = "Cheese",
                  "......Ice cream and other dairy products" = "Ice cream and other dairy products",
                  "....Fruit and vegetables" = "Fruit and vegetables",
                  "......Fruit" = "Fruit",
                  "......Vegetables" = "Vegetables",
                  "....Non-alcoholic beverages" = "Non-alcoholic beverages",
                  "......Coffee, tea and cocoa" = "Coffee, tea and cocoa",
                  "......Waters, soft drinks and juices" = "Waters, soft drinks and juices",
                  "....Meals out and take away foods" = "Meals out and take away foods",
                  "......Restaurant meals" = "Restaurant meals",
                  "......Take away and fast foods" = "Take away and fast foods",
                  "....Food products n.e.c." = "Food products n.e.c.",
                  "......Eggs" = "Eggs",
                  "......Oils and fats" = "Oils and fats",
                  "......Snacks and confectionery" = "Snacks and confectionery",
                  "......Other food products n.e.c." = "Other food products n.e.c.",
                  "..Clothing and footwear" = "Clothing and footwear",
                  "....Garments" = "Garments",
                  "..Housing" = "Housing",
                  "....Rents" = "Rents",
                  "....New dwelling purchase by owner-occupiers" = "New dwelling purchase by owner-occupiers",
                  "....Electricity" = "Electricity",
                  "....Gas and other household fuels" = "Gas and other household fuels",
                  "..Furnishings, household equipment and services" = "Furnishings, household equipment and services",
                  "..Health" = "Health",
                  "..Transport" = "Transport",
                  "....Automotive fuel" = "Automotive fuel",
                  "....Holiday travel and accommodation" = "Holiday travel and accommodation",
                  "..Communication" = "Communication",
                  "..Recreation and culture" = "Recreation and culture",
                  "..Education" = "Education",
                  "..Alcohol and tobacco" = "Alcohol and tobacco",
                  "....Alcoholic beverages" = "Alcoholic beverages",
                  "....Tobacco" = "Tobacco",
                  "..Insurance and financial services" = "Insurance and financial services",
                  "--- Analytical series ---" = "--- Analytical series ---",
                  "All groups, goods component" = "All groups, goods component",
                  "All groups, services component" = "All groups, services component",
                  "All groups CPI excl. volatile items" = "All groups CPI excl. volatile items",
                  "All groups excl. volatile items and holiday travel" = "All groups excl. volatile items and holiday travel",
                  "Tradables" = "Tradables",
                  "Non-tradables" = "Non-tradables"
                ),
                selected = "All groups CPI",
                multiple = TRUE,
                options = list(`actions-box` = TRUE)),

    downloadButton("downloadTb", "Download selection:")
  ),

  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Graph", highchartOutput("hcontainer", height = "720px")),
                tabPanel("Table", verbatimTextOutput("message"), DT::dataTableOutput("table"))
    ),
    uiOutput("footer_ui")
  )
)


#------------
# Server ---
#------------
server <- function(input, output, session) {

  #--- Load data once per session ---
  dat <- read_sdmx(sdmx_dat_m)

  CPIdat <- dat %>%
    select(INDEX, ObsDimension, ObsValue) %>%
    rename("Month" = ObsDimension, "Index value" = ObsValue) %>%
    mutate(CPI_components = "",
           code = as.numeric(INDEX),
           `Index value` = as.numeric(`Index value`)) %>%
    left_join(cpi_m_labels, by = "code") %>%
    mutate(CPI_components = label) %>%
    select(INDEX, CPI_components, Month, `Index value`) %>%
    filter(!is.na(CPI_components))   # drop any codes not in label table

  #--- Parse dates ---
  CPIdat <- CPIdat %>%
    mutate(date = ym(Month))

  dmax <- max(CPIdat$date)
  latest <- format(dmax, "%b %Y")
  now    <- format(today(), "%d %B %Y")

  CPIdatL <- CPIdat  # all available history

  #--- Month index for slider filtering ---
  Month_df <- CPIdatL %>%
    select(Month, date) %>%
    distinct() %>%
    arrange(date) %>%
    mutate(idx      = row_number(),
           MonthStr = format(date, "%b %Y"))

  Month_label <- as.list(Month_df$MonthStr)

  CPIdatL <- CPIdatL %>%
    mutate(MonthStr = format(date, "%b %Y")) %>%
    left_join(Month_df %>% select(MonthStr, idx), by = "MonthStr")

  #--- Dynamic UI: month slider ---
  output$MonthRange_ui <- renderUI({
    sliderTextInput("MonthRange", "Month range:",
                    choices  = Month_label,
                    selected = c(Month_label[1], Month_label[length(Month_label)]),
                    grid     = TRUE)
  })

  #--- Dynamic UI: footer ---
  output$footer_ui <- renderUI({
    tags$div(class = "header", checked = NA,
             tags$p("Source:",
                    tags$a(href = "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/monthly-consumer-price-index-indicator/latest-release",
                           paste0("Australian Bureau of Statistics, Monthly CPI Indicator (Cat. 6484.0): ", latest))),
             tags$p("Retrieved from",
                    tags$a(href = "https://explore.data.abs.gov.au/?fs[0]=ABS%20Topics%2C0%7CECONOMY%23ECONOMY%23&pg=0&fc=ABS%20Topics",
                           paste0("ABS Data Explorer: ", now)))
    )
  })


  I <- reactive({
    list(FoodGROUP = input$List)
  })

  #--- Filter to selected month range ---
  filteredCPI <- reactive({
    req(input$MonthRange)
    start_str <- input$MonthRange[1]
    end_str   <- input$MonthRange[2]
    start_idx <- CPIdatL %>% filter(MonthStr == start_str) %>% pull(idx) %>% first()
    end_idx   <- CPIdatL %>% filter(MonthStr == end_str)   %>% pull(idx) %>% first()
    CPIdatL %>% filter(idx >= start_idx & idx <= end_idx)
  })

  #--- Cumulative % change from start of selected period ---
  dfc <- reactive({
    filteredCPI() %>%
      group_by(CPI_components) %>%
      arrange(date) %>%
      mutate(indcum = cumsum(`Index value`),
             change = round(`Index value` / min(indcum) * 100 - 100, 1),
             Date   = format(date, "%b %Y")) %>%
      filter(CPI_components %in% I()$FoodGROUP)
  })

  #--- First & last only, for bar charts and annual average ---
  dfs <- reactive({
    req(input$MonthRange)
    CPIdatL %>%
      filter(CPI_components %in% I()$FoodGROUP) %>%
      filter(MonthStr == input$MonthRange[1] | MonthStr == input$MonthRange[2]) %>%
      group_by(CPI_components) %>%
      mutate(Date = format(date, "%b %Y")) %>%
      summarize(
        change      = if (n() == 2) round((last(`Index value`) - first(`Index value`)) / first(`Index value`) * 100, 1) else NA_real_,
        index_change = if (n() == 2) last(`Index value`) - first(`Index value`) else NA_real_,
        num_years   = (max(idx) - min(idx)) / 12,
        first_index = first(`Index value`),
        last_index  = last(`Index value`),
        ann_avg     = if (n() == 2) round(((last_index / first_index)^(1 / num_years)) - 1, 4) * 100 else NA_real_,
        period      = paste0(input$MonthRange[1], " to ", input$MonthRange[2]),
        .groups     = "drop"
      )
  })


  output$message <- renderText({
    if (is.null(I()$FoodGROUP)) "No CPI items selected" else ""
  })


  ## Font-setting helpers
  apply_font_settings_bar <- function(hc) {
    hc %>%
      hc_yAxis(title = list(text = "Percent change", style = list(fontSize = "16px")),
               labels = list(style = list(fontSize = "14px"))) %>%
      hc_xAxis(title = list(text = "CPI items", style = list(fontSize = "18px")),
               labels = list(style = list(fontSize = "16px"))) %>%
      hc_title(style = list(fontSize = "20px")) %>%
      hc_plotOptions(series = list(
        dataLabels = list(enabled = TRUE, style = list(fontSize = "14px")),
        marker = list(enabled = FALSE),
        stacking = FALSE, grouping = FALSE
      ))
  }

  apply_font_settings_line <- function(hc) {
    hc %>%
      hc_yAxis(title = list(text = "Percent change", style = list(fontSize = "18px")),
               labels = list(style = list(fontSize = "16px"))) %>%
      hc_xAxis(title = list(text = "Month", style = list(fontSize = "16px")),
               labels = list(style = list(fontSize = "16px"))) %>%
      hc_title(style = list(fontSize = "20px")) %>%
      hc_legend(itemStyle = list(fontSize = "16px")) %>%
      hc_plotOptions(series = list(
        dataLabels = list(enabled = FALSE),
        marker     = list(enabled = TRUE),
        lineWidth  = 3,
        stacking   = FALSE, grouping = FALSE
      ))
  }


  output$hcontainer <- renderHighchart({
    req(input$MonthRange)

    if (is.null(I()$FoodGROUP)) {
      hc <- dfc() %>%
        hchart(type = "line",
               hcaes(x = date, y = round(change, 1), group = CPI_components)) %>%
        hc_xAxis(title = list(text = "Month")) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = "No CPI items selected") %>%
        hc_colors(abscol) %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE)))
    }

    if (input$choosetable == "index" & !is.null(I()$FoodGROUP)) {
      hc <- dfc() %>%
        hchart(type = "line",
               hcaes(x = Date, y = `Index value`, group = CPI_components)) %>%
        hc_xAxis(title = list(text = "Month"), crosshair = TRUE) %>%
        apply_font_settings_line() %>%
        hc_yAxis(title = list(text = "Index")) %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("Monthly CPI index value, ", input$MonthRange[1], " to ", input$MonthRange[2])) %>%
        hc_colors(abscol) %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE)))
    }

    if (input$choosetable == "pc" & !is.null(I()$FoodGROUP) & input$plottype == "Line") {
      hc <- dfc() %>%
        hchart(type = "line",
               hcaes(x = Date, y = round(change, 1), group = CPI_components)) %>%
        hc_xAxis(title = list(text = "Month"), crosshair = TRUE) %>%
        hc_yAxis(title = list(text = "Percent change")) %>%
        apply_font_settings_line() %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("Cumulative percent change, ", input$MonthRange[1], " to ", input$MonthRange[2])) %>%
        hc_colors(abscol) %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE)))
    }

    if (input$choosetable == "pc" & !is.null(I()$FoodGROUP) & input$plottype == "Bar") {
      hc <- dfs() %>%
        filter(!is.na(change)) %>%
        mutate(CPI_components = fct_reorder(CPI_components, desc(change))) %>%
        hchart(type = "bar",
               hcaes(x = CPI_components, y = round(change, 1), group = CPI_components)) %>%
        hc_xAxis(title = list(text = "Component")) %>%
        hc_yAxis(title = list(text = "Percent change")) %>%
        apply_font_settings_bar() %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("Total percent change, ", input$MonthRange[1], " to ", input$MonthRange[2])) %>%
        hc_colors("#4FADE7") %>%
        hc_plotOptions(series = list(
          marker = list(enabled = FALSE), stacking = FALSE, grouping = FALSE
        ))
    }

    if (input$choosetable == "avg" & !is.null(I()$FoodGROUP)) {
      hc <- dfs() %>%
        filter(!is.na(ann_avg)) %>%
        mutate(CPI_components = fct_reorder(CPI_components, desc(ann_avg))) %>%
        hchart(type = "bar",
               hcaes(x = CPI_components, y = round(ann_avg, 1), group = CPI_components)) %>%
        hc_xAxis(title = list(text = "Component")) %>%
        hc_yAxis(title = list(text = "Percent change")) %>%
        apply_font_settings_bar() %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("Annual average change, ", input$MonthRange[1], " to ", input$MonthRange[2])) %>%
        hc_colors("#4FADE7") %>%
        hc_plotOptions(series = list(
          marker = list(enabled = FALSE), stacking = FALSE, grouping = FALSE
        ))
    }

    hc
  })


  #--- Table reactives ---
  index_tbl <- reactive({
    dfc() %>%
      group_by(CPI_components) %>%
      select(date, CPI_components, `Index value`) %>%
      spread(CPI_components, `Index value`) %>%
      rename_if(is.numeric, ~ paste0(., ", Index value")) %>%
      mutate(Month = format(date, "%b %Y")) %>%
      arrange(date) %>%
      select(-date) %>%
      select(Month, everything())
  })

  pcl_tbl <- reactive({
    dfc() %>%
      group_by(CPI_components) %>%
      select(date, CPI_components, change) %>%
      spread(CPI_components, change) %>%
      rename_if(is.numeric, ~ paste0(., ", Cuml. chg (%)")) %>%
      mutate(Month = format(date, "%b %Y")) %>%
      arrange(date) %>%
      select(-date) %>%
      select(Month, everything())
  })

  pcb_tbl <- reactive({
    dfs() %>%
      group_by(CPI_components) %>%
      select(period, CPI_components, change) %>%
      spread(CPI_components, change) %>%
      rename_if(is.numeric, ~ paste0(., ", Total chg (%)")) %>%
      select(period, everything())
  })

  avg_tbl <- reactive({
    dfs() %>%
      group_by(CPI_components) %>%
      select(period, CPI_components, ann_avg) %>%
      spread(CPI_components, ann_avg) %>%
      rename_if(is.numeric, ~ paste0(., ", Annual chg (%)")) %>%
      select(period, everything())
  })

  active_tbl <- reactive({
    if (input$choosetable == "index")                              return(index_tbl())
    if (input$choosetable == "avg")                               return(avg_tbl())
    if (input$choosetable == "pc" & input$plottype == "Line")     return(pcl_tbl())
    if (input$choosetable == "pc" & input$plottype == "Bar")      return(pcb_tbl())
    index_tbl()
  })

  output$table <- DT::renderDataTable({ active_tbl() })

  output$downloadTb <- downloadHandler(
    filename = function() { paste("Monthly CPI percent change", ".xlsx") },
    content  = function(file) { writexl::write_xlsx(active_tbl(), path = file) }
  )

}

#========================================
shinyApp(ui, server)
#========================================
