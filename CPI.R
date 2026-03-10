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
library(readxl)
library(scales)
library(openxlsx)
library(rsconnect)

#-----------------------------------------------------
#---- Load CPI data from ABS.Stat SDMX-XML API -
#-----------------------------------------------------

#--- SDMX data query URL---
sdmx_dat <- "https://api.data.abs.gov.au/data/ABS,CPI,2.0.0/1.114120+115492+115522+115528+131179+131180+131181+131182+131184+131186+131187+131188+131189+131191+131193+131195+30001+30002+30003+30007+30012+30016+30022+30024+30025+30026+30027+30033+40106+97556+97561+97563+97565+114121+114122+1144+115484+115485+115495+115496+115497+115498+115500+115501+115520+115524+115529+131178+131183+131185+131190+131192+131194+30014+40001+40002+40004+40005+40006+40007+40008+40009+40010+40012+40014+40015+40025+40026+40027+40029+40030+40034+40045+40046+40047+40048+40053+40055+40058+40060+40066+40067+40072+40073+40077+40078+40080+40081+40083+40084+40085+40086+40087+40088+40089+40090+40091+40092+40093+40094+40095+40096+40098+40101+40102+97549+97550+97551+97554+97555+97557+97558+97559+97560+97564+97567+97571+97572+97573+97574+10001+20001+20002+20003+20004+20005+20006+115486+115488+115489+115493+126670+999901+999902+999903.10.50.Q?startPeriod=2000-Q3&endPeriod=2027-Q1"

#--- ABS preferred colours---
abscol <- c("#4FADE7", 	"#1A4472", 	"#F29000", 	"#993366", 	"#669966", 	"#99CC66",
            "#CC9966", 	"#666666", 	"#8DD3C7", 	"#BEBADA", 	"#FB8072", 	"#80B1D3",
            "#FDB462", 	"#B3DE69", 	"#FCCDE5", 	"#D9D9D9", 	"#BC80BD", 	"#CCEBC5", 	"#ffcc99")

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

  headerPanel("CPI time series - weighted average of eight capital cities"),
  sidebarPanel(

    radioButtons ("choosetable", "Index or Percent change:",
                  choices = c("Index value   " = "index", "Percent change" = "pc", "Annual average percent" = "avg"),
                   selected = c("pc"),inline = T),

   conditionalPanel(
      condition = "input.choosetable == 'pc'",

    radioButtons ("plottype", "Chart type:",
                  choices = c("Line (time-series)" = "Line",
                              "Bar (total change)" = "Bar"),
                  selected = c("Line"),inline = F)
    ),

    uiOutput("QtrRange_ui"),


    tags$div( tags$hr()),
    tags$div(
      tags$h4(tags$strong("Select CPI items:"))),



    pickerInput ("List", " ", choices= c("All groups CPI " = "All groups CPI",
                                         "Food and non-alcoholic beverages " = "Food and non-alcoholic beverages",
                                         "..Bread and cereal products " = "Bread and cereal products",
                                         "......Bread " = "Bread",
                                         "......Cakes and biscuits " = "Cakes and biscuits",
                                         "......Breakfast cereals " = "Breakfast cereals",
                                         "......Other cereal products " = "Other cereal products",
                                         "..Meat and seafoods " = "Meat and seafoods",
                                         "......Beef and veal " = "Beef and veal",
                                         "......Pork " = "Pork",
                                         "......Lamb and goat " = "Lamb and goat",
                                         "......Poultry " = "Poultry",
                                         "......Other meats " = "Other meats",
                                         "......Fish and other seafood " = "Fish and other seafood",
                                         "..Dairy and related products " = "Dairy and related products",
                                         "......Milk " = "Milk",
                                         "......Cheese " = "Cheese",
                                         "......Ice cream and other dairy products " = "Ice cream and other dairy products",
                                         "..Fruit and vegetables " = "Fruit and vegetables",
                                         "......Fruit " = "Fruit",
                                         "......Vegetables " = "Vegetables",
                                         "..Food products n.e.c. " = "Food products n.e.c.",
                                         "......Eggs " = "Eggs",
                                         "......Jams, honey and spreads " = "Jams, honey and spreads",
                                         "......Food additives and condiments " = "Food additives and condiments",
                                         "......Oils and fats " = "Oils and fats",
                                         "......Snacks and confectionery " = "Snacks and confectionery",
                                         "......Other food products n.e.c. " = "Other food products n.e.c.",
                                         "..Non-alcoholic beverages " = "Non-alcoholic beverages",
                                         "......Coffee, tea and cocoa " = "Coffee, tea and cocoa",
                                         "......Waters, soft drinks and juices " = "Waters, soft drinks and juices",
                                         "..Meals out and take away foods " = "Meals out and take away foods",
                                         "......Restaurant meals " = "Restaurant meals",
                                         "......Take away and fast foods " = "Take away and fast foods",
                                         "Alcohol and tobacco " = "Alcohol and tobacco",
                                         "..Alcoholic beverages " = "Alcoholic beverages",
                                         "......Spirits " = "Spirits",
                                         "......Wine " = "Wine",
                                         "......Beer " = "Beer",
                                         "..Tobacco " = "Tobacco",
                                         "Clothing and footwear " = "Clothing and footwear",
                                         "..Garments " = "Garments",
                                         "......Garments for men " = "Garments for men",
                                         "......Garments for women " = "Garments for women",
                                         "......Garments for infants and children " = "Garments for infants and children",
                                         "..Footwear " = "Footwear",
                                         "......Footwear for men " = "Footwear for men",
                                         "......Footwear for women " = "Footwear for women",
                                         "......Footwear for infants and children " = "Footwear for infants and children",
                                         "..Accessories and clothing services " = "Accessories and clothing services",
                                         "......Accessories " = "Accessories",
                                         "......Cleaning, repair and hire of clothing and footwear " = "Cleaning, repair and hire of clothing and footwear",
                                         "Housing " = "Housing",
                                         "......Rents " = "Rents",
                                         "......New dwelling purchase by owner-occupiers " = "New dwelling purchase by owner-occupiers",
                                         "......Other housing " = "Other housing",
                                         "..Maintenance and repair of the dwelling " = "Maintenance and repair of the dwelling",
                                         "......Property rates and charges " = "Property rates and charges",
                                         "......Utilities " = "Utilities",
                                         "......Water and sewerage " = "Water and sewerage",
                                         "......Electricity " = "Electricity",
                                         "......Gas and other household fuels " = "Gas and other household fuels",
                                         "Furnishings, household equipment and services " = "Furnishings, household equipment and services",
                                         "..Furniture and furnishings " = "Furniture and furnishings",
                                         "......Furniture " = "Furniture",
                                         "......Carpets and other floor coverings " = "Carpets and other floor coverings",
                                         "......Household textiles " = "Household textiles",
                                         "..Household appliances, utensils and tools " = "Household appliances, utensils and tools",
                                         "......Major household appliances " = "Major household appliances",
                                         "......Small electric household appliances " = "Small electric household appliances",
                                         "......Glassware, tableware and household utensils " = "Glassware, tableware and household utensils",
                                         "......Tools and equipment for house and garden " = "Tools and equipment for house and garden",
                                         "..Non-durable household products " = "Non-durable household products",
                                         "......Cleaning and maintenance products " = "Cleaning and maintenance products",
                                         "......Personal care products " = "Personal care products",
                                         "......Other non-durable household products " = "Other non-durable household products",
                                         "..Domestic and household services " = "Domestic and household services",
                                         "......Child care " = "Child care",
                                         "......Hairdressing and personal grooming services " = "Hairdressing and personal grooming services",
                                         "......Other household services " = "Other household services",
                                         "Health " = "Health",
                                         "..Medical products, appliances and equipment " = "Medical products, appliances and equipment",
                                         "......Pharmaceutical products " = "Pharmaceutical products",
                                         "......Therapeutic appliances and equipment " = "Therapeutic appliances and equipment",
                                         "..Medical, dental and hospital services " = "Medical, dental and hospital services",
                                         "......Medical and hospital services " = "Medical and hospital services",
                                         "......Dental services " = "Dental services",
                                         "Transport " = "Transport",
                                         "......Private motoring " = "Private motoring",
                                         "......Motor vehicles " = "Motor vehicles",
                                         "......Spare parts and accessories for motor vehicles " = "Spare parts and accessories for motor vehicles",
                                         "......Automotive fuel " = "Automotive fuel",
                                         "......Maintenance and repair of motor vehicles " = "Maintenance and repair of motor vehicles",
                                         "......Other services in respect of motor vehicles " = "Other services in respect of motor vehicles",
                                         "......Urban transport fares " = "Urban transport fares",
                                         "Communication " = "Communication",
                                         "......Postal services " = "Postal services",
                                         "......Telecommunication equipment and services " = "Telecommunication equipment and services",
                                         "Recreation and culture " = "Recreation and culture",
                                         "..Audio, visual and computing equipment and services " = "Audio, visual and computing equipment and services",
                                         "......Audio, visual and computing equipment " = "Audio, visual and computing equipment",
                                         "......Audio, visual and computing media and services " = "Audio, visual and computing media and services",
                                         "..Newspapers, books and stationery " = "Newspapers, books and stationery",
                                         "......Books " = "Books",
                                         "......Newspapers, magazines and stationery " = "Newspapers, magazines and stationery",
                                         "..Holiday travel and accommodation " = "Holiday travel and accommodation",
                                         "......Domestic holiday travel and accommodation " = "Domestic holiday travel and accommodation",
                                         "......International holiday travel and accommodation " = "International holiday travel and accommodation",
                                         "......Other recreation, sport and culture " = "Other recreation, sport and culture",
                                         "......Equipment for sports, camping and open-air recreation " = "Equipment for sports, camping and open-air recreation",
                                         "......Games, toys and hobbies " = "Games, toys and hobbies",
                                         "..Pets and related products " = "Pets and related products",
                                         "......Veterinary and other services for pets " = "Veterinary and other services for pets",
                                         "..Sports participation " = "Sports participation",
                                         "..Other recreational, sporting and cultural services " = "Other recreational, sporting and cultural services",
                                         "Education " = "Education",
                                         "..Preschool and primary education " = "Preschool and primary education",
                                         "......Secondary education " = "Secondary education",
                                         "......Tertiary education " = "Tertiary education",
                                         "Insurance and financial services " = "Insurance and financial services",
                                         "......Insurance " = "Insurance",
                                         "......Financial services " = "Financial services"
    ),

                 selected = c("All groups CPI"), multiple = TRUE, options = list(`actions-box` = TRUE)),

    downloadButton("downloadTb", "Download selection:")

  ),

  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Graph", highchartOutput("hcontainer",height = "720px")),
                tabPanel("Table", verbatimTextOutput("message"), DT::dataTableOutput("table"))
    ),


    # adding the div tag to the mainpanel
    uiOutput("footer_ui")

  ))



#------------
# Server ---
#------------
server <- function(input, output, session) {

  #--- Load data once per session ---
  dat <- read_sdmx(sdmx_dat)

  CPIdat <- dat %>%
    select(INDEX, ObsDimension, ObsValue) %>%
    rename("Qtr" = ObsDimension, "Index value" = ObsValue) %>%
    mutate(CPI_components = "")

  label <- read.xlsx("https://github.com/Atyepa/Australian-CPI-App/raw/main/CPI%20code%20labels.xlsx") %>%
    mutate(code = as.numeric(code)) %>%
    select(-Classn_level) %>%
    group_by(Group, label) %>%
    summarise(code = max(code)) %>%
    ungroup()

  CPIdat <- CPIdat %>%
    mutate(code = INDEX,
           INDEX = as.numeric(INDEX),
           `Index value` = as.numeric(`Index value`)) %>%
    select(INDEX, code, CPI_components, Qtr, `Index value`) %>%
    left_join(label, by = c("INDEX" = "code")) %>%
    mutate(CPI_components = label) %>%
    select(-label, -code)

  CPIdat <- CPIdat %>%
    mutate(startqtr = yq(Qtr)) %>%
    mutate(endqtr = ceiling_date(startqtr, "quarter")) %>%
    mutate(date = endqtr - 1) %>%
    mutate(date = floor_date(date, "month"))

  dmax <- max(CPIdat$date)
  dmin <- dmax - months(240)

  CPIdatL <- CPIdat %>%
    filter(date >= dmin)

  latest <- format(dmax, "%b %Y")
  now <- format(today(), "%d %B %Y")

  Qtr <- CPIdatL %>%
    select(Qtr) %>%
    distinct() %>%
    mutate(idx = row_number())

  Qtr_label <- Qtr %>%
    select(Qtr) %>%
    rename(Qtr_label = Qtr)

  Qtr_label <- as.list(Qtr_label$Qtr_label)

  CPIdatL <- CPIdatL %>%
    left_join(Qtr, by = "Qtr")

  #--- Render dynamic UI: quarter slider ---
  output$QtrRange_ui <- renderUI({
    sliderTextInput("QtrRange", "Quarter range:",
                    choices = Qtr_label,
                    selected = c(Qtr_label[1], Qtr_label[length(Qtr_label)]),
                    grid = TRUE)
  })

  #--- Render dynamic UI: footer ---
  output$footer_ui <- renderUI({
    tags$div(class="header", checked=NA,
             tags$p(paste0("Source:"),
                    tags$a(href="https://www.abs.gov.au/ausstats/abs@.nsf/mf/6401.0",
                           (paste0("Australian Bureau of Statistics,
                                   Consumer Price Index (CPI): ", latest)))),
             tags$p(paste0("Retrieved from"),
                    tags$a(href="https://explore.data.abs.gov.au/?fs[0]=ABS%20Topics%2C0%7CECONOMY%23ECONOMY%23&pg=0&fc=ABS%20Topics",
                           (paste0(".Stat Data Explorer: ", now))))
    )
  })


 I <- reactive({
    list (FoodGROUP = input$List) })


 # Filter user-selected quarters

 filteredCPI <- reactive({
    req(input$QtrRange)
    start_quarter <- input$QtrRange[1]
    end_quarter <- input$QtrRange[2]
    start_idx <- CPIdatL %>% filter(Qtr == start_quarter) %>% select(idx) %>% pull() %>% first()
    end_idx <- CPIdatL %>% filter(Qtr == end_quarter) %>% select(idx) %>% pull() %>% first()
    filteredCPI <- CPIdatL %>% filter(idx >= start_idx & idx <= end_idx)
    filteredCPI

  })


  dfc <- reactive({ filteredCPI() %>%
      group_by(CPI_components) %>%
      arrange(date) %>%
      mutate(indcum = cumsum(`Index value`)) %>%
      mutate(change = round(`Index value`/ min(indcum)*100-100,1)) %>%
      mutate(Date = format(date, "%b %Y")) %>%
      filter(CPI_components %in% I()$FoodGROUP)
  })

  # df that only keeps first & last dates & make 'ann_avg'
  dfs <- reactive({
    req(input$QtrRange)
    CPIdatL %>%
      filter(CPI_components %in% I()$FoodGROUP) %>%
      filter(Qtr == input$QtrRange[1] | Qtr == input$QtrRange[2]) %>%
      group_by(CPI_components) %>%
      mutate(Date = format(date, "%b %Y")) %>%
      summarize(
        change = if(n() == 2) round((last(`Index value`) - first(`Index value`)) / first(`Index value`) * 100, 1) else NA_real_,
        index_change = if(n() == 2) (last(`Index value`) - first(`Index value`)) else NA_real_,
        num_years = (max(idx) - min(idx)) / 4,
        first_index = first(`Index value`),
        last_index = last(`Index value`),
        ann_avg = if(n() == 2) round(((last_index / first_index)^(1 / num_years)) - 1, 4) * 100 else NA_real_,
        period = paste0(input$QtrRange[1], " to ", input$QtrRange[2]),
        .groups = "drop"
      )
  })




output$message <- renderText({

  if(is.null(I()$FoodGROUP)){
    msg <- "No CPI items selected"}


  if(!is.null(I()$FoodGROUP)){
    msg <- ""}

  msg

})

##
# Define the custom function to apply font settings
apply_font_settings_bar <- function(hc) {
  hc %>%
    hc_yAxis(title = list(text = "Percent change", style = list(fontSize = '16px')),
             labels = list(style = list(fontSize = '14px'))) %>%
    hc_xAxis(title = list(text = "CPI items", style = list(fontSize = '18px')),
             labels = list(style = list(fontSize = '16px'))) %>%
    hc_title(style = list(fontSize = '20px')) %>%
    hc_plotOptions(
      series = list(
        dataLabels = list(
          enabled = TRUE,
          style = list(fontSize = '14px')
        ),
        marker = list(enabled = FALSE),
        stacking = FALSE,
        grouping = FALSE  # Disable automatic grouping
      ))
}

apply_font_settings_line <- function(hc) {
  hc %>%
    hc_yAxis(title = list(text = "Percent change", style = list(fontSize = '18px')),
             labels = list(style = list(fontSize = '16px'))) %>%
    hc_xAxis(title = list(text = "Year", style = list(fontSize = '16px')),
             labels = list(style = list(fontSize = '16px'))) %>%
    hc_title(style = list(fontSize = '20px')) %>%
    hc_legend(itemStyle = list(fontSize = '16px')) %>%  # Set the font size for the legend
    hc_plotOptions(
      series = list(
        dataLabels = list(
          enabled = FALSE
        ),
        marker = list(enabled = TRUE),
        lineWidth = 3,  # Set the line width
        stacking = FALSE,
        grouping = FALSE  # Disable automatic grouping
      ))
}

  output$hcontainer <- renderHighchart({

    req(input$QtrRange)

    if(is.null(I()$FoodGROUP)) {

      hc <- dfc() %>%
        hchart(.,
               type = "line",
               hcaes(x = date,
                     y = round(change,1),
                     group = CPI_components)) %>%
        hc_xAxis(title = list(text = "Quarter")) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("No CPI items selected")) %>%
        hc_colors(abscol) %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE)))
    }


    if(input$choosetable == "index" & !is.null(I()$FoodGROUP)) {

        hc <- dfc() %>%
        hchart(.,
               type = "line",
               hcaes(x = Date,
                     y = `Index value`,
                     group = CPI_components)) %>%
        hc_xAxis(title = list(text = "Quarter"),
                 crosshair = TRUE) %>%
        apply_font_settings_line() %>%
        hc_yAxis(title = list(text = "Index")) %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("Consumer price index value,"," ", input$QtrRange[1] ," ", "to ", input$QtrRange[2])) %>%
        hc_colors(abscol) %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE)))
    }


    if(input$choosetable == "pc" & !is.null(I()$FoodGROUP) & input$plottype == "Line") {

      hc <- dfc() %>%
        hchart(.,
               type = "line",
               hcaes(x = Date,
                     y = round(change,1),
                     group = CPI_components)) %>%
        hc_xAxis(title = list(text = "Quarter"),
                 crosshair = TRUE)%>%
        hc_yAxis(title = list(text = "Percent change")) %>%
        apply_font_settings_line() %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("Percent change in inflation,"," ", input$QtrRange[1] ," ", "to ", input$QtrRange[2])) %>%
         hc_colors(abscol) %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE)))
    }


    if(input$choosetable == "pc" & !is.null(I()$FoodGROUP) & input$plottype == "Bar") {

      hc <- dfs() %>%
        filter(!is.na(change)) %>%
        mutate(CPI_components = fct_reorder(CPI_components, desc(change))) %>%

        hchart(.,
               type = "bar",
               hcaes(x = CPI_components, y = round(change,1), group = CPI_components)) %>%
        hc_xAxis(title = list(text = "Component")) %>%
        hc_yAxis(title = list(text = "Percent change")) %>%
        apply_font_settings_bar() %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("Percent change in inflation,"," ", input$QtrRange[1] ," ", "to ", input$QtrRange[2])) %>%
        hc_colors("#4FADE7") %>%
        hc_plotOptions(
          series = list(
            marker = list(enabled = FALSE),
            stacking = FALSE,
            grouping = FALSE
          ))
    }


    if(input$choosetable == "avg" & !is.null(I()$FoodGROUP)) {

      hc <- dfs() %>%
        filter(!is.na(change)) %>%
        mutate(CPI_components = fct_reorder(CPI_components, desc(change))) %>%

        hchart(.,
               type = "bar",
               hcaes(x = CPI_components, y = round(ann_avg,1), group = CPI_components)) %>%
        hc_xAxis(title = list(text = "Component")) %>%
        hc_yAxis(title = list(text = "Percent change")) %>%
        apply_font_settings_bar() %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("Annual average change in inflation,"," ", input$QtrRange[1] ," ", "to ", input$QtrRange[2])) %>%
        hc_colors("#4FADE7") %>%
        hc_plotOptions(
          series = list(
            marker = list(enabled = FALSE),
            stacking = FALSE,
            grouping = FALSE
          ))
    }


    hc

    })


#--- Spread items into columns and combine into table for output tab ---
  index  <- reactive({ dfc() %>%
      group_by(CPI_components) %>%
      select(date, CPI_components, `Index value`) %>%
      spread(CPI_components, `Index value`) %>%
      rename_if(is.numeric, ~(paste0(., ", Index value" ))) %>%
      mutate(Quarter = format(date, "%b %Y")) %>%
      arrange(date) %>%
      select(-date) %>%
      select(Quarter, everything())

  })

 pcl <- reactive({ dfc() %>%
      group_by(CPI_components) %>%
      select(date, CPI_components, change) %>%
      spread(CPI_components, change) %>%
      rename_if(is.numeric, ~(paste0(., ", Cuml. chg (%)" ))) %>%
      mutate(Quarter = format(date, "%b %Y")) %>%
      arrange(date) %>%
      select(-date) %>%
      select(Quarter, everything())

  })

 pcb  <- reactive({ dfs() %>%
      group_by(CPI_components) %>%
      select(period, CPI_components, change) %>%
      spread(CPI_components, change) %>%
      rename_if(is.numeric, ~(paste0(., ", Total chg (%)" ))) %>%
      select(period, everything())

  })

  avg  <- reactive({ dfs() %>%
      group_by(CPI_components) %>%
      select(period, CPI_components, ann_avg) %>%
      spread(CPI_components, ann_avg) %>%
      rename_if(is.numeric, ~(paste0(., ", Annual chg (%)" ))) %>%
      select(period, everything())

  })


  # Table for display
  output$table = DT::renderDataTable({

  if(input$choosetable == "index") {
    table <- index
    }

  if(input$choosetable == "avg") {
    table <- avg
  }

  if(input$choosetable == "pc" & input$plottype == "Line") {
    table <- pcl
  }

  if(input$choosetable == "pc" & input$plottype == "Bar") {
    table <- pcb
  }

  table()

  })

  # Table for downloadable xlsx
  table  <- reactive({

    if(input$choosetable == "index") {
      table <- index
    }

    if(input$choosetable == "avg") {
      table <- avg
    }

    if(input$choosetable == "pc" & input$plottype == "Line") {
      table <- pcl
    }

    if(input$choosetable == "pc" & input$plottype == "Bar") {
      table <- pcb  }

    table()

    })

  output$downloadTb <- downloadHandler(
    filename = function() { paste("CPI group percent change", ".xlsx") },
    content = function(file) { write_xlsx(table(), path = file) }
  )

}

#========================================
shinyApp(ui, server)
#========================================
