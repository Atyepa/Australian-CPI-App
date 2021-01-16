library(tidyverse)
library(highcharter)
library(ggthemes)
library(DT)
library(rsdmx)
library(shinyWidgets)
library(lubridate)
library(zoo)
library(rsconnect)
library(writexl)
library(readxl)
library(scales)

#-----------------------------------------------------
#---- Load CPI data from ABS.Stat SDMX-XML API ----
#-----------------------------------------------------

#--- SDMX data query URL---
sdmx_dat <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.10001+20001+30002+40005+40006+40007+40008+30003+40009+131178+40010+40012+40014+40015+30001+40001+40002+40004+114120+114121+114122+131179+40027+40029+97549+97550+115501+40034+131180+40030+115520+30007+40025+40026+20006+30026+40089+40088+40087+30027+20002+131181+97551+97554+97555+30012+40045+40046+40047+97556+97557+40048+20003+115522+131186+131187+40053+97560+30016+97558+40055+115524+20004+131184+40058+131185+131182+97561+40060+115484+115485+40066+97563+40067+40095+97564+97565+115498+40096+115500+115486+131188+40094+40092+131189+40091+40093+20005+30024+40080+40084+40081+40085+40083+30025+115488+40077+40078+115489+131193+40098+131190+131191+97567+131192+30033+40101+40102+115492+97571+97572+40073+40072+97573+97574+40106+115495+115496+115497+126670+115528+131195.10.Q/all?startTime=1990-Q1&endTime=2025-Q1"

#--- SDMX DSD URL---
sdmx_dsd <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetDataStructure/CPI"

#---Read in SDMX---
dat <- readSDMX(sdmx_dat, dsd = TRUE)

dsd <- readSDMX(sdmx_dsd)

#--- Associate data and dsd ---
dat <- setDSD(dat, dsd)

#---Make dataframe
dat <- as.data.frame(dat, labels = TRUE)

#--- Select only useful cols ---
CPIdat <- dat %>% 
  select(7,9,18,19) %>% 
  rename("CPI_components" = INDEX_label.en, "Qtr" = obsTime, "Index value" = obsValue)

#---Make Qtr into date formats---
CPIdat <- CPIdat %>% 
  mutate(startqtr = yq(Qtr)) %>% 
  mutate(endqtr = ceiling_date(startqtr, "quarter")) %>% 
  mutate(date = endqtr -1) %>% 
  mutate(date = floor_date(date, "month"))

#---Make INDEX numeric ---
CPIdat <- CPIdat %>% 
  mutate(INDEX = as.numeric(INDEX))


#--- Start series from 20 years earlier---
dmax <- max(CPIdat$date)
dmin <- dmax- months(240)

CPIdatL <- CPIdat %>%
  filter(date >= dmin)

#--- Make other date objects ----
latest <- format(dmax,"%b %Y")
now <- format(today(),"%d %B %Y")


#---CPI components ---
allcpi <- CPIdatL %>% 
  mutate(order = row_number()) %>% 
  group_by(INDEX, CPI_components) %>% 
  filter(order == min(order)) %>% 
  ungroup() %>% 
  select(8,2) %>% 
  mutate(order = row_number())
  
#---Make CPI components list ---
list <- allcpi 
List <- as.list(list$CPI_components)

#---dummy df for when no CPI item selected---
INDEX <- c(0,0)
CPI_components <- c("NULL")
`Index value` <- c(NA,NA)
date <- c(dmin, dmax)
change <- c(NA, NA)
index <- c(1,2)
dfd <- data.frame(INDEX, CPI_components, `Index value`, date, change, index)

#--- ABS preferred colours---
abscol <- c("#336699", "#669966", "#99CC66", "#993366", "#CC9966", "#666666")

#----------------------------------------
#---- SHINY DASHBOARD----
#----------------------------------------
library(shiny)
library(shinydashboard)
library(shinyWidgets)

#------------
#----UI----
#------------
ui <- fluidPage(
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  headerPanel("CPI time series - weighted average of eight capital cities"),
  sidebarPanel(
    
    radioButtons ("choosetable", "Index or Percent change:",
                  choices = c("Index value   " = "index", "Percent change" = "pc"),
                  selected = c("pc"),inline = T),    
    
    sliderInput("dateRange","Date range:",
                min = as.Date(dmin),
                max = as.Date(dmax),
                value=as.Date(c(dmin, dmax)),
                step = 3,
                timeFormat="%b %Y"),
    
    tags$div( tags$hr()),
    tags$div(
      tags$h4(tags$strong("Select from CPI groups, sub-groups and expenditure classes:"))),
    
    # pickerInput ("List", " ", choices= c(List),
    #              selected = c("All groups CPI"), multiple = TRUE ),
    
    
    pickerInput ("List", " ", choices= c("All groups CPI " = "All groups CPI", 
                                         ".Food and non-alcoholic beverages " = "Food and non-alcoholic beverages", 
                                         "..Bread and cereal products " = "Bread and cereal products", 
                                         "...Bread " = "Bread", 
                                         "...Cakes and biscuits " = "Cakes and biscuits", 
                                         "...Breakfast cereals " = "Breakfast cereals", 
                                         "...Other cereal products " = "Other cereal products", 
                                         ".. Meat and seafoods " = "Meat and seafoods", 
                                         "...Beef and veal " = "Beef and veal", 
                                         "...Pork " = "Pork", 
                                         "...Lamb and goat " = "Lamb and goat", 
                                         "...Poultry " = "Poultry", 
                                         "...Other meats " = "Other meats", 
                                         "...Fish and other seafood " = "Fish and other seafood", 
                                         "..Dairy and related products " = "Dairy and related products", 
                                         "...Milk " = "Milk", 
                                         "...Cheese " = "Cheese", 
                                         "...Ice cream and other dairy products " = "Ice cream and other dairy products", 
                                         "..Fruit and vegetables " = "Fruit and vegetables", 
                                         "...Fruit " = "Fruit", 
                                         "...Vegetables " = "Vegetables", 
                                         "..Food products n.e.c. " = "Food products n.e.c.", 
                                         "...Eggs " = "Eggs", 
                                         "...Jams, honey and spreads " = "Jams, honey and spreads", 
                                         "...Food additives and condiments " = "Food additives and condiments", 
                                         "...Oils and fats " = "Oils and fats", 
                                         "...Snacks and confectionery " = "Snacks and confectionery", 
                                         "...Other food products n.e.c. " = "Other food products n.e.c.", 
                                         ".. Non-alcoholic beverages " = "Non-alcoholic beverages", 
                                         "...Coffee, tea and cocoa " = "Coffee, tea and cocoa", 
                                         "...Waters, soft drinks and juices " = "Waters, soft drinks and juices", 
                                         "..Meals out and take away foods " = "Meals out and take away foods", 
                                         "...Restaurant meals " = "Restaurant meals", 
                                         "...Take away and fast foods " = "Take away and fast foods", 
                                         "..Alcohol and tobacco " = "Alcohol and tobacco", 
                                         "..Alcoholic beverages " = "Alcoholic beverages", 
                                         "...Spirits " = "Spirits", 
                                         "...Wine " = "Wine", 
                                         "...Beer " = "Beer", 
                                         "..Tobacco " = "Tobacco", 
                                         "..Clothing and footwear " = "Clothing and footwear", 
                                         "...Garments " = "Garments", 
                                         "....Garments for men " = "Garments for men", 
                                         "....Garments for women " = "Garments for women", 
                                         "....Garments for infants and children " = "Garments for infants and children", 
                                         "...Footwear " = "Footwear", 
                                         "....Footwear for men " = "Footwear for men", 
                                         "....Footwear for women " = "Footwear for women", 
                                         "....Footwear for infants and children " = "Footwear for infants and children", 
                                         "...Accessories and clothing services " = "Accessories and clothing services", 
                                         "....Accessories " = "Accessories", 
                                         "...Cleaning, repair and hire of clothing and footwear " = "Cleaning, repair and hire of clothing and footwear", 
                                         "..Housing " = "Housing", 
                                         "...Rents " = "Rents", 
                                         "...New dwelling purchase by owner-occupiers " = "New dwelling purchase by owner-occupiers", 
                                         "...Other housing " = "Other housing", 
                                         "..Maintenance and repair of the dwelling " = "Maintenance and repair of the dwelling", 
                                         "...Property rates and charges " = "Property rates and charges", 
                                         "...Utilities " = "Utilities", 
                                         "...Water and sewerage " = "Water and sewerage", 
                                         "...Electricity " = "Electricity", 
                                         "...Gas and other household fuels " = "Gas and other household fuels", 
                                         "..Furnishings, household equipment and services " = "Furnishings, household equipment and services", 
                                         "...Furniture and furnishings " = "Furniture and furnishings", 
                                         "....Furniture " = "Furniture", 
                                         "....Carpets and other floor coverings " = "Carpets and other floor coverings", 
                                         "....Household textiles " = "Household textiles", 
                                         "...Household appliances, utensils and tools " = "Household appliances, utensils and tools", 
                                         "....Major household appliances " = "Major household appliances", 
                                         "....Small electric household appliances " = "Small electric household appliances", 
                                         "....Glassware, tableware and household utensils " = "Glassware, tableware and household utensils", 
                                         "....Tools and equipment for house and garden " = "Tools and equipment for house and garden", 
                                         "...Non-durable household products " = "Non-durable household products", 
                                         "....Cleaning and maintenance products " = "Cleaning and maintenance products", 
                                         "....Personal care products " = "Personal care products", 
                                         "....Other non-durable household products " = "Other non-durable household products", 
                                         "....Domestic and household services " = "Domestic and household services", 
                                         "...Child care " = "Child care", 
                                         "...Hairdressing and personal grooming services " = "Hairdressing and personal grooming services", 
                                         "...Other household services " = "Other household services", 
                                         "..Health " = "Health", 
                                         "...Medical products, appliances and equipment " = "Medical products, appliances and equipment", 
                                         "....Pharmaceutical products " = "Pharmaceutical products", 
                                         "....Therapeutic appliances and equipment " = "Therapeutic appliances and equipment", 
                                         "...Medical, dental and hospital services " = "Medical, dental and hospital services", 
                                         "....Medical and hospital services " = "Medical and hospital services", 
                                         "....Dental services " = "Dental services", 
                                         "..Transport " = "Transport", 
                                         "...Private motoring " = "Private motoring", 
                                         "...Motor vehicles " = "Motor vehicles", 
                                         "...Spare parts and accessories for motor vehicles " = "Spare parts and accessories for motor vehicles", 
                                         "...Automotive fuel " = "Automotive fuel", 
                                         "...Maintenance and repair of motor vehicles " = "Maintenance and repair of motor vehicles", 
                                         "...Other services in respect of motor vehicles " = "Other services in respect of motor vehicles", 
                                         "...Urban transport fares " = "Urban transport fares", 
                                         "..Communication " = "Communication", 
                                         "...Postal services " = "Postal services", 
                                         "...Telecommunication equipment and services " = "Telecommunication equipment and services", 
                                         "..Recreation and culture " = "Recreation and culture", 
                                         "...Audio, visual and computing equipment and services " = "Audio, visual and computing equipment and services", 
                                         "....Audio, visual and computing equipment " = "Audio, visual and computing equipment", 
                                         "....Audio, visual and computing media and services " = "Audio, visual and computing media and services", 
                                         "...Newspapers, books and stationery " = "Newspapers, books and stationery", 
                                         "....Books " = "Books", 
                                         "....Newspapers, magazines and stationery " = "Newspapers, magazines and stationery", 
                                         "...Holiday travel and accommodation " = "Holiday travel and accommodation", 
                                         "....Domestic holiday travel and accommodation " = "Domestic holiday travel and accommodation", 
                                         "....International holiday travel and accommodation " = "International holiday travel and accommodation", 
                                         "...Other recreation, sport and culture " = "Other recreation, sport and culture", 
                                         "....Equipment for sports, camping and open-air recreation " = "Equipment for sports, camping and open-air recreation", 
                                         "....Games, toys and hobbies " = "Games, toys and hobbies", 
                                         "...Pets and related products " = "Pets and related products", 
                                         "....Veterinary and other services for pets " = "Veterinary and other services for pets", 
                                         "...Sports participation " = "Sports participation", 
                                         "...Other recreational, sporting and cultural services " = "Other recreational, sporting and cultural services", 
                                         "..Education " = "Education", 
                                         "...Preschool and primary education " = "Preschool and primary education", 
                                         "...Secondary education " = "Secondary education", 
                                         "...Tertiary education " = "Tertiary education", 
                                         "..Insurance and financial services " = "Insurance and financial services", 
                                         "...Insurance " = "Insurance", 
                                         "...Financial services " = "Financial services"
    ),
                                         
                 selected = c("All groups CPI"), multiple = TRUE ),
    
    downloadButton("downloadTb", "Download selection:")
    
  ),
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Graph", highchartOutput("hcontainer",height = "720px")),
                tabPanel("Table", verbatimTextOutput("message"), DT::dataTableOutput("table"))
    ),
    
    
    # adding the div tag to the mainpanel
    tags$div(class="header", checked=NA,
             tags$p(paste0("Source:"),
                    tags$a(href="https://www.abs.gov.au/ausstats/abs@.nsf/mf/6401.0",
                           (paste0("Australian Bureau of Statistics,
                                   Consumer Price Index (CPI) 17th series: ", latest)))),
                            tags$p(paste0("Retrieved from"),
                                     tags$a(href="http://stat.data.abs.gov.au/",
                                            (paste0("ABS.Stat: ", now))))
             
    )))
    
    

#==========================================
# Server
#=========================================
server <- function(input, output) {  
  
  
  I <- reactive({
    list (FoodGROUP = input$List) })    
  
  df_ <- reactive({ dfd %>% 
      mutate(date = case_when(
        index == 1 ~ input$dateRange[1],
        index == 2 ~ input$dateRange[2]
        ))
    }) 
  
  
  df <- reactive({ CPIdatL %>%
      group_by(CPI_components) %>%
      arrange(date) %>%
      mutate(nxt = if_else(date< max(date), lead(date), date))%>%
      filter(nxt >= input$dateRange[1]) %>%  
      filter(date <= input$dateRange[2]) %>%  
      mutate(Date = format(date, "%b %Y")) %>% 
      filter(CPI_components %in% I()$FoodGROUP)
  
  })
  
  dfc <- reactive({ CPIdatL %>%
      group_by(CPI_components) %>%
      arrange(date) %>%
      mutate(nxt = if_else(date< max(date), lead(date), date))%>%
      filter(nxt >= input$dateRange[1]) %>%  
      filter(date <= input$dateRange[2]) %>%  
      mutate(indcum = cumsum(`Index value`)) %>%
      mutate(change = if_else(date <= input$dateRange[1],0, round(`Index value`/ min(indcum)*100-100,1))) %>% 
      mutate(Date = format(date, "%b %Y")) %>% 
      filter(CPI_components %in% I()$FoodGROUP) 
  })
  
  #--- Spread items into columns and combine into table for output tab ---
  
  tabc  <- reactive({ dfc() %>%
      group_by(CPI_components) %>%
      select(date, CPI_components, change) %>%
      spread(CPI_components, change) %>%
      rename_if(is.numeric, ~(paste0(., ", % change" ))) %>%  
      # select(sort(tidyselect::peek_vars())) %>% 
      mutate(Quarter = format(date, "%b %Y")) %>% 
      arrange(date) %>% 
      select(-date) %>% 
      select(Quarter, everything()) 
     
          })
  
  
  output$message <- renderText({ 
    
    if(is.null(I()$FoodGROUP)){
        msg <- "No CPI items selected"}
      
    
    if(!is.null(I()$FoodGROUP)){
      msg <- ""}
    
    msg
    
    }) 
  
  
  output$table = DT::renderDataTable({
    tabc()
    
  })
  
  
  output$hcontainer <- renderHighchart({
    
    
    if(is.null(I()$FoodGROUP)) {
      
      hc <- df_() %>%
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
      
        hc <- df() %>%
        hchart(.,
               type = "line",
               hcaes(x = Date,
                     y = `Index value`,
                     group = CPI_components)) %>%
        hc_xAxis(title = list(text = "Quarter")) %>%
        hc_yAxis(title = list(text = "Index")) %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("Inflation index of selected CPI groups")) %>%
        hc_colors(abscol) %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE)))
    }
    
    
    if(input$choosetable == "pc" & !is.null(I()$FoodGROUP)) {

      hc <- dfc() %>%
        hchart(.,
               type = "line",
               hcaes(x = Date,
                     y = round(change,1),
                     group = CPI_components)) %>%
        hc_xAxis(title = list(text = "Quarter")) %>%
        hc_yAxis(title = list(text = "Percent change")) %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(text = paste0("Percent inflation over selected period")) %>%
        hc_colors(abscol) %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE)))
    }
    
    
    hc
  
    })
  
  # Downloadable xlsx --
  output$downloadTb <- downloadHandler(
    filename = function() { paste("CPI group percent change", ".xlsx") },
    content = function(file) { write_xlsx(tabc(), path = file) }
  )
  
}

#========================================  
shinyApp(ui, server)
#========================================
