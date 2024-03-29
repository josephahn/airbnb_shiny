# import libraries
library(shiny)
library(shinyWidgets)
library(dplyr)
library(leaflet)
library(rgdal)

# import data
data <- read.csv("ABNB_NYC_2019.csv")
data[is.na(data)] <- 0
topoData <- readLines("nyc_neighbourhoods.json") %>%
  paste(collapse = "\n") %>%
  fromJSON(simplifyVector = FALSE)
nyc_neighbourhoods <- rgdal::readOGR("nyc_neighbourhoods.json") 

# hosts
hosts <- data %>%
  group_by(host_id) %>%
  mutate(host_id_name = paste(host_id, host_name)) %>%
  select(host_id, host_id_name)

# https://stackoverflow.com/questions/33105044/make-list-for-shiny-dropdown-selectinput
host_input_choices <- as.list(hosts$host_id)
names(host_input_choices) <- hosts$host_id_name
host_input_choices <- c("", host_input_choices)

boroughs <- data %>%
  distinct(neighbourhood_group) %>%
  arrange(neighbourhood_group)
# https://stackoverflow.com/questions/10496992/how-to-add-row-on-top-of-data-frame-r
boroughs <- rbind(c(""), boroughs)

neighbourhoods <- data %>%
  group_by(neighbourhood_group) %>%
  distinct(neighbourhood) %>%
  select(neighbourhood_group, neighbourhood)

bronx_neighbourhoods <- neighbourhoods %>%
  filter(neighbourhood_group == 'Bronx') %>%
  arrange(neighbourhood) %>%
  select(neighbourhood_group, neighbourhood)

brooklyn_neighbourhoods <- neighbourhoods %>%
  filter(neighbourhood_group == 'Brooklyn') %>%
  arrange(neighbourhood) %>%
  select(neighbourhood_group, neighbourhood)

manhattan_neighbourhoods <- neighbourhoods %>%
  filter(neighbourhood_group == 'Manhattan') %>%
  arrange(neighbourhood) %>%
  select(neighbourhood_group, neighbourhood)

queens_neighbourhoods <- neighbourhoods %>%
  filter(neighbourhood_group == 'Queens') %>%
  arrange(neighbourhood) %>%
  select(neighbourhood_group, neighbourhood)

staten_island_neighbourhoods <- neighbourhoods %>%
  filter(neighbourhood_group == 'Staten Island') %>%
  arrange(neighbourhood) %>%
  select(neighbourhood_group, neighbourhood)

neighbourhoods_choices <- setNames(list(
  as.list(bronx_neighbourhoods$neighbourhood),
  as.list(brooklyn_neighbourhoods$neighbourhood),
  as.list(manhattan_neighbourhoods$neighbourhood),
  as.list(queens_neighbourhoods$neighbourhood),
  as.list(staten_island_neighbourhoods$neighbourhood)
), c('Bronx', 'Brooklyn', 'Manhattan', 'Queens', 'Staten Island'))

room_types <- data %>%
  distinct(room_type) %>%
  arrange(room_type)
room_types <- rbind(c(""), room_types)

max_price <- max(data$price)
max_min_nights <- max(data$minimum_nights)
max_reviews <- max(data$number_of_reviews)
max_reviews_per_month <- max(data$reviews_per_month)
max_host_listings <- max(data$calculated_host_listings_count)
max_availability <- max(data$availability_365)
min_last_review <- min(data[data$last_review != "", ]$last_review)
max_last_review <- max(data[data$last_review != "", ]$last_review)

is_between <- function(number, range) {
  ifelse(is.na(number), TRUE, number >= min(range) & number <= max(range)  )
}

ui <- fluidPage(
  fluidRow(
    column(4, 
      selectizeInput(inputId = "host",
                    label = "Host",
                    choices = NULL,
                    multiple = TRUE),
      selectInput(inputId = "borough",
                 label = "Borough",
                 choices = boroughs$neighbourhood_group,
                 multiple = TRUE),
      selectInput(inputId = "neighbourhood",
                 label = "Neighbourhood",
                 choices = neighbourhoods_choices,
                 multiple = TRUE),
      selectInput(inputId = "room_type",
                 label = "Room Type",
                 choices = room_types$room_type,
                 multiple = TRUE)
    ),
    column(4, 
      numericRangeInput("price", "Price", value = c(0, max_price), min = 0, max = max_price),
      numericRangeInput("min_nights", "Min. Nights", value = c(1, max_min_nights), min = 1, max = max_min_nights),
      numericRangeInput("host_listings", "Host Listings", value = c(1, max_host_listings), min = 1, max = max_host_listings),
      numericRangeInput("availability", "Availability", value = c(0, max_availability), min = 0, max = max_availability)
    ),
    column(4, 
      numericRangeInput("reviews", "Reviews", value = c(0, max_reviews), min = 0, max = max_reviews),
      numericRangeInput("reviews_per_month", "Reviews Per Month", value = c(0, max_reviews_per_month), min = 0, max = max_reviews_per_month),
      dateRangeInput("last_review", "Last Review", start = min_last_review, end = max_last_review),
      actionButton("filter", "Filter")
    )
  ),
  fluidRow(
    column(12, 
      leafletOutput("map")
    )
  ),
  fluidRow(
    column(4, 
      plotOutput("priceHistogram")
    ),
    column(4, 
      plotOutput("reviewsHistogram")
    ),
    column(4, 
      plotOutput("roomBarchart")
    )
  ),
  fluidRow(
    column(3, 
      plotOutput("boroughBarchart")
    ),
    column(9, 
      plotOutput("neighbourhoodBarchart")
    )
  ),
  fluidRow(
    column(12, 
      dataTableOutput("data")
    )
  )
)

server <- function(input, output, session) {
  # https://stackoverflow.com/questions/40811903/in-shiny-update-datatable-with-new-values-from-user-input
  df <- eventReactive(input$filter, {
    data %>%
      filter(if (is.null(input$host)) TRUE else (host_id %in% input$host)) %>%
      filter(if (is.null(input$borough)) TRUE else (neighbourhood_group %in% input$borough)) %>%
      filter(if (is.null(input$neighbourhood)) TRUE else (neighbourhood %in% input$neighbourhood)) %>%
      filter(if (is.null(input$room_type)) TRUE else (room_type %in% input$room_type)) %>%
      filter(if (is.null(input$price)) TRUE else (is_between(price, input$price))) %>%
      filter(if (is.null(input$min_nights)) TRUE else (is_between(minimum_nights, input$min_nights))) %>%
      filter(if (is.null(input$reviews)) TRUE else (is_between(number_of_reviews, input$reviews))) %>%
      filter(if (is.null(input$reviews_per_month)) TRUE else (is_between(reviews_per_month, input$reviews_per_month))) %>%
      filter(if (is.null(input$host_listings)) TRUE else (is_between(calculated_host_listings_count, input$host_listings))) %>%
      filter(if (is.null(input$availability)) TRUE else (is_between(availability_365, input$availability))) %>%
      filter(if (is.null(input$last_review)) TRUE else (is_between(last_review, input$last_review)))
  })

  updateSelectizeInput(session, 'host', choices = host_input_choices, server = TRUE)

  observeEvent(input$borough, {
    updateSelectInput(session, "neighbourhood",
                      choices = neighbourhoods_choices[input$borough])
  }, ignoreNULL = FALSE)
  
  # https://stackoverflow.com/questions/28393310/how-to-prevent-leaflet-map-from-resetting-zoom-in-shiny-app
  observeEvent(input$filter, {
    new_data <- df()
    # https://www.drdataking.com/post/how-to-add-multiple-lines-label-on-a-leaflet-map/
    labels <- sprintf(
      "Host: %s (%s)<br/>Borough: %s<br/>Neighbourhood: %s<br/>Room Type: %s<br/>Price: $%s",
      new_data$host_name,
      new_data$host_id,
      new_data$neighbourhood_group,
      new_data$neighbourhood,
      new_data$room_type,
      new_data$price
      ) %>% lapply(htmltools::HTML)

    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = new_data,
        lng = ~longitude,
        lat = ~latitude,
        radius = 1,
        label = ~labels
      )
  })
  
  output$map <- renderLeaflet({
    leaflet(nyc_neighbourhoods, options = leafletOptions(preferCanvas = TRUE)) %>%
      # https://community.rstudio.com/t/plotting-thousands-of-points-in-leaflet-a-way-to-improve-the-speed/8196/3
      addProviderTiles(providers$Esri.WorldGrayCanvas, options = providerTileOptions(
        updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
        updateWhenIdle = FALSE           # map won't load new tiles when panning
      )) %>%
      setView(lat = 40.730610, lng = -73.935242, zoom = 10) %>%
      addPolygons(label = ~neighborhood, color = "ff7800", weight = 2)
  })
  
  output$priceHistogram <- renderPlot({
    hist(df()$price, main = "Price", xlab = "Price")
  })
  
  output$reviewsHistogram <- renderPlot({
    hist(df()$number_of_reviews, main = "Reviews", xlab = "Reviews")
  })
  
  output$roomBarchart <- renderPlot({
    barplot(table(df()$room_type), main = "Room Type", xlab = "Count", ylab = "Room Type", horiz = TRUE)
  })
  
  output$boroughBarchart <- renderPlot({
    barplot(sort(table(df()$neighbourhood_group), decreasing = TRUE),
            main = "Borough",
            ylab = "Count",
            las = 3)
  })
  
  output$neighbourhoodBarchart <- renderPlot({
    barplot(sort(table(df()$neighbourhood), decreasing = TRUE),
            main = "Neighbourhood",
            ylab = "Count",
            las = 3)
  })
  
  output$data <- renderDataTable(df())
}

shinyApp(ui = ui, server = server)

