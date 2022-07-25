# import libraries
library(shiny)
library(dplyr)
library(leaflet)

# import data
data <- read.csv("ABNB_NYC_2019.csv")
data[is.na(data)] <- 0

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
max_reviews_per_month <- max(data$reviews_per_month, na.rm = TRUE)
max_host_listings <- max(data$calculated_host_listings_count)
max_availability <- max(data$availability_365)

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
      sliderInput("price", "Price", value = c(0, max_price), min = 0, max = max_price),
      sliderInput("min_nights", "Min. Nights", value = c(1, max_min_nights), min = 1, max = max_min_nights),
      sliderInput("host_listings", "Host Listings", value = c(1, max_host_listings), min = 1, max = max_host_listings),
      sliderInput("availability", "Availability", value = c(0, max_availability), min = 0, max = max_availability)
    ),
    column(4, 
      sliderInput("reviews", "Reviews", value = c(0, max_reviews), min = 0, max = max_reviews),
      sliderInput("reviews_per_month", "Reviews Per Month", value = c(0, max_reviews_per_month), min = 0, max = max_reviews_per_month),
      dateRangeInput("last_review", "Last Review")
    )
  ),
  fluidRow(
    column(12, 
      leafletOutput("map")
    )
  ),
  fluidRow(
    column(12, 
      dataTableOutput("data")
    )
  )
)

server <- function(input, output, session) {
  updateSelectizeInput(session, 'host', choices = host_input_choices, server = TRUE)

  observeEvent(input$borough, {
    updateSelectInput(session, "neighbourhood",
                      choices = neighbourhoods_choices[input$borough])
  }, ignoreNULL = FALSE)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat = 40.730610, lng = -73.935242, zoom = 10)
  })

  output$data <- renderDataTable(
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
      filter(if (is.null(input$availability)) TRUE else (is_between(availability_365, input$availability)))
  )
}

shinyApp(ui = ui, server = server)

