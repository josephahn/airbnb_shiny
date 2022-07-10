# import libraries
library(shiny)
library(dplyr)

# import data
data <- read.csv("nyc_airbnb/AB_NYC_2019.csv")

# hosts
hosts <- data %>%
  group_by(host_id) %>%
  mutate(host_id_name = paste(host_id, host_name)) %>%
  select(host_id, host_id_name)

# https://stackoverflow.com/questions/33105044/make-list-for-shiny-dropdown-selectinput
host_input_choices <- as.list(hosts$host_id)
names(host_input_choices) <- hosts$host_id_name

boroughs <- data %>%
  distinct(neighbourhood_group) %>%
  arrange(neighbourhood_group)
# https://stackoverflow.com/questions/10496992/how-to-add-row-on-top-of-data-frame-r
boroughs <- rbind(c(""), boroughs)

neighbourhoods <- data %>%
  distinct(neighbourhood) %>%
  arrange(neighbourhood)
neighbourhoods <- rbind(c(""), neighbourhoods)

room_types <- data %>%
  distinct(room_type) %>%
  arrange(room_type)
room_types <- rbind(c(""), room_types)

ui <- fluidPage(
  # TODO: improve performance https://shiny.rstudio.com/reference/shiny/latest/selectInput.html
  # selectInput(inputId = "host",
  #             label = "Host",
  #             choices = host_input_choices),
  selectInput(inputId = "borough",
              label = "Borough",
              choices = boroughs$neighbourhood_group,
              multiple = TRUE),
  selectInput(inputId = "neighbourhood",
              label = "Neighbourhood",
              choices = neighbourhoods$neighbourhood,
              multiple = TRUE),
  selectInput(inputId = "room_type",
              label = "Room Type",
              choices = room_types$room_type,
              multiple = TRUE),
  dataTableOutput("data")
)

server <- function(input, output) {
  # filter data by ALL inputs
  
  output$data <- renderDataTable(
    data %>%
      # filter(host_id == input$host) %>%
      filter(neighbourhood_group %in% input$borough) %>%
      filter(neighbourhood %in% input$neighbourhood) %>%
      filter(room_type %in% input$room_type)
  )
}

shinyApp(ui = ui, server = server)
