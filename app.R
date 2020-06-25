library(shiny)
library(dplyr)
library(leaflet)
library(shinyWidgets)
library(shinycssloaders)
library(htmltools)


ab.data <- read.csv("cleaned_abdata.csv",header=T,stringsAsFactors = F)

ab.data$labs <- lapply(ab.data$labs,htmltools::HTML)
neighbourhood.groups <- unique(ab.data$neighbourhood_group)
room.types <- unique(ab.data$room_type)


ui <- fluidPage(
  titlePanel("AirBnB Explorer"),
  fluidRow(
    column(4,
           pickerInput("NeighbourhoodGroup",label="Neighbourhood Group",
                       choices = neighbourhood.groups,
                       options = list("actions-box" = TRUE),
                       multiple=T)
           
    ),
    column(4,
           pickerInput("RoomType",label="Room Type",
                       choices = room.types,
                       options = list("actions-box" = T),
                       multiple=T)
           
    ),
    column(4,
           checkboxInput(inputId="onlyreviewed",
                         "Show only reviewed listings",
                         value = F
           )
    )
  ),
  fluidRow(
    withSpinner(leafletOutput(outputId = "map"),color="#0dc5c1")
  )
)

server <- function(input,output){
  output$map <- renderLeaflet({
    current_subset <- ab.data
    if (!is.null(input$NeighbourhoodGroup)){
      current_subset <- filter(current_subset,
                               neighbourhood_group %in% input$NG)
    }
    if (!is.null(input$RoomType)){
      current_subset <- filter(current_subset,
                               room_type %in% input$RoomType)
    }
    
    if (input$onlyreviewed){
      current_subset <- filter(current_subset,
                               received_review == "Yes")
    }
    
    leaflet(current_subset) %>%
      addTiles() %>%
      addMarkers(lat=~latitude,lng=~longitude,
                 label = current_subset$labs,
                 clusterOptions = markerClusterOptions())
  })
}

shinyApp(ui=ui,server=server)