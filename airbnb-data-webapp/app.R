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
           pickerInput("NeighbourhoodGroup",label="Location",
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
        div(
           checkboxInput(inputId="onlyreviewed",
                         "Show only reviewed listings",
                         value = F
           ),
         style="position:relative; top:20px;"
         )
    )
  ),
  fluidRow(
    div(
    withSpinner(leafletOutput(outputId = "map",height=600),color="#0dc5c1"),
    style="position:relative; top:20px;"
    )
  )
)

server <- function(input,output){
  output$map <- renderLeaflet({
    current_subset <- ab.data
    if (!is.null(input$NeighbourhoodGroup)){
      current_subset <- filter(current_subset,
                               neighbourhood_group %in% input$NeighbourhoodGroup)
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
                 label = ~labs,
                 clusterOptions = markerClusterOptions())
  })
}

shinyApp(ui=ui,server=server)
