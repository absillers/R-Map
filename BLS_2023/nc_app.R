library(shiny)
library(leaflet)
library(sf)

# Load sample spatial data and transform to WGS84 (EPSG:4326)
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) |>
  st_transform(4326)

ui <- fluidPage(
  leafletOutput("map", height = "400px"),
  verbatimTextOutput("click_info")
)

server <- function(input, output, session) {
  
  # 1. Render the map with a unique layerId for each polygon
  output$map <- renderLeaflet({
    leaflet(nc) |>
      addTiles() |>
      addPolygons(
        layerId = ~NAME,          # Assign unique ID to track clicks
        label = ~NAME,            # Show name on hover
        color = "#444444",
        weight = 1,
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          color = "white",
          weight = 2,
          bringToFront = TRUE
        )
      )
  })
  
  # 2. Observe the click event (format: input$<outputId>_shape_click)
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    
    output$click_info <- renderPrint({
      list(
        clicked_polygon_id = click$id,
        latitude = click$lat,
        longitude = click$lng
      
      )

    })
  })
}

shinyApp(ui, server)
