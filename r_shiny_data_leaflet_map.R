library(shiny)
library(leaflet)
library(dplyr)
library(sf)

#add API
####BASED OFF LEAFLET_MAP_PERCENT
###RUN THE r_shiny_data 

#msa_employment_2025 <- read.csv("C:/Users/annas/OneDrive/Github/plotly_bls_map/plotly_msa_employment.csv")
#my_shapefile <- st_read("C:/Users/annas/OneDrive/Github/plotly_bls_map/cbsa_shape/cb_2025_us_cbsa_500k.shp")

occupations <- unique(r_shiny_bls_data$OCC_TITLE[r_shiny_bls_data$OCC_GROUP == 'major'],)

# --- USER INTERFACE ---
ui <- fluidPage(
  titlePanel("Dynamic R Shiny Leaflet Choropleth"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "metric",
        label = "Select Variable to Display:",
        choices = setNames(unlist(occupations), unlist(occupations)),
        #choices = c("Population Density" = "Community and Social Service Occupations (21-0000)" , "Poverty Rate (%)" = "Healthcare Support Occupations (31-0000)")
      )
    ),
    
    mainPanel(
      leafletOutput(outputId = "map", height = "600px")
    )
  )
)

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  
  url <- paste0(switch(
    'CartoDB', 
    'CartoDB'="https://basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png",
    'CartoDB.Positron'="https://basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png")
    ,'?key=cb1_30r9_1_53870c6197c8ba2546c66893')
  
  # Initialize the base map layer
  output$map <- renderLeaflet({
    
    leaflet(msa_employment_2025) %>%
      addTiles(
        urlTemplate = url,
        attribution = paste(
          '&copy; <a href="https://carto.com/attributions">CARTO</a>,',
          '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
        )) %>%
      setView(lng = -96, lat = 37.8, zoom = 4) # Centers over the US
  })
  
  # DYNAMIC reactive updates via leafletProxy
  observe({
    # Select the metric column based on user input
    selected_metric <- msa_employment_2025$Percent[msa_employment_2025$Occupation == input$metric]
    
    # 1. Define color palette dynamically based on the range of the selected metric
    pal <- colorNumeric(
      palette = "YlOrRd", 
      domain = selected_metric
    )
    
    # 2. Build interactive dynamic labels
    labels <- sprintf(
      "<strong>%s</strong><br/>%s: %s",
      msa_employment_2025$Area[msa_employment_2025$Occupation == input$metric], 
      input$metric, 
      round(selected_metric, 2)
    ) %>% lapply(htmltools::HTML)
    
    # 3. Update the existing map instance using proxy
    leafletProxy("map", data = msa_employment_2025[msa_employment_2025$Occupation == input$metric,]) %>%
      clearShapes() %>%  # Clear older polygons before drawing new colors
      clearControls() %>% # Clear older legends
      addPolygons(
        fillColor = ~pal(selected_metric),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      # Add dynamic legend matching the current distribution
      addLegend(
        pal = pal, 
        values = ~selected_metric, 
        opacity = 0.7, 
        title = input$metric,
        position = "bottomright"
      )
  })
}

# Run the Application 
shinyApp(ui = ui, server = server)
