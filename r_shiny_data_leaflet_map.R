library(shiny)
library(leaflet)
library(dplyr)
library(sf)

#add API
####BASED OFF LEAFLET_MAP_PERCENT

source("r_shiny_data.R", local = TRUE)
occupations_major <- unique(r_shiny_bls_data$OCC_TITLE[r_shiny_bls_data$OCC_GROUP == 'major'])

r_shiny_bls_data <- r_shiny_bls_data %>%
  filter(YEAR == 2023)

# --- USER INTERFACE ---
ui <- fluidPage(
  titlePanel("Dynamic R Shiny Leaflet Choropleth"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "metric",
        label = "Select Occupation Group:",
        choices = c(setNames(unlist(occupations_major), unlist(occupations_major))),
        ),
      selectInput(
        inputId = "subvalue",
        label = "Select Occupation:",
        choices = NULL)),
    
    mainPanel(
      leafletOutput(outputId = "map", height = "600px")
    )
  )
)

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  # Initialize the base map layer
  url <- paste0(switch(
    'CartoDB', 
    'CartoDB'="https://basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png",
    'CartoDB.Positron'="https://basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png")
    ,'?key=cb1_30r9_1_53870c6197c8ba2546c66893')
  
  output$map <- renderLeaflet({
    leaflet(r_shiny_bls_data) %>%
      addTiles(
        urlTemplate = url,
        attribution = paste(
          '&copy; <a href="https://carto.com/attributions">CARTO</a>,',
          '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
        )) %>%
      setView(lng = -96, lat = 37.8, zoom = 4) # Centers over the US
  })
  
  #dropdown menu values - react
  observeEvent(input$metric, {
      if (input$metric == "None") {
        choices <- c("")
      } else {
        occupation_type_input <- r_shiny_bls_data %>% 
          filter(OCC_TITLE == input$metric) %>% 
          pull(OCC_TYPE) %>% unique()
        
        choices <- r_shiny_bls_data %>% 
          filter(OCC_TYPE == occupation_type_input & OCC_GROUP == 'detailed') %>% 
          pull(OCC_TITLE) %>% 
          unique()
      }
      updateSelectInput(session, "subvalue", choices = c(None = "None", choices))
        })
  
    # Select the metric column based on user input
  
  observe({

    #selected_metric <- as.numeric(r_shiny_bls_data$TOT_EMP[r_shiny_bls_data$OCC_TITLE == input$metric])
    selected_metric <- if ((input$subvalue != "None")){
      as.numeric(r_shiny_bls_data$TOT_EMP[r_shiny_bls_data$OCC_TITLE == input$subvalue])
    } else {
      as.numeric(r_shiny_bls_data$TOT_EMP[r_shiny_bls_data$OCC_TITLE == input$metric])} #this is the problem - input$subvalue

    # 1. Define color palette dynamically based on the range of the selected metric
    pal <- colorNumeric(
      palette = "viridis",
      reverse = TRUE,
      domain = selected_metric
    )

    # # 2. Build interactive dynamic labels
    labels <- sprintf(
      "<strong>%s</strong><br/>%s: %s",
      r_shiny_bls_data$AREA_TITLE[r_shiny_bls_data$OCC_TITLE == input$metric],
      input$metric,
      round(selected_metric, 2)
    ) %>% lapply(htmltools::HTML)

    
    ##create df

    # data <- if (input$subvalue == "Select occupation"){
    #   as.numeric(r_shiny_bls_data$TOT_EMP[r_shiny_bls_data$OCC_TITLE == input$metric])
    # } else {
    #   as.numeric(r_shiny_bls_data$TOT_EMP[r_shiny_bls_data$OCC_TITLE == input$subvalue])}

    # 3. Update the existing map instance using proxy
    leafletProxy("map", 
      data = r_shiny_bls_data[r_shiny_bls_data$OCC_TITLE == input$metric,]) %>%
      clearShapes() %>%  # Clear older polygons before drawing new colors
      clearControls() %>% # Clear older legends
      addPolygons(
        fillColor = ~pal(selected_metric),
        weight = .5,
        opacity = 1,
        color = "#667",
        dashArray = "",
        fillOpacity = 1,
        highlightOptions = highlightOptions(
          weight = 1.3,
          opacity = 1,
          color = "#667",
          dashArray = "",
          fillOpacity = 1,
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
      )})
}

# Run the Application 
shinyApp(ui = ui, server = server)
