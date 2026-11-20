library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(shinyWidgets)

#source("r_shiny_data.R", local = TRUE)

r_shiny_bls_data <- readRDS('r_shiny_bls_data.rds')

occupations_major <- unique(r_shiny_bls_data$OCC_TITLE[r_shiny_bls_data$OCC_GROUP == 'major'])

data_types <- colnames(r_shiny_bls_data)[which(colnames(r_shiny_bls_data) == "TOT_EMP"):which(colnames(r_shiny_bls_data) == "A_MEDIAN")]
data_types_names <- c("Total employment", "Jobs per 1,000", "Location Quotient", "Hourly Mean (Wage)", "Annual Mean (Wage)", 
                      "Hourly Median (Wage)", "Hourly Median (Wage)")

# --- USER INTERFACE ---
ui <- fluidPage(
  titlePanel(
    title = tags$span("Occupation by MSA", style = "font-family: 'Calibri', serif; font-size: 32px;"),
    ),
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
        choices = NULL),
      
      radioGroupButtons(
        inputId = "data_value",
        label = "Choose data:",
        choices = c(setNames(unlist(data_types), unlist(data_types_names))),
        individual = TRUE, 
        selected = "TOT_EMP",
        status = "primary",
        checkIcon = list(
          yes = icon("check")
        )
      ),
      verbatimTextOutput("result"),
      width = 3),
    
    mainPanel(
      leafletOutput(outputId = "map", height = "600px")
    )
  )
)

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  
  #base layer
  base_map <- paste0(switch(
    'CartoDB', 
    'CartoDB'="https://basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png",
    'CartoDB.Positron'="https://basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png")
    ,'?key=cb1_30r9_1_53870c6197c8ba2546c66893')
  
  #base map - what is rendered 
  output$map <- renderLeaflet({
    leaflet(r_shiny_bls_data) %>%
      addTiles(
        urlTemplate = base_map,
        attribution = paste(
          '&copy; <a href="https://carto.com/attributions">CARTO</a>,',
          '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
        )) %>%
      setView(lng = -96, lat = 37.8, zoom = 4) # Centers over the US
  })
  
  #Dropdown menu values based on occupation group
  observeEvent(input$metric, {
        
    occupation_type_input <- r_shiny_bls_data %>%
          filter(OCC_TITLE == input$metric) %>%
          pull(OCC_TYPE) %>% unique()

        choices <- r_shiny_bls_data %>%
          filter(OCC_TYPE == occupation_type_input & OCC_GROUP == 'detailed') %>%
          pull(OCC_TITLE) %>%
          unique()
    
         updateSelectInput(session, "subvalue", choices = c(All = "All", choices))
        })
  
  
  #find occupation value
  ###########
  ###CHANGE tottemp to different options
  
  
  observe({
    data <- input$data_value
    
    if (input$subvalue == 'All'){
      selected_metric <- as.numeric(r_shiny_bls_data[[data]][r_shiny_bls_data$OCC_TITLE == input$metric])
      occupation <- input$metric}
    else {
      selected_metric <- as.numeric(r_shiny_bls_data[[data]][r_shiny_bls_data$OCC_TITLE == input$subvalue])
      occupation <- input$subvalue}

    #Define color palette
    pal <- colorNumeric(
      palette = "GnBu",
      #reverse = TRUE,
      domain = selected_metric
    )
    
    #Build interactive dynamic labels
    labels <-
      if (occupation == ''){
        'LABELS'
    } else {
      sprintf(
        "<strong>%s</strong><br/>%s: %s",
        r_shiny_bls_data$AREA_TITLE[r_shiny_bls_data$OCC_TITLE == occupation],
        occupation,
        round(selected_metric, 2)) %>% 
        lapply(htmltools::HTML)}
    
    
    #Update the existing map instance using proxy
    leafletProxy("map", 
        data =  r_shiny_bls_data[r_shiny_bls_data$OCC_TITLE == occupation,]) %>%
      clearShapes() %>%  # Clear older polygons before drawing new colors
      clearControls() %>% # Clear older legends
      addPolygons(
        fillColor = ~pal(selected_metric),
        weight = .5,
        opacity = 1,
        color = "gray",
        dashArray = "",
        fillOpacity = .8,
        highlightOptions = highlightOptions(
          weight = 1.3,
          opacity = 1,
          color = "gray",
          dashArray = "",
          fillOpacity = .8,
          bringToFront = TRUE
        ),
        
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      # Add legend 
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
#rsconnect::deployApp(appPrimaryDoc = "app_bls.R", appDir = "C:/Users/annas/OneDrive/R-Map/BLS_2023")
