library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(shinyWidgets)

#2024 data

r_shiny_bls_data <- readRDS("C:/Users/annas/OneDrive/R-Map/BLS_2023/r_shiny_bls_data.rds")

occupations_major <- unique(r_shiny_bls_data$OCC_TITLE[r_shiny_bls_data$OCC_GROUP == 'major'])

data_types <- colnames(r_shiny_bls_data)[which(colnames(r_shiny_bls_data) == "TOT_EMP"):which(colnames(r_shiny_bls_data) == "A_MEDIAN")]
data_types_names <- c("Total employment", "Jobs per 1,000", "Location Quotient", "Hourly Mean (Wage)", "Annual Mean (Wage)", 
                      "Hourly Median (Wage)", "Hourly Median (Wage)")

# --- USER INTERFACE ---
ui <- fluidPage(
  titlePanel(
    title = tags$span("Occupation by MSA", style = "font-family: 'Calibri', serif; font-size: 32px;")
    ),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "metric",
        label = "Select Occupation Group:",
        choices = c(setNames(unlist(occupations_major), unlist(occupations_major)))
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
      verbatimTextOutput("click_info"),
      width = 3),
  
    fluidRow(
      column(width = 6, leafletOutput(outputId = "map")),
      column(width = 6, leafletOutput(outputId = "chart"))
    )
    # mainPanel(
    #   leafletOutput(outputId = "map", height = "500px")
    # )
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
          #filter(YEAR == 2023) %>%
          pull(OCC_TYPE) %>% unique()

        choices <- r_shiny_bls_data %>%
          filter(OCC_TYPE == occupation_type_input & OCC_GROUP == 'detailed') %>%
          #filter(YEAR == 2023) %>%
          pull(OCC_TITLE) %>%
          unique()

         updateSelectInput(session, "subvalue", choices = c(All = "All", choices))
        })


  #find occupation value

  observe({
    data <- input$data_value
    
    if (input$subvalue == 'All'){
      
      selected_metric <- r_shiny_bls_data %>%
        filter(OCC_TITLE == input$metric) %>%
        st_drop_geometry() %>%
        select(all_of(data)) %>%
        unlist() %>%
        as.numeric()

      occupation <- input$metric}
    else {

      selected_metric <- r_shiny_bls_data %>%
        filter(OCC_TITLE == input$subvalue) %>%
        st_drop_geometry() %>%
        select(all_of(data)) %>%
        unlist() %>%
        as.numeric()

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
        data =  r_shiny_bls_data %>%
          filter(OCC_TITLE == occupation)) %>%
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
        layerId = ~GEOID,
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

  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    clicked_polygon_geoid = click$id
    # output$click_info <- renderPrint({
    #   list(
    #     clicked_polygon_geoid = click$id,
    #   )
    # 
    # })
    })
  
  # 
  # output$chart <- renderPlotly({
  # 
  #   r_shiny_bls_data <- r_shiny_bls_data %>%
  #     filter(GEOID == clicked_polygon_geoid) %>%
  #     filter(grepl("11-", OCC_CODE))
  # 
  #   plot_ly(data = r_shiny_bls_data_ab_tx %>% filter(OCC_TITLE != 'Management'), labels = ~OCC_TITLE, values = ~TOT_EMP, type = 'pie',
  #           insidetextorientation = 'radial')
  # 
  # })
  

 }

# Run the Application
shinyApp(ui = ui, server = server)
#rsconnect::deployApp(appPrimaryDoc = "app_bls.R", appDir = "C:/Users/annas/OneDrive/R-Map/BLS_2023")
