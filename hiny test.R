library(shiny)
library(dplyr)


fulldf <- data.frame(
  country = c(rep("A", 6), rep("B", 6)),
  gender = rep(c("M", "F"), 6),
  age = rep(c("baby", "child", "adult"), 4),
  value = c(10, 40, 100, 8, 50, 75, 9, 39, 99, 7, 49, 74)
)

ui <- fluidPage(
  fluidRow(
    #column(4, selectInput("country", "Country", sort(unique(fulldf$country)))),
    column(4, selectInput("subgroup_var", "Subgroup variable", c("None", "gender", "age"))),
    column(4, selectInput("subgroup_value", "Subgroup value", NULL))
  ),
  tableOutput("results")
)

server <- function(input, output, session) {
  
  observeEvent(input$subgroup_var, {
    if (input$subgroup_var == "None") {
      choices <- c("")
    } else {
      choices <- fulldf %>% pull(input$subgroup_var) %>% unique()
    }
    updateSelectInput(session, "subgroup_value", choices = choices)
  })
  
  filtered_data <- reactive({
    message("calculating data")
    data <- fulldf %>% filter(country == input$country)
    if (input$subgroup_var != "None") {
      data <- data %>% filter(!!sym(input$subgroup_var) == input$subgroup_value)
    }
    data
  }) %>% bindEvent(input$subgroup_value)
  
  output$results <- renderTable({
    message("rendering table")
    filtered_data()
  })
  
}

shinyApp(ui, server)