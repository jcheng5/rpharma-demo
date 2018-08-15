library(shiny)
library(dplyr)
library(rlang)

ui <- fluidPage(
  selectInput("dataset", "Dataset", c("mtcars", "iris", "pressure")),
  numericInput("nrows", "Number of rows", 5),
  tableOutput("table")
)

server <- function(input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  
  subset <- reactive({
    dataset() %>% head(input$nrows)
  })

  output$table <- renderTable({
    subset()
  })
}

shinyApp(ui, server)
