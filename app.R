library(shiny)
library(DT)

source("utils.R", local = environment())
source("select_module.R", local = environment())
source("filter_module.R", local = environment())
source("bookmark_module.R", local = environment())
bmi <- bookmark_init("bookmarks.sqlite")

ui <- function(req) {
  navbarPage(
    "Demo",
    tabPanel("Foo",
      fluidRow(
        column(9,
          tableOutput("table")
        ),
        column(3,
          wellPanel(
            select_vars_ui("select")
          ),
          wellPanel(
            filter_ui("filter")
          )
        )
      )
    ),
    tabPanel("Bar",
      numericInput("x2", "x2", 2),
      textOutput("out2")
    ),
    tabPanel("Save/Load",
      fluidRow(
        column(8,
          h3("Load"),
          bookmark_load_ui("bookmark")
        ),
        column(4,
          h3("Save"),
          bookmark_save_ui("bookmark")
        )
      )
    )
  )
}

server <- function(input, output, session) {
  dataset <- reactive(mtcars %>% mutate(cyl = factor(cyl)))
  filterExprs <- callModule(filter_mod, "filter", dataset, data_symbol = quote(data))
  selectExpr <- callModule(select_vars, "select", dataset)
  
  output$table <- renderTable({
    fullExpr <- expr_pipeline(quote(data), filterExprs(), selectExpr())
    print(fullExpr)
    eval(fullExpr, list(data = dataset(), .GlobalEnv))
  }, rownames = TRUE)
  
  output$out2 <- renderText({
    input$x2
  })
  
  callModule(bookmark_mod, "bookmark", bmi)()
}

enableBookmarking("server")

shinyApp(ui, server)
