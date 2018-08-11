library(shiny)
library(DT)

source("format.R", local = environment())
source("utils.R", local = environment())
source("select_module.R", local = environment())
source("filter_module.R", local = environment())
source("bookmark_module.R", local = environment())
source("summarize_module.R", local = environment())

bmi <- bookmark_init("bookmarks.sqlite")

ui <- function(req) {
  navbarPage(
    "Demo",
    tabPanel("Foo",
      fluidRow(
        column(9,
          plotOutput("plot"),
          tableOutput("table")
        ),
        column(3,
          wellPanel(
            select_vars_ui("select")
          ),
          wellPanel(
            filter_ui("filter")
          ),
          wellPanel(
            downloadButton("download", "Download report")
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
  datasetExpr <- reactive(expr(mtcars %>% mutate(cyl = factor(cyl))))
  filterExpr <- callModule(filter_mod, "filter", datasetExpr)
  selectExpr <- callModule(select_vars, "select",
    reactive(names(eval_clean(datasetExpr()))), filterExpr)
  
  data <- reactive({
    resultExpr <- selectExpr()
    print(resultExpr)
    eval_clean(resultExpr)
  })

  output$table <- renderTable({
    data()
  }, rownames = TRUE)
  
  output$plot <- renderPlot({
    plot(data())
  })
  
  output$out2 <- renderText({
    input$x2
  })
  
  output$download <- downloadHandler(
    filename = "report.zip",
    content = function(file) {
      withProgress(message = "Compiling report...", value = NULL,
        make_report_bundle(title = "My great report",
          author = "Joe Cheng",
          description = "This is a high quality report!",
          body_expr = expr({
            df <- !!selectExpr()
            plot(df)
            knitr::kable(df)
          }),
          packages = c("dplyr"),
          files = NULL,
          output_file = file
        )
      )
    }
  )
  
  callModule(bookmark_mod, "bookmark", bmi)()
}

enableBookmarking("server")

shinyApp(ui, server)
