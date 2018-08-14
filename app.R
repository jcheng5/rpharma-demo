library(shiny)
library(DT)
library(shinythemes)

source("format.R")
source("utils.R")
source("select_module.R")
source("filter_module.R")
source("bookmark_module.R")
source("summarize_module.R")

bmi <- bookmark_init("bookmarks.sqlite")

ui <- function(req) {
  tagList(
    # Bootstrap header
    tags$header(class = "navbar navbar-default navbar-static-top",
      tags$div(class = "container-fluid",
        tags$div(class = "navbar-header",
          tags$div(class = "navbar-brand", "R/Pharma demo")
        ),
        # Links for restoring/loading sessions
        tags$ul(class = "nav navbar-nav navbar-right",
          tags$li(
            bookmark_modal_load_ui("bookmark")
          ),
          tags$li(
            bookmark_modal_save_ui("bookmark")
          )
        )
      )
    ),
    fluidPage(theme = shinythemes::shinytheme("united"),
      sidebarLayout(position = "right",
        column(width = 4,
          wellPanel(
            select_vars_ui("select")
          ),
          wellPanel(
            filter_ui("filter")
          ),
          wellPanel(
            p(downloadButton("download", "Download report", class = "btn-primary")),
            tags$details(
              tags$summary("Code preview"),
              verbatimTextOutput("code")
            )
          )
        ),
        mainPanel(
          tabsetPanel(id = "tabs",
            tabPanel("Plot", tags$br(),
              plotOutput("plot", height = 600)
            ),
            tabPanel("Summary", tags$br(),
              verbatimTextOutput("summary")
            ),
            tabPanel("Table", tags$br(),
              tableOutput("table")
            )
          )
        )
      )
    )
  )
}

server <- function(input, output, session) {
  callModule(bookmark_mod, "bookmark", bmi,
    thumbnailFunc = function() { do_plot() }
  )()
  
  datasetExpr <- reactive(expr(mtcars %>% mutate(cyl = factor(cyl))))
  filterExpr <- callModule(filter_mod, "filter", datasetExpr)
  selectExpr <- callModule(select_vars, "select",
    reactive(names(eval_clean(datasetExpr()))), filterExpr)
  
  data <- reactive({
    resultExpr <- selectExpr()
    df <- eval_clean(resultExpr)
    validate(need(nrow(df) > 0, "No data matches the filter"))
    df
  })

  output$table <- renderTable({
    data()
  }, rownames = TRUE)
  
  do_plot <- function() {
    plot(data())
  }
  
  output$plot <- renderPlot({
    do_plot()
  })
  
  output$summary <- renderPrint({
    summary(data())
  })
  
  output$code <- renderText({
    format_tidy_code(selectExpr())
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
            summary(df)
            knitr::kable(df)
          }),
          packages = c("dplyr"),
          files = NULL,
          output_file = file
        )
      )
    }
  )
}

enableBookmarking("server")

shinyApp(ui, server)
