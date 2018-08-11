library(shiny)
library(dplyr)
library(rlang)

select_vars_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("vars_ui"))
  )
}

select_vars <- function(input, output, session, vars, data_expr) {
  ns <- session$ns

  output$vars_ui <- renderUI({
    freezeReactiveValue(input, "vars")
    selectInput(ns("vars"), "Variables to display", vars(), multiple = TRUE)
    #checkboxGroupInput(ns("vars"), "Variables", names(data), selected = names(data))
  })
  
  reactive({
    if (length(input$vars) == 0) {
      data_expr()
    } else {
      expr(!!data_expr() %>% select(!!!syms(input$vars)))
    }
  })
}
