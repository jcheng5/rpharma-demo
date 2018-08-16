library(shiny)
library(dplyr)
library(rlang)

summarize_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("summarize_ui"))
  )
}

summarize_mod <- function(input, output, session, vars, data_expr) {
  output$summarize_ui <- renderUI({
    ns <- session$ns
    
    tagList(
      selectInput(ns("group_by"), "Group by", choices = vars(), multiple = TRUE),
      selectInput(ns("operation"), "Summary operation", c("mean", "sum", "count")),
      selectInput(ns("aggregate"), "Summary value", choices = vars(), multiple = TRUE)
    )
  })
  
  reactive({
    result_expr <- data_expr()
    if (length(input$group_by) > 0) {
      result_expr <- expr(!!result_expr %>% group_by(!!!syms(input$group_by)))
    }
    if (length(input$aggregate) > 0) {
      op <- switch(input$operation,
        mean = quote(mean),
        sum = quote(sum),
        count = quote(length)
      )
      agg_exprs <- lapply(input$aggregate, function(var) {
        col_name <- deparse(expr((!!sym(input$operation))(!!sym(var))))
        expr(!!col_name := (!!op)(!!sym(var)))
      })
      result_expr <- expr(!!result_expr %>% summarise(!!!agg_exprs))
    }
    result_expr
  })
}