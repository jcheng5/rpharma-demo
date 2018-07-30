library(shiny)
library(dplyr)

select_vars_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("vars_ui"))
  )
}

select_vars <- function(input, output, session, data_in) {
  ns <- session$ns
  force(data_in)
  
  output$vars_ui <- renderUI({
    #varSelectInput(ns("vars"), "Variables", data_in(), multiple = TRUE)
    checkboxGroupInput(ns("vars"), "Variables", names(data_in()), selected = names(data_in()))
  })
  
  reactive({
    input$vars
  })
}
