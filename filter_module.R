library(shiny)
library(dplyr)
library(rlang)

filter_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(id = ns("filter_container")),
    actionButton(ns("show_filter_dialog_btn"), "Add filter")
  )
}

filter_mod <- function(input, output, session, data_in) {
  ns <- session$ns
  
  setBookmarkExclude(c("show_filter_dialog_btn", "add_filter_btn"))
  
  filter_fields <- list()
  makeReactiveBinding("filter_fields")
  
  onBookmark(function(state) {
    state$values$filter_field_names <- names(filter_fields)
  })
  
  onRestore(function(state) {
    filter_field_names <- state$values$filter_field_names
    for (fieldname in filter_field_names) {
      addFilter(fieldname)
    }
  })
  
  observeEvent(input$show_filter_dialog_btn, {
    available_fields <- names(data_in()) %>% base::setdiff(names(filter_fields))

    showModal(modalDialog(
      title = "Add filter",
      
      radioButtons(ns("filter_field"), "Field to filter",
        available_fields),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("add_filter_btn"), "Add filter")
      )
    ))
  })
  
  observeEvent(input$add_filter_btn, {
    addFilter(input$filter_field)
    removeModal()
  })
  
  addFilter <- function(fieldname) {
    id <- paste0("filter__", fieldname)
    
    filter <- createFilter(
      data = data_in()[[fieldname]],
      id = ns(id),
      fieldname = fieldname)
    
    insertUI(
      paste0("#", ns("filter_container")),
      "beforeEnd",
      # TODO: escape special characters in fieldname
      filter$ui
    )

    filter$inputId <- id
    filter_fields[[fieldname]] <<- filter
  }
  
  reactive({
    df <- data_in()
    idx <- rep_len(TRUE, nrow(df))
    for (name in names(filter_fields)) {
      filter <- filter_fields[[name]]
      x <- df[[name]]
      param <- input[[ filter[["inputId"]] ]]
      idx <- idx & eval(filter[["filterExpr"]], list(x = x, param = param))
    }
    idx
  })
}

createFilter <- function(data, id, fieldname) {
  UseMethod("createFilter")
}

createFilter.character <- function(data, id, fieldname) {
  list(
    ui = textInput(id, fieldname, ""),
    filterExpr = expr(if (!nzchar(param)) rep_len(TRUE, length(x)) else grepl(param, x, ignore.case = TRUE, fixed = TRUE))
  )
}

createFilter.numeric <- function(data, id, fieldname) {
  list(
    ui = sliderInput(id, fieldname, min = min(data), max = max(data),
      value = range(data)),
    filterExpr = expr(x >= param[1] & x <= param[2])
  )
}

createFilter.integer <- createFilter.numeric

createFilter.factor <- function(data, id, fieldname) {
  list(
    ui = selectInput(id, fieldname, levels(data), character(0), multiple = TRUE),
    filterExpr = expr(x %in% param)
  )
}
