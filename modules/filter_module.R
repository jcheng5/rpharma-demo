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

filter_mod <- function(input, output, session, data_expr) {
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
    available_fields <- names(eval_clean(data_expr())) %>% base::setdiff(names(filter_fields))

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
      data = eval_clean(data_expr())[[fieldname]],
      id = ns(id),
      fieldname = fieldname)
    
    freezeReactiveValue(input, id)
    
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
    result_expr <- data_expr()
    
    if (length(filter_fields) == 0) {
      return(result_expr)
    }
    
    # Gather up all filter expressions
    exprs <- lapply(names(filter_fields), function(name) {
      filter <- filter_fields[[name]]
      x <- as.symbol(name) #df[[name]]
      param <- input[[ filter[["inputId"]] ]]
      cond_expr <- filter[["filterExpr"]](x = x, param = param)
      if (!is.null(cond_expr)) {
        result_expr <<- expr(!!result_expr %>% filter(!!cond_expr))
      }
      invisible()
    })

    result_expr    
  })
}

createFilter <- function(data, id, fieldname) {
  UseMethod("createFilter")
}

createFilter.character <- function(data, id, fieldname) {
  list(
    ui = textInput(id, fieldname, ""),
    filterExpr = function(x, param) {
      if (!nzchar(param)) {
        NULL
      } else {
        expr(grepl(!!param, !!x, ignore.case = TRUE, fixed = TRUE))
      }
    }
  )
}

createFilter.numeric <- function(data, id, fieldname) {
  list(
    ui = sliderInput(id, fieldname, min = min(data), max = max(data),
      value = range(data)),
    filterExpr = function(x, param) {
      expr(!!x >= !!param[1] & !!x <= !!param[2])
    }
  )
}

createFilter.integer <- createFilter.numeric

createFilter.factor <- function(data, id, fieldname) {
  inputControl <- if (length(levels(data)) > 6) {
    selectInput(id, fieldname, levels(data), character(0), multiple = TRUE)
  } else {
    checkboxGroupInput(id, fieldname, levels(data))
  }
  
  list(
    ui = inputControl,
    filterExpr = function(x, param) {
      if (length(param) == 0)
        NULL
      else
        expr(!!x %in% !!param)
    }
  )
}

createFilter.POSIXt <- createFilter.numeric
