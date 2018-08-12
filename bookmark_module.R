library(shiny)
library(DBI)
library(RSQLite)
library(pool)
library(dplyr)
library(lubridate)
library(shinyIncubator) # devtools::install_github("rstudio/shiny-incubator")

bookmark_modal_save_ui <- function(id) {
  ns <- NS(id)

  tagList(
    actionLink(ns("show_save_modal"), "Save session")
  )
}

bookmark_modal_load_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionLink(ns("show_load_modal"), "Restore session")
  )
}

bookmark_load_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("saved_sessions"))
}

bookmark_init <- function(filepath = "bookmarks.sqlite") {
  bookmark_pool <- local({
    pool <- dbPool(SQLite(), dbname = filepath)
    onStop(function() {
      poolClose(pool)
    })
    pool
  })
  
  bookmarks <- reactivePoll(1000, NULL,
    function() {
      file.info("bookmarks.sqlite")$mtime
    },
    function() {
      bookmark_pool %>% tbl("bookmarks") %>%
        arrange(desc(timestamp)) %>%
        collect() %>%
        mutate(
          timestamp = friendly_time(as.POSIXct(timestamp, origin = "1970-01-01")),
          link = sprintf("<a href=\"%s\">%s</a>",
            htmltools::htmlEscape(url, TRUE),
            htmltools::htmlEscape(label, TRUE))
        )
    }
  )
  
  list(
    pool = bookmark_pool,
    reader = bookmarks
  )
}

bookmark_mod <- function(input, output, session, instance, thumbnailFunc) {
  
  output$saved_sessions <- renderUI({
    fluidRow(
      instance$reader() %>%
        select(url, label, author, timestamp, thumbnail) %>%
        rowwise() %>%
        do(ui = with(., {
          tags$div(class = "col-md-4",
            tags$div(class = "thumbnail",
              if (!is.null(thumbnail) && isTRUE(!is.na(thumbnail))) {
                tags$a(href = url, tags$img(src = thumbnail))
              },
              tags$div(class = "caption",
                tags$h4(tags$a(href = url, label)),
                tags$p(
                  author,
                  tags$br(),
                  tags$small(timestamp)
                )
              )
            )
          )
        })) %>%
        pull(ui)
    )
  })
  
  shiny::setBookmarkExclude(c("show_save_modal", "show_load_modal", "save_name", "save"))
  
  observeEvent(input$show_load_modal, {
    showModal(modalDialog(size = "l", title = "Restore session",
      tags$style(".modal-body { max-height: 600px; overflow-y: scroll; }"),
      uiOutput(session$ns("saved_sessions"))
    ))
  })
  
  observeEvent(input$show_save_modal, {
    showModal(modalDialog(
      textInput(session$ns("save_name"), "Give this session a name"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(session$ns("save"), "Save", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$save, ignoreInit = TRUE, {
    tryCatch(
      {
        if (!isTruthy(input$save_name)) {
          stop("Please specify a bookmark name")
        } else {
          removeModal()
          session$doBookmark()
          showNotification(
            "Session successfully saved"
          )
        }
      },
      error = function(e) {
        showNotification(
          conditionMessage(e),
          type = "error"
        )
      }
    )
  })
  
  function() {
    onBookmarked(function(url) {
      url <- sub("^[^?]+", "", url, perl = TRUE)
      updateQueryString(url)
      
      thumbnail <- if (!is.null(thumbnailFunc)) {
        pngfile <- plotPNG(function() {
          try(thumbnailFunc(), silent = TRUE)
        }, height = 300)
        on.exit(unlink(pngfile), add = TRUE)
        base64enc::dataURI(mime = "image/png", file = pngfile)
      } else {
        NA_character_
      }
      
      df <- data.frame(
        timestamp = Sys.time(),
        url = url,
        label = input$save_name,
        author = if (!is.null(session$user))
          session$user
        else
          paste("Anonymous @", session$request$REMOTE_ADDR),
        thumbnail = thumbnail,
        stringsAsFactors = FALSE
      )
      
      # db <- dbConnect(SQLite(), "bookmarks.sqlite")
      # on.exit(dbDisconnect(db), add = TRUE)
      dbWriteTable(instance$pool, "bookmarks", df, append = TRUE)
    })
  }
}



### Utility functions ==============

friendly_time <- function(t) {
  t <- round_date(t, "seconds")
  now <- round_date(Sys.time(), "seconds")

  abs_day_diff <- abs(day(now) - day(t))
  age <- now - t
  
  abs_age <- abs(age)
  future <- age != abs_age
  dir <- ifelse(future, "from now", "ago")
  
  
  format_rel <- function(singular, plural = paste0(singular, "s")) {
    x <- as.integer(round(time_length(abs_age, singular)))
    sprintf("%d %s %s",
      x,
      ifelse(x == 1, singular, plural),
      dir
    )
  }
  
  ifelse(abs_age == seconds(0), "Now",
    ifelse(abs_age < minutes(1), format_rel("second"),
      ifelse(abs_age < hours(1), format_rel("minute"),
        ifelse(abs_age < hours(6), format_rel("hour"),
          # Less than 24 hours, and during the same calendar day
          ifelse(abs_age < days(1) & abs_day_diff == 0, strftime(t, "%I:%M:%S %p"),
            ifelse(abs_age < days(3), strftime(t, "%a %I:%M:%S %p"),
              strftime(t, "%Y/%m/%d %I:%M:%S %p")
            ))))))
}
