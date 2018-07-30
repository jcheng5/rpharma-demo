library(shiny)
library(DBI)
library(RSQLite)
library(pool)
library(dplyr)
library(lubridate)

bookmark_save_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    textInput(ns("save_name"), "Session name"),
    actionButton(ns("save"), "Save")
  )
}

bookmark_load_ui <- function(id) {
  ns <- NS(id)
  
  DTOutput(ns("saved_sessions"))
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

bookmark_mod <- function(input, output, session, instance) {
  output$saved_sessions <- renderDT({
    instance$reader() %>%
      select(link, author, timestamp) %>%
      datatable(escape = FALSE)
  })
  
  shiny::setBookmarkExclude(c("save_name", "save"))
  
  observeEvent(input$save, {
    tryCatch(
      {
        if (!isTruthy(input$save_name)) {
          stop("Please specify a bookmark name")
        } else {
          session$doBookmark()
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
      updateQueryString(url)
      
      df <- data.frame(
        timestamp = Sys.time(),
        url = url,
        label = input$save_name,
        author = if (!is.null(session$user))
          session$user
        else
          paste("Anonymous @", session$request$REMOTE_ADDR),
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
