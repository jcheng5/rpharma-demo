library(sourcetools)
library(dplyr)

str <- "mtcars %>% mutate(cyl = factor(cyl)) %>% filter(mpg >= 19.8 & \n    mpg <= 31.4) %>% filter(cyl %in% c(\"4\", \"6\")) %>% select(mpg, \n    cyl, disp) %>% {\nfoo\nbar\n}\nfoo()\nbar()"

rebreak <- function(str) {
  tokens <- tokenize_string(str)
  tokens$value <- paste0(tokens$value, ifelse(tokens$type == "operator" & tokens$value == "%>%", "\n", ""))
  operator_newline <- grepl("\n", tokens$value) & tokens$type == "whitespace" & c(FALSE, head(tokens$type %in% c("comma", "operator"), -1))
  tokens$value[operator_newline] <- " "
  new_str <- paste(tokens$value, collapse = "")
  gsub("\\s*\\r?\\n\\s*", "\n", new_str)
}

str <- rebreak(str)

reindent <- function(str) {
  tokens <- tokenize_string(str)
  tokens <- tokens %>% mutate(
    type = ifelse(type != "bracket", type,
      ifelse(value %in% c("(", "{", "["), "bracket-open",
        "bracket-close")
    )
  )
  
  # indent_level <- tokens %>% group_by(row) %>% summarize(
  #   bracket_count = sum(ifelse(type == "bracket-open", 1, ifelse(type == "bracket-close", -1, 0)))
  # )
  # indent_level$bracket_count <- cumsum(c(0, head(indent_level$bracket_count, -1)))
  
  continuation <- tokens %>%
    group_by(row) %>%
    summarize(is_continuation = identical(tail(type[type != "whitespace"], 1), "operator"))
  continuation$is_continuation <- c(FALSE, head(continuation$is_continuation, -1))
  
  # tokens %>%
  #   inner_join(continuation, by = "row") %>%
  #   group_by(row) %>% summarize(
  #     line = {
  #       str <- paste0(rep_len("  ", max(0, bracket_count[[1]]) + as.integer(is_continuation[[1]])), paste(value, collapse = ""))
  #       str
  #     }
  #   ) %>%
  #   pull("line") %>% 
  #   paste(collapse = "")
  
  bracket_stack <- shiny:::Stack$new()
  bracket_stack$push(list(row = 0, level = 0))
  
  indent_level <- tokens %>%
    #inner_join(indent_level, by = "row") %>%
    inner_join(continuation, by = "row") %>%
    group_by(row) %>%
    summarise(
      indent = {
        start_level <- bracket_stack$peek()$level + as.integer(is_continuation[[1]])
        cat("\n")
        for (i in seq_len(length(type))) {
          switch(type[[i]],
            "bracket-open" = {
              cat("+")
              cur <- bracket_stack$peek()
              level <- if (cur$row == row[[1]]) {
                cur$level
              } else {
                start_level + 1L
              }
              bracket_stack$push(list(row = row[[1]], level = level))
            },
            "bracket-close" = {
              cat("-")
              bracket_stack$pop()
            }
          )
        }
        if (identical(head(type[type != "whitespace"], 1), "bracket-close")) {
          start_level <- start_level - 1L
        }
        start_level
      }
    )
  
  tokens %>%
    inner_join(indent_level, by = "row") %>%
    group_by(row) %>%
    summarise(output = paste0(
      paste0(rep_len("  ", indent[[1]]), collapse = ""),
      paste0(value, collapse = "")
    )) %>%
    pull(output)
}

format_tidy_code <- function(code_str) {
  #styler::style_text(strsplit(code_str, "\n")[[1]])
  code_str %>% rebreak() %>% reindent()
}