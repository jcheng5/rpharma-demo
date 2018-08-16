library(sourcetools)
library(dplyr)
library(styler)

# str <- "mtcars %>% mutate(cyl = factor(cyl)) %>% filter(mpg >= 19.8 & \n    mpg <= 31.4) %>% filter(cyl %in% c(\"4\", \"6\")) %>% select(mpg, \n    cyl, disp) %>% {\nfoo\nbar\n}\nfoo()\nbar()"

# Neither deparse() nor styler will go out of their way to break on %>%, and
# deparse will break on other random operators instead. This function inserts
# newlines after %>%, and replaces newlines that follow operators or commas with
# a single space. The resulting code string will not contain indentation, and
# must be processed further to be considered readable.
rebreak <- function(str) {
  if (typeof(str) == "language" || typeof(str) == "symbol") {
    str <- deparse(str)
  }
  str <- paste(str, collapse = "\n")
  tokens <- tokenize_string(str)
  tokens$value <- paste0(tokens$value, ifelse(tokens$type == "operator" & tokens$value == "%>%", "\n", ""))
  operator_newline <- grepl("\n", tokens$value) & tokens$type == "whitespace" & c(FALSE, head(tokens$type %in% c("comma", "operator"), -1))
  tokens$value[operator_newline] <- " "
  new_str <- paste(tokens$value, collapse = "")
  gsub("\\s*\\r?\\n\\s*", "\n", new_str)
}

format_tidy_code <- function(code_str) {
  code_str %>% rebreak() %>% style_text() %>% paste(collapse = "\n")
}