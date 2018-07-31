library(rlang)

expr_pipeline <- function(..., .list = list(...)) {
  exprs <- .list
  if (length(exprs) == 0) {
    return(NULL)
  }

  exprs <- rlang::flatten(exprs)

  exprs <- Filter(Negate(is.null), exprs)
  
  if (length(exprs) == 0) {
    return(NULL)
  }
  
  Reduce(function(memo, expr) {
    expr(!!memo %>% !!expr)
  }, tail(exprs, -1), exprs[[1]])
}
