library(rlang)

#' Evaluate an expression in a fresh environment
#'
#' Like eval_tidy, but with different defaults. By default, instead of running
#' in the caller's environment, it runs in a fresh environment.
#' @export
eval_clean <- function(expr, env = list(), enclos = clean_env()) {
  eval_tidy(expr, env, enclos)
}

#' Create a clean environment
#' 
#' Creates a new environment whose parent is the global environment.
#' @export
clean_env <- function() {
  new.env(parent = globalenv())
}

#' Join calls into a pipeline
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

#' Make a report bundle (.pdf and .rmd)
make_report_bundle <- function(template, title, author, description, body_expr,
  packages, files, output_file = tempfile("bundle", fileext = ".zip")) {
  
  bundle_dir <- tempfile("report")
  dir.create(bundle_dir)
  
  setup_chunk <- sapply(packages, function(package) {
    deparse(expr(library(!!sym(package))))
  }, USE.NAMES = FALSE)
  setup_chunk <- paste(setup_chunk, collapse = "\n")
  
  report_source <- knitr::knit_expand(template,
    frontmatter = yaml::as.yaml(list(title = title, author = author)),
    title = title,
    description = description,
    setup = setup_chunk,
    code = format_tidy_code(paste(collapse = "\n", deparse_flatten(body_expr)))
  )
  
  writeLines(report_source, file.path(bundle_dir, "report.Rmd"))
  file.copy(files, bundle_dir)
  rmarkdown::render(file.path(bundle_dir, "report.Rmd"),
    envir = clean_env(),
    output_format = rmarkdown::pdf_document())
  
  old_wd <- getwd()
  setwd(bundle_dir)
  on.exit(setwd(old_wd), add = TRUE)
  
  files <- list.files(all.files = TRUE, no.. = TRUE)
  zip(output_file, files)
}

deparse_flatten <- function(expr, width.cutoff = 500L) {
  if (is.call(expr) && length(expr) > 1 && identical(expr[[1]], quote(`{`))) {
    unlist(lapply(expr[-1], deparse_flatten))
  } else {
    deparse(expr, width.cutoff = width.cutoff)
  }
}
