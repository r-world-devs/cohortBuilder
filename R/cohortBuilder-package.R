#' Create data source cohort
#'
#' @name cohortBuilder-package
#' @importFrom magrittr %>%
#' @importFrom dplyr sym
NULL

globalVariables(c(
  ":=", "!!", ".data",
  "arrow_end_position_y", "excl_end_position_x", "excl_position_x",
  "excl_position_y", "label", "label_excl",
  "label_heights", "label_position_x", "label_position_y", "level",
  "dataset", "type", "expr", "new_expr", "expr1", "!<-", "x"
))

force_import <- function() {
  R6::R6Class
  formatR::tidy_source
  jsonlite::toJSON
}

`%:::%` <- function(pkg, name) {
  pkg <- as.character(substitute(pkg))
  name <- as.character(substitute(name))
  get(name, envir = asNamespace(pkg), inherits = FALSE)
}
