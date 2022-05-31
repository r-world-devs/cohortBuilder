#' Create data source cohort
#'
#' @name cohortBuilder-package
#' @importFrom magrittr %>%
#' @importFrom dplyr sym

globalVariables(c(
  ":=", "!!", ".data",
  "arrow_end_position_y", "excl_end_position_x", "excl_position_x",
  "excl_position_y", "label", "label_excl",
  "label_heights", "label_position_x", "label_position_y", "level"
))

NULL

`%:::%` <- function(pkg, name) {
  pkg <- as.character(substitute(pkg))
  name <- as.character(substitute(name))
  get(name, envir = asNamespace(pkg), inherits = FALSE)
}
