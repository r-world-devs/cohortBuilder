#' Create data source cohort
#'
#' @name cohortBuilder-package
#' @importFrom magrittr %>%
#' @importFrom dplyr sym

globalVariables(c(":=", "!!", ".data"))

NULL

`%:::%` <- function(pkg, name) {
  pkg <- as.character(substitute(pkg))
  name <- as.character(substitute(name))
  get(name, envir = asNamespace(pkg), inherits = FALSE)
}
