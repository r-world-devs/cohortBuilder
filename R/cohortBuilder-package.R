#' Create data source cohort
#'
#' @name cohortBuilder-package
#' @importFrom dplyr sym
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

#' Package load hook
#'
#' Registers the package's S7 methods when the namespace is loaded.
#'
#' @param libname,pkgname Standard `.onLoad` arguments.
#' @return Invisibly `NULL`.
#' @noRd
.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}

globalVariables(c(
  ":=", "!!", ".data",
  "arrow_end_position_y", "excl_end_position_x", "excl_position_x",
  "excl_position_y", "label", "label_excl",
  "label_heights", "label_position_x", "label_position_y", "level",
  "dataset", "type", "expr", "new_expr", "expr1", "!<-", "x",
  "action", "step"
))

#' Keep otherwise-unused imports from being flagged
#'
#' References packages used indirectly (e.g. in generated code) so `R CMD check`
#' does not report them as unused imports. Never called.
#'
#' @return Invisibly `NULL`.
#' @noRd
force_import <- function() {
  R6::R6Class
  formatR::tidy_source
  jsonlite::toJSON
}

#' Access an unexported object from another namespace
#'
#' Convenience operator for `getFromNamespace()`: `pkg %:::% name`.
#'
#' @param pkg Package name (unquoted).
#' @param name Object name (unquoted).
#' @return The requested object from `pkg`'s namespace.
#' @noRd
`%:::%` <- function(pkg, name) {
  pkg <- as.character(substitute(pkg))
  name <- as.character(substitute(name))
  get(name, envir = asNamespace(pkg), inherits = FALSE)
}

#' Faster `%in%` backed by `collapse::%iin%`
#'
#' Drop-in replacement for [base::match] `%in%` using `collapse` for speed.
#'
#' @param x Values to look up.
#' @param table Values to match against.
#' @return A logical vector the length of `x`.
#' @noRd
`%in%` <- function(x, table) {
  out <- logical(length(x))
  out[collapse::`%iin%`(x, table)] <- TRUE
  out
}
