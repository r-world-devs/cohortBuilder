adjust_names <- function(list_obj) {
  if (length(list_obj) == 0) {
    # removed the last existing object
    return(list())
  }
  names(list_obj) <- as.character(1:length(list_obj))
  return(list_obj)
}

list_names <- function(list_obj) {
  list_obj %>% purrr::map(names) %>% stats::setNames(names(list_obj))
}

last_item <- function(list_obj) {
  list_length <- length(list_obj)
  if (list_length == 0) {
    return(NULL)
  }
  list_obj[[list_length]]
}

step_filter_state <- function(steps, method = length, raw = FALSE) {
  if (length(steps) == 0) {
    if (raw) return(steps)
    return(method(steps))
  }
  steps %>%
    purrr::map(~method(.x$filters))
}

rename_item <- function(list_obj, old_name, new_name) {
  old_name_mask <- names(list_obj) == old_name
  names(list_obj)[old_name_mask] <- new_name

  return(list_obj)
}

modify_item <- function(list_obj, new_val, what) {
  list_obj[[what]] <- new_val
  return(list_obj)
}

#' Get function definition
#'
#' Whenever the function with provided name exists anywhere, the one is
#' returned (or the first one if multiple found).
#' Return NULL otherwise.
#'
#' @param name Name of the function.
#' @return Function - when found in any namespace or NULL otherwise.
#'
#' @export
.get_method <- function(name) {
  found_methods <- utils::getAnywhere(name)
  if (length(found_methods$objs) == 0) {
    return(NULL)
  }
  namespace <- gsub(
    "namespace:", "", fixed = TRUE,
    grep("namespace:", found_methods$where, value = TRUE, fixed = TRUE)[1]
  )
  utils::getFromNamespace(name, namespace)
}

#' Return list of objects matching provided condition.
#'
#' @param list_obj List of R objects.
#' @param attribute Object attribute name.
#' @param value Object value.
#' @param operator Logical operator - two-argument function taking `list_obj` attribute
#'   value as the first one, and `value` as the second one.
#' @return A subset of list object matching provided condition.
#'
#' @examples
#' my_list <- list(
#'   list(id = 1, name = "a"),
#'   list(id = 2, name = "b")
#' )
#' .get_item(my_list, "id", 1)
#' .get_item(my_list, "name", c("b", "c"), identical)
#'
#' @export
.get_item <- function(list_obj, attribute, value, operator = `==`) {
  purrr::keep(list_obj, ~operator(value, .[[attribute]]))
}

#' Return default value if values are equal
#'
#' @param x Condition to be compared with value.
#' @param value Value to be compared with x.
#' @param default Default value to be returned when `x` is identical to `value`.
#' @return Evaluated condition or provided default value.
#'
#' @export
.if_value <- function(x, value, default) {
  if (identical(x, value)) return(default)
  return(x)
}
