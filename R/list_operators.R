#' Rename a list's elements to sequential character indices
#'
#' Resets names to `"1"`, `"2"`, ... matching element positions. Used to keep
#' step ids contiguous after a step is removed.
#'
#' @param list_obj A list.
#' @return The list with names set to sequential indices (empty list if input
#'   was empty).
#' @noRd
adjust_names <- function(list_obj) {
  if (length(list_obj) == 0L) {
    # removed the last existing object
    return(list())
  }
  names(list_obj) <- as.character(seq_along(list_obj))
  return(list_obj)
}

#' Get the element names of each item in a list
#'
#' @param list_obj A list of lists.
#' @return A named list mapping each top-level name to the names of its element.
#' @noRd
list_names <- function(list_obj) {
  list_obj |> purrr::map(names) |> stats::setNames(names(list_obj))
}

#' Return the last element of a list
#'
#' @param list_obj A list.
#' @return The last element, or `NULL` when the list is empty.
#' @noRd
last_item <- function(list_obj) {
  list_length <- length(list_obj)
  if (list_length == 0L) {
    return(NULL)
  }
  list_obj[[list_length]]
}

#' Drop `NULL` elements from a list
#'
#' @param x A list.
#' @return The list with `NULL` elements removed.
#' @noRd
drop_nulls <- function(x) {
  purrr::keep(x, ~!is.null(.))
}

#' Summarize filter state across steps
#'
#' Applies `method` to each step's `filters` list, e.g. to count filters per
#' step.
#'
#' @param steps A list of step objects.
#' @param method Function applied to each step's `filters`.
#' @param raw If `TRUE` and there are no steps, return `steps` unchanged instead
#'   of applying `method`.
#' @return A list of per-step results (or `method(steps)` / `steps` when empty).
#' @noRd
step_filter_state <- function(steps, method = length, raw = FALSE) {
  if (length(steps) == 0L) {
    if (raw) return(steps)
    return(method(steps))
  }
  steps |>
    purrr::map(~method(.x$filters))
}

#' Rename a single named element of a list
#'
#' @param list_obj A named list.
#' @param old_name Current element name.
#' @param new_name Replacement name.
#' @return The list with the matching name replaced.
#' @noRd
rename_item <- function(list_obj, old_name, new_name) {
  old_name_mask <- names(list_obj) == old_name
  names(list_obj)[old_name_mask] <- new_name

  return(list_obj)
}

#' Set a named element of a list
#'
#' @param list_obj A list.
#' @param new_val Value to assign.
#' @param what Name of the element to set.
#' @return The modified list.
#' @noRd
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
  if (length(found_methods$objs) == 0L) {
    return(NULL)
  }
  namespace <- gsub(
    "namespace:", "", fixed = TRUE,
    grep("namespace:", found_methods$where, value = TRUE, fixed = TRUE)[1L]
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
